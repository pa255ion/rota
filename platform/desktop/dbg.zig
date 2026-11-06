//! rota-dbg: the desktop emulator's debugger in a terminal (side track R,
//! R.2; PLAN.md "Side track R"). It talks to `rota --debug` over its
//! socket (debug.zig's commands) and shows the Z80's registers, the
//! disassembly around PC, the breakpoints and memory.
//!
//!   rota-dbg [SOCKET]      (default $XDG_RUNTIME_DIR/rota-debug.sock)
//!
//! Keys: Space pause/continue, s step, o step over a CALL/RST, f run to the
//! frame's end, b breakpoint on the cursor's line, Up/Down the cursor,
//! g go to an address (disassembly), m memory at an address, PgUp/PgDn
//! memory, : a raw command (its answer on the last line), q quit.
//!
//! Cheap on purpose (the user, 2026-11-03: a terminal redrawn constantly
//! is CPU-hungry): only changed rows are rewritten; while the game runs
//! the view refreshes about 10 times a second, while paused only after a
//! key; between them the program sleeps in poll().

const std = @import("std");
const linux = std.os.linux;
const z80 = @import("z80dis.zig");

const rows_min = 24;
const cols_min = 80;

pub fn main(init: std.process.Init) !void {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    var pbuf: [256]u8 = undefined;
    const path = if (args.len > 1) args[1] else blk: {
        const dir = init.environ_map.get("XDG_RUNTIME_DIR") orelse "/tmp";
        break :blk try std.fmt.bufPrint(&pbuf, "{s}/rota-debug.sock", .{dir});
    };
    var c = Client.connect(path) catch {
        std.debug.print("rota-dbg: no debugger at {s} (start the emulator with: rota --debug GAME)\n", .{path});
        std.process.exit(1);
    };
    defer c.close();
    _ = c.reply() catch {}; // the greeting

    var term = Terminal.enter();
    defer term.leave();
    var ui: Ui = .{ .c = &c };
    ui.refresh();
    ui.draw(&term);
    while (!ui.quit) {
        var fds = [_]linux.pollfd{
            .{ .fd = 0, .events = linux.POLL.IN, .revents = 0 },
            .{ .fd = c.fd, .events = linux.POLL.IN, .revents = 0 },
        };
        // running: refresh ten times a second; paused: wait for a key
        const timeout: i32 = if (ui.running) 100 else -1;
        _ = linux.poll(&fds, fds.len, timeout);
        if (fds[1].revents != 0) {
            // an event unasked (a breakpoint hit), or the emulator gone
            const line = c.event() catch {
                ui.quit = true;
                break;
            };
            if (line) |l| ui.onEvent(l);
        }
        if (fds[0].revents != 0) {
            var kb: [32]u8 = undefined;
            const n = linux.read(0, &kb, kb.len);
            if (linux.errno(n) == .SUCCESS and n > 0) ui.key(kb[0..n], &term);
        }
        ui.refresh();
        ui.draw(&term);
    }
}

// --- the socket

const Client = struct {
    fd: i32,
    buf: [8192]u8 = undefined,
    len: usize = 0,
    /// events read while waiting for an answer
    pending: [8][128]u8 = undefined,
    pending_len: [8]u8 = undefined,
    n_pending: usize = 0,

    fn connect(path: []const u8) !Client {
        const rc = linux.socket(linux.AF.UNIX, linux.SOCK.STREAM | linux.SOCK.CLOEXEC, 0);
        if (linux.errno(rc) != .SUCCESS) return error.Socket;
        const fd: i32 = @intCast(rc);
        var addr: linux.sockaddr.un = .{ .family = linux.AF.UNIX, .path = undefined };
        @memset(&addr.path, 0);
        if (path.len >= addr.path.len) return error.PathTooLong;
        @memcpy(addr.path[0..path.len], path);
        if (linux.errno(linux.connect(fd, @ptrCast(&addr), @sizeOf(linux.sockaddr.un))) != .SUCCESS) {
            _ = linux.close(fd);
            return error.Connect;
        }
        return .{ .fd = fd };
    }

    fn close(c: *Client) void {
        _ = linux.close(c.fd);
    }

    /// One line from the socket (blocking), without its newline.
    fn line(c: *Client, out: []u8) ![]const u8 {
        while (true) {
            if (std.mem.indexOfScalar(u8, c.buf[0..c.len], '\n')) |nl| {
                const n = @min(nl, out.len);
                @memcpy(out[0..n], c.buf[0..n]);
                std.mem.copyForwards(u8, c.buf[0 .. c.len - nl - 1], c.buf[nl + 1 .. c.len]);
                c.len -= nl + 1;
                return out[0..n];
            }
            if (c.len == c.buf.len) c.len = 0;
            const rc = linux.read(c.fd, c.buf[c.len..].ptr, c.buf.len - c.len);
            if (linux.errno(rc) != .SUCCESS or rc == 0) return error.Closed;
            c.len += rc;
        }
    }

    var reply_buf: [2048]u8 = undefined;

    /// The answer to the last command ("ok ..." or "error ..."); events
    /// that come first are kept for 'event'.
    fn reply(c: *Client) ![]const u8 {
        while (true) {
            const l = try c.line(&reply_buf);
            if (std.mem.startsWith(u8, l, "stopped")) {
                if (c.n_pending < c.pending.len) {
                    const n = @min(l.len, 128);
                    @memcpy(c.pending[c.n_pending][0..n], l[0..n]);
                    c.pending_len[c.n_pending] = @intCast(n);
                    c.n_pending += 1;
                }
                continue;
            }
            return l;
        }
    }

    fn ask(c: *Client, cmd: []const u8) ![]const u8 {
        try writeAll(c.fd, cmd);
        try writeAll(c.fd, "\n");
        return c.reply();
    }

    /// An event: one kept earlier, or a line now (the socket is readable).
    fn event(c: *Client) !?[]const u8 {
        if (c.n_pending > 0) {
            c.n_pending -= 1;
            @memcpy(reply_buf[0..c.pending_len[c.n_pending]], c.pending[c.n_pending][0..c.pending_len[c.n_pending]]);
            return reply_buf[0..c.pending_len[c.n_pending]];
        }
        const l = try c.line(&reply_buf);
        return if (std.mem.startsWith(u8, l, "stopped")) l else null;
    }
};

fn writeAll(fd: i32, data: []const u8) !void {
    var off: usize = 0;
    while (off < data.len) {
        const rc = linux.write(fd, data[off..].ptr, data.len - off);
        if (linux.errno(rc) != .SUCCESS) return error.Closed;
        off += rc;
    }
}

// --- the terminal

const Terminal = struct {
    saved: ?linux.termios = null,
    rows: usize = rows_min,
    cols: usize = cols_min,
    /// what the screen shows now (for redrawing only what changed)
    shown: [64][200]u8 = undefined,
    shown_ok: [64]bool = [_]bool{false} ** 64,

    fn enter() Terminal {
        var t: Terminal = .{};
        var tio: linux.termios = undefined;
        if (linux.errno(linux.tcgetattr(0, &tio)) == .SUCCESS) {
            var r = tio;
            r.lflag.ICANON = false;
            r.lflag.ECHO = false;
            r.lflag.ISIG = true;
            r.cc[@intFromEnum(linux.V.MIN)] = 1;
            r.cc[@intFromEnum(linux.V.TIME)] = 0;
            _ = linux.tcsetattr(0, .NOW, &r);
            t.saved = tio;
        }
        var ws: std.posix.winsize = undefined;
        if (linux.errno(linux.ioctl(1, linux.T.IOCGWINSZ, @intFromPtr(&ws))) == .SUCCESS) {
            t.rows = @min(@max(ws.row, rows_min), 64);
            t.cols = @min(@max(ws.col, cols_min), 200);
        }
        // the alternate screen, the cursor hidden
        emit("\x1b[?1049h\x1b[?25l\x1b[2J");
        return t;
    }

    fn leave(t: *Terminal) void {
        emit("\x1b[?25h\x1b[?1049l");
        if (t.saved) |s| _ = linux.tcsetattr(0, .NOW, &s);
    }

    /// Row `y` as `text` (padded to the width), written only if it changed.
    fn row(t: *Terminal, y: usize, text: []const u8, invert: bool) void {
        if (y >= t.rows) return;
        var line: [200]u8 = undefined;
        @memset(line[0..t.cols], ' ');
        const n = @min(text.len, t.cols);
        @memcpy(line[0..n], text[0..n]);
        // (an inverted row's mark: its first cell's high bit, so a change
        // of highlight counts as a change)
        if (invert) line[0] |= 0x80;
        if (t.shown_ok[y] and std.mem.eql(u8, t.shown[y][0..t.cols], line[0..t.cols])) return;
        t.shown[y] = line;
        t.shown_ok[y] = true;
        line[0] &= 0x7F;
        var buf: [256]u8 = undefined;
        emit(std.fmt.bufPrint(&buf, "\x1b[{d};1H{s}", .{ y + 1, if (invert) "\x1b[7m" else "\x1b[0m" }) catch return);
        emit(line[0..t.cols]);
        emit("\x1b[0m");
    }
};

fn emit(s: []const u8) void {
    writeAll(1, s) catch {};
}

// --- the view

const Ui = struct {
    c: *Client,
    quit: bool = false,
    running: bool = true,
    regs_line: [256]u8 = undefined,
    regs_len: usize = 0,
    regs: [22]u16 = [_]u16{0} ** 22,
    /// the disassembly's first address and the cursor's line in it
    dis_at: u16 = 0,
    dis_follow: bool = true,
    cursor: usize = 0,
    dis_addrs: [32]u16 = undefined,
    n_dis: usize = 0,
    mem_at: u16 = 0xC000,
    breaks: [64]u16 = undefined,
    n_breaks: usize = 0,
    msg: [200]u8 = undefined,
    msg_len: usize = 0,
    mem: [128]u8 = undefined,
    code: [128]u8 = undefined,
    frames: u64 = 0,

    fn say(u: *Ui, comptime fmt: []const u8, args: anytype) void {
        const t = std.fmt.bufPrint(&u.msg, fmt, args) catch return;
        u.msg_len = t.len;
    }

    /// Ask the emulator for what the view shows.
    fn refresh(u: *Ui) void {
        if (u.c.ask("status")) |st| {
            // ok paused|running PC frames N
            var it = std.mem.tokenizeScalar(u8, st, ' ');
            _ = it.next();
            u.running = std.mem.eql(u8, it.next() orelse "", "running");
            _ = it.next();
            _ = it.next();
            u.frames = std.fmt.parseInt(u64, it.next() orelse "0", 10) catch 0;
        } else |_| {
            u.quit = true;
            return;
        }
        if (u.c.ask("regs")) |r| {
            const n = @min(r.len, u.regs_line.len);
            @memcpy(u.regs_line[0..n], r[0..n]);
            u.regs_len = n;
            var it = std.mem.tokenizeScalar(u8, r, ' ');
            var i: usize = 0;
            _ = it.next();
            while (it.next()) |kv| : (i += 1) {
                if (i >= 22) break;
                const eq = std.mem.indexOfScalar(u8, kv, '=') orelse continue;
                u.regs[i] = std.fmt.parseInt(u16, kv[eq + 1 ..], if (i < 18) 16 else 10) catch 0;
            }
        } else |_| {}
        const pc = u.regs[15];
        if (u.dis_follow) {
            // PC on the view's fourth line or so: start a little before it
            // only when PC left the window
            const inside = for (u.dis_addrs[0..u.n_dis], 0..) |a, k| {
                if (a == pc and k + 4 < u.n_dis) break true;
            } else false;
            if (!inside) u.dis_at = pc;
        }
        fetch(u.c, u.dis_at, &u.code);
        fetch(u.c, u.mem_at, &u.mem);
        if (u.c.ask("breaks")) |b| {
            var it = std.mem.tokenizeScalar(u8, b, ' ');
            _ = it.next();
            u.n_breaks = 0;
            while (it.next()) |t| {
                if (u.n_breaks == u.breaks.len) break;
                u.breaks[u.n_breaks] = std.fmt.parseInt(u16, t, 16) catch continue;
                u.n_breaks += 1;
            }
        } else |_| {}
    }

    fn onEvent(u: *Ui, l: []const u8) void {
        u.say("{s}", .{l});
        u.running = false;
        u.dis_follow = true;
        u.cursor = 0;
    }

    fn isBreak(u: *const Ui, a: u16) bool {
        return std.mem.indexOfScalar(u16, u.breaks[0..u.n_breaks], a) != null;
    }

    fn draw(u: *Ui, t: *Terminal) void {
        var b: [256]u8 = undefined;
        t.row(0, std.fmt.bufPrint(&b, " rota-dbg  {s}  PC {X:0>4}  frames {d}   Space pause/continue  s step  o over  f frame  b break  g goto  m mem  : cmd  q quit", .{ if (u.running) "RUNNING" else "PAUSED ", u.regs[15], u.frames }) catch "", true);
        // registers
        const r = u.regs;
        t.row(1, std.fmt.bufPrint(&b, " A {X:0>2}  F {X:0>2} [{s}]   BC {X:0>2}{X:0>2}  DE {X:0>2}{X:0>2}  HL {X:0>2}{X:0>2}   IX {X:0>4}  IY {X:0>4}  SP {X:0>4}  PC {X:0>4}", .{ r[0], r[1], flags(@intCast(r[1])), r[2], r[3], r[4], r[5], r[6], r[7], r[12], r[13], r[14], r[15] }) catch "", false);
        t.row(2, std.fmt.bufPrint(&b, " AF' {X:0>4}  BC' {X:0>4}  DE' {X:0>4}  HL' {X:0>4}   I {X:0>2}  R {X:0>2}  IFF1 {d}  IFF2 {d}  IM {d}{s}", .{ r[8], r[9], r[10], r[11], r[16], r[17], r[18], r[19], r[20], if (r[21] != 0) "  HALTED" else "" }) catch "", false);
        t.row(3, "", false);
        // the disassembly, and the breakpoints beside it
        // (the rest of the screen: header, 2 register rows, a gap, then
        // memory's 9 rows and the message line)
        const dis_rows = t.rows - 14;
        u.n_dis = 0;
        var off: usize = 0;
        var y: usize = 4;
        while (y < 4 + dis_rows) : (y += 1) {
            const a = u.dis_at +% @as(u16, @intCast(off));
            var line: [200]u8 = undefined;
            var w: std.Io.Writer = .fixed(&line);
            if (off + 4 <= u.code.len) {
                const i = z80.decode(u.code[off..], a);
                if (u.n_dis < u.dis_addrs.len) {
                    u.dis_addrs[u.n_dis] = a;
                    u.n_dis += 1;
                }
                w.print(" {s}{s}{X:0>4}  ", .{ if (u.isBreak(a)) "*" else " ", if (a == r[15]) ">" else " ", a }) catch {};
                for (0..4) |k| {
                    if (k < i.len) w.print("{X:0>2} ", .{u.code[off + k]}) catch {} else w.writeAll("   ") catch {};
                }
                w.print(" {s}", .{i.str()}) catch {};
                off += i.len;
            }
            // the breakpoints' column, from x = 50
            const k = y - 4;
            const pad = @max(@as(isize, 50) - @as(isize, @intCast(w.buffered().len)), 0);
            for (0..@intCast(pad)) |_| w.writeAll(" ") catch {};
            if (k == 0) w.writeAll("breakpoints") catch {} else if (k - 1 < u.n_breaks) w.print("  {X:0>4}", .{u.breaks[k - 1]}) catch {};
            t.row(y, w.buffered(), k == u.cursor and !u.running);
        }
        // memory
        t.row(y, std.fmt.bufPrint(&b, " memory at {X:0>4} (m: another address, PgUp/PgDn)", .{u.mem_at}) catch "", false);
        y += 1;
        for (0..8) |k| {
            var line: [200]u8 = undefined;
            var w: std.Io.Writer = .fixed(&line);
            const a = u.mem_at +% @as(u16, @intCast(k * 16));
            w.print(" {X:0>4} ", .{a}) catch {};
            for (0..16) |j| w.print(" {X:0>2}", .{u.mem[k * 16 + j]}) catch {};
            w.writeAll("  ") catch {};
            for (0..16) |j| {
                const ch = u.mem[k * 16 + j];
                w.writeByte(if (ch >= 0x20 and ch < 0x7F) ch else '.') catch {};
            }
            t.row(y, w.buffered(), false);
            y += 1;
        }
        t.row(t.rows - 1, u.msg[0..u.msg_len], false);
    }

    fn key(u: *Ui, k: []const u8, t: *Terminal) void {
        u.msg_len = 0;
        if (eqs(k, "q")) {
            u.quit = true;
        } else if (eqs(k, " ")) {
            _ = u.c.ask(if (u.running) "pause" else "continue") catch {};
            u.dis_follow = true;
        } else if (eqs(k, "s")) {
            _ = u.c.ask("step") catch {};
            u.dis_follow = true;
        } else if (eqs(k, "o")) {
            u.stepOver();
        } else if (eqs(k, "f")) {
            _ = u.c.ask("frame") catch {};
        } else if (eqs(k, "b")) {
            if (u.cursor < u.n_dis) {
                const a = u.dis_addrs[u.cursor];
                var cb: [32]u8 = undefined;
                _ = u.c.ask(std.fmt.bufPrint(&cb, "{s} {X:0>4}", .{ if (u.isBreak(a)) "delete" else "break", a }) catch return) catch {};
            }
        } else if (eqs(k, "\x1b[A")) {
            if (u.cursor > 0) u.cursor -= 1 else if (u.dis_at > 0) {
                u.dis_at -%= 1; // (no way back by whole instructions: a byte)
                u.dis_follow = false;
            }
        } else if (eqs(k, "\x1b[B")) {
            if (u.cursor + 1 < u.n_dis) u.cursor += 1;
        } else if (eqs(k, "\x1b[5~")) {
            u.mem_at -%= 128;
        } else if (eqs(k, "\x1b[6~")) {
            u.mem_at +%= 128;
        } else if (eqs(k, "g") or eqs(k, "m")) {
            const a = prompt(t, if (eqs(k, "g")) "disassemble at (hex): " else "memory at (hex): ") orelse return;
            const v = std.fmt.parseInt(u16, std.mem.trimStart(u8, a, "$"), 16) catch return u.say("not a hex address: {s}", .{a});
            if (eqs(k, "g")) {
                u.dis_at = v;
                u.dis_follow = false;
                u.cursor = 0;
            } else u.mem_at = v;
        } else if (eqs(k, ":")) {
            const cmd = prompt(t, ": ") orelse return;
            const ans = u.c.ask(cmd) catch return;
            u.say("{s}", .{ans});
        }
    }

    /// A CALL or RST at PC: run to the instruction after it (a temporary
    /// breakpoint); anything else: a step.
    fn stepOver(u: *Ui) void {
        const pc = u.regs[15];
        var code: [4]u8 = undefined;
        fetch(u.c, pc, &code);
        const i = z80.decode(&code, pc);
        const s = i.str();
        if (!(std.mem.startsWith(u8, s, "CALL") or std.mem.startsWith(u8, s, "RST"))) {
            _ = u.c.ask("step") catch {};
            return;
        }
        const next = pc +% i.len;
        const had = u.isBreak(next);
        var cb: [32]u8 = undefined;
        if (!had) _ = u.c.ask(std.fmt.bufPrint(&cb, "break {X:0>4}", .{next}) catch return) catch {};
        _ = u.c.ask("continue") catch {};
        // wait for the stop (the call returns), then the temporary one goes
        while (true) {
            const ev = u.c.event() catch return;
            if (ev) |l| {
                u.onEvent(l);
                break;
            }
        }
        if (!had) _ = u.c.ask(std.fmt.bufPrint(&cb, "delete {X:0>4}", .{next}) catch return) catch {};
    }
};

/// 128 bytes from `a` into `out` ("peek A 128"), zeros where the answer
/// lacks them.
fn fetch(c: *Client, a: u16, outb: []u8) void {
    @memset(outb, 0);
    var cb: [32]u8 = undefined;
    const ans = c.ask(std.fmt.bufPrint(&cb, "peek {X:0>4} {d}", .{ a, outb.len }) catch return) catch return;
    var it = std.mem.tokenizeScalar(u8, ans, ' ');
    _ = it.next();
    _ = it.next();
    var i: usize = 0;
    while (it.next()) |t| : (i += 1) {
        if (i >= outb.len) break;
        outb[i] = std.fmt.parseInt(u8, t, 16) catch 0;
    }
}

/// A line typed on the last row (Enter ends it, Esc cancels).
fn prompt(t: *Terminal, label: []const u8) ?[]const u8 {
    const S = struct {
        var buf: [64]u8 = undefined;
    };
    var n: usize = 0;
    while (true) {
        var b: [128]u8 = undefined;
        t.row(t.rows - 1, std.fmt.bufPrint(&b, "{s}{s}_", .{ label, S.buf[0..n] }) catch "", false);
        var kb: [8]u8 = undefined;
        const rc = linux.read(0, &kb, kb.len);
        if (linux.errno(rc) != .SUCCESS or rc == 0) return null;
        const ch = kb[0];
        if (ch == '\r' or ch == '\n') return S.buf[0..n];
        if (ch == 0x1b) return null;
        if ((ch == 0x7F or ch == 8) and n > 0) {
            n -= 1;
        } else if (ch >= 0x20 and ch < 0x7F and n < S.buf.len) {
            S.buf[n] = ch;
            n += 1;
        }
    }
}

fn eqs(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

/// F as its flags' letters (S Z - H - P/V N C), a dash where clear.
fn flags(f: u8) [8]u8 {
    const names = "SZ5H3PNC";
    var o: [8]u8 = undefined;
    for (0..8) |i| o[i] = if (f & (@as(u8, 0x80) >> @intCast(i)) != 0) names[i] else '-';
    return o;
}

test "flags" {
    try std.testing.expectEqualStrings("-Z-----C", &flags(0x41));
}
