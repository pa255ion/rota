//! The desktop emulator's debug socket (side track R, R.1; PLAN.md "Side
//! track R"): `rota --debug` listens on a Unix socket, and a client (the
//! terminal UI `rota-dbg`, R.2, or anything that writes lines) drives the
//! machine with the commands below, one line each, one answer line each:
//! "ok ..." or "error ...". When a breakpoint stops the machine while it
//! runs, the server sends "stopped break PC" unasked.
//!
//!   regs                 ok A=12 F=34 ... PC=0123 ... (sms_spec.reg_names)
//!   peek ADDR [N]        ok ADDR: XX XX ... (the CPU's map; N up to 256)
//!   poke ADDR XX [XX..]  ok (writes as the CPU's: RAM, cartridge RAM, mapper)
//!   vram ADDR [N]        ok ADDR: XX ...;  vdp: ok R0=.. R10=..
//!   step [N]             ok PC (pauses first; N instructions, up to 100000)
//!   pause                ok PC;  continue: ok (runs on, breakpoints armed)
//!   frame                ok PC (pauses at the next frame's end)
//!   break ADDR           ok (up to 64);  delete ADDR, delete all;  breaks: ok A B ...
//!   status               ok paused|running PC frames N
//!
//! Addresses and bytes are hex, with or without $ or 0x. The Game Gear and
//! the Master System first (the user, 2026-11-03); other systems answer
//! "error no debugger for this system yet". The fast version only, as save
//! states (not with --reference). One client at a time; the socket is
//! serviced once a frame loop pass (non-blocking), so it costs the
//! emulator nothing while nobody is connected.

const std = @import("std");
const linux = std.os.linux;
const sms = @import("sms_spec.zig");

pub const max_breaks = 64;

pub const Server = struct {
    listen_fd: i32,
    client: ?i32 = null,
    in_buf: [4096]u8 = undefined,
    in_len: usize = 0,
    /// the debugger holds the machine (the frame loop runs no frames)
    paused: bool = false,
    /// "frame": pause at the next frame's end
    pause_at_frame: bool = false,
    breaks: [max_breaks]u16 = undefined,
    n_breaks: usize = 0,
    path: [108]u8 = [_]u8{0} ** 108,

    pub const Error = error{ PathTooLong, SocketFailed };

    /// Listen on `path` (an old socket file there removed first).
    pub fn open(path: []const u8) Error!Server {
        if (path.len >= 108) return error.PathTooLong;
        var s: Server = .{ .listen_fd = -1 };
        @memcpy(s.path[0..path.len], path);
        const fd = check(linux.socket(linux.AF.UNIX, linux.SOCK.STREAM | linux.SOCK.NONBLOCK | linux.SOCK.CLOEXEC, 0)) catch return error.SocketFailed;
        s.listen_fd = @intCast(fd);
        var addr: linux.sockaddr.un = .{ .family = linux.AF.UNIX, .path = undefined };
        @memset(&addr.path, 0);
        @memcpy(addr.path[0..path.len], path);
        _ = linux.unlink(@ptrCast(&s.path));
        _ = check(linux.bind(s.listen_fd, @ptrCast(&addr), @sizeOf(linux.sockaddr.un))) catch return error.SocketFailed;
        _ = check(linux.listen(s.listen_fd, 1)) catch return error.SocketFailed;
        return s;
    }

    pub fn close(s: *Server) void {
        if (s.client) |c| _ = linux.close(c);
        _ = linux.close(s.listen_fd);
        _ = linux.unlink(@ptrCast(&s.path));
    }

    /// Accept a client if none, read what it sent, answer each complete
    /// line (`m`: the machine, null for a system without a debugger).
    pub fn service(s: *Server, m: ?*sms.Machine, frames: u64) void {
        if (s.client == null) {
            const rc = linux.accept4(s.listen_fd, null, null, linux.SOCK.NONBLOCK | linux.SOCK.CLOEXEC);
            if (check(rc)) |fd| {
                s.client = @intCast(fd);
                s.in_len = 0;
                s.send("ok rota debugger (side track R); commands: regs peek poke vram vdp step pause continue frame break delete breaks status");
            } else |_| return;
        }
        const c = s.client.?;
        while (true) {
            if (s.in_len == s.in_buf.len) s.in_len = 0; // a line too long: dropped
            const rc = linux.read(c, s.in_buf[s.in_len..].ptr, s.in_buf.len - s.in_len);
            const n = check(rc) catch |e| {
                if (e != error.Again) s.drop();
                return;
            };
            if (n == 0) return s.drop(); // the client left
            s.in_len += n;
            while (std.mem.indexOfScalar(u8, s.in_buf[0..s.in_len], '\n')) |nl| {
                var line_buf: [512]u8 = undefined;
                const len = @min(nl, line_buf.len);
                @memcpy(line_buf[0..len], s.in_buf[0..len]);
                std.mem.copyForwards(u8, s.in_buf[0 .. s.in_len - nl - 1], s.in_buf[nl + 1 .. s.in_len]);
                s.in_len -= nl + 1;
                s.command(m, std.mem.trim(u8, line_buf[0..len], " \t\r"), frames);
                if (s.client == null) return;
            }
        }
    }

    fn drop(s: *Server) void {
        if (s.client) |c| _ = linux.close(c);
        s.client = null;
        // nobody to resume it: the machine runs on (breakpoints kept)
        s.paused = false;
    }

    /// A line to the client (dropped if none).
    pub fn send(s: *Server, line: []const u8) void {
        const c = s.client orelse return;
        writeAll(c, line) catch return s.drop();
        writeAll(c, "\n") catch return s.drop();
    }

    fn sendf(s: *Server, comptime fmt: []const u8, args: anytype) void {
        var buf: [2048]u8 = undefined;
        s.send(std.fmt.bufPrint(&buf, fmt, args) catch return s.send("error answer too long"));
    }

    /// The frame loop's report of a breakpoint hit (or a "frame" pause).
    pub fn stopped(s: *Server, m: *sms.Machine, why: []const u8) void {
        s.paused = true;
        const pc = if (m.regs()) |r| r[15] else |_| 0;
        s.sendf("stopped {s} {X:0>4}", .{ why, pc });
    }

    fn command(s: *Server, m_opt: ?*sms.Machine, line: []const u8, frames: u64) void {
        var it = std.mem.tokenizeAny(u8, line, " \t");
        const cmd = it.next() orelse return;
        const m = m_opt orelse return s.send("error no debugger for this system yet (the Game Gear and Master System first)");
        if (eq(cmd, "regs")) {
            const r = m.regs() catch return s.send("error no debugger with --reference");
            var buf: [512]u8 = undefined;
            var w: std.Io.Writer = .fixed(&buf);
            w.writeAll("ok") catch {};
            for (sms.reg_names, r, 0..) |name, v, i| {
                // the 8-bit registers as two digits, the pairs as four
                if (i < 8 or (i >= 16 and i < 18)) w.print(" {s}={X:0>2}", .{ name, v }) catch {} else if (i < 16) w.print(" {s}={X:0>4}", .{ name, v }) catch {} else w.print(" {s}={d}", .{ name, v }) catch {};
            }
            s.send(w.buffered());
        } else if (eq(cmd, "peek") or eq(cmd, "vram")) {
            const a = hex16(it.next()) orelse return s.send("error usage: peek ADDR [N]");
            const n: usize = if (it.next()) |t| (std.fmt.parseInt(usize, t, 10) catch return s.send("error N: a decimal count")) else 1;
            if (n == 0 or n > 256) return s.send("error N: 1 to 256");
            var buf: [16 + 3 * 256]u8 = undefined;
            var w: std.Io.Writer = .fixed(&buf);
            w.print("ok {X:0>4}:", .{a}) catch {};
            for (0..n) |k| {
                const addr: u16 = a +% @as(u16, @intCast(k));
                const v = (if (eq(cmd, "peek")) m.peek(addr) else m.vram(addr & 0x3FFF)) catch return s.send("error no debugger with --reference");
                w.print(" {X:0>2}", .{v}) catch {};
            }
            s.send(w.buffered());
        } else if (eq(cmd, "poke")) {
            var a = hex16(it.next()) orelse return s.send("error usage: poke ADDR XX [XX ...]");
            var any = false;
            while (it.next()) |t| {
                const v = std.fmt.parseInt(u8, stripHex(t), 16) catch return s.send("error a byte: two hex digits");
                m.poke(a, v) catch return s.send("error no debugger with --reference");
                a +%= 1;
                any = true;
            }
            if (!any) return s.send("error usage: poke ADDR XX [XX ...]");
            s.send("ok");
        } else if (eq(cmd, "vdp")) {
            var buf: [256]u8 = undefined;
            var w: std.Io.Writer = .fixed(&buf);
            w.writeAll("ok") catch {};
            for (0..11) |i| {
                const v = m.vdpReg(@intCast(i)) catch return s.send("error no debugger with --reference");
                w.print(" R{d}={X:0>2}", .{ i, v }) catch {};
            }
            s.send(w.buffered());
        } else if (eq(cmd, "step")) {
            const n: usize = if (it.next()) |t| (std.fmt.parseInt(usize, t, 10) catch return s.send("error N: a decimal count")) else 1;
            if (n == 0 or n > 100000) return s.send("error N: 1 to 100000");
            s.paused = true;
            for (0..n) |_| _ = m.step() catch return s.send("error no debugger with --reference");
            s.sendPc(m);
        } else if (eq(cmd, "pause")) {
            s.paused = true;
            s.sendPc(m);
        } else if (eq(cmd, "continue")) {
            s.paused = false;
            s.send("ok");
        } else if (eq(cmd, "frame")) {
            s.paused = false;
            s.pause_at_frame = true;
            s.send("ok");
        } else if (eq(cmd, "break")) {
            const a = hex16(it.next()) orelse return s.send("error usage: break ADDR");
            if (std.mem.indexOfScalar(u16, s.breaks[0..s.n_breaks], a) == null) {
                if (s.n_breaks == max_breaks) return s.send("error 64 breakpoints at most");
                s.breaks[s.n_breaks] = a;
                s.n_breaks += 1;
            }
            s.send("ok");
        } else if (eq(cmd, "delete")) {
            const t = it.next() orelse return s.send("error usage: delete ADDR | delete all");
            if (eq(t, "all")) {
                s.n_breaks = 0;
            } else {
                const a = hex16(t) orelse return s.send("error usage: delete ADDR | delete all");
                if (std.mem.indexOfScalar(u16, s.breaks[0..s.n_breaks], a)) |i| {
                    s.breaks[i] = s.breaks[s.n_breaks - 1];
                    s.n_breaks -= 1;
                }
            }
            s.send("ok");
        } else if (eq(cmd, "breaks")) {
            var buf: [8 + 5 * max_breaks]u8 = undefined;
            var w: std.Io.Writer = .fixed(&buf);
            w.writeAll("ok") catch {};
            for (s.breaks[0..s.n_breaks]) |b| w.print(" {X:0>4}", .{b}) catch {};
            s.send(w.buffered());
        } else if (eq(cmd, "status")) {
            const pc = if (m.regs()) |r| r[15] else |_| 0;
            s.sendf("ok {s} {X:0>4} frames {d}", .{ if (s.paused) "paused" else "running", pc, frames });
        } else {
            s.sendf("error unknown command: {s}", .{cmd});
        }
    }

    fn sendPc(s: *Server, m: *sms.Machine) void {
        const r = m.regs() catch return s.send("error no debugger with --reference");
        s.sendf("ok {X:0>4}", .{r[15]});
    }

    /// The breakpoints, for the frame loop's run.
    pub fn breakList(s: *const Server) []const u16 {
        return s.breaks[0..s.n_breaks];
    }
};

/// The default socket: $XDG_RUNTIME_DIR/rota-debug.sock, else /tmp's.
pub fn defaultPath(buf: []u8, env: *const std.process.Environ.Map) []const u8 {
    const dir = env.get("XDG_RUNTIME_DIR") orelse "/tmp";
    return std.fmt.bufPrint(buf, "{s}/rota-debug.sock", .{dir}) catch "/tmp/rota-debug.sock";
}

fn eq(a: []const u8, b: []const u8) bool {
    return std.ascii.eqlIgnoreCase(a, b);
}

fn stripHex(t: []const u8) []const u8 {
    if (std.mem.startsWith(u8, t, "$")) return t[1..];
    if (std.ascii.startsWithIgnoreCase(t, "0x")) return t[2..];
    return t;
}

fn hex16(t_opt: ?[]const u8) ?u16 {
    const t = t_opt orelse return null;
    return std.fmt.parseInt(u16, stripHex(t), 16) catch null;
}

const SysError = error{ Again, Failed };

fn check(rc: usize) SysError!usize {
    return switch (linux.errno(rc)) {
        .SUCCESS => rc,
        .AGAIN => error.Again,
        else => error.Failed,
    };
}

fn writeAll(fd: i32, data: []const u8) SysError!void {
    var off: usize = 0;
    while (off < data.len) {
        const n = check(linux.write(fd, data[off..].ptr, data.len - off)) catch |e| {
            if (e == error.Again) continue;
            return e;
        };
        off += n;
    }
}

test "hex16: plain, $ and 0x" {
    try std.testing.expectEqual(@as(?u16, 0x1234), hex16("1234"));
    try std.testing.expectEqual(@as(?u16, 0xC000), hex16("$C000"));
    try std.testing.expectEqual(@as(?u16, 0xff), hex16("0xff"));
    try std.testing.expectEqual(@as(?u16, null), hex16("xyz"));
}
