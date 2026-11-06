//! Desktop side of the board link (`make keys`, `make load`): keyboards and
//! gamepads on this PC -> CHIP-8 keys on the ZedBoard, and ROM loading,
//! through the USB-UART (J14) and the PS relay (platform/ps/relay.zig,
//! protocol in platform/ps/protocol.zig).
//!
//!   link [--port DEV] [--system chip8|gb|nes|a800xl|gg|sms] [--keymap FILE] [--bios FILE]
//!        [--load ROM [--preset ADDR=VAL]... [--no-input]]
//!        [--slow] [--no-grab] [--ping N] [--verbose] [--baud-cache FILE]
//!        [--sd-image FILE] [--device NAME]
//!        [--ddr-test suite|SPEC] [--ddr-load FILE] [--saves-pull DIR [--states-dir DIR] [--movies-dir DIR]]
//!   link --save-slots DIR [--system S]            (tools/mksd.sh; saves.zig)
//!   link --state-slots DIR [--system S]           (the same for save states, side track L)
//!   link --movie-slots DIR [--system S] [--movie-frames N]  (input movies' slots, side track O)
//!   link [--port DEV] --movies-pull DIR             (the card's /movies, make movie-pull)
//!   link --stamp-clocks DIR                       (clock saves below DIR get the time: make sd-sync)
//!   link [--port DEV] --screenshot DIR            (one taken on the board, copied into DIR: make screenshot)
//!   link [--port DEV] --movie-put FILE --movie-slot PATH  (a movie into a card slot, make movie-play)
//!   link --remap [--system S] [--keymap FILE]     (make keymap; remap.zig)
//!   link --settings [--system S] [--keymap FILE]  (make settings; settings.zig)
//!   link --show-keymap [--system S] [--keymap FILE]
//!
//! --system picks the key map and the load path (--gb = --system gb);
//! CHIP-8 by default. The key maps are data (keymap.zig): defaults,
//! overridden by --keymap FILE or else
//! $XDG_CONFIG_HOME/emulation_clash/keymap.conf when it exists; a bad file
//! stops link with its line number. --remap asks for each button of the
//! system and writes the file; --show-keymap prints the map in force.
//! Neither needs the board. --device NAME uses only the input devices
//! whose name contains NAME (tools/test_link.py: its virtual pad alone).
//!
//! Low latency by construction: input comes straight from the kernel
//! (evdev, /dev/input/event*; key mapping in keymap.zig), one poll() loop
//! waits on every device and the serial port, and each change is written
//! at once as a 3-byte binary key frame (protocol.zig). The fastest rate
//! that works is found by trying 2000000 (the Arty's FTDI bridge; side
//! track G phase 2), 921600, 460800 and 230400 (falling back to 115200),
//! and remembered per port in build/host/link-baud-<port> so later runs
//! start there (one board's bridge says nothing about the other's). The
//! state is also
//! resent every 100 ms, and the relay releases everything if that stops.
//! Gamepads are grabbed (exclusive) unless --no-grab; keyboards aren't, so
//! the keys keep working everywhere. All devices are opened once at start;
//! devices plugged in later are opened when inotify reports their node
//! (opening an input device can take 10 to 35 ms, so the loop never polls
//! for new ones). --ping N measures N round trips to the PS and exits;
//! --verbose prints each key change with the time from the kernel event to
//! the write. --sd-image FILE writes a card image (tools/mksd.sh) to the
//! board's SD card through the PS (sdimage.zig; `make sd`), then reopens
//! the board's ROM menu, and exits. --saves-pull DIR copies the card's
//! `/saves` folder (battery saves, side track I) into DIR through the
//! relay's block reads (R; saves.zig), and exits: `make saves-pull`, and
//! `make sd` before it erases the card. --save-slots DIR needs no board:
//! it prints the ROMs below DIR that get a save file, and their sizes
//! (the PS app's system table decides), for tools/mksd.sh.
//!
//! On M0e's bitstream (side track C): --ddr-test suite (or a run, 17 hex
//! digits: base, mode, words, flags) runs the DDR test and prints the
//! board's report (`make ddr-test`); --ddr-load FILE writes FILE into DDR
//! at 0x01000000 and has the PL read it back (`make ddr-load`).

const std = @import("std");
const linux = std.os.linux;
const sys = @import("sys.zig");
const evdev = @import("evdev.zig");
const keymap = @import("keymap");
const protocol = @import("protocol"); // platform/ps/protocol.zig, shared with the relay
const Serial = @import("serial.zig").Serial;
const sdimage = @import("sdimage.zig");
const remap = @import("remap.zig");
const settings = @import("settings.zig");
const saves = @import("saves.zig");
const fat = @import("fat"); // platform/ps/fat.zig

const max_devices = 32;
const max_event_index = 64;

const Options = struct {
    port: [:0]const u8 = "/dev/ttyACM0",
    load: ?[]const u8 = null,
    presets: [16]Preset = undefined,
    n_presets: usize = 0,
    input: bool = true,
    fast: bool = true,
    baud_cache: [:0]const u8 = default_baud_cache,
    grab: bool = true,
    ping: u32 = 0,
    verbose: bool = false,
    sd_image: ?[]const u8 = null,
    /// --sd-rewrite: write every chunk, without asking the card what it has
    sd_rewrite: bool = false,
    system: keymap.System = .chip8,
    /// the system's folder on the card, if not the key map's name (--system
    /// gbc: the Game Boy's keys and cartridge loading, /gbc's save slots)
    card_dir: ?[]const u8 = null,
    keymap: ?[:0]const u8 = null,
    /// (M6) a companion file for the cartridge: the Master System's BIOS
    bios: ?[:0]const u8 = null,
    remap: bool = false,
    show_keymap: bool = false,
    /// the settings screen (settings.zig)
    settings: bool = false,
    device: ?[]const u8 = null,
    ddr_test: ?[]const u8 = null,
    ddr_load: ?[]const u8 = null,
    saves_pull: ?[]const u8 = null,
    save_slots: ?[]const u8 = null,
    /// with --saves-pull: the card's /states (save-state slots, side
    /// track L) into this folder too
    states_dir: ?[]const u8 = null,
    /// side track L: the save-state slots' sizes, as save_slots
    state_slots: ?[]const u8 = null,
    /// side track O: the movie slots' sizes, as save_slots, for
    /// `movie_frames` frames; with --saves-pull, the card's /movies into
    /// movies_dir too
    movie_slots: ?[]const u8 = null,
    movie_frames: ?u32 = null,
    movies_dir: ?[]const u8 = null,
    /// side track O, O.2c: the card's /movies alone; a movie file into the
    /// card's slot at movie_slot
    movies_pull: ?[]const u8 = null,
    /// side track O, O.4: a screenshot taken on the board and copied into
    /// this folder; with --saves-pull, the card's /screens into screens_dir
    screenshot: ?[]const u8 = null,
    screens_dir: ?[]const u8 = null,
    /// make sd-sync: the clock saves below this folder stamped (no board)
    stamp_clocks: ?[]const u8 = null,
    movie_put: ?[]const u8 = null,
    movie_slot: ?[]const u8 = null,
    /// side track K, the Controls page: with --saves-pull, the card's
    /// /keymap.conf into the desktop's file if the board wrote it
    /// (remap.takeBoard; make sd); --keymap-take FILE: the same from a
    /// mounted card's file (make sd-sync, no board)
    keymap_pull: bool = false,
    keymap_take: ?[]const u8 = null,
};

const Preset = struct { addr: u12, value: u8 };

fn usage() noreturn {
    std.debug.print(
        \\usage: link [--port DEV] [--system chip8|gb|gbc|nes|a800xl|gg|sms] [--keymap FILE] [--bios FILE]
        \\            [--load ROM [--preset ADDR=VAL]... [--no-input]]
        \\            [--slow] [--no-grab] [--ping N] [--verbose] [--baud-cache FILE]
        \\            [--sd-image FILE [--sd-rewrite]] [--device NAME]
        \\            [--ddr-test suite|SPEC] [--ddr-load FILE] [--saves-pull DIR [--states-dir DIR] [--movies-dir DIR]]
        \\       link --save-slots DIR [--system S]
        \\       link --state-slots DIR [--system S]
        \\       link --movie-slots DIR [--system S] [--movie-frames N]
        \\       link [--port DEV] --movies-pull DIR
        \\       link --stamp-clocks DIR
        \\       link [--port DEV] --movie-put FILE --movie-slot PATH
        \\       link --remap [--system S] [--keymap FILE] [--no-grab] [--device NAME]
        \\       link --show-keymap [--system S] [--keymap FILE]
        \\       link --settings [--system S] [--keymap FILE] [--device NAME]
        \\
    , .{});
    std.process.exit(2);
}

fn parseArgs(args: []const [:0]const u8) Options {
    var o: Options = .{};
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const a = args[i];
        if (std.mem.eql(u8, a, "--no-input")) {
            o.input = false;
        } else if (std.mem.eql(u8, a, "--slow")) {
            o.fast = false;
        } else if (std.mem.eql(u8, a, "--no-grab")) {
            o.grab = false;
        } else if (std.mem.eql(u8, a, "--sd-rewrite")) {
            o.sd_rewrite = true;
        } else if (std.mem.eql(u8, a, "--gb")) {
            o.system = .gb;
        } else if (std.mem.eql(u8, a, "--remap")) {
            o.remap = true;
        } else if (std.mem.eql(u8, a, "--settings")) {
            o.settings = true;
        } else if (std.mem.eql(u8, a, "--show-keymap")) {
            o.show_keymap = true;
        } else if (std.mem.eql(u8, a, "--keymap-pull")) {
            o.keymap_pull = true;
        } else if (std.mem.eql(u8, a, "--verbose")) {
            o.verbose = true;
        } else {
            if (i + 1 >= args.len) usage();
            i += 1;
            const v = args[i];
            if (std.mem.eql(u8, a, "--port")) {
                o.port = v;
            } else if (std.mem.eql(u8, a, "--load")) {
                o.load = v;
            } else if (std.mem.eql(u8, a, "--system")) {
                if (std.mem.eql(u8, v, "gbc")) {
                    o.system = .gb;
                    o.card_dir = "gbc";
                } else o.system = std.meta.stringToEnum(keymap.System, v) orelse usage();
            } else if (std.mem.eql(u8, a, "--device")) {
                o.device = v;
            } else if (std.mem.eql(u8, a, "--keymap")) {
                o.keymap = v;
            } else if (std.mem.eql(u8, a, "--bios")) {
                o.bios = v;
            } else if (std.mem.eql(u8, a, "--sd-image")) {
                o.sd_image = v;
            } else if (std.mem.eql(u8, a, "--ddr-test")) {
                o.ddr_test = v;
            } else if (std.mem.eql(u8, a, "--ddr-load")) {
                o.ddr_load = v;
            } else if (std.mem.eql(u8, a, "--saves-pull")) {
                o.saves_pull = v;
            } else if (std.mem.eql(u8, a, "--states-dir")) {
                o.states_dir = v;
            } else if (std.mem.eql(u8, a, "--save-slots")) {
                o.save_slots = v;
            } else if (std.mem.eql(u8, a, "--state-slots")) {
                o.state_slots = v;
            } else if (std.mem.eql(u8, a, "--movie-slots")) {
                o.movie_slots = v;
            } else if (std.mem.eql(u8, a, "--movie-frames")) {
                o.movie_frames = std.fmt.parseInt(u32, v, 0) catch usage();
            } else if (std.mem.eql(u8, a, "--movies-dir")) {
                o.movies_dir = v;
            } else if (std.mem.eql(u8, a, "--screenshot")) {
                o.screenshot = v;
            } else if (std.mem.eql(u8, a, "--screens-dir")) {
                o.screens_dir = v;
            } else if (std.mem.eql(u8, a, "--stamp-clocks")) {
                o.stamp_clocks = v;
            } else if (std.mem.eql(u8, a, "--movies-pull")) {
                o.movies_pull = v;
            } else if (std.mem.eql(u8, a, "--movie-put")) {
                o.movie_put = v;
            } else if (std.mem.eql(u8, a, "--movie-slot")) {
                o.movie_slot = v;
            } else if (std.mem.eql(u8, a, "--keymap-take")) {
                o.keymap_take = v;
            } else if (std.mem.eql(u8, a, "--baud-cache")) {
                o.baud_cache = v;
            } else if (std.mem.eql(u8, a, "--ping")) {
                o.ping = std.fmt.parseInt(u32, v, 0) catch usage();
            } else if (std.mem.eql(u8, a, "--preset")) {
                const eq = std.mem.indexOfScalar(u8, v, '=') orelse usage();
                if (o.n_presets == o.presets.len) usage();
                o.presets[o.n_presets] = .{
                    .addr = std.fmt.parseInt(u12, v[0..eq], 0) catch usage(),
                    .value = std.fmt.parseInt(u8, v[eq + 1 ..], 0) catch usage(),
                };
                o.n_presets += 1;
            } else usage();
        }
    }
    return o;
}

fn fail(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print("link: " ++ fmt ++ "\n", args);
    std.process.exit(1);
}

/// Puts the relay's parser back at the start of a line, whatever came
/// before: a complete key frame with no keys (a byte >= 0x80 always starts
/// a new frame), then a newline that ends any partial line. Without it a
/// stray byte >= 0x80 (the J14 bridge sends one when it powers up with the
/// board, side track F's SD boot; or our own bytes at a wrong rate) opens
/// a key frame that swallows the next "P\n" (the board answered "kA80A").
/// Sent before every ping, so only during negotiation; releasing all keys
/// there is harmless.
const resync_parser = [_]u8{ 0x80, 0x00, 0x00, '\n' };

/// Is the relay there at the port's current rate?
fn ping(s: *Serial, timeout_ms: i64) bool {
    var buf: [128]u8 = undefined;
    s.writeAll(&resync_parser ++ "P\n") catch return false;
    return s.expect("p", timeout_ms, &buf) != null;
}

/// Back to a known state after a failed switch: the relay drops to 115200
/// on its own after 0.6 s without a valid line, so wait that out, then talk
/// at 115200.
fn resync(s: *Serial) void {
    s.setBaud(0) catch {};
    sys.sleepMs(800);
    s.flush();
    if (!ping(s, 500)) fail("no answer from the relay on the serial port (did make prog run?)", .{});
}

/// Ask the relay for rate `code`; true if it then answers at that rate. On
/// failure both sides are back at 115200.
fn tryRate(s: *Serial, code: u3) bool {
    var buf: [128]u8 = undefined;
    var cmd = [_]u8{ 'B', '0' + @as(u8, code), '\n' };
    s.writeAll(&cmd) catch return false;
    // The relay switches right after its reply, whether or not we read it.
    _ = s.expect("b", 300, &buf);
    s.setBaud(code) catch {};
    sys.sleepMs(5); // the relay waits ~0.2 ms after the reply, then switches
    s.flush();
    if (ping(s, 200) or ping(s, 200)) return true;
    std.debug.print("link: {d} baud doesn't work here\n", .{protocol.rates[code]});
    resync(s);
    return false;
}

const default_baud_cache = "build/host/link-baud";

/// build/host/link-baud-<the port's file name, with anything but letters,
/// digits, '-' and '_' as '_'>.
fn portCache(buf: []u8, port: []const u8) [:0]const u8 {
    const name = std.fs.path.basename(port);
    const prefix = default_baud_cache ++ "-";
    const n = @min(name.len, buf.len - prefix.len - 1);
    @memcpy(buf[0..prefix.len], prefix);
    for (name[0..n], buf[prefix.len..][0..n]) |c, *d| {
        d.* = if (std.ascii.isAlphanumeric(c) or c == '-' or c == '_') c else '_';
    }
    buf[prefix.len + n] = 0;
    return buf[0 .. prefix.len + n :0];
}

test "the baud cache is per port" {
    var b: [512]u8 = undefined;
    try std.testing.expectEqualStrings("build/host/link-baud-usb-Digilent_Adept_003017B3FA08-if01-port0", portCache(&b, "/dev/serial/by-id/usb-Digilent_Adept_003017B3FA08-if01-port0"));
    try std.testing.expectEqualStrings("build/host/link-baud-ttyACM0", portCache(&b, "/dev/ttyACM0"));
    try std.testing.expectEqualStrings("build/host/link-baud-a_b", portCache(&b, "/tmp/a:b"));
}

fn readCache(path: [:0]const u8) ?u3 {
    const fd = sys.open(path, false) catch return null;
    defer sys.close(fd);
    var b: [1]u8 = undefined;
    const n = sys.read(fd, &b) catch return null;
    if (n != 1 or b[0] < '0' or b[0] > '4') return null;
    return @intCast(b[0] - '0');
}

fn writeCache(path: [:0]const u8, code: u3) void {
    const rc = linux.open(path, .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true, .CLOEXEC = true }, 0o644);
    const fd: i32 = @intCast(sys.check(rc) catch return);
    defer sys.close(fd);
    _ = sys.write(fd, &[_]u8{'0' + @as(u8, code)}) catch {};
}

/// Find the relay (at 115200, or still at a fast rate from an earlier run
/// that ended without B0), then move to the fastest rate that works:
/// the cached one first, then 921600, 460800, 230400. Returns the rate code.
fn negotiate(s: *Serial, want_fast: bool, cache: [:0]const u8) u3 {
    const cached = readCache(cache);
    // The J14 bridge applies the port settings a moment after we set them,
    // and glitches both ways when it does: a stray byte to the board (seen
    // inside the first resync frame: "board: k8000") and lost bytes from
    // it (side track F, on the board). Let it settle, drop what came, then
    // talk.
    sys.sleepMs(100);
    s.flush();
    if (!ping(s, 300)) {
        // maybe left at a fast rate: try there before waiting for its fallback
        var found = false;
        if (cached) |c| if (c != 0) {
            s.setBaud(c) catch {};
            if (ping(s, 200)) found = true;
        };
        if (found) {
            if (want_fast) return cached.?;
            _ = s.writeAll("B0\n") catch {};
        }
        resync(s);
    }
    if (!want_fast) return 0;
    if (cached) |c| {
        if (c == 0) {
            std.debug.print("link: staying at 115200 (no faster rate worked last time; delete {s} to try again)\n", .{cache});
            return 0;
        }
        if (tryRate(s, c)) return c;
    }
    for ([_]u3{ 4, 1, 2, 3 }) |c| {
        if (cached != null and cached.? == c) continue;
        if (tryRate(s, c)) {
            writeCache(cache, c);
            return c;
        }
    }
    writeCache(cache, 0);
    return 0;
}

fn hexLine(buf: []u8, comptime fmt: []const u8, args: anytype) []const u8 {
    return std.fmt.bufPrint(buf, fmt, args) catch unreachable;
}

fn loadBlock(s: *Serial, addr: u12, data: []const u8, fast: bool) void {
    var hdr: [16]u8 = undefined;
    s.writeAll(hexLine(&hdr, "L{X:0>3}{X:0>4}\n", .{ addr, data.len })) catch fail("write failed", .{});
    s.writeAll(data) catch fail("write failed", .{});
    var sum: u16 = 0;
    for (data) |b| sum +%= b;
    var want: [16]u8 = undefined;
    const w = hexLine(&want, "l{X:0>3}{X:0>4}{X:0>4}", .{ addr, data.len, sum });
    var buf: [128]u8 = undefined;
    const per_byte_us: i64 = if (fast) 44 else 87;
    const got = s.expect("l", 2000 + @divTrunc(@as(i64, @intCast(data.len)) * per_byte_us, 500), &buf) orelse
        fail("no reply to a load of {d} bytes at 0x{X:0>3}", .{ data.len, addr });
    if (!std.mem.eql(u8, got, w)) fail("load at 0x{X:0>3} failed: relay said '{s}', expected '{s}'", .{ addr, got, w });
}

/// A block with a 24-bit address (the L command's wide form, Game Boy
/// cartridges; any address of the PS link, Platform.Ps.Link).
fn loadBlockWide(s: *Serial, addr: u24, data: []const u8, fast: bool) void {
    var hdr: [16]u8 = undefined;
    s.writeAll(hexLine(&hdr, "L{X:0>6}{X:0>4}\n", .{ addr, data.len })) catch fail("write failed", .{});
    s.writeAll(data) catch fail("write failed", .{});
    var sum: u16 = 0;
    for (data) |b| sum +%= b;
    var want: [16]u8 = undefined;
    const w = hexLine(&want, "l{X:0>6}{X:0>4}{X:0>4}", .{ addr, data.len, sum });
    var buf: [128]u8 = undefined;
    const per_byte_us: i64 = if (fast) 44 else 87;
    const got = s.expect("l", 2000 + @divTrunc(@as(i64, @intCast(data.len)) * per_byte_us, 500), &buf) orelse
        fail("no reply to a load of {d} bytes at 0x{X:0>6}", .{ data.len, addr });
    if (!std.mem.eql(u8, got, w)) fail("load at 0x{X:0>6} failed: relay said '{s}', expected '{s}'", .{ addr, got, w });
}

// --- DDR (side track C; make ddr-test, make ddr-load) ------------------------------
const ddr_load_base: u32 = 0x0100_0000;
const ddr_load_max: usize = 16 << 20;

/// The DDR test on an M0e bitstream (relay T command, ddrtest.zig): the
/// suite ("suite") or one run (17 hex digits: base, mode, words, flags).
/// Prints the board's report; exits 1 if a run failed.
fn ddrTest(s: *Serial, spec: []const u8) void {
    var line: [32]u8 = undefined;
    const cmd = if (std.mem.eql(u8, spec, "suite"))
        "T\n"
    else if (spec.len == 17)
        std.fmt.bufPrint(&line, "T{s}\n", .{spec}) catch unreachable
    else
        fail("--ddr-test: 'suite' or 17 hex digits (base 8, mode 2, words 6, flags 1)", .{});
    s.writeAll(cmd) catch fail("write failed", .{});
    if (!ddrReport(s)) std.process.exit(1);
}

/// The relay's "t ..." lines until "t end ..." (true: ok), or "x ...".
fn ddrReport(s: *Serial) bool {
    var buf: [256]u8 = undefined;
    while (true) {
        // a run of the suite takes well under a second; 20 s means trouble
        const l = s.expect("", 20_000, &buf) orelse fail("no answer from the relay's DDR test", .{});
        if (l.len == 0) continue;
        if (std.mem.startsWith(u8, l, "x")) fail("board: {s}", .{l});
        if (std.mem.startsWith(u8, l, "t end")) {
            std.debug.print("{s}\n", .{l[2..]});
            return std.mem.endsWith(u8, l, " ok");
        }
        if (std.mem.startsWith(u8, l, "t ")) {
            std.debug.print("{s}\n", .{l[2..]});
        } else std.debug.print("board: {s}\n", .{l});
    }
}

/// A file into DDR (relay W command, 64 KiB at a time), then the PL reads
/// it back through the HP port (one run with flags = 2: the data in DDR)
/// and the PS compares the checksums.
/// `data` into DDR from `base` through the relay's W command, 64 KiB at a
/// time, each checked by its checksum; `progress` prints KiB done.
fn ddrWrite(s: *Serial, base: u32, data: []const u8, fast: bool, progress: bool) void {
    var buf: [128]u8 = undefined;
    var off: usize = 0;
    while (off < data.len) {
        const n = @min(data.len - off, 64 << 10);
        const addr: u32 = base + @as(u32, @intCast(off));
        var hdr: [24]u8 = undefined;
        s.writeAll(std.fmt.bufPrint(&hdr, "W{X:0>8}{X:0>6}\n", .{ addr, n }) catch unreachable) catch fail("write failed", .{});
        s.writeAll(data[off..][0..n]) catch fail("write failed", .{});
        var sum: u16 = 0;
        for (data[off..][0..n]) |b| sum +%= b;
        var want: [24]u8 = undefined;
        const w = std.fmt.bufPrint(&want, "w{X:0>8}{X:0>6}{X:0>4}", .{ addr, n, sum }) catch unreachable;
        const per_byte_us: i64 = if (fast) 44 else 87;
        const got = s.expect("w", 2000 + @divTrunc(@as(i64, @intCast(n)) * per_byte_us, 500), &buf) orelse
            fail("no reply to a DDR write of {d} bytes at 0x{X:0>8}", .{ n, addr });
        if (!std.ascii.eqlIgnoreCase(got, w)) fail("DDR write at 0x{X:0>8}: relay said '{s}', expected '{s}'", .{ addr, got, w });
        off += n;
        if (progress) std.debug.print("\rlink: {d} / {d} KiB", .{ off / 1024, data.len / 1024 });
    }
    if (progress) std.debug.print("\n", .{});
}

/// Y: the relay copies these bytes (already in DDR at `addr`) onto the PS
/// link at `at`; `data` is the same bytes, for the checksum.
fn ddrToLink(s: *Serial, addr: u32, at: u24, data: []const u8) void {
    var hdr: [32]u8 = undefined;
    s.writeAll(std.fmt.bufPrint(&hdr, "Y{X:0>8}{X:0>6}{X:0>6}\n", .{ addr, at, data.len }) catch unreachable) catch fail("write failed", .{});
    var sum: u16 = 0;
    for (data) |b| sum +%= b;
    var want: [32]u8 = undefined;
    const w = std.fmt.bufPrint(&want, "y{X:0>6}{X:0>6}{X:0>4}", .{ at, data.len, sum }) catch unreachable;
    var buf: [128]u8 = undefined;
    // about 4 us a byte onto the link, plus margin
    const got = s.expect("y", 2000 + @as(i64, @intCast(data.len / 100)), &buf) orelse
        fail("no reply to a DDR-to-link copy of {d} bytes at 0x{X:0>6}", .{ data.len, at });
    if (!std.ascii.eqlIgnoreCase(got, w)) fail("DDR-to-link copy at 0x{X:0>6}: relay said '{s}', expected '{s}'", .{ at, got, w });
}

fn ddrLoad(s: *Serial, data: []const u8, fast: bool) void {
    const t0 = sys.nowUs();
    ddrWrite(s, ddr_load_base, data, fast, false);
    const ms = @divTrunc(sys.nowUs() - t0, 1000);
    std.debug.print("link: {d} bytes into DDR at 0x{X:0>8} in {d} ms (the serial line is the limit)\n", .{ data.len, ddr_load_base, ms });
    const words = (data.len + 7) / 8;
    var cmd: [32]u8 = undefined;
    s.writeAll(std.fmt.bufPrint(&cmd, "T{X:0>8}00{X:0>6}2\n", .{ ddr_load_base, words }) catch unreachable) catch fail("write failed", .{});
    if (!ddrReport(s)) std.process.exit(1);
}

/// The largest cartridge file for a system: the SD menu's limits
/// (platform/ps/menu.zig `systems`, the board's own table): Game Boy 8
/// MiB (in DDR, side track I), NES the .nes file with PRG 512 KiB + CHR
/// 256 KiB (in DDR, then header and CHR over the link; side track D).
fn cartridgeLimit(system: keymap.System) usize {
    return (saves.systemByDir(@tagName(system)) orelse unreachable).max_size;
}

/// A cartridge file as it is (Game Boy ROM, or a whole .nes file) at link
/// address 0, in 16 KiB blocks; the core reads the header when the hold is
/// released (a NES core routes the file by offset, Nes.Loader).
fn loadCartridge(s: *Serial, file: []const u8, fast: bool, system: keymap.System, companion: ?[]const u8) void {
    const rom = file;
    var buf: [128]u8 = undefined;
    if (rom.len > cartridgeLimit(system)) fail("ROM is {d} bytes, more than {d} for the {s}", .{ rom.len, cartridgeLimit(system), system.title() });
    // (M6) a companion file with it (--bios: the Master System's BIOS, the
    // user's own dump), checked as the menu checks it
    if (companion) |c| {
        const comp = (saves.systemByDir(@tagName(system)) orelse unreachable).companion orelse fail("--bios: the {s} takes none", .{system.title()});
        if (comp.check(@intCast(c.len))) |why| fail("{s}", .{why});
    }
    switch (system) {
        .nes => {
            if (rom.len < 16 or !std.mem.eql(u8, rom[0..4], "NES\x1a")) fail("not an iNES file (no NES<1A> header)", .{});
            // (side track D) what the core runs, as the SD menu checks it
            // (mapper, sizes up to 512 KiB PRG / 256 KiB CHR, RAM, trainer)
            if ((saves.systemByDir("nes") orelse unreachable).check.?(rom[0..@min(rom.len, 512)], @intCast(rom.len))) |why| fail("{s}", .{why});
        },
        // (M4 step 4.8) what the 800XL's loader accepts, as the SD menu
        .a800xl => if ((saves.systemByDir("a800xl") orelse unreachable).check.?(rom[0..@min(rom.len, 512)], @intCast(rom.len))) |why| fail("{s}", .{why}),
        // (M5) as the SD menu
        .gg => if ((saves.systemByDir("gg") orelse unreachable).check.?(rom[0..@min(rom.len, 512)], @intCast(rom.len))) |why| fail("{s}", .{why}),
        // (M6) as the SD menu
        .sms => if ((saves.systemByDir("sms") orelse unreachable).check.?(rom[0..@min(rom.len, 512)], @intCast(rom.len))) |why| fail("{s}", .{why}),
        else => if (rom.len < 0x150) fail("ROM is {d} bytes, too short for a cartridge header", .{rom.len}),
    }
    // the whole ROM's check, as the board makes it once the ROM is in DDR
    // (M7's request)
    if ((saves.systemByDir(@tagName(system)) orelse unreachable).check_ddr) |check|
        if (check(rom)) |why| fail("{s}", .{why});
    // the form the PL reads, as the menu makes it in DDR (side track Q,
    // menu.System.ddr_image: the 800XL's tapes)
    const entry = saves.systemByDir(@tagName(system)) orelse unreachable;
    var img: ?[]u8 = null;
    defer if (img) |b| std.heap.page_allocator.free(b);
    if (entry.ddr_image) |di| {
        const n = di.size(file);
        if (n > 0) {
            const b = std.heap.page_allocator.alloc(u8, n) catch fail("out of memory", .{});
            di.write(file, b);
            img = b;
        }
    }
    const t0 = sys.nowUs();
    s.writeAll("H1\n") catch fail("write failed", .{});
    if (s.expect("h1", 1000, &buf) == null) fail("relay didn't confirm hold", .{});
    if (entry.ddr_addr) |base_| {
        const base = base_;
        const rom_in = img orelse file;
        // the companion first, as the menu (M6)
        if (companion) |c| ddrWrite(s, entry.companion.?.ddr_addr, c, fast, true);
        // the Game Boy (side track I): the cartridge into DDR, as the menu
        ddrWrite(s, base, rom_in, fast, true);
        // the NES (side track D): then its header and CHR ROM onto the
        // link at their file offsets, copied by the relay from DDR (Y), as
        // the menu does it (menu.zig, ddr_link): at 2 Mbaud the UART brings
        // bytes faster than the relay can strobe them out (L blocks overran)
        if (entry.ddr_link) |parts| for (parts(rom_in[0..@min(rom_in.len, 512)])) |p| {
            // clipped to the file's end, as the menu's load.sendFromDdr
            // (side track Q, 2026-10-21: the 800XL's executables)
            if (p.len == 0 or p.off >= rom_in.len) continue;
            const len = @min(p.len, @as(u32, @intCast(rom_in.len)) - p.off);
            ddrToLink(s, base + p.off, @intCast(p.off), rom_in[p.off..][0..len]);
        };
        // still held: what the core takes from the whole ROM (the Game
        // Gear's mapper and size; menu.zig `ddr_info`)
        if (entry.ddr_info) |di| {
            const w0 = di.of(rom_in);
            const w = if (entry.companion) |c| c.word(w0, if (companion) |b| @intCast(b.len) else 0) else w0;
            loadBlockWide(s, di.addr, &w, fast);
        }
    } else {
        var a: usize = 0;
        while (a < rom.len) : (a += 0x4000) {
            loadBlockWide(s, @intCast(a), rom[a..@min(rom.len, a + 0x4000)], fast);
            std.debug.print("\rlink: {d} / {d} KiB", .{ @min(rom.len, a + 0x4000) / 1024, rom.len / 1024 });
        }
        std.debug.print("\n", .{});
    }
    s.writeAll("H0\n") catch fail("write failed", .{});
    if (s.expect("h0", 1000, &buf) == null) fail("relay didn't confirm release", .{});
    std.debug.print("link: loaded {d} bytes in {d} ms; running\n", .{ rom.len, @divTrunc(sys.nowUs() - t0, 1000) });
}

fn loadRom(s: *Serial, rom: []const u8, presets: []const Preset, fast: bool) void {
    var buf: [128]u8 = undefined;
    if (rom.len > 0x1000 - 0x200) fail("ROM is {d} bytes, more than 3584", .{rom.len});
    const t0 = sys.nowUs();
    s.writeAll("H1\n") catch fail("write failed", .{});
    if (s.expect("h1", 1000, &buf) == null) fail("relay didn't confirm hold", .{});
    var image = [_]u8{0} ** (0x1000 - 0x200);
    @memcpy(image[0..rom.len], rom);
    loadBlock(s, 0x200, &image, fast);
    for (presets) |p| loadBlock(s, p.addr, &.{p.value}, fast);
    s.writeAll("H0\n") catch fail("write failed", .{});
    if (s.expect("h0", 1000, &buf) == null) fail("relay didn't confirm release", .{});
    std.debug.print("link: loaded {d} bytes (+{d} preset(s)) in {d} ms; running\n", .{ rom.len, presets.len, @divTrunc(sys.nowUs() - t0, 1000) });
}

fn measurePing(s: *Serial, n: u32) void {
    var samples: [1000]i64 = undefined;
    const count = @min(n, samples.len);
    var buf: [128]u8 = undefined;
    for (0..count) |i| {
        const t0 = sys.nowUs();
        s.writeAll("P\n") catch fail("write failed", .{});
        _ = s.expect("p", 1000, &buf) orelse fail("ping {d} lost", .{i});
        samples[i] = sys.nowUs() - t0;
    }
    std.mem.sort(i64, samples[0..count], {}, std.sort.asc(i64));
    std.debug.print("link: {d} round trips to the PS: min {d} us, median {d} us, max {d} us\n", .{ count, samples[0], samples[count / 2], samples[count - 1] });
    std.debug.print("link: one-way PC -> PL is about half the median (plus ~0.1 us in the PL)\n", .{});
}

/// The relay's answer to an SD command: the next line that is one ("s",
/// "d", "z", "x" followed by data, or exactly "o"); other lines (the
/// relay's and the menu's messages) are shown.
fn sdReply(s: *Serial, timeout_ms: i64, buf: []u8) ?[]const u8 {
    const end = sys.nowUs() + timeout_ms * 1000;
    while (true) {
        const left = @divTrunc(end - sys.nowUs(), 1000);
        if (left <= 0) return null;
        const l = s.expect("", left, buf) orelse return null;
        if (l.len == 0) continue;
        if (std.mem.eql(u8, l, "o")) return l;
        if (isSdReply(l)) return l;
        std.debug.print("board: {s}\n", .{l});
    }
}

/// "s2", "d<lba>", "x<lba> reason", ...: the letter, then only hex digits
/// up to the end or a space. Not "serial: ..." (a game's text, which the
/// relay passes on at any time: 'e' is a hex digit, so a look at the
/// second character alone took it for a reply).
fn isSdReply(l: []const u8) bool {
    if (l.len < 2 or std.mem.indexOfScalar(u8, "sdzxrcm", l[0]) == null) return false;
    const end = std.mem.indexOfScalar(u8, l, ' ') orelse l.len;
    if (end < 2) return false;
    for (l[1..end]) |c| if (!std.ascii.isHex(c)) return false;
    return true;
}

test "SD replies: the relay's answers, not a game's serial text" {
    for ([_][]const u8{ "s2", "d00000800", "z0000000000000010", "x00000800 checksum", "r00000800BEEF", "c00000000A96097CCE9746D25", "m00000800" }) |l|
        try std.testing.expect(isSdReply(l));
    try std.testing.expect(!isSdReply("serial: Passed"));
    try std.testing.expect(!isSdReply("start-up: relay: ..."));
    try std.testing.expect(!isSdReply("d"));
}

/// The card in chunks (side track G phase 2, sdimage.chunks): each chunk's
/// hash asked of the relay (C) unless `rewrite`, the chunk skipped if the
/// card holds it already, else written as Z (zeros) or M (one multi-block
/// write). Falls back to writeSdBlocks with a relay that doesn't know C.
fn writeSd(s: *Serial, image: []const u8, gpa: std.mem.Allocator, rewrite: bool) void {
    if (image.len % sdimage.block != 0) fail("the image is not a whole number of 512-byte blocks", .{});
    var buf: [256]u8 = undefined;
    s.writeAll("S\n") catch fail("write failed", .{});
    const open = sdReply(s, 5000, &buf) orelse fail("no answer to S (is the relay new enough? make prog)", .{});
    if (open[0] != 's') fail("the board can't open the SD card: {s}", .{open[1..]});
    const kinds = [_][]const u8{ "SDSC 1.x", "SDSC 2.0", "SDHC/SDXC" };
    const kind = if (open[1] >= '0' and open[1] <= '2') kinds[open[1] - '0'] else open[1..];

    // A relay from before M and C ignores the line: the old way, then.
    // A new one answers within milliseconds (one block read); the wait
    // stays under the relay's 0.6 s fallback to 115200, which an ignored
    // line doesn't reset, and a ping right after counts as valid input.
    s.writeAll("C0000000001\n") catch fail("write failed", .{});
    const probe = sdReply(s, 300, &buf);
    if (probe == null and !ping(s, 300)) fail("the relay stopped answering after S", .{});
    if (probe == null or probe.?[0] != 'c') {
        std.debug.print("link: the relay doesn't know C/M (make prog for a newer one): writing block by block\n", .{});
        return writeSdBlocks(s, image, gpa, true);
    }

    const cs = sdimage.chunks(gpa, image) catch fail("out of memory", .{});
    defer gpa.free(cs);
    var total: u64 = 0;
    for (cs) |c| total += c.count;
    std.debug.print("link: SD card open ({s}); {d} blocks in {d} chunks{s}, every write read back\n", .{ kind, total, cs.len, if (rewrite) ", all rewritten" else ", unchanged chunks skipped" });
    const t0 = sys.nowUs();
    var done: u64 = 0;
    var skipped: u64 = 0;
    var written: u64 = 0;
    var next_report: u64 = 0;
    for (cs) |c| {
        if (done >= next_report) {
            std.debug.print("link: {d}% ({d}/{d} blocks, {d} written, {d} s)\n", .{ done * 100 / total, done, total, written, @divTrunc(sys.nowUs() - t0, 1_000_000) });
            next_report = done + total / 10;
        }
        defer done += c.count;
        var lb: [40]u8 = undefined;
        if (!rewrite) {
            s.writeAll(sdimage.hashLine(&lb, c)) catch fail("write failed", .{});
            const r = sdReply(s, 5000, &buf) orelse fail("no answer to the hash of blocks {d}..{d}", .{ c.lba, c.lba + c.count - 1 });
            var want: [12]u8 = undefined;
            const w = std.fmt.bufPrint(&want, "c{X:0>8}", .{c.lba}) catch unreachable;
            if (r.len != 25 or !std.mem.startsWith(u8, r, w)) fail("hashing blocks {d}..{d} failed: {s}", .{ c.lba, c.lba + c.count - 1, r });
            const on_card = std.fmt.parseInt(u64, r[9..25], 16) catch fail("hashing blocks {d}: bad reply {s}", .{ c.lba, r });
            if (on_card == protocol.chunkHash(sdimage.chunkBytes(image, c))) {
                skipped += c.count;
                continue;
            }
        }
        const cmd = sdimage.writeLine(&lb, c, image);
        var want: [24]u8 = undefined;
        const w = if (c.zero)
            std.fmt.bufPrint(&want, "z{X:0>8}{X:0>8}", .{ c.lba, @as(u32, c.count) }) catch unreachable
        else
            std.fmt.bufPrint(&want, "m{X:0>8}", .{c.lba}) catch unreachable;
        var tries: u32 = 0;
        while (true) : (tries += 1) {
            s.writeAll(cmd) catch fail("write failed", .{});
            if (!c.zero) s.writeAll(sdimage.chunkBytes(image, c)) catch fail("write failed", .{});
            const r = sdReply(s, 10000, &buf) orelse fail("no answer to writing blocks {d}..{d}", .{ c.lba, c.lba + c.count - 1 });
            if (std.mem.eql(u8, r, w)) break;
            // a garbled transfer: the relay saw a wrong checksum
            if (tries < 3 and std.mem.indexOf(u8, r, "checksum") != null) {
                std.debug.print("link: blocks {d}..: {s}; again\n", .{ c.lba, r });
                continue;
            }
            fail("writing blocks {d}..{d} failed: {s}", .{ c.lba, c.lba + c.count - 1, r });
        }
        written += c.count;
    }
    std.debug.print("link: card written: {d} blocks, {d} of them unchanged and skipped, in {d} s; reopening the board's menu\n", .{ total, skipped, @divTrunc(sys.nowUs() - t0, 1_000_000) });
    s.writeAll("O\n") catch fail("write failed", .{});
    const m = sdReply(s, 10000, &buf) orelse fail("no answer to O", .{});
    if (!std.mem.eql(u8, m, "o")) fail("reopening the menu: {s}", .{m});
}

/// The card block by block (D and Z), for a relay without M and C;
/// `opened`: S has been answered already.
fn writeSdBlocks(s: *Serial, image: []const u8, gpa: std.mem.Allocator, opened: bool) void {
    const ops = sdimage.plan(gpa, image) catch fail("out of memory", .{});
    defer gpa.free(ops);
    var total: u64 = 0;
    var sent: u64 = 0;
    for (ops) |op| total += switch (op) {
        .zero => |z| z.count,
        .write => 1,
    };
    var buf: [256]u8 = undefined;
    const t0 = sys.nowUs();

    if (!opened) {
        s.writeAll("S\n") catch fail("write failed", .{});
        const open = sdReply(s, 5000, &buf) orelse fail("no answer to S (is the relay new enough? make prog)", .{});
        if (open[0] != 's') fail("the board can't open the SD card: {s}", .{open[1..]});
    }
    std.debug.print("link: writing {d} blocks ({d} commands), every block read back\n", .{ total, ops.len });

    var next_report: u64 = 0;
    for (ops) |op| {
        if (sent >= next_report) {
            std.debug.print("link: {d}% ({d}/{d} blocks, {d} s)\n", .{ sent * 100 / total, sent, total, @divTrunc(sys.nowUs() - t0, 1_000_000) });
            next_report = sent + total / 10;
        }
        var lb: [32]u8 = undefined;
        const cmd = sdimage.line(&lb, op, image);
        switch (op) {
            .zero => |z| {
                s.writeAll(cmd) catch fail("write failed", .{});
                var want: [24]u8 = undefined;
                const w = std.fmt.bufPrint(&want, "z{X:0>8}{X:0>8}", .{ z.lba, z.count }) catch unreachable;
                // up to ~25 ms per block for a slow card (write + read back)
                const r = sdReply(s, 5000 + @as(i64, z.count) * 25, &buf) orelse fail("no answer to zero-filling {d} blocks at {d}", .{ z.count, z.lba });
                if (!std.mem.eql(u8, r, w)) fail("zero-filling blocks {d}..{d} failed: {s}", .{ z.lba, z.lba + z.count - 1, r });
                sent += z.count;
            },
            .write => |lba| {
                var want: [16]u8 = undefined;
                const w = std.fmt.bufPrint(&want, "d{X:0>8}", .{lba}) catch unreachable;
                var tries: u32 = 0;
                while (true) : (tries += 1) {
                    s.writeAll(cmd) catch fail("write failed", .{});
                    s.writeAll(sdimage.blk(image, lba)) catch fail("write failed", .{});
                    const r = sdReply(s, 5000, &buf) orelse fail("no answer to writing block {d}", .{lba});
                    if (std.mem.eql(u8, r, w)) break;
                    // a garbled transfer: the relay saw a wrong checksum
                    if (tries < 3 and std.mem.indexOf(u8, r, "checksum") != null) {
                        std.debug.print("link: block {d}: {s}; again\n", .{ lba, r });
                        continue;
                    }
                    fail("writing block {d} failed: {s}", .{ lba, r });
                }
                sent += 1;
            },
        }
    }
    std.debug.print("link: card written: {d} blocks in {d} s; reopening the board's menu\n", .{ total, @divTrunc(sys.nowUs() - t0, 1_000_000) });
    s.writeAll("O\n") catch fail("write failed", .{});
    const m = sdReply(s, 10000, &buf) orelse fail("no answer to O", .{});
    if (!std.mem.eql(u8, m, "o")) fail("reopening the menu: {s}", .{m});
}

/// The board's SD card as a block device, through the relay's R command
/// (for saves.pull): "r<lba><sum>" and the 512 bytes, checked; a garbled
/// block is asked for again.
const RemoteCard = struct {
    s: *Serial,

    /// D: one block, which the relay reads back (S first).
    pub fn writeBlock(c: *RemoteCard, lba: u32, b: *const [512]u8) !void {
        var line_buf: [256]u8 = undefined;
        var cmd: [24]u8 = undefined;
        var want: [16]u8 = undefined;
        const w = hexLine(&want, "d{X:0>8}", .{lba});
        var tries: u32 = 0;
        while (true) : (tries += 1) {
            c.s.writeAll(hexLine(&cmd, "D{X:0>8}{X:0>4}\n", .{ lba, sdimage.checksum(b) })) catch return error.WriteFailed;
            c.s.writeAll(b) catch return error.WriteFailed;
            const r = sdReply(c.s, 5000, &line_buf) orelse return error.NoAnswer;
            if (std.mem.eql(u8, r, w)) return;
            if (tries < 3 and std.mem.indexOf(u8, r, "checksum") != null) continue;
            std.debug.print("link: writing block {d}: {s}\n", .{ lba, r });
            return error.WriteFailed;
        }
    }

    pub fn readBlock(c: *RemoteCard, lba: u32, buf: *[512]u8) !void {
        var line_buf: [256]u8 = undefined;
        var cmd: [16]u8 = undefined;
        var tries: u32 = 0;
        while (true) : (tries += 1) {
            c.s.writeAll(hexLine(&cmd, "R{X:0>8}\n", .{lba})) catch return error.WriteFailed;
            const r = sdReply(c.s, 5000, &line_buf) orelse return error.NoAnswer;
            var want: [16]u8 = undefined;
            const w = hexLine(&want, "r{X:0>8}", .{lba});
            if (r.len != 13 or !std.mem.startsWith(u8, r, w)) {
                std.debug.print("link: reading block {d}: {s}\n", .{ lba, r });
                return error.ReadFailed;
            }
            const sum = std.fmt.parseInt(u16, r[9..13], 16) catch return error.ReadFailed;
            if (!c.s.readExact(buf, 2000)) return error.NoAnswer;
            if (sdimage.checksum(buf) == sum) return;
            if (tries == 3) return error.Checksum;
            std.debug.print("link: block {d}: checksum; again\n", .{lba});
        }
    }
};

/// --saves-pull: the card's /saves into `dst`.
fn pullSaves(s: *Serial, io: std.Io, gpa: std.mem.Allocator, dst: []const u8) void {
    var card: RemoteCard = .{ .s = s };
    const t0 = sys.nowUs();
    var ts: linux.timespec = undefined;
    _ = linux.clock_gettime(.REALTIME, &ts);
    const p = saves.pull(&card, io, gpa, dst, true, @intCast(ts.sec)) catch |e| fail("pulling the saves failed: {s}", .{@errorName(e)});
    if (p.no_folder) {
        std.debug.print("link: saves: the card has no /saves folder, nothing to pull\n", .{});
    } else std.debug.print("link: saves: {d} file(s), {d} bytes into {s} ({d} ms)\n", .{ p.files, p.bytes, dst, @divTrunc(sys.nowUs() - t0, 1000) });
}

/// --keymap-pull: the card's /keymap.conf (after the saves), into the
/// desktop's file at `path` if the board wrote it.
fn pullKeymap(s: *Serial, path: [:0]const u8) void {
    var card: RemoteCard = .{ .s = s };
    var fs = fat.Fs(RemoteCard).mount(&card) catch |e| fail("the card's file system: {s}", .{@errorName(e)});
    const e = (fs.find(fs.root(), "keymap.conf") catch |err| fail("the card's keymap.conf: {s}", .{@errorName(err)})) orelse {
        std.debug.print("link: keymap: the card has no keymap.conf\n", .{});
        return;
    };
    var buf: [remap.max_file]u8 = undefined;
    if (e.is_dir or e.size > buf.len) fail("the card's keymap.conf: not a file under 64 KiB", .{});
    const n = fs.readFile(e, &buf) catch |err| fail("reading the card's keymap.conf: {s}", .{@errorName(err)});
    takeKeymap(path, buf[0..n]);
}

/// The card's key map text into the desktop's file if the board wrote it.
fn takeKeymap(path: [:0]const u8, card: []const u8) void {
    var err: keymap.ParseError = undefined;
    const took = remap.takeBoard(path, card, &err) catch |e| switch (e) {
        error.Invalid => fail("the card's keymap.conf (the board's) line {d}: {s}; the desktop's {s} kept", .{ err.line, err.msg, path }),
        else => fail("keymap: can't update {s} ({s})", .{ path, @errorName(e) }),
    };
    std.debug.print("link: keymap: {s}\n", .{switch (took) {
        .not_board => "the card's keymap.conf is the desktop's (not changed on the board)",
        .same => "the card's keymap.conf, changed on the board, is already the desktop's",
        .copied => "the card's keymap.conf, changed on the board, copied to the desktop's file (the old one: .bak)",
    }});
}

/// --states-dir: the card's /states into `dst` (after the saves).
fn pullStates(s: *Serial, io: std.Io, gpa: std.mem.Allocator, dst: []const u8) void {
    var card: RemoteCard = .{ .s = s };
    const t0 = sys.nowUs();
    const p = saves.pullStates(&card, io, gpa, dst, true) catch |e| fail("pulling the save states failed: {s}", .{@errorName(e)});
    if (p.no_folder) {
        std.debug.print("link: states: the card has no /states folder, nothing to pull\n", .{});
    } else std.debug.print("link: states: {d} file(s), {d} bytes into {s} ({d} ms)\n", .{ p.files, p.bytes, dst, @divTrunc(sys.nowUs() - t0, 1000) });
}

/// --movies-dir: the card's /movies into `dst` (after the states).
fn pullMovies(s: *Serial, io: std.Io, gpa: std.mem.Allocator, dst: []const u8) void {
    var card: RemoteCard = .{ .s = s };
    const t0 = sys.nowUs();
    const p = saves.pullMovies(&card, io, gpa, dst, true) catch |e| fail("pulling the movies failed: {s}", .{@errorName(e)});
    if (p.no_folder) {
        std.debug.print("link: movies: the card has no /movies folder, nothing to pull\n", .{});
    } else std.debug.print("link: movies: {d} file(s), {d} bytes into {s} ({d} ms)\n", .{ p.files, p.bytes, dst, @divTrunc(sys.nowUs() - t0, 1000) });
}

/// --screens-dir: the card's /screens into `dst` (after the movies).
fn pullScreens(s: *Serial, io: std.Io, gpa: std.mem.Allocator, dst: []const u8) void {
    var card: RemoteCard = .{ .s = s };
    const p = saves.pullScreens(&card, io, gpa, dst, true) catch |e| fail("pulling the screenshots failed: {s}", .{@errorName(e)});
    if (p.no_folder) {
        std.debug.print("link: screens: the card has no /screens folder, nothing to pull\n", .{});
    } else std.debug.print("link: screens: {d} file(s), {d} bytes into {s}\n", .{ p.files, p.bytes, dst });
}

/// --screenshot: G (the board takes one into its card), then that file
/// read off the card into `dst`.
fn screenshot(s: *Serial, io: std.Io, gpa: std.mem.Allocator, dst: []const u8) void {
    var buf: [256]u8 = undefined;
    s.writeAll("G\n") catch fail("write failed", .{});
    const end = sys.nowUs() + 5_000_000;
    const path = while (true) {
        const left = @divTrunc(end - sys.nowUs(), 1000);
        if (left <= 0) fail("no answer to G (is a game from the card running? make prog for a newer relay?)", .{});
        const l = s.expect("", left, &buf) orelse continue;
        if (l.len > 1 and l[0] == 'g' and l[1] == '/') break l[1..];
        if (l.len > 1 and l[0] == 'x' and !isSdReply(l)) fail("the board: {s}", .{l[1..]});
        if (l.len > 0) std.debug.print("board: {s}\n", .{l});
    };
    var path_buf: [256]u8 = undefined;
    @memcpy(path_buf[0..path.len], path);
    const p = path_buf[0..path.len];
    var card: RemoteCard = .{ .s = s };
    const n = saves.pullFile(&card, io, gpa, p, dst) catch |e| fail("copying {s} off the card failed: {s}", .{ p, @errorName(e) });
    std.debug.print("link: screenshot {s} ({d} bytes) into {s}\n", .{ p, n, dst });
}

/// --movie-put: a movie file into the card's slot (S opens the card for
/// writes, O reopens the board's menu after), then Play movie on the board.
fn putMovie(s: *Serial, io: std.Io, gpa: std.mem.Allocator, file: []const u8, slot: []const u8) void {
    const bytes = std.Io.Dir.cwd().readFileAlloc(io, file, gpa, .limited(64 << 20)) catch |e| fail("can't read {s} ({s})", .{ file, @errorName(e) });
    defer gpa.free(bytes);
    var buf: [256]u8 = undefined;
    s.writeAll("S\n") catch fail("write failed", .{});
    const open = sdReply(s, 5000, &buf) orelse fail("no answer to S (is the relay new enough? make prog)", .{});
    if (open[0] != 's') fail("the board can't open the SD card: {s}", .{open[1..]});
    var card: RemoteCard = .{ .s = s };
    const t0 = sys.nowUs();
    const frames = saves.putMovie(&card, slot, bytes) catch |e| fail("putting {s} into {s} failed: {s}", .{ file, slot, @errorName(e) });
    std.debug.print("link: movies: {s} ({d} frames) into {s} ({d} ms); Play movie in the board's Options\n", .{ file, frames, slot, @divTrunc(sys.nowUs() - t0, 1000) });
    s.writeAll("O\n") catch fail("write failed", .{});
    const m = sdReply(s, 10000, &buf) orelse fail("no answer to O", .{});
    if (!std.mem.eql(u8, m, "o")) fail("reopening the menu: {s}", .{m});
}

var stop = std.atomic.Value(bool).init(false);

fn onSignal(_: linux.SIG) callconv(.c) void {
    stop.store(true, .monotonic);
}

pub fn main(init: std.process.Init) !void {
    const o = parseArgs(try init.minimal.args.toSlice(init.arena.allocator()));

    // no board needed: clock saves copied off a mounted card, stamped as a
    // pull through the board stamps them (make sd-sync)
    if (o.stamp_clocks) |dir| {
        var ts: linux.timespec = undefined;
        _ = linux.clock_gettime(.REALTIME, &ts);
        const n = saves.stampDir(init.io, init.gpa, dir, @intCast(ts.sec)) catch |e| fail("stamping the clock saves in {s} failed: {s}", .{ dir, @errorName(e) });
        std.debug.print("link: saves: {d} clock save(s) in {s} stamped\n", .{ n, dir });
        return;
    }

    // no board needed: the save slots for tools/mksd.sh, on stdout
    if (o.save_slots orelse o.state_slots orelse o.movie_slots) |dir| {
        const dir_name = o.card_dir orelse @tagName(o.system);
        const sv = saves.systemByDir(dir_name) orelse fail("no system {s}", .{dir_name});
        var out: std.ArrayList(u8) = .empty;
        defer out.deinit(init.gpa);
        if (o.movie_frames) |f| saves.movie_frames = f;
        (if (o.save_slots != null) saves.slots(init.io, init.gpa, sv, dir, &out) else if (o.state_slots != null) saves.stateSlots(init.io, init.gpa, sv, dir, &out) else saves.movieSlots(init.io, init.gpa, sv, dir, &out)) catch |e|
            fail("can't list {s} ({s})", .{ dir, @errorName(e) });
        std.Io.File.stdout().writeStreamingAll(init.io, out.items) catch fail("write failed", .{});
        return;
    }

    // the key maps first: a bad file stops link before it talks to the board
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const map_path: [:0]const u8 = o.keymap orelse remap.defaultPath(init.environ_map, &path_buf) orelse
        fail("no HOME or XDG_CONFIG_HOME for the key map file; give --keymap FILE", .{});
    // no board needed: a mounted card's keymap.conf, if the board wrote it
    // (make sd-sync, before the card is written)
    if (o.keymap_take) |file| {
        const fz = init.arena.allocator().dupeZ(u8, file) catch fail("out of memory", .{});
        var cb: [remap.max_file]u8 = undefined;
        const t = remap.readFile(fz, &cb) catch |e| fail("can't read {s} ({s})", .{ file, @errorName(e) });
        takeKeymap(map_path, t orelse "");
        return;
    }
    var config = keymap.Config.initDefaults();
    var file_buf: [remap.max_file]u8 = undefined;
    const text = remap.readFile(map_path, &file_buf) catch |e| fail("can't read the key map {s} ({s})", .{ map_path, @errorName(e) });
    if (text) |t| {
        var err: keymap.ParseError = undefined;
        config = keymap.parse(t, &err) catch fail("{s}:{d}: {s}", .{ map_path, err.line, err.msg });
    } else if (o.keymap != null and !o.remap and !o.settings) fail("no key map file {s}", .{map_path});
    const map_source: []const u8 = if (text != null) map_path else "defaults";
    if (o.show_keymap) {
        std.debug.print("link: {s} key map ({s}):\n", .{ o.system.title(), map_source });
        remap.show(config.map(o.system));
        return;
    }
    if (o.remap) {
        if (!remap.run(&config, o.system, map_path, o.grab, o.device)) std.process.exit(1);
        return;
    }
    if (o.settings) {
        if (!settings.run(config, o.system, map_path, o.device)) std.process.exit(1);
        return;
    }
    const lookup = keymap.Lookup.init(config.map(o.system));

    var s = Serial.open(o.port) catch fail("can't open {s} (J14 connected? dialout group?)", .{o.port});
    // the default cache, per port (side track G phase 2: the ZedBoard's
    // bridge stops at 230400, the Arty's does 2000000)
    var cache_buf: [512]u8 = undefined;
    const cache: [:0]const u8 = if (std.mem.eql(u8, o.baud_cache, default_baud_cache))
        portCache(&cache_buf, o.port)
    else
        o.baud_cache;
    const rate = negotiate(&s, o.fast, cache);
    const fast = rate != 0;
    std.debug.print("link: relay on {s} at {d} baud\n", .{ o.port, protocol.rates[rate] });

    if (o.ping > 0) {
        measurePing(&s, o.ping);
        return;
    }
    if (o.saves_pull) |dst| {
        pullSaves(&s, init.io, init.gpa, dst);
        if (o.states_dir) |sd| pullStates(&s, init.io, init.gpa, sd);
        if (o.movies_dir) |md| pullMovies(&s, init.io, init.gpa, md);
        if (o.screens_dir) |sd| pullScreens(&s, init.io, init.gpa, sd);
        if (o.keymap_pull) pullKeymap(&s, map_path);
        return;
    }
    if (o.screenshot) |dst| {
        screenshot(&s, init.io, init.gpa, dst);
        return;
    }
    if (o.movies_pull) |dst| {
        pullMovies(&s, init.io, init.gpa, dst);
        return;
    }
    if (o.movie_put) |file| {
        putMovie(&s, init.io, init.gpa, file, o.movie_slot orelse fail("--movie-put needs --movie-slot PATH (/movies/...)", .{}));
        return;
    }
    if (o.sd_image) |path| {
        const image = std.Io.Dir.cwd().readFileAlloc(init.io, path, init.gpa, .limited(1 << 30)) catch fail("can't read {s}", .{path});
        defer init.gpa.free(image);
        writeSd(&s, image, init.gpa, o.sd_rewrite);
        return;
    }
    if (o.ddr_load) |path| {
        const data = std.Io.Dir.cwd().readFileAlloc(init.io, path, init.gpa, .limited(ddr_load_max)) catch fail("can't read {s} (or it is over 16 MiB)", .{path});
        defer init.gpa.free(data);
        ddrLoad(&s, data, fast);
        return;
    }
    if (o.ddr_test) |spec| {
        ddrTest(&s, spec);
        return;
    }
    if (o.load) |path| {
        const limit: usize = if (o.system == .chip8) 4096 else cartridgeLimit(o.system) + 1;
        const rom = std.Io.Dir.cwd().readFileAlloc(init.io, path, init.gpa, .limited(limit)) catch fail("can't read {s} (or it is too large)", .{path});
        defer init.gpa.free(rom);
        const bios: ?[]u8 = if (o.bios) |b| std.Io.Dir.cwd().readFileAlloc(init.io, b, init.gpa, .limited(1 << 20)) catch fail("can't read {s} (or it is over 1 MiB)", .{b}) else null;
        defer if (bios) |b| init.gpa.free(b);
        if (o.system == .chip8) loadRom(&s, rom, o.presets[0..o.n_presets], fast) else loadCartridge(&s, rom, fast, o.system, bios);
        if (!o.input) return;
    }
    if (!o.input) return;

    var act: linux.Sigaction = .{ .handler = .{ .handler = onSignal }, .mask = linux.sigemptyset(), .flags = 0 };
    _ = linux.sigaction(linux.SIG.INT, &act, null);
    _ = linux.sigaction(linux.SIG.TERM, &act, null);

    var devices: [max_devices]evdev.Device = undefined;
    var n_dev: usize = 0;
    var open_mask = std.StaticBitSet(max_event_index).initEmpty();
    var state: keymap.Word = 0;
    var last_send: i64 = 0;
    var frame_prev: u3 = 0;
    const ino: i32 = blk: {
        const fd = sys.check(linux.inotify_init1(linux.IN.NONBLOCK | linux.IN.CLOEXEC)) catch break :blk -1;
        _ = sys.check(linux.inotify_add_watch(@intCast(fd), "/dev/input", linux.IN.CREATE | linux.IN.ATTRIB)) catch {};
        break :blk @intCast(fd);
    };
    // every device once now; later only the ones inotify names
    var to_try = std.StaticBitSet(max_event_index).initFull();
    var line_buf: [128]u8 = undefined;
    std.debug.print("link: {s} key map: {s} (link --show-keymap lists it, make keymap SYSTEM={s} changes it). Ctrl-C quits.\n", .{ o.system.title(), map_source, @tagName(o.system) });
    remap.warnDuplicates(config.map(o.system));

    while (!stop.load(.monotonic)) {
        var it = to_try.iterator(.{});
        while (it.next()) |i| {
            if (open_mask.isSet(i) or n_dev == max_devices) continue;
            if (evdev.openDevice(@intCast(i), o.grab, o.device)) |d| {
                devices[n_dev] = d;
                n_dev += 1;
                open_mask.set(i);
                std.debug.print("link: + {s} ({s}, event{d})\n", .{ d.nameSlice(), @tagName(d.kind), i });
            }
        }
        to_try = .initEmpty();

        var fds: [max_devices + 2]linux.pollfd = undefined;
        for (devices[0..n_dev], 0..) |d, i| fds[i] = .{ .fd = d.fd, .events = linux.POLL.IN, .revents = 0 };
        fds[n_dev] = .{ .fd = s.fd, .events = linux.POLL.IN, .revents = 0 };
        fds[n_dev + 1] = .{ .fd = ino, .events = linux.POLL.IN, .revents = 0 };
        _ = sys.poll(fds[0 .. n_dev + 2], 100);

        // hotplug: "eventN" created, or its permissions set by udev a moment later
        if (fds[n_dev + 1].revents & linux.POLL.IN != 0) {
            var ibuf: [1024]u8 align(4) = undefined;
            const n = sys.read(ino, &ibuf) catch 0;
            var off: usize = 0;
            while (off + 16 <= n) {
                const len = std.mem.readInt(u32, ibuf[off + 12 ..][0..4], .little);
                const name = std.mem.sliceTo(ibuf[off + 16 .. off + 16 + len], 0);
                if (std.mem.startsWith(u8, name, "event")) {
                    if (std.fmt.parseInt(u8, name[5..], 10)) |idx| {
                        if (idx < max_event_index) to_try.set(idx);
                    } else |_| {}
                }
                off += 16 + len;
            }
        }

        var newest: ?i64 = null;
        var i: usize = 0;
        while (i < n_dev) {
            var gone = false;
            if (fds[i].revents & (linux.POLL.IN | linux.POLL.ERR | linux.POLL.HUP) != 0) {
                var evs: [64]evdev.Event = undefined;
                while (true) {
                    const n = sys.read(devices[i].fd, std.mem.sliceAsBytes(&evs)) catch |e| {
                        if (e == error.NoDevice) gone = true;
                        break;
                    };
                    if (n == 0) break;
                    for (evs[0 .. n / @sizeOf(evdev.Event)]) |ev| {
                        if (devices[i].apply(ev)) |t| newest = t;
                    }
                }
            }
            if (gone) {
                std.debug.print("link: - {s}\n", .{devices[i].nameSlice()});
                open_mask.unset(devices[i].index);
                devices[i].close();
                devices[i] = devices[n_dev - 1];
                fds[i] = fds[n_dev - 1];
                n_dev -= 1;
                continue;
            }
            i += 1;
        }
        while (s.line(&line_buf)) |l| {
            // the system's text output (the Game Boy's serial port) always
            if (std.mem.startsWith(u8, l, "serial: ")) {
                std.debug.print("{s}\n", .{l});
            } else if (l.len > 1 and l[0] == 'g' and l[1] == '/') {
                std.debug.print("link: screenshot on the card: {s} (make screens-pull, or make sd's pull, copies it)\n", .{l[1..]});
            } else if (o.verbose and l.len > 0) std.debug.print("board: {s}\n", .{l});
        }

        var keys: keymap.Word = 0;
        for (devices[0..n_dev]) |*d| keys |= d.keys(&lookup);
        // frame advance (side track O, O.3): [global] frame_advance (F11)
        // and frame_run (F10), on their press, as F1 / F0
        var frame_now: u3 = 0;
        for (devices[0..n_dev]) |*d| {
            if (d.holds(&config.frame_advance)) frame_now |= 1;
            if (d.holds(&config.frame_run)) frame_now |= 2;
            // a screenshot (side track O, O.4): [global] screenshot (F12), as G
            if (d.holds(&config.screenshot)) frame_now |= 4;
        }
        const frame_pressed = frame_now & ~frame_prev;
        frame_prev = frame_now;
        if (frame_pressed & 1 != 0) s.writeAll("F1\n") catch fail("write to {s} failed", .{o.port});
        if (frame_pressed & 2 != 0) s.writeAll("F0\n") catch fail("write to {s} failed", .{o.port});
        if (frame_pressed & 4 != 0) s.writeAll("G\n") catch fail("write to {s} failed", .{o.port});
        const t = sys.nowUs();
        if (keys != state or t - last_send > 100_000) {
            var frame_buf: [6]u8 = undefined;
            s.writeAll(protocol.keyFrameFor(keys, &frame_buf)) catch fail("write to {s} failed", .{o.port});
            last_send = t;
            if (keys != state and o.verbose) {
                // kernel event time (CLOCK_REALTIME by default) vs now
                var ts: linux.timespec = undefined;
                _ = linux.clock_gettime(.REALTIME, &ts);
                const wall = @as(i64, ts.sec) * 1_000_000 + @divTrunc(@as(i64, ts.nsec), 1000);
                if (newest) |ev_t| std.debug.print("link: keys {X:0>4} ({d} us after the input event)\n", .{ keys, wall - ev_t }) else std.debug.print("link: keys {X:0>4}\n", .{keys});
            }
            state = keys;
        }
    }
    s.writeAll(&protocol.keyFrame(0)) catch {};
    std.debug.print("link: keys released, bye\n", .{});
}

test {
    _ = @import("saves.zig");
    _ = @import("keymap");
    _ = @import("evdev.zig");
    _ = @import("remap.zig");
    _ = @import("settings.zig");
}
