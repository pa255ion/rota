//! The Game Gear and Master System as a library (their fast version since Q.7)
//! (side track Q, PLAN.md Q.5): the C ABI of
//! systems/sms/desktop/Sms/Desktop.hs (cabal's foreign library
//! sms-desktop: Gg.Spec with the Master System's FM unit), loaded at run
//! time (spec.zig) from the build's path (`-Dsms-lib`, from make) or
//! $EMU_SMS_LIB.

const std = @import("std");
const build_options = @import("build_options");
const spec = @import("spec.zig");

/// The largest frame: the Master System's 256 × 192 (the Game Gear's
/// window is 160 × 144).
pub const max_width = 256;
pub const max_height = 192;
pub const audio_cap = 2048;
/// A frame: 262 lines of 228 CPU cycles at 3579545 Hz (59.92 Hz).
pub const frame_ns: u64 = 16_688_154;

pub const Mode = enum(c_int) { gg = 0, sms = 1, gg_sms = 2 };

/// `Machine.regs`' order (the library's sms_dbg_regs).
pub const reg_names = [22][]const u8{ "A", "F", "B", "C", "D", "E", "H", "L", "AF'", "BC'", "DE'", "HL'", "IX", "IY", "SP", "PC", "I", "R", "IFF1", "IFF2", "IM", "halted" };

const Api = struct {
    sms_new: *const fn (rom: [*]const u8, len: usize, mode: c_int, fm: c_int, bios: ?[*]const u8, bios_len: usize, reference: c_int, out: *?*anyopaque) callconv(.c) c_int,
    sms_free: *const fn (h: ?*anyopaque) callconv(.c) void,
    sms_frame: *const fn (h: ?*anyopaque, keys: u32) callconv(.c) c_int,
    sms_audio: *const fn (h: ?*anyopaque, out: [*]i16, cap: c_int) callconv(.c) c_int,
    sms_width: *const fn (h: ?*anyopaque) callconv(.c) c_int,
    sms_height: *const fn (h: ?*anyopaque) callconv(.c) c_int,
    sms_pixels: *const fn (h: ?*anyopaque, out: [*]u32) callconv(.c) void,
    sms_hash: *const fn (h: ?*anyopaque) callconv(.c) u64,
    sms_cram_size: *const fn (h: ?*anyopaque) callconv(.c) usize,
    sms_cram_get: *const fn (h: ?*anyopaque, out: [*]u8, len: usize) callconv(.c) void,
    sms_cram_set: *const fn (h: ?*anyopaque, in: [*]const u8, len: usize) callconv(.c) void,
    sms_state_save: *const fn (h: ?*anyopaque) callconv(.c) usize,
    sms_state_get: *const fn (h: ?*anyopaque, out: [*]u8, len: usize) callconv(.c) void,
    sms_state_load: *const fn (h: ?*anyopaque, in: [*]const u8, len: usize) callconv(.c) c_int,
    // the debugger (side track R): the fast version only, -1 otherwise
    sms_dbg_regs: *const fn (h: ?*anyopaque, out: *[22]u16) callconv(.c) c_int,
    sms_dbg_peek: *const fn (h: ?*anyopaque, a: u16) callconv(.c) c_int,
    sms_dbg_poke: *const fn (h: ?*anyopaque, a: u16, v: u8) callconv(.c) c_int,
    sms_dbg_vram: *const fn (h: ?*anyopaque, a: u16) callconv(.c) c_int,
    sms_dbg_vdp: *const fn (h: ?*anyopaque, i: c_int) callconv(.c) c_int,
    sms_dbg_step: *const fn (h: ?*anyopaque) callconv(.c) c_int,
    sms_dbg_run: *const fn (h: ?*anyopaque, keys: u32, bps: [*]const u16, n: c_int) callconv(.c) c_int,
};

var api: ?Api = null;

pub fn start() spec.LoadError!*const Api {
    if (api == null) api = try spec.load(Api, build_options.sms_lib, "EMU_SMS_LIB");
    return &api.?;
}

pub const Machine = struct {
    h: ?*anyopaque,
    api: *const Api,

    pub const Error = spec.LoadError || error{BadMode};

    /// `fm`: the Master System's FM unit; `bios`: its BIOS image (the
    /// user's own dump), or none.
    pub fn init(rom: []const u8, mode: Mode, fm: bool, bios: ?[]const u8) Error!Machine {
        return initWith(rom, mode, fm, bios, false);
    }

    /// `reference`: the reference model, not the fast version (the
    /// desktop's --reference).
    pub fn initWith(rom: []const u8, mode: Mode, fm: bool, bios: ?[]const u8, reference: bool) Error!Machine {
        const a = try start();
        var h: ?*anyopaque = null;
        const b = bios orelse &[_]u8{};
        if (a.sms_new(rom.ptr, rom.len, @intFromEnum(mode), @intFromBool(fm), if (bios != null) b.ptr else null, b.len, @intFromBool(reference), &h) != 0) return error.BadMode;
        return .{ .h = h, .api = a };
    }

    pub fn deinit(m: *Machine) void {
        m.api.sms_free(m.h);
        m.h = null;
    }

    /// One frame with the key word `w` held (the key maps' bits); its
    /// stereo samples.
    pub fn frame(m: *Machine, w: u32) usize {
        return @intCast(m.api.sms_frame(m.h, w));
    }

    pub fn audio(m: *Machine, out: []i16) usize {
        return @intCast(m.api.sms_audio(m.h, out.ptr, @intCast(out.len / 2)));
    }

    pub fn width(m: *Machine) c_int {
        return m.api.sms_width(m.h);
    }

    pub fn height(m: *Machine) c_int {
        return m.api.sms_height(m.h);
    }

    /// The frame, `width` × `height` pixels of 0xFFRRGGBB.
    pub fn pixels(m: *Machine, out: []u32) void {
        std.debug.assert(out.len >= @as(usize, @intCast(m.width() * m.height())));
        m.api.sms_pixels(m.h, out.ptr);
    }

    /// Gg.Screen's frameHash of the frame (the goldens' hashes).
    pub fn hash(m: *Machine) u64 {
        return m.api.sms_hash(m.h);
    }

    // --- the debugger (side track R, R.1; the fast version only:
    // error.NoDebugger on the reference model)

    /// The Z80's registers, in the order of `reg_names`.
    pub fn regs(m: *Machine) error{NoDebugger}![22]u16 {
        var out: [22]u16 = undefined;
        if (m.api.sms_dbg_regs(m.h, &out) < 0) return error.NoDebugger;
        return out;
    }

    /// A byte of the CPU's map, without side effects.
    pub fn peek(m: *Machine, a: u16) error{NoDebugger}!u8 {
        const v = m.api.sms_dbg_peek(m.h, a);
        return if (v < 0) error.NoDebugger else @intCast(v);
    }

    /// A write as the CPU's (RAM, cartridge RAM, the mapper's registers).
    pub fn poke(m: *Machine, a: u16, v: u8) error{NoDebugger}!void {
        if (m.api.sms_dbg_poke(m.h, a, v) < 0) return error.NoDebugger;
    }

    pub fn vram(m: *Machine, a: u16) error{NoDebugger}!u8 {
        const v = m.api.sms_dbg_vram(m.h, a);
        return if (v < 0) error.NoDebugger else @intCast(v);
    }

    pub fn vdpReg(m: *Machine, i: u8) error{NoDebugger}!u8 {
        const v = m.api.sms_dbg_vdp(m.h, i);
        return if (v < 0) error.NoDebugger else @intCast(v);
    }

    /// One instruction; true if a frame ended in it (its sound then in
    /// `audio`).
    pub fn step(m: *Machine) error{NoDebugger}!bool {
        const v = m.api.sms_dbg_step(m.h);
        return if (v < 0) error.NoDebugger else v == 1;
    }

    pub const Stop = enum { frame_end, breakpoint };

    /// The rest of the frame with the key word `w`, stopped before an
    /// instruction at one of `bps` (not before the first); the sound so
    /// far in `audio` either way.
    pub fn run(m: *Machine, w: u32, bps: []const u16) error{NoDebugger}!Stop {
        const v = m.api.sms_dbg_run(m.h, w, bps.ptr, @intCast(bps.len));
        return switch (v) {
            0 => .frame_end,
            1 => .breakpoint,
            else => error.NoDebugger,
        };
    }

    pub fn cramSize(m: *Machine) usize {
        return m.api.sms_cram_size(m.h);
    }

    pub fn cramGet(m: *Machine, out: []u8) void {
        m.api.sms_cram_get(m.h, out.ptr, out.len);
    }

    pub fn cramSet(m: *Machine, in: []const u8) void {
        m.api.sms_cram_set(m.h, in.ptr, in.len);
    }

    /// A save state (Q.8; the fast version only: error.NoStates on the
    /// reference model), as bytes the caller frees.
    pub fn stateSave(m: *Machine, gpa: std.mem.Allocator) ![]u8 {
        return spec.stateSave(gpa, m.h, m.api.sms_state_save, m.api.sms_state_get);
    }

    /// The machine as the state says; false (nothing changed) if it
    /// doesn't fit.
    pub fn stateLoad(m: *Machine, blob: []const u8) bool {
        return m.api.sms_state_load(m.h, blob.ptr, blob.len) == 0;
    }
};

// --- tests (make test-desktop): the library against the spec's goldens

const testing = std.testing;

fn readFile(path: []const u8) ![]u8 {
    return std.Io.Dir.cwd().readFileAlloc(testing.io, path, testing.allocator, .limited(4 << 20)) catch |e| {
        std.debug.print("sms_spec test: {s}: {s} (tools/fetch-roms.sh)\n", .{ path, @errorName(e) });
        return e;
    };
}

/// Gg.Input's script ("FRAME BUTTONS", the buttons from that frame on) as
/// the key word at frame `k`: U D L R 1 2 S, and u d l r 3 4 for the
/// Master System's pad 2.
fn inputAt(script: []const u8, k: usize) u32 {
    var w: u32 = 0;
    var lines = std.mem.splitScalar(u8, script, '\n');
    while (lines.next()) |raw| {
        const line = if (std.mem.indexOfScalar(u8, raw, '#')) |c| raw[0..c] else raw;
        var it = std.mem.tokenizeAny(u8, line, " \t\r");
        const f = it.next() orelse continue;
        const bs = it.next() orelse continue;
        const at = std.fmt.parseInt(usize, f, 10) catch continue;
        if (at > k) break;
        w = 0;
        for (bs) |ch| w |= switch (ch) {
            'R' => 1 << 0, 'L' => 1 << 1, 'U' => 1 << 2, 'D' => 1 << 3, '2' => 1 << 4, '1' => 1 << 5, 'S' => 1 << 7,
            'r' => 1 << 8, 'l' => 1 << 9, 'u' => 1 << 10, 'd' => 1 << 11, '4' => 1 << 12, '3' => 1 << 13,
            else => 0,
        };
    }
    return w;
}

/// Plays `script` and compares the frame hashes at the golden's frames
/// (lines "FRAME HASH ...", as the gg and sms spec tests).
fn expectHashes(rom_path: []const u8, mode: Mode, script_path: []const u8, golden_path: []const u8) !void {
    const rom = try readFile(rom_path);
    defer testing.allocator.free(rom);
    const script = try readFile(script_path);
    defer testing.allocator.free(script);
    const golden = try readFile(golden_path);
    defer testing.allocator.free(golden);
    var m = try Machine.init(rom, mode, false, null);
    defer m.deinit();
    var k: usize = 0;
    var lines = std.mem.tokenizeScalar(u8, golden, '\n');
    while (lines.next()) |line| {
        var it = std.mem.tokenizeScalar(u8, line, ' ');
        const at = try std.fmt.parseInt(usize, it.next().?, 10);
        const want = try std.fmt.parseInt(u64, it.next().?, 16);
        while (k <= at) : (k += 1) _ = m.frame(inputAt(script, k));
        if (m.hash() != want) std.debug.print("sms_spec test: {s} frame {d}: hash {x:0>16}, want {x:0>16}\n", .{ rom_path, at, m.hash(), want });
        try testing.expectEqual(want, m.hash());
    }
}

test "Waternet (Game Gear) with its script: the spec's frame hashes" {
    try expectHashes("roms/gg/games/waternet.gg", .gg, "systems/gg/test/input/waternet-start.txt", "systems/gg/test/golden/waternet.txt");
    // and the window's size
    const rom = try readFile("roms/gg/games/waternet.gg");
    defer testing.allocator.free(rom);
    var m = try Machine.init(rom, .gg, false, null);
    defer m.deinit();
    _ = m.frame(0);
    try testing.expectEqual(@as(c_int, 160), m.width());
    try testing.expectEqual(@as(c_int, 144), m.height());
}

test "Raposa do Sol (Master System, no FM) with its script: the spec's frame hashes" {
    try expectHashes("roms/sms/games/raposadosol_v20.sms", .sms, "systems/sms/test/input/raposa-start.txt", "systems/sms/test/golden/raposadosol.txt");
}

test "a Master System frame's sound with the FM unit: about 801 stereo samples, the picture 256 x 192" {
    const rom = try readFile("roms/sms/games/raposadosol_v20.sms");
    defer testing.allocator.free(rom);
    var m = try Machine.init(rom, .sms, true, null);
    defer m.deinit();
    var buf: [2 * audio_cap]i16 = undefined;
    _ = m.frame(0);
    for (0..30) |_| {
        const n = m.frame(0);
        try testing.expect(n >= 799 and n <= 803);
        try testing.expectEqual(n, m.audio(&buf));
    }
    try testing.expectEqual(@as(c_int, 256), m.width());
    try testing.expectEqual(@as(c_int, 192), m.height());
    try testing.expectEqual(@as(usize, 0x8000), m.cramSize());
}

test "--reference (the reference model): Raposa do Sol's pictures and FM sound the same as the fast version's" {
    const rom = try readFile("roms/sms/games/raposadosol_v20.sms");
    defer testing.allocator.free(rom);
    var f = try Machine.init(rom, .sms, true, null);
    defer f.deinit();
    var r = try Machine.initWith(rom, .sms, true, null, true);
    defer r.deinit();
    var fa: [2 * audio_cap]i16 = undefined;
    var ra: [2 * audio_cap]i16 = undefined;
    var fp: [max_width * 192]u32 = undefined;
    var rp: [max_width * 192]u32 = undefined;
    for (0..240) |k| {
        // Start (Pause on the Master System is bit 7) left alone; buttons now and then
        const w: u32 = if (k % 40 >= 30) 0x30 else 0;
        const n = f.frame(w);
        try testing.expectEqual(n, r.frame(w));
        try testing.expectEqualSlices(i16, ra[0 .. r.audio(&ra) * 2], fa[0 .. f.audio(&fa) * 2]);
        if (k % 30 == 29) {
            f.pixels(&fp);
            r.pixels(&rp);
            try testing.expectEqualSlices(u32, &rp, &fp);
        }
    }
}

test "the input script's letters as the key maps' bits" {
    try testing.expectEqual(@as(u32, (1 << 2) | (1 << 5)), inputAt("0 -\n700 U1\n760 L\n", 720));
    try testing.expectEqual(@as(u32, 0), inputAt("0 -\n700 U1\n", 10));
}

fn stateStep(m: *Machine, k: usize) u64 {
    // button 1 and Right now and then
    const n = m.frame(if (k % 30 < 5) (1 << 5) | (1 << 0) else 0);
    var px: [max_width * max_height]u32 = undefined;
    var snd: [2 * audio_cap]i16 = undefined;
    _ = m.audio(&snd);
    m.pixels(px[0..@intCast(m.width() * m.height())]);
    var h = std.hash.Wyhash.init(n);
    h.update(std.mem.sliceAsBytes(px[0..@intCast(m.width() * m.height())]));
    h.update(std.mem.sliceAsBytes(snd[0 .. 2 * n]));
    return h.final();
}

test "a save state: loaded into a new machine, the same pictures and sound (Waternet; Raposa do Sol; fmtune.sms on the FM unit); none on the reference model" {
    const Case = struct { path: []const u8, mode: Mode, fm: bool };
    const cases = [_]Case{
        .{ .path = "roms/gg/games/waternet.gg", .mode = .gg, .fm = false },
        .{ .path = "roms/sms/games/raposadosol_v20.sms", .mode = .sms, .fm = false },
        .{ .path = "build/sms/asm/fmtune.sms", .mode = .sms, .fm = true },
    };
    for (cases) |c| {
        const rom = try readFile(c.path);
        defer testing.allocator.free(rom);
        var m = try Machine.init(rom, c.mode, c.fm, null);
        defer m.deinit();
        for (0..120) |k| _ = stateStep(&m, k);
        var fresh = try Machine.init(rom, c.mode, c.fm, null);
        defer fresh.deinit();
        try spec.expectStateReplays(&m, &fresh, 120, stateStep);
    }
    const rom = try readFile("roms/gg/games/waternet.gg");
    defer testing.allocator.free(rom);
    var r = try Machine.initWith(rom, .gg, false, null, true);
    defer r.deinit();
    try testing.expectError(error.NoStates, r.stateSave(testing.allocator));
}

test "an input movie of Waternet's script (build/desktop/waternet.mv1, for make test-desktop's gg-run and desktop runs)" {
    const movie = @import("movie");
    const script = try readFile("systems/gg/test/input/waternet-start.txt");
    defer testing.allocator.free(script);
    var words: [600]u16 = undefined;
    for (&words, 0..) |*x, k| x.* = @truncate(inputAt(script, k));
    const bytes = try movie.encodeFile(testing.allocator, .{ .system = 7, .frames = 0 }, &.{}, &words);
    defer testing.allocator.free(bytes);
    try std.Io.Dir.cwd().createDirPath(testing.io, "build/desktop");
    try std.Io.Dir.cwd().writeFile(testing.io, .{ .sub_path = "build/desktop/waternet.mv1", .data = bytes });
    const d = try movie.decodeFile(testing.allocator, bytes);
    defer testing.allocator.free(d.words);
    try testing.expectEqualSlices(u16, &words, d.words);
}

test "a movie from a state (build/desktop/waternet-state.mv1: Waternet's script, the state at frame 300, then its frames 300-599; for make test-desktop)" {
    const movie = @import("movie");
    const script = try readFile("systems/gg/test/input/waternet-start.txt");
    defer testing.allocator.free(script);
    const rom = try readFile("roms/gg/games/waternet.gg");
    defer testing.allocator.free(rom);
    var m = try Machine.init(rom, .gg, false, null);
    defer m.deinit();
    for (0..300) |k| _ = m.frame(inputAt(script, k));
    const state = try m.stateSave(testing.allocator);
    defer testing.allocator.free(state);
    var words: [300]u16 = undefined;
    for (&words, 300..) |*x, k| x.* = @truncate(inputAt(script, k));
    const bytes = try movie.encodeFile(testing.allocator, .{ .system = 7, .frames = 0, .rom_size = @intCast(rom.len) }, state, &words);
    defer testing.allocator.free(bytes);
    try std.Io.Dir.cwd().createDirPath(testing.io, "build/desktop");
    try std.Io.Dir.cwd().writeFile(testing.io, .{ .sub_path = "build/desktop/waternet-state.mv1", .data = bytes });
    const d = try movie.decodeFile(testing.allocator, bytes);
    defer testing.allocator.free(d.words);
    try testing.expectEqual(movie.Start.state, d.header.start);
    try testing.expectEqualSlices(u8, state, d.state);
}

test "the debugger (R.1): a breakpoint stops before its instruction, step runs one, the run goes on from it; peek and poke; none on the reference model" {
    const rom = try readFile("roms/gg/games/waternet.gg");
    defer testing.allocator.free(rom);
    var m = try Machine.init(rom, .gg, false, null);
    defer m.deinit();
    for (0..60) |_| _ = m.frame(0);
    // a PC the game passes every frame: where it is at the next frame's end
    while ((try m.run(0, &.{})) != .frame_end) {}
    const pc = (try m.regs())[15];
    // from here a breakpoint at that PC stops there, before running it
    // (within a few frames: the game comes back to it)
    var hit = false;
    for (0..5) |_| if ((try m.run(0, &.{pc})) == .breakpoint) {
        hit = true;
        break;
    };
    try testing.expect(hit);
    try testing.expectEqual(pc, (try m.regs())[15]);
    // one step moves on; a run from a breakpoint doesn't stop at once
    _ = try m.step();
    try testing.expect((try m.regs())[15] != pc or (try m.regs())[21] == 1);
    // RAM: a poke reads back, through the mirror too ($C000-$DFFF at $E000)
    const old = try m.peek(0xC100);
    try m.poke(0xC100, old ^ 0xA5);
    try testing.expectEqual(old ^ 0xA5, try m.peek(0xC100));
    try testing.expectEqual(old ^ 0xA5, try m.peek(0xE100));
    try m.poke(0xC100, old);
    _ = try m.vdpReg(1);
    _ = try m.vram(0x3800);
    var r = try Machine.initWith(rom, .gg, false, null, true);
    defer r.deinit();
    try testing.expectError(error.NoDebugger, r.regs());
}
