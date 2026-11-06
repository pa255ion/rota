//! The NES as a library (side track Q, PLAN.md Q.4; its fast version since Q.7):
//! the C ABI of systems/nes/desktop/Nes/Desktop.hs (cabal's foreign
//! library nes-desktop), loaded at run time (spec.zig) from the build's
//! path (`-Dnes-lib`, from make) or $EMU_NES_LIB.

const std = @import("std");
const build_options = @import("build_options");
const spec = @import("spec.zig");

pub const width = 256;
pub const height = 240;
/// Stereo samples a frame can hand over (Nes.Desktop's audioCap).
pub const audio_cap = 2048;
pub const sample_rate = 48000;
/// A frame's length: 89341.5 PPU dots of 21477272 / 4 Hz (60.10 Hz).
pub const frame_ns: u64 = 16_639_267;

/// Controller 1's buttons, in $4016's order (Nes.Spec.Machine's setButtons).
pub const Pad = packed struct(u8) {
    a: bool = false,
    b: bool = false,
    select: bool = false,
    start: bool = false,
    up: bool = false,
    down: bool = false,
    left: bool = false,
    right: bool = false,

    /// From the key maps' word: the NES uses the Game Boy's bits (bit 0
    /// Right, 1 Left, 2 Up, 3 Down, 4 A, 5 B, 6 Select, 7 Start;
    /// gb_keys.zig), reordered as Nes.Top does for $4016.
    pub fn fromWord(w: u32) Pad {
        const bit = struct {
            fn f(x: u32, k: u5) bool {
                return (x >> k) & 1 != 0;
            }
        }.f;
        return .{ .right = bit(w, 0), .left = bit(w, 1), .up = bit(w, 2), .down = bit(w, 3), .a = bit(w, 4), .b = bit(w, 5), .select = bit(w, 6), .start = bit(w, 7) };
    }
};

const Api = struct {
    nes_new: *const fn (rom: [*]const u8, len: usize, reference: c_int, out: *?*anyopaque) callconv(.c) c_int,
    nes_free: *const fn (h: ?*anyopaque) callconv(.c) void,
    nes_frame: *const fn (h: ?*anyopaque, pad: u8) callconv(.c) c_int,
    nes_audio: *const fn (h: ?*anyopaque, out: [*]i16, cap: c_int) callconv(.c) c_int,
    nes_pixels: *const fn (h: ?*anyopaque, out: [*]u32) callconv(.c) void,
    nes_skip: *const fn (h: ?*anyopaque, on: c_int) callconv(.c) void,
    nes_state_save: *const fn (h: ?*anyopaque) callconv(.c) usize,
    nes_state_get: *const fn (h: ?*anyopaque, out: [*]u8, len: usize) callconv(.c) void,
    nes_state_load: *const fn (h: ?*anyopaque, in: [*]const u8, len: usize) callconv(.c) c_int,
};

var api: ?Api = null;

pub fn start() spec.LoadError!*const Api {
    if (api == null) api = try spec.load(Api, build_options.nes_lib, "EMU_NES_LIB");
    return &api.?;
}

pub const Machine = struct {
    h: ?*anyopaque,
    api: *const Api,

    pub const Error = spec.LoadError || error{RomRefused};

    pub fn init(rom: []const u8) Error!Machine {
        return initWith(rom, false);
    }

    /// `reference`: the reference model, not the fast version (the
    /// desktop's --reference).
    pub fn initWith(rom: []const u8, reference: bool) Error!Machine {
        const a = try start();
        var h: ?*anyopaque = null;
        if (a.nes_new(rom.ptr, rom.len, @intFromBool(reference), &h) != 0) return error.RomRefused;
        return .{ .h = h, .api = a };
    }

    pub fn deinit(m: *Machine) void {
        m.api.nes_free(m.h);
        m.h = null;
    }

    /// The next frames' picture not drawn (frame skipping, Q.7); the game
    /// runs on as ever.
    pub fn skip(m: *Machine, on: bool) void {
        m.api.nes_skip(m.h, @intFromBool(on));
    }

    /// Runs to the PPU's next frame with `pad` held; the stereo samples
    /// it made.
    pub fn frame(m: *Machine, pad: Pad) usize {
        return @intCast(m.api.nes_frame(m.h, @bitCast(pad)));
    }

    pub fn audio(m: *Machine, out: []i16) usize {
        return @intCast(m.api.nes_audio(m.h, out.ptr, @intCast(out.len / 2)));
    }

    /// The last frame, 0xFFRRGGBB (Nes.Palette's colours).
    pub fn pixels(m: *Machine, out: *[width * height]u32) void {
        m.api.nes_pixels(m.h, out);
    }

    /// A save state (Q.8; the fast version only: error.NoStates on the
    /// reference model), as bytes the caller frees.
    pub fn stateSave(m: *Machine, gpa: std.mem.Allocator) ![]u8 {
        return spec.stateSave(gpa, m.h, m.api.nes_state_save, m.api.nes_state_get);
    }

    /// The machine as the state says; false (nothing changed) if it
    /// doesn't fit.
    pub fn stateLoad(m: *Machine, blob: []const u8) bool {
        return m.api.nes_state_load(m.h, blob.ptr, blob.len) == 0;
    }
};

// --- tests (make test-desktop): the library against the spec's goldens

const testing = std.testing;
const png = @import("png");

fn readFile(path: []const u8) ![]u8 {
    return std.Io.Dir.cwd().readFileAlloc(testing.io, path, testing.allocator, .limited(16 << 20)) catch |e| {
        std.debug.print("nes_spec test: {s}: {s} (make nes-asm, tools/fetch-roms.sh)\n", .{ path, @errorName(e) });
        return e;
    };
}

/// As Nes.SpecPpu's goldens (runFramesPress 30 pad n): `n` frames, the
/// buttons from frame 30 on.
fn expectGolden(rom_path: []const u8, pad: Pad, n: usize, golden: []const u8) !void {
    const rom = try readFile(rom_path);
    defer testing.allocator.free(rom);
    var m = try Machine.init(rom);
    defer m.deinit();
    for (0..n) |f| _ = m.frame(if (f >= 30) pad else .{});
    var px: [width * height]u32 = undefined;
    m.pixels(&px);
    const bytes = try readFile(golden);
    defer testing.allocator.free(bytes);
    var img = try png.decode(testing.allocator, bytes);
    defer img.deinit(testing.allocator);
    try testing.expectEqual(@as(u32, width), img.width);
    try testing.expectEqual(@as(u32, height), img.height);
    var diff: usize = 0;
    for (px, 0..) |p, k| {
        const want = 0xFF000000 | (@as(u32, img.rgb[3 * k]) << 16) | (@as(u32, img.rgb[3 * k + 1]) << 8) | img.rgb[3 * k + 2];
        if (p != want) diff += 1;
    }
    if (diff != 0) std.debug.print("nes_spec test: {s}: {d} pixels differ from {s}\n", .{ rom_path, diff, golden });
    try testing.expectEqual(@as(usize, 0), diff);
}

test "nestest through the library: the spec's golden after 30 frames" {
    try expectGolden("roms/nes/nestest/nestest.nes", .{}, 30, "systems/nes/test/golden/spec/nestest.png");
}

test "our ppu_scene.nes: the spec's golden after 10 frames" {
    try expectGolden("build/nes/asm/ppu_scene.nes", .{}, 10, "systems/nes/test/golden/spec/ppu_scene.png");
}

test "mmc5test_v2 with Select from frame 30: the spec's golden (a pad through the library)" {
    try expectGolden("roms/nes/mmc5/mmc5test_v2.nes", .{ .select = true }, 120, "systems/nes/test/golden/spec/mmc5test_v2-exattr.png");
}

test "a frame's sound: about 799 stereo samples at 48 kHz" {
    const rom = try readFile("roms/nes/nestest/nestest.nes");
    defer testing.allocator.free(rom);
    var m = try Machine.init(rom);
    defer m.deinit();
    var buf: [2 * audio_cap]i16 = undefined;
    _ = m.frame(.{});
    var total: usize = 0;
    for (0..60) |_| {
        const n = m.frame(.{});
        try testing.expect(n >= 797 and n <= 801);
        try testing.expectEqual(n, m.audio(&buf));
        total += n;
    }
    // 60 frames of 29780.5 CPU cycles at 1789773 Hz: 0.99837 s, 47922 samples
    try testing.expect(total >= 47900 and total <= 47945);
}

test "tri_ultra.nes: the triangle at period 0 (56 kHz, how games silence it) stays inaudible" {
    // sampled once per 48 kHz sample it aliased to a 7.9 kHz whine (the
    // user heard it, 2026-10-20): the levels are filtered every CPU cycle
    // as the board's audio path does (M3 step 3.9 found the same there)
    const rom = try readFile("build/nes/asm/tri_ultra.nes");
    defer testing.allocator.free(rom);
    var m = try Machine.init(rom);
    defer m.deinit();
    var buf: [2 * audio_cap]i16 = undefined;
    for (0..30) |_| _ = m.frame(.{});
    // the board's criterion (make sim's --expect-quiet 4): consecutive
    // samples differ by at most 4 (a settling DC level passes, a whine
    // doesn't; before the fix: up to about 530)
    var prev: ?i32 = null;
    var step: i32 = 0;
    for (0..30) |_| {
        const n = m.frame(.{});
        _ = m.audio(&buf);
        var k: usize = 0;
        while (k < n) : (k += 1) {
            const x: i32 = buf[2 * k];
            if (prev) |p| step = @max(step, @as(i32, @intCast(@abs(x - p))));
            prev = x;
        }
    }
    if (step > 4) std.debug.print("nes_spec test: tri_ultra's samples step by up to {d}\n", .{step});
    try testing.expect(step <= 4);
}

test "--reference (the reference model): Thwaite's pictures and sound the same as the fast version's" {
    const rom = try readFile("roms/nes/homebrew/thwaite.nes");
    defer testing.allocator.free(rom);
    var f = try Machine.init(rom);
    defer f.deinit();
    var r = try Machine.initWith(rom, true);
    defer r.deinit();
    var fa: [2 * audio_cap]i16 = undefined;
    var ra: [2 * audio_cap]i16 = undefined;
    var fp: [width * height]u32 = undefined;
    var rp: [width * height]u32 = undefined;
    var loud: usize = 0;
    for (0..240) |k| {
        // Start now and then: through the title into a game, with sound
        const pad: Pad = .{ .start = k % 60 == 50, .a = k % 7 == 0 };
        const n = f.frame(pad);
        try testing.expectEqual(n, r.frame(pad));
        try testing.expectEqualSlices(i16, ra[0..r.audio(&ra) * 2], fa[0..f.audio(&fa) * 2]);
        for (fa[0 .. 2 * n]) |x| if (@abs(x) > 64) {
            loud += 1;
        };
        if (k % 30 == 29) {
            f.pixels(&fp);
            r.pixels(&rp);
            try testing.expectEqualSlices(u32, &rp, &fp);
        }
    }
    try testing.expect(loud > 0);
}

test "the key word's bits as $4016's" {
    const p = Pad.fromWord(0b1001_0001); // Start, A, Right
    try testing.expectEqual(@as(u8, 0b1000_1001), @as(u8, @bitCast(p)));
}

test "a file that isn't iNES is refused" {
    try testing.expectError(error.RomRefused, Machine.init("not a ROM at all, too short"));
}

fn stateStep(m: *Machine, k: usize) u64 {
    // Start now and then, A on and off: the game moves
    const n = m.frame(.{ .start = k % 60 == 10, .a = k % 16 < 4, .right = k % 32 < 16 });
    var px: [width * height]u32 = undefined;
    var snd: [2 * audio_cap]i16 = undefined;
    _ = m.audio(&snd);
    m.pixels(&px);
    var h = std.hash.Wyhash.init(n);
    h.update(std.mem.sliceAsBytes(&px));
    h.update(std.mem.sliceAsBytes(snd[0 .. 2 * n]));
    return h.final();
}

test "a save state: loaded, the same pictures and sound (Thwaite, dabg; MMC3 IRQs, mmc3_bg1000.nes; mmc5test_v2), refused from another game and on the reference model" {
    const files = [_][]const u8{ "roms/nes/homebrew/thwaite.nes", "build/nes/asm/mmc3_bg1000.nes", "roms/nes/homebrew/dabg.nes", "roms/nes/mmc5/mmc5test_v2.nes" };
    for (files) |f| {
        const rom = try readFile(f);
        defer testing.allocator.free(rom);
        var m = try Machine.init(rom);
        defer m.deinit();
        for (0..90) |k| _ = stateStep(&m, k);
        var fresh = try Machine.init(rom);
        defer fresh.deinit();
        try spec.expectStateReplays(&m, &fresh, 90, stateStep);
    }
    const a = try readFile(files[0]);
    defer testing.allocator.free(a);
    const b = try readFile(files[1]);
    defer testing.allocator.free(b);
    var ma = try Machine.init(a);
    defer ma.deinit();
    var mb = try Machine.init(b);
    defer mb.deinit();
    const blob = try mb.stateSave(testing.allocator);
    defer testing.allocator.free(blob);
    try testing.expect(!ma.stateLoad(blob));
    var mr = try Machine.initWith(a, true);
    defer mr.deinit();
    try testing.expectError(error.NoStates, mr.stateSave(testing.allocator));
}
