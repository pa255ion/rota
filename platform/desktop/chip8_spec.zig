//! CHIP-8's reference model as a library (side track Q, PLAN.md Q.5):
//! the C ABI of systems/chip8/desktop/Chip8/Desktop.hs (cabal's foreign
//! library chip8-desktop), loaded at run time (spec.zig) from the build's
//! path (`-Dchip8-lib`, from make) or $EMU_CHIP8_LIB.

const std = @import("std");
const build_options = @import("build_options");
const spec = @import("spec.zig");

pub const width = 64;
pub const height = 32;
pub const audio_cap = 800;
/// 60 Hz.
pub const frame_ns: u64 = 16_666_667;
/// The board's speeds (docs/systems/chip8.md, SW5): about 700 Hz (12 a
/// frame), and about 10 kHz (167) for the test ROMs.
pub const ipf_slow = 12;
pub const ipf_fast = 167;

const Api = struct {
    chip8_new: *const fn (rom: [*]const u8, len: usize, quirks: u8, ipf: c_int, out: *?*anyopaque) callconv(.c) c_int,
    chip8_free: *const fn (h: ?*anyopaque) callconv(.c) void,
    chip8_frame: *const fn (h: ?*anyopaque, keys: u16) callconv(.c) c_int,
    chip8_audio: *const fn (h: ?*anyopaque, out: [*]i16, cap: c_int) callconv(.c) c_int,
    chip8_pixels: *const fn (h: ?*anyopaque, out: [*]u32) callconv(.c) void,
    chip8_halted: *const fn (h: ?*anyopaque) callconv(.c) c_int,
    chip8_state_save: *const fn (h: ?*anyopaque) callconv(.c) usize,
    chip8_state_get: *const fn (h: ?*anyopaque, out: [*]u8, len: usize) callconv(.c) void,
    chip8_state_load: *const fn (h: ?*anyopaque, in: [*]const u8, len: usize) callconv(.c) c_int,
};

var api: ?Api = null;

pub fn start() spec.LoadError!*const Api {
    if (api == null) api = try spec.load(Api, build_options.chip8_lib, "EMU_CHIP8_LIB");
    return &api.?;
}

pub const Machine = struct {
    h: ?*anyopaque,
    api: *const Api,

    pub const Error = spec.LoadError || error{RomRefused};

    /// `quirks`: the board's SW0 to SW4 bits (0: the COSMAC VIP's).
    pub fn init(rom: []const u8, quirks: u8, ipf: u16) Error!Machine {
        const a = try start();
        var h: ?*anyopaque = null;
        if (a.chip8_new(rom.ptr, rom.len, quirks, ipf, &h) != 0) return error.RomRefused;
        return .{ .h = h, .api = a };
    }

    pub fn deinit(m: *Machine) void {
        m.api.chip8_free(m.h);
        m.h = null;
    }

    /// One 60 Hz frame with `keys` held (bit k = key k); its stereo samples.
    pub fn frame(m: *Machine, keys: u16) usize {
        return @intCast(m.api.chip8_frame(m.h, keys));
    }

    pub fn audio(m: *Machine, out: []i16) usize {
        return @intCast(m.api.chip8_audio(m.h, out.ptr, @intCast(out.len / 2)));
    }

    pub fn pixels(m: *Machine, out: *[width * height]u32) void {
        m.api.chip8_pixels(m.h, out);
    }

    pub fn halted(m: *Machine) bool {
        return m.api.chip8_halted(m.h) != 0;
    }

    /// A save state (Q.8), as bytes the caller frees.
    pub fn stateSave(m: *Machine, gpa: std.mem.Allocator) ![]u8 {
        return spec.stateSave(gpa, m.h, m.api.chip8_state_save, m.api.chip8_state_get);
    }

    /// The machine as the state says; false (nothing changed) if it
    /// doesn't fit (another program, quirks or speed).
    pub fn stateLoad(m: *Machine, blob: []const u8) bool {
        return m.api.chip8_state_load(m.h, blob.ptr, blob.len) == 0;
    }
};

// --- tests (make test-desktop): the library against the spec's goldens

const testing = std.testing;
const png = @import("png");

fn readFile(path: []const u8) ![]u8 {
    return std.Io.Dir.cwd().readFileAlloc(testing.io, path, testing.allocator, .limited(1 << 20)) catch |e| {
        std.debug.print("chip8_spec test: {s}: {s} (tools/fetch-roms.sh)\n", .{ path, @errorName(e) });
        return e;
    };
}

/// As Chip8 spec's romGolden (runRom: VIP quirks, 200 instructions a
/// frame): `n` frames, then the display against the golden.
fn expectGolden(name: []const u8, keys: u16, n: usize) !void {
    var buf: [256]u8 = undefined;
    const rom = try readFile(try std.fmt.bufPrint(&buf, "roms/chip8/timendus/{s}.ch8", .{name}));
    defer testing.allocator.free(rom);
    var m = try Machine.init(rom, 0, 200);
    defer m.deinit();
    for (0..n) |_| _ = m.frame(keys);
    try testing.expect(!m.halted());
    var px: [width * height]u32 = undefined;
    m.pixels(&px);
    const bytes = try readFile(try std.fmt.bufPrint(&buf, "systems/chip8/test/golden/spec/{s}.png", .{name}));
    defer testing.allocator.free(bytes);
    var img = try png.decode(testing.allocator, bytes);
    defer img.deinit(testing.allocator);
    try testing.expectEqual(@as(u32, width), img.width);
    var diff: usize = 0;
    for (px, 0..) |p, k| {
        const want = 0xFF000000 | (@as(u32, img.rgb[3 * k]) << 16) | (@as(u32, img.rgb[3 * k + 1]) << 8) | img.rgb[3 * k + 2];
        if (p != want) diff += 1;
    }
    if (diff != 0) std.debug.print("chip8_spec test: {s}: {d} pixels differ\n", .{ name, diff });
    try testing.expectEqual(@as(usize, 0), diff);
}

test "the IBM logo through the library: the spec's golden after 60 frames" {
    try expectGolden("2-ibm-logo", 0, 60);
}

test "corax+ (the opcode test): the spec's golden after 300 frames" {
    try expectGolden("3-corax+", 0, 300);
}

test "the beeper: 800 samples a frame, silent with the sound timer at 0" {
    // 6000 0x6005 F518: V0 = 5, sound timer = V0, then loop (1206)
    const rom = [_]u8{ 0x60, 0x05, 0xF0, 0x18, 0x12, 0x04 };
    var m = try Machine.init(&rom, 0, 12);
    defer m.deinit();
    var buf: [2 * audio_cap]i16 = undefined;
    _ = m.frame(0); // sets the timer
    try testing.expectEqual(@as(usize, 800), m.frame(0));
    _ = m.audio(&buf);
    var loud = false;
    for (buf) |x| if (x != 0) {
        loud = true;
    };
    try testing.expect(loud);
    for (0..10) |_| _ = m.frame(0); // the timer runs out
    _ = m.audio(&buf);
    for (buf) |x| try testing.expectEqual(@as(i16, 0), x);
}

test "an empty or too long program is refused" {
    try testing.expectError(error.RomRefused, Machine.init("", 0, 12));
    const big = [_]u8{0} ** 0xE01;
    try testing.expectError(error.RomRefused, Machine.init(&big, 0, 12));
}

fn stateStep(m: *Machine, k: usize) u64 {
    // key 5 (Dino Run's jump; Space Invaders' fire) now and then
    const n = m.frame(if (k % 25 < 4) 1 << 5 else 0);
    var px: [width * height]u32 = undefined;
    var snd: [2 * audio_cap]i16 = undefined;
    _ = m.audio(&snd);
    m.pixels(&px);
    var h = std.hash.Wyhash.init(n);
    h.update(std.mem.sliceAsBytes(&px));
    h.update(std.mem.sliceAsBytes(snd[0 .. 2 * n]));
    return h.final();
}

test "a save state: loaded into a new machine, the same pictures and sound (Dino Run, Space Invaders, the beep test); refused with other quirks" {
    for ([_][]const u8{ "roms/chip8/games/dinorun.ch8", "roms/chip8/games/space-invaders.ch8", "roms/chip8/timendus/7-beep.ch8" }) |f| {
        const rom = try readFile(f);
        defer testing.allocator.free(rom);
        var m = try Machine.init(rom, 0, 12);
        defer m.deinit();
        for (0..120) |k| _ = stateStep(&m, k);
        var fresh = try Machine.init(rom, 0, 12);
        defer fresh.deinit();
        try spec.expectStateReplays(&m, &fresh, 120, stateStep);
        var other = try Machine.init(rom, 1, 12);
        defer other.deinit();
        const blob = try m.stateSave(testing.allocator);
        defer testing.allocator.free(blob);
        try testing.expect(!other.stateLoad(blob));
    }
}
