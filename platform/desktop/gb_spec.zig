//! The Game Boy's reference model as a library (side track Q, PLAN.md
//! Q.1): the C ABI of systems/gb/desktop/Gb/Desktop.hs (cabal's
//! foreign library gb-desktop, libgb-desktop.so), with GHC's runtime
//! started once (`hs_init`). Everything is called from one thread.
//!
//! The library is loaded at run time (spec.zig says why), from the
//! build's path (`-Dgb-lib`, from make) or $EMU_GB_LIB.

const std = @import("std");
const build_options = @import("build_options");
const spec = @import("spec.zig");

pub const width = 160;
pub const height = 144;
/// Stereo samples a frame can hand over (Gb.Desktop's audioCap).
pub const audio_cap = 2048;
/// The sample rate of `Machine.audio` (Gb.Desktop samples at 48 kHz).
pub const sample_rate = 48000;
/// A frame's length: 70224 dots of 4.194304 MHz (59.73 Hz).
pub const frame_ns: u64 = 70224 * 1_000_000_000 / 4194304;

/// The buttons held, as Gb.Spec's joypad byte.
pub const Keys = packed struct(u8) {
    right: bool = false,
    left: bool = false,
    up: bool = false,
    down: bool = false,
    a: bool = false,
    b: bool = false,
    select: bool = false,
    start: bool = false,
};

pub const Model = enum(c_int) { dmg = 0, cgb = 1 };

const Api = struct {
    gb_new: *const fn (rom: [*]const u8, len: usize, model: c_int, out: *?*anyopaque) callconv(.c) c_int,
    gb_free: *const fn (h: ?*anyopaque) callconv(.c) void,
    gb_frame: *const fn (h: ?*anyopaque, keys: u8) callconv(.c) c_int,
    gb_audio: *const fn (h: ?*anyopaque, out: [*]i16, cap: c_int) callconv(.c) c_int,
    gb_pixels: *const fn (h: ?*anyopaque, out: *[width * height]u32) callconv(.c) void,
    gb_cram_size: *const fn (h: ?*anyopaque) callconv(.c) usize,
    gb_cram_get: *const fn (h: ?*anyopaque, out: [*]u8, len: usize) callconv(.c) void,
    gb_cram_set: *const fn (h: ?*anyopaque, in: [*]const u8, len: usize) callconv(.c) void,
    gb_state_save: *const fn (h: ?*anyopaque) callconv(.c) usize,
    gb_state_get: *const fn (h: ?*anyopaque, out: [*]u8, len: usize) callconv(.c) void,
    gb_state_load: *const fn (h: ?*anyopaque, in: [*]const u8, len: usize) callconv(.c) c_int,
};

var api: ?Api = null;

pub const StartError = spec.LoadError;

/// Loads the library (once).
pub fn start() StartError!*const Api {
    if (api == null) api = try spec.load(Api, build_options.gb_lib, "EMU_GB_LIB");
    return &api.?;
}

pub const Machine = struct {
    h: ?*anyopaque,
    api: *const Api,

    pub const Error = StartError || error{ RomRefused, BadModel };

    pub fn init(rom: []const u8, model: Model) Error!Machine {
        const a = try start();
        var h: ?*anyopaque = null;
        return switch (a.gb_new(rom.ptr, rom.len, @intFromEnum(model), &h)) {
            0 => .{ .h = h, .api = a },
            -1 => error.RomRefused,
            else => error.BadModel,
        };
    }

    pub fn deinit(m: *Machine) void {
        m.api.gb_free(m.h);
        m.h = null;
    }

    /// Runs to the next frame with `keys` held; the stereo samples it made.
    pub fn frame(m: *Machine, keys: Keys) usize {
        return @intCast(m.api.gb_frame(m.h, @bitCast(keys)));
    }

    /// The last frame's samples (left, right, ...) into `out`; how many
    /// stereo samples.
    pub fn audio(m: *Machine, out: []i16) usize {
        return @intCast(m.api.gb_audio(m.h, out.ptr, @intCast(out.len / 2)));
    }

    /// The last complete frame, 0xFFRRGGBB.
    pub fn pixels(m: *Machine, out: *[width * height]u32) void {
        m.api.gb_pixels(m.h, out);
    }

    /// Cartridge RAM's size (0 without RAM): the battery save's.
    pub fn cramSize(m: *Machine) usize {
        return m.api.gb_cram_size(m.h);
    }

    pub fn cramGet(m: *Machine, out: []u8) void {
        m.api.gb_cram_get(m.h, out.ptr, out.len);
    }

    pub fn cramSet(m: *Machine, in: []const u8) void {
        m.api.gb_cram_set(m.h, in.ptr, in.len);
    }

    /// A save state (Q.8): the machine as it is now, as bytes (the
    /// caller frees them).
    pub fn stateSave(m: *Machine, gpa: std.mem.Allocator) ![]u8 {
        const buf = try gpa.alloc(u8, m.api.gb_state_save(m.h));
        m.api.gb_state_get(m.h, buf.ptr, buf.len);
        return buf;
    }

    /// The machine as the state says; false (nothing changed) if it
    /// doesn't fit this machine.
    pub fn stateLoad(m: *Machine, blob: []const u8) bool {
        return m.api.gb_state_load(m.h, blob.ptr, blob.len) == 0;
    }
};

// --- tests (make test-desktop): the library against the spec's goldens

const testing = std.testing;
const png = @import("png");

fn readFile(path: []const u8) ![]u8 {
    return std.Io.Dir.cwd().readFileAlloc(testing.io, path, testing.allocator, .limited(16 << 20)) catch |e| {
        std.debug.print("gb_spec test: {s}: {s} (make gb-asm, tools/fetch-roms.sh)\n", .{ path, @errorName(e) });
        return e;
    };
}

/// Runs `rom` for `n` frames with `keys` held (cartridge RAM first set
/// to `cram`) and compares the last frame with the golden PNG (as
/// Gb.SpecRoms' screenGolden, screenWithKeys and saveRun); the machine,
/// for more checks.
fn expectGolden(rom_path: []const u8, cram: []const u8, keys: Keys, n: usize, golden: []const u8) !Machine {
    return expectGoldenOn(.dmg, rom_path, cram, keys, n, golden);
}

fn expectGoldenOn(model: Model, rom_path: []const u8, cram: []const u8, keys: Keys, n: usize, golden: []const u8) !Machine {
    const rom = try readFile(rom_path);
    defer testing.allocator.free(rom);
    var m = try Machine.init(rom, model);
    errdefer m.deinit();
    m.cramSet(cram);
    for (0..n) |_| _ = m.frame(keys);
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
    if (diff != 0) std.debug.print("gb_spec test: {s}: {d} pixels differ from {s}\n", .{ rom_path, diff, golden });
    try testing.expectEqual(@as(usize, 0), diff);
    return m;
}

test "dmg-acid2 through the library: the spec's golden after 30 frames" {
    var m = try expectGolden("roms/gb/dmg-acid2/dmg-acid2.gb", "", .{}, 30, "systems/gb/test/golden/spec/dmg-acid2.png");
    defer m.deinit();
    try testing.expectEqual(@as(usize, 0), m.cramSize()); // no cartridge RAM
}

test "our buttons.gb with Right and A held: the spec's golden" {
    var m = try expectGolden("build/gb/asm/buttons.gb", "", .{ .right = true, .a = true }, 30, "systems/gb/test/golden/spec/buttons-right-a.png");
    m.deinit();
}

test "a frame's sound: about 804 stereo samples at 48 kHz, not all silent" {
    const rom = try readFile("roms/gb/blargg/dmg_sound.gb");
    defer testing.allocator.free(rom);
    var m = try Machine.init(rom, .dmg);
    defer m.deinit();
    var buf: [2 * audio_cap]i16 = undefined;
    var steady: usize = 0;
    var loud = false;
    for (0..120) |_| {
        const n = m.frame(.{});
        try testing.expectEqual(n, m.audio(&buf));
        // a frame is 17556 M-cycles: 803.6 samples; shorter when the game
        // turns the LCD off and on (dmg_sound does, once)
        try testing.expect(n <= 805);
        if (n >= 803) steady += 1;
        for (buf[0 .. 2 * n]) |x| if (x != 0) {
            loud = true;
        };
    }
    try testing.expect(steady >= 115);
    try testing.expect(loud);
}

test "battery RAM in and out: save.gb booted from a save with count 41 shows 00042 and stores 42" {
    var m = try expectGolden("build/gb/asm/save.gb", "SAVE" ++ [_]u8{ 41, 0 }, .{}, 5, "systems/gb/test/golden/spec/save-42.png");
    defer m.deinit();
    try testing.expectEqual(@as(usize, 8192), m.cramSize());
    var ram: [8192]u8 = undefined;
    m.cramGet(&ram);
    try testing.expectEqualSlices(u8, "SAVE" ++ [_]u8{ 42, 0 }, ram[0..6]);
}

test "the GBC: cgb-acid2 through the library, the spec's golden after 30 frames" {
    var m = try expectGoldenOn(.cgb, "roms/gbc/cgb-acid2/cgb-acid2.gbc", "", .{}, 30, "systems/gb/test/golden/spec/cgb-acid2.png");
    m.deinit();
}

test "the GBC in double speed: still about 804 samples a frame (speed.gb switches twice)" {
    const rom = try readFile("build/gb/asm/speed.gb");
    defer testing.allocator.free(rom);
    var m = try Machine.init(rom, .cgb);
    defer m.deinit();
    var steady: usize = 0;
    for (0..60) |_| {
        const n = m.frame(.{});
        try testing.expect(n <= 805); // double speed's M-cycles counted as half
        if (n >= 803) steady += 1;
    }
    try testing.expect(steady >= 50);
}

test "a ROM whose header is refused" {
    const rom = try readFile("roms/gb/dmg-acid2/dmg-acid2.gb");
    defer testing.allocator.free(rom);
    try testing.expectError(error.RomRefused, Machine.init(rom[0..0x100], .dmg));
}

fn stateStep(m: *Machine, k: usize) u64 {
    const ns = m.frame(.{ .a = k % 20 < 5 });
    var px: [width * height]u32 = undefined;
    var snd: [2 * audio_cap]i16 = undefined;
    _ = m.audio(&snd);
    m.pixels(&px);
    var h = std.hash.Wyhash.init(ns);
    h.update(std.mem.sliceAsBytes(&px));
    h.update(std.mem.sliceAsBytes(snd[0 .. 2 * ns]));
    return h.final();
}

test "a save state: loaded, the same pictures and sound as after taking it (dmg_sound.gb; the GBC's speed.gb in double speed)" {
    inline for (.{ .{ Model.dmg, "roms/gb/blargg/dmg_sound.gb" }, .{ Model.cgb, "build/gb/asm/speed.gb" } }) |c| {
        const rom = try readFile(c[1]);
        defer testing.allocator.free(rom);
        var m = try Machine.init(rom, c[0]);
        defer m.deinit();
        for (0..60) |k| _ = stateStep(&m, k);
        var fresh = try Machine.init(rom, c[0]);
        defer fresh.deinit();
        try spec.expectStateReplays(&m, &fresh, 60, stateStep);
    }
}

test "a save state from another game is refused, the machine left as it was" {
    const a = try readFile("roms/gb/dmg-acid2/dmg-acid2.gb");
    defer testing.allocator.free(a);
    const b = try readFile("roms/gbc/cgb-acid2/cgb-acid2.gbc");
    defer testing.allocator.free(b);
    var ma = try Machine.init(a, .dmg);
    defer ma.deinit();
    var mb = try Machine.init(b, .cgb);
    defer mb.deinit();
    const blob = try mb.stateSave(testing.allocator);
    defer testing.allocator.free(blob);
    try testing.expect(!ma.stateLoad(blob));
    try testing.expect(!ma.stateLoad("not a state"));
}
