//! The Super Nintendo's model as a library (side track Q, PLAN.md Q.5):
//! the C ABI of systems/snes/desktop/Snes/Desktop.hs (cabal's foreign
//! library snes-desktop, on the fast version Snes.SpecFast), loaded at run
//! time (spec.zig) from the build's path (`-Dsnes-lib`, from make) or
//! $EMU_SNES_LIB.

const std = @import("std");
const build_options = @import("build_options");
const spec = @import("spec.zig");

/// The picture: 512 × 239, each line twice (Snes.Desktop's snes_pixels).
pub const width = 512;
pub const height = 478;
pub const audio_cap = 2048;
/// A frame: 262 lines of 1364 master clocks at 21477272 Hz (60.10 Hz).
pub const frame_ns: u64 = 16_639_267;

const Api = struct {
    snes_new: *const fn (ipl: [*]const u8, ipl_len: usize, rom: [*]const u8, len: usize, threaded: c_int, reference: c_int, out: *?*anyopaque) callconv(.c) c_int,
    snes_free: *const fn (h: ?*anyopaque) callconv(.c) void,
    snes_skip: *const fn (h: ?*anyopaque, on: c_int) callconv(.c) void,
    snes_lag: *const fn (h: ?*anyopaque, on: c_int) callconv(.c) void,
    snes_frame: *const fn (h: ?*anyopaque, keys: u32) callconv(.c) c_int,
    snes_audio: *const fn (h: ?*anyopaque, out: [*]i16, cap: c_int) callconv(.c) c_int,
    snes_pixels: *const fn (h: ?*anyopaque, out: [*]u32) callconv(.c) void,
    snes_hash: *const fn (h: ?*anyopaque) callconv(.c) u64,
    snes_state_save: *const fn (h: ?*anyopaque) callconv(.c) usize,
    snes_state_get: *const fn (h: ?*anyopaque, out: [*]u8, len: usize) callconv(.c) void,
    snes_state_load: *const fn (h: ?*anyopaque, in: [*]const u8, len: usize) callconv(.c) c_int,
};

var api: ?Api = null;

pub fn start() spec.LoadError!*const Api {
    if (api == null) api = try spec.load(Api, build_options.snes_lib, "EMU_SNES_LIB");
    return &api.?;
}

pub const Machine = struct {
    h: ?*anyopaque,
    api: *const Api,

    pub const Error = spec.LoadError || error{RomRefused};

    pub fn init(ipl: []const u8, rom: []const u8) Error!Machine {
        return initWith(ipl, rom, 2);
    }

    /// `render_threads`: the pixels drawn by that many threads on other
    /// cores (Q.6), 0 inline (--no-render-thread).
    pub fn initWith(ipl: []const u8, rom: []const u8, render_threads: u8) Error!Machine {
        return initFull(ipl, rom, render_threads, false);
    }

    /// `reference`: the reference model (Snes.Spec), not the fast version
    /// (--reference; its drawing inline).
    pub fn initFull(ipl: []const u8, rom: []const u8, render_threads: u8, reference: bool) Error!Machine {
        const a = try start();
        var h: ?*anyopaque = null;
        if (a.snes_new(ipl.ptr, ipl.len, rom.ptr, rom.len, render_threads, @intFromBool(reference), &h) != 0) return error.RomRefused;
        return .{ .h = h, .api = a };
    }

    pub fn deinit(m: *Machine) void {
        m.api.snes_free(m.h);
        m.h = null;
    }

    /// The picture one frame late (Q.6): the machine runs on while the
    /// render threads finish the frame.
    pub fn lag(m: *Machine, on: bool) void {
        m.api.snes_lag(m.h, @intFromBool(on));
    }

    /// The next frames' picture not drawn (frame skipping, Q.6); the game
    /// runs on unchanged.
    pub fn skip(m: *Machine, on: bool) void {
        m.api.snes_skip(m.h, @intFromBool(on));
    }

    /// One frame with the key word `w` (pad 1 bits 0-11, pad 2 bits 16-27);
    /// its stereo samples at 48 kHz.
    pub fn frame(m: *Machine, w: u32) usize {
        return @intCast(m.api.snes_frame(m.h, w));
    }

    pub fn audio(m: *Machine, out: []i16) usize {
        return @intCast(m.api.snes_audio(m.h, out.ptr, @intCast(out.len / 2)));
    }

    pub fn pixels(m: *Machine, out: *[width * height]u32) void {
        m.api.snes_pixels(m.h, out);
    }

    pub fn hash(m: *Machine) u64 {
        return m.api.snes_hash(m.h);
    }

    /// A save state (Q.8; the fast version only: error.NoStates on the
    /// reference model), as bytes the caller frees. The render threads
    /// finish first.
    pub fn stateSave(m: *Machine, gpa: std.mem.Allocator) ![]u8 {
        return spec.stateSave(gpa, m.h, m.api.snes_state_save, m.api.snes_state_get);
    }

    /// The machine as the state says (the render threads' copies made
    /// again); false (nothing changed) if it doesn't fit.
    pub fn stateLoad(m: *Machine, blob: []const u8) bool {
        return m.api.snes_state_load(m.h, blob.ptr, blob.len) == 0;
    }
};

// --- tests (make test-desktop): the library against the spec's goldens

const testing = std.testing;

fn readFile(path: []const u8) ![]u8 {
    return std.Io.Dir.cwd().readFileAlloc(testing.io, path, testing.allocator, .limited(8 << 20)) catch |e| {
        std.debug.print("snes_spec test: {s}: {s} (tools/fetch-roms.sh, make snes-asm)\n", .{ path, @errorName(e) });
        return e;
    };
}

/// As the snes spec's picture tests (test/golden/frames/<rom>-<n>.txt).
fn expectHash(rom_rel: []const u8, n: usize) !void {
    var buf: [256]u8 = undefined;
    const ipl = try readFile("build/snes/ipl.bin");
    defer testing.allocator.free(ipl);
    const rom = try readFile(try std.fmt.bufPrint(&buf, "roms/snes/{s}.sfc", .{rom_rel}));
    defer testing.allocator.free(rom);
    const base = std.fs.path.basename(rom_rel);
    const golden = try readFile(try std.fmt.bufPrint(&buf, "systems/snes/test/golden/frames/{s}-{d}.txt", .{ base, n }));
    defer testing.allocator.free(golden);
    const want = try std.fmt.parseInt(u64, std.mem.trim(u8, golden, " \n"), 16);
    var m = try Machine.init(ipl, rom);
    defer m.deinit();
    for (0..n) |_| _ = m.frame(0);
    if (m.hash() != want) std.debug.print("snes_spec test: {s}: hash {x:0>16}, want {x:0>16}\n", .{ rom_rel, m.hash(), want });
    try testing.expectEqual(want, m.hash());
}

test "krom's 8x8 BG map through the library: the spec's frame hash" {
    try expectHash("krom/8x8BG1Map2BPP32x328PAL", 60);
}

test "the drawing inline (--no-render-thread) gives the same frame hash" {
    const ipl = try readFile("build/snes/ipl.bin");
    defer testing.allocator.free(ipl);
    const rom = try readFile("roms/snes/opensnes/extbg.sfc");
    defer testing.allocator.free(rom);
    var a = try Machine.initWith(ipl, rom, 2);
    defer a.deinit();
    var b = try Machine.initWith(ipl, rom, 0);
    defer b.deinit();
    for (0..60) |_| {
        _ = a.frame(0);
        _ = b.frame(0);
    }
    try testing.expectEqual(a.hash(), b.hash());
}

test "--reference (the reference model) gives the same frame hash" {
    const ipl = try readFile("build/snes/ipl.bin");
    defer testing.allocator.free(ipl);
    const rom = try readFile("roms/snes/opensnes/extbg.sfc");
    defer testing.allocator.free(rom);
    var a = try Machine.initFull(ipl, rom, 2, false);
    defer a.deinit();
    var b = try Machine.initFull(ipl, rom, 0, true);
    defer b.deinit();
    for (0..60) |_| {
        _ = a.frame(0);
        _ = b.frame(0);
    }
    try testing.expectEqual(a.hash(), b.hash());
}

test "OpenSNES extbg and mode 5 hi-res: the spec's frame hashes" {
    try expectHash("opensnes/extbg", 60);
    try expectHash("opensnes/mode5_hires", 120);
}

test "the unnamed tech demo: the spec's frame hash; about 799 samples a frame at 48 kHz" {
    try expectHash("games/unnamed-tech-demo-v3", 120);
    const ipl = try readFile("build/snes/ipl.bin");
    defer testing.allocator.free(ipl);
    const rom = try readFile("roms/snes/games/unnamed-tech-demo-v3.sfc");
    defer testing.allocator.free(rom);
    var m = try Machine.init(ipl, rom);
    defer m.deinit();
    var a: [2 * audio_cap]i16 = undefined;
    // (the first frames after the reset are uneven: the sound module
    // catches up with the CPU)
    for (0..4) |_| _ = m.frame(0);
    for (0..30) |_| {
        const n = m.frame(0);
        try testing.expect(n >= 795 and n <= 803);
        try testing.expectEqual(n, m.audio(&a));
    }
}

fn stateStep(m: *Machine, k: usize) u64 {
    // Start (bit 8) and B (bit 11) of pad 1 now and then
    const n = m.frame(if (k % 40 < 4) 0x900 else 0);
    var snd: [2 * audio_cap]i16 = undefined;
    _ = m.audio(&snd);
    var h = std.hash.Wyhash.init(m.hash());
    h.update(std.mem.sliceAsBytes(snd[0 .. 2 * n]));
    return h.final();
}

test "a save state: loaded into a new machine, the same pictures and sound, with the render threads and inline (the unnamed tech demo, extbg); none on the reference model" {
    const ipl = try readFile("build/snes/ipl.bin");
    defer testing.allocator.free(ipl);
    for ([_][]const u8{ "roms/snes/games/unnamed-tech-demo-v3.sfc", "roms/snes/opensnes/extbg.sfc" }) |f| {
        const rom = try readFile(f);
        defer testing.allocator.free(rom);
        for ([_]u8{ 2, 0 }) |threads| {
            var m = try Machine.initWith(ipl, rom, threads);
            defer m.deinit();
            for (0..90) |k| _ = stateStep(&m, k);
            var fresh = try Machine.initWith(ipl, rom, threads);
            defer fresh.deinit();
            try spec.expectStateReplays(&m, &fresh, 90, stateStep);
        }
    }
    const rom = try readFile("roms/snes/opensnes/extbg.sfc");
    defer testing.allocator.free(rom);
    var r = try Machine.initFull(ipl, rom, 0, true);
    defer r.deinit();
    try testing.expectError(error.NoStates, r.stateSave(testing.allocator));
}

test "a movie from a state (build/desktop/snes-state.mv1, 4 bytes a frame: the tech demo's state at frame 90, then 120 frames, and their last picture; for make test-desktop)" {
    const movie = @import("movie");
    const png = @import("png");
    const ipl = try readFile("build/snes/ipl.bin");
    defer testing.allocator.free(ipl);
    const rom = try readFile("roms/snes/games/unnamed-tech-demo-v3.sfc");
    defer testing.allocator.free(rom);
    var m = try Machine.init(ipl, rom);
    defer m.deinit();
    for (0..90) |k| _ = stateStep(&m, k);
    const state = try m.stateSave(testing.allocator);
    defer testing.allocator.free(state);
    var data: [120 * 4]u8 = undefined;
    for (0..120) |j| {
        const k = 90 + j;
        // pad 2 too (bits 16-27): a word wider than the board's 16 bits
        const w: u32 = (if (k % 40 < 4) @as(u32, 0x900) else 0) | (if (k % 30 < 3) @as(u32, 0x0100_0000) else 0);
        std.mem.writeInt(u32, data[j * 4 ..][0..4], w, .little);
        _ = m.frame(w);
    }
    const bytes = try movie.encodeFileRaw(testing.allocator, .{ .system = 9, .frames = 0, .rom_size = @intCast(rom.len) }, state, 4, &data);
    defer testing.allocator.free(bytes);
    var px: [width * height]u32 = undefined;
    m.pixels(&px);
    var rgb: [width * height * 3]u8 = undefined;
    for (px, 0..) |p, i| {
        rgb[3 * i] = @truncate(p >> 16);
        rgb[3 * i + 1] = @truncate(p >> 8);
        rgb[3 * i + 2] = @truncate(p);
    }
    const pic = try png.encode(testing.allocator, width, height, &rgb);
    defer testing.allocator.free(pic);
    try std.Io.Dir.cwd().createDirPath(testing.io, "build/desktop");
    try std.Io.Dir.cwd().writeFile(testing.io, .{ .sub_path = "build/desktop/snes-state.mv1", .data = bytes });
    try std.Io.Dir.cwd().writeFile(testing.io, .{ .sub_path = "build/desktop/snes-state.png", .data = pic });
    const d = try movie.decodeFileRaw(bytes);
    try testing.expectEqual(@as(u16, 4), d.header.frame_bytes);
    try testing.expectEqual(movie.Start.state, d.header.start);
    try testing.expectEqualSlices(u8, &data, d.data);
}
