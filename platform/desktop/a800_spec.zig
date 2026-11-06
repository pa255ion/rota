//! The Atari 800XL's model as a library (side track Q, PLAN.md Q.5): the
//! C ABI of systems/a800xl/desktop/A800/Desktop.hs (cabal's foreign
//! library a800xl-desktop, on the fast version A800.SpecFast), loaded at
//! run time (spec.zig) from the build's path (`-Da800-lib`, from make) or
//! $EMU_A800_LIB.

const std = @import("std");
const build_options = @import("build_options");
const spec = @import("spec.zig");

/// The picture as the board shows it (A800.Picture).
pub const width = 320;
pub const height = 240;
pub const audio_cap = 2048;
/// A frame: 262 lines of 114 machine cycles at 1789773 Hz (59.92 Hz).
pub const frame_ns: u64 = 16_688_154;
/// The PS link's key bytes (a800_keys.zig's `len`).
pub const key_bytes = 11;

pub const Program = union(enum) {
    /// BASIC's READY
    none,
    /// an executable, through the loader page (`make a800-asm`'s xexload.bin)
    xex: struct { loader: []const u8, file: []const u8 },
    /// a cartridge (.car, or a raw image)
    cart: []const u8,
    /// a cassette (.cas): the library boots it (START held, then Space),
    /// the OS's SIO call answered from it (side track Q)
    cas: []const u8,
    /// a cassette played by the 410 recorder itself (depth 2: at the
    /// tape's speed, through POKEY's serial input; custom loaders too)
    cas_real: []const u8,
};

const Api = struct {
    a800_new: *const fn (os: [*]const u8, os_len: usize, basic: [*]const u8, basic_len: usize, loader: [*]const u8, loader_len: usize, file: [*]const u8, file_len: usize, kind: c_int, reference: c_int, out: *?*anyopaque) callconv(.c) c_int,
    a800_free: *const fn (h: ?*anyopaque) callconv(.c) void,
    a800_skip: *const fn (h: ?*anyopaque, on: c_int) callconv(.c) void,
    a800_frame: *const fn (h: ?*anyopaque, keys: *const [key_bytes]u8) callconv(.c) c_int,
    a800_audio: *const fn (h: ?*anyopaque, out: [*]i16, cap: c_int) callconv(.c) c_int,
    a800_pixels: *const fn (h: ?*anyopaque, out: [*]u32) callconv(.c) void,
    a800_state_save: *const fn (h: ?*anyopaque) callconv(.c) usize,
    a800_state_get: *const fn (h: ?*anyopaque, out: [*]u8, len: usize) callconv(.c) void,
    a800_state_load: *const fn (h: ?*anyopaque, in: [*]const u8, len: usize) callconv(.c) c_int,
};

var api: ?Api = null;

pub fn start() spec.LoadError!*const Api {
    if (api == null) api = try spec.load(Api, build_options.a800_lib, "EMU_A800_LIB");
    return &api.?;
}

pub const Machine = struct {
    h: ?*anyopaque,
    api: *const Api,

    pub const Error = spec.LoadError || error{CartRefused};

    pub fn init(os: []const u8, basic: []const u8, program: Program) Error!Machine {
        return initWith(os, basic, program, false);
    }

    /// `reference`: the reference model (A800.Spec), not the fast version
    /// (--reference).
    pub fn initWith(os: []const u8, basic: []const u8, program: Program, reference: bool) Error!Machine {
        const a = try start();
        var h: ?*anyopaque = null;
        const none = &[_]u8{};
        const loader: []const u8, const file: []const u8, const kind: c_int = switch (program) {
            .none => .{ none, none, 0 },
            .xex => |x| .{ x.loader, x.file, 1 },
            .cart => |c| .{ none, c, 2 },
            .cas => |c| .{ none, c, 3 },
            .cas_real => |c| .{ none, c, 4 },
        };
        if (a.a800_new(os.ptr, os.len, basic.ptr, basic.len, loader.ptr, loader.len, file.ptr, file.len, kind, @intFromBool(reference), &h) != 0) return error.CartRefused;
        return .{ .h = h, .api = a };
    }

    pub fn deinit(m: *Machine) void {
        m.api.a800_free(m.h);
        m.h = null;
    }

    /// The next frames' picture not drawn (frame skipping, Q.6); the game
    /// runs on unchanged.
    pub fn skip(m: *Machine, on: bool) void {
        m.api.a800_skip(m.h, @intFromBool(on));
    }

    /// One frame with the key bytes held (a800_keys.zig's `bytes`); its
    /// stereo samples.
    pub fn frame(m: *Machine, keys: *const [key_bytes]u8) usize {
        return @intCast(m.api.a800_frame(m.h, keys));
    }

    pub fn audio(m: *Machine, out: []i16) usize {
        return @intCast(m.api.a800_audio(m.h, out.ptr, @intCast(out.len / 2)));
    }

    pub fn pixels(m: *Machine, out: *[width * height]u32) void {
        m.api.a800_pixels(m.h, out);
    }

    /// A save state (Q.8; the fast version only: error.NoStates on the
    /// reference model), as bytes the caller frees.
    pub fn stateSave(m: *Machine, gpa: std.mem.Allocator) ![]u8 {
        return spec.stateSave(gpa, m.h, m.api.a800_state_save, m.api.a800_state_get);
    }

    /// The machine as the state says; false (nothing changed) if it
    /// doesn't fit.
    pub fn stateLoad(m: *Machine, blob: []const u8) bool {
        return m.api.a800_state_load(m.h, blob.ptr, blob.len) == 0;
    }
};

// --- tests (make test-desktop): the library against the spec's goldens

const testing = std.testing;
const png = @import("png");

fn readFile(path: []const u8) ![]u8 {
    return std.Io.Dir.cwd().readFileAlloc(testing.io, path, testing.allocator, .limited(4 << 20)) catch |e| {
        std.debug.print("a800_spec test: {s}: {s} (tools/fetch-roms.sh, make a800-asm)\n", .{ path, @errorName(e) });
        return e;
    };
}

/// As A800.SpecDisplay's goldens (runPicture, runPictureKeys): `n`
/// frames, `keys` from frame `from` on, then the picture.
fn expectGolden(xex: ?[]const u8, keys: [key_bytes]u8, from: usize, n: usize, golden: []const u8) !void {
    const os = try readFile("roms/a800xl/os/altirraos_xl.rom");
    defer testing.allocator.free(os);
    const basic = try readFile("roms/a800xl/os/altirra_basic.rom");
    defer testing.allocator.free(basic);
    const loader = try readFile("build/a800xl/xexload.bin");
    defer testing.allocator.free(loader);
    const file: ?[]u8 = if (xex) |x| try readFile(x) else null;
    defer if (file) |f| testing.allocator.free(f);
    var m = try Machine.init(os, basic, if (file) |f| .{ .xex = .{ .loader = loader, .file = f } } else .none);
    defer m.deinit();
    const idle = [_]u8{0} ** key_bytes;
    for (0..n) |k| _ = m.frame(if (k >= from) &keys else &idle);
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
    if (diff != 0) std.debug.print("a800_spec test: {s}: {d} pixels differ\n", .{ golden, diff });
    try testing.expectEqual(@as(usize, 0), diff);
}

const no_keys = [_]u8{0} ** key_bytes;

test "BASIC's READY through the library (the fast version): the spec's golden after 300 frames" {
    try expectGolden(null, no_keys, 0, 300, "systems/a800xl/test/golden/ready.png");
}

test "rasterbars.xex: colour writes on exact colour clocks, the spec's golden" {
    try expectGolden("build/a800xl/asm/rasterbars.xex", no_keys, 0, 100, "systems/a800xl/test/golden/rasterbars.png");
}

test "keys.xex with A held from frame 90 (key code 0x3F: byte 7 bit 7)" {
    var keys = no_keys;
    keys[7] = 0x80;
    try expectGolden("build/a800xl/asm/keys.xex", keys, 90, 100, "systems/a800xl/test/golden/keys.png");
}

test "--reference (the reference model) draws BASIC's READY the same" {
    const os = try readFile("roms/a800xl/os/altirraos_xl.rom");
    defer testing.allocator.free(os);
    const basic = try readFile("roms/a800xl/os/altirra_basic.rom");
    defer testing.allocator.free(basic);
    var a = try Machine.initWith(os, basic, .none, false);
    defer a.deinit();
    var b = try Machine.initWith(os, basic, .none, true);
    defer b.deinit();
    for (0..120) |_| {
        _ = a.frame(&no_keys);
        _ = b.frame(&no_keys);
    }
    var pa: [width * height]u32 = undefined;
    var pb: [width * height]u32 = undefined;
    a.pixels(&pa);
    b.pixels(&pb);
    try testing.expectEqualSlices(u32, &pa, &pb);
}

test "a raw cartridge image (our cart_pass.rom, no CART header) runs, the same on the reference model" {
    // (the user's Miner 2049er is such an image: refused before, as the
    // library parsed every cartridge as a .car)
    const os = try readFile("roms/a800xl/os/altirraos_xl.rom");
    defer testing.allocator.free(os);
    const basic = try readFile("roms/a800xl/os/altirra_basic.rom");
    defer testing.allocator.free(basic);
    const cart = try readFile("build/a800xl/asm/cart_pass.rom");
    defer testing.allocator.free(cart);
    var a = try Machine.initWith(os, basic, .{ .cart = cart }, false);
    defer a.deinit();
    var b = try Machine.initWith(os, basic, .{ .cart = cart }, true);
    defer b.deinit();
    var r = try Machine.initWith(os, basic, .none, false);
    defer r.deinit();
    for (0..120) |_| {
        _ = a.frame(&no_keys);
        _ = b.frame(&no_keys);
        _ = r.frame(&no_keys);
    }
    var pa: [width * height]u32 = undefined;
    var pb: [width * height]u32 = undefined;
    var pr: [width * height]u32 = undefined;
    a.pixels(&pa);
    b.pixels(&pb);
    r.pixels(&pr);
    try testing.expectEqualSlices(u32, &pa, &pb);
    // the cartridge ran: not BASIC's READY screen
    try testing.expect(!std.mem.eql(u32, &pa, &pr));
}

test "our cassette (cas_boot.cas) boots, the same on the reference model, not BASIC's screen" {
    const os = try readFile("roms/a800xl/os/altirraos_xl.rom");
    defer testing.allocator.free(os);
    const basic = try readFile("roms/a800xl/os/altirra_basic.rom");
    defer testing.allocator.free(basic);
    const cas = try readFile("build/a800xl/asm/cas_boot.cas");
    defer testing.allocator.free(cas);
    var a = try Machine.initWith(os, basic, .{ .cas = cas }, false);
    defer a.deinit();
    var b = try Machine.initWith(os, basic, .{ .cas = cas }, true);
    defer b.deinit();
    var r = try Machine.initWith(os, basic, .none, false);
    defer r.deinit();
    for (0..400) |_| {
        _ = a.frame(&no_keys);
        _ = b.frame(&no_keys);
        _ = r.frame(&no_keys);
    }
    var pa: [width * height]u32 = undefined;
    var pb: [width * height]u32 = undefined;
    var pr: [width * height]u32 = undefined;
    a.pixels(&pa);
    b.pixels(&pb);
    r.pixels(&pr);
    try testing.expectEqualSlices(u32, &pa, &pb);
    try testing.expect(!std.mem.eql(u32, &pa, &pr));
}

test "a frame's sound: about 801 stereo samples at 48 kHz" {
    const os = try readFile("roms/a800xl/os/altirraos_xl.rom");
    defer testing.allocator.free(os);
    const basic = try readFile("roms/a800xl/os/altirra_basic.rom");
    defer testing.allocator.free(basic);
    var m = try Machine.init(os, basic, .none);
    defer m.deinit();
    var buf: [2 * audio_cap]i16 = undefined;
    _ = m.frame(&no_keys);
    for (0..30) |_| {
        const n = m.frame(&no_keys);
        try testing.expect(n >= 799 and n <= 803);
        try testing.expectEqual(n, m.audio(&buf));
    }
}

fn stateStep(m: *Machine, k: usize) u64 {
    // A held now and then (key code 0x3F, as keys.xex's test): the programs see keys
    var keys = no_keys;
    if (k % 30 < 3) keys[7] = 0x80;
    const n = m.frame(&keys);
    var px: [width * height]u32 = undefined;
    var snd: [2 * audio_cap]i16 = undefined;
    _ = m.audio(&snd);
    m.pixels(&px);
    var h = std.hash.Wyhash.init(n);
    h.update(std.mem.sliceAsBytes(&px));
    h.update(std.mem.sliceAsBytes(snd[0 .. 2 * n]));
    return h.final();
}

test "a save state: loaded into a new machine, the same pictures and sound (tone.xex, scene.xex, XEGS banks, a tape half loaded); none on the reference model" {
    const os = try readFile("roms/a800xl/os/altirraos_xl.rom");
    defer testing.allocator.free(os);
    const basic = try readFile("roms/a800xl/os/altirra_basic.rom");
    defer testing.allocator.free(basic);
    const loader = try readFile("build/a800xl/xexload.bin");
    defer testing.allocator.free(loader);
    const Case = struct { path: []const u8, kind: enum { xex, cart, cas }, at: usize };
    const cases = [_]Case{
        .{ .path = "build/a800xl/asm/tone.xex", .kind = .xex, .at = 120 },
        .{ .path = "build/a800xl/asm/scene.xex", .kind = .xex, .at = 120 },
        .{ .path = "build/a800xl/asm/xegs_banks.car", .kind = .cart, .at = 60 },
        .{ .path = "build/a800xl/asm/cas_irq.cas", .kind = .cas, .at = 140 },
    };
    for (cases) |c| {
        const file = try readFile(c.path);
        defer testing.allocator.free(file);
        const prog: Program = switch (c.kind) {
            .xex => .{ .xex = .{ .loader = loader, .file = file } },
            .cart => .{ .cart = file },
            .cas => .{ .cas = file },
        };
        var m = try Machine.init(os, basic, prog);
        defer m.deinit();
        for (0..c.at) |k| _ = stateStep(&m, k);
        var fresh = try Machine.init(os, basic, prog);
        defer fresh.deinit();
        try spec.expectStateReplays(&m, &fresh, 120, stateStep);
    }
    var r = try Machine.initWith(os, basic, .none, true);
    defer r.deinit();
    try testing.expectError(error.NoStates, r.stateSave(testing.allocator));
}

test "an 11-byte movie (build/desktop/keys.mv1: keys.xex, A held from frame 90, 100 frames; make test-desktop plays it to keys.png)" {
    const movie = @import("movie");
    var data: [100 * key_bytes]u8 = [_]u8{0} ** (100 * key_bytes);
    for (90..100) |k| data[k * key_bytes + 7] = 0x80;
    const bytes = try movie.encodeFileRaw(testing.allocator, .{ .system = 5, .frames = 0 }, &.{}, key_bytes, &data);
    defer testing.allocator.free(bytes);
    try std.Io.Dir.cwd().createDirPath(testing.io, "build/desktop");
    try std.Io.Dir.cwd().writeFile(testing.io, .{ .sub_path = "build/desktop/keys.mv1", .data = bytes });
    try testing.expectError(error.WideFrames, movie.decodeFile(testing.allocator, bytes));
}

test "cas_boot.cas through the recorder itself (--real-tape, its leader cut to 10 s: the OS waits about that long) ends on the instant load's screen" {
    const os = try readFile("roms/a800xl/os/altirraos_xl.rom");
    defer testing.allocator.free(os);
    const basic = try readFile("roms/a800xl/os/altirra_basic.rom");
    defer testing.allocator.free(basic);
    const cas = try readFile("build/a800xl/asm/cas_boot.cas");
    defer testing.allocator.free(cas);
    // the chunks: FUJI (8 bytes, no body), baud (8), then the first data
    // chunk, whose aux (its leading gap in ms) is at 16 + 6
    try testing.expectEqualStrings("data", cas[16..20]);
    std.mem.writeInt(u16, cas[22..24], 10000, .little);
    var a = try Machine.initWith(os, basic, .{ .cas = cas }, false);
    defer a.deinit();
    var t = try Machine.initWith(os, basic, .{ .cas_real = cas }, false);
    defer t.deinit();
    var pa: [width * height]u32 = undefined;
    var pt: [width * height]u32 = undefined;
    // (about 1440 frames to its end on the spec)
    for (0..2000) |_| {
        _ = a.frame(&no_keys);
        _ = t.frame(&no_keys);
    }
    a.pixels(&pa);
    t.pixels(&pt);
    try testing.expectEqualSlices(u32, &pa, &pt);
}
