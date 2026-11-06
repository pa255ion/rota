//! The Atari 800XL: the menu's row and its program check (side track K, D4: one file per system;
//! its track owns it; platform/ps/systems.zig lists them).

const std = @import("std");
const root = @import("root");
const menu = @import("../menu.zig");
const state_board = @import("state_board.zig");
const a800xl_state = @import("a800xl_state.zig");
const cas = @import("a800xl_cas.zig");
const testing = std.testing;

/// The Arty's bitstream reads every cartridge from DDR, XEGS ones too
/// (side track Q, 2026-10-21, A800.CartDdr); the ZedBoard's keeps it in
/// block RAM (standard cartridges only). The app's root says which board
/// (relay.zig's `ps_board`, as state_board.zig); the desktop's tools and
/// the tests have none and take the Arty's.
pub const ddr = !(@hasDecl(root, "ps_board") and std.mem.eql(u8, root.ps_board, "zedboard"));

/// Atari 800XL (M4 step 4.8): an executable (.xex), a CAR file or a raw
/// 8/16 KiB cartridge image (.rom) as it is from 0 in A800.Loader's map
/// (it tells them apart by their first bytes); 64 KiB of executable
/// RAM. Up+Down as the Game Boy (a joystick can't push both). With 'ddr':
/// the file into DDR at the media region (the cartridge's image read from
/// there, after a .car's header), its first 64 KiB also onto the link
/// ('a800LinkParts'), so the loader sees the file as before and an
/// executable arrives whole. A cassette (.cas, or any file starting
/// "FUJI"; side track Q, cassette depth 1 on the board): made into its
/// tape image in DDR (a800xl_cas.zig, `ddr_image`), the image's header
/// onto the link (the loader knows a tape by it); the core answers the
/// OS's cassette calls from it, and holds START, then presses Space, to
/// boot it (A800.System).
pub const system: menu.System = .{
    .id = 5, .name = "Atari 800XL", .dir = "a800xl", .exts = &.{ ".xex", ".car", ".rom", ".cas" }, .load_addr = 0,
    // (with 'ddr' a tape may be longer than a cartridge: 512 KiB, about
    // 3700 records; the image takes 256 bytes a record)
    .max_size = if (ddr) 0x80000 else 0x10000, .check = a800Check,
    .ddr_addr = if (ddr) 0x0200_0000 else null,
    .ddr_link = if (ddr) a800LinkParts else null,
    .ddr_image = if (ddr) .{ .size = cas.imageSize, .write = cas.writeImage } else null,
    .volume = true, // side track L: Platform.Audio.Volume in its tops
    .speed_top = 5, // side track L, L.4: 125 %, measured: the core tops out at 1.43x
    .state = state_board.spec(a800xl_state.spec), // side track N, N.6: the Arty's bitstream (A800.Hooks)
    .save = a800xl_state.save, // no battery saves: the state's read-back control only
    .menu_combo = .{ .c = false, .d = true, .l = false, .r = false, .u = true }, .menu_combo_name = "Up+Down",
};

/// With 'ddr': the file's first 64 KiB onto the link (load.sendFromDdr
/// clips it to a shorter file): all of an executable, the start of a
/// cartridge (its header is what A800.Loader reads).
fn a800LinkParts(head: []const u8) [2]menu.Range {
    // a tape: its image's header only
    const len: u32 = if (isTape(head)) 16 else 0x10000;
    return .{ .{ .off = 0, .len = len }, .{ .off = 0, .len = 0 } };
}

fn isTape(head: []const u8) bool {
    return head.len >= 4 and std.mem.eql(u8, head[0..4], "FUJI");
}

/// What A800.Loader.loadedCfg accepts: an executable (FF FF first) of at
/// most 64 KiB, a CAR file of type 1 or 2 (8 or 16 KiB standard), or with
/// 'ddr' 12, 13, 14 (XEGS of 32, 64, 128 KiB) and tapes (FUJI), or a raw
/// image of exactly 8 or 16 KiB.
pub fn a800Check(head: []const u8, size: u32) ?[]const u8 {
    if (isTape(head)) return if (ddr) null else "Cassettes need the Arty's bitstream.";
    if (head.len >= 2 and head[0] == 0xFF and head[1] == 0xFF) {
        return if (size <= 0x10000) null else "Executable over 64 KiB.";
    }
    if (head.len >= 16 and std.mem.eql(u8, head[0..4], "CART")) {
        const t = std.mem.readInt(u32, head[4..8], .big);
        const want: u32 = switch (t) {
            1 => 0x2000,
            2 => 0x4000,
            12 => if (ddr) 0x8000 else return "XEGS cartridges need the Arty's bitstream.",
            13 => if (ddr) 0x10000 else return "XEGS cartridges need the Arty's bitstream.",
            14 => if (ddr) 0x20000 else return "XEGS cartridges need the Arty's bitstream.",
            else => return "Cartridge type not supported (8/16 KiB standard, XEGS).",
        };
        return if (size == 16 + want) null else "CAR file shorter or longer than its type.";
    }
    if (size == 0x2000 or size == 0x4000) return null;
    return "Not an 800XL program (XEX, CAR, or an 8/16 KiB image).";
}

test "a800Check: XEX, CAR types 1 and 2, raw 8/16 KiB; refusals" {
    var car = [_]u8{0} ** 16;
    @memcpy(car[0..4], "CART");
    car[7] = 2;
    try testing.expect(a800Check(&.{ 0xFF, 0xFF, 0, 0x20 }, 300) == null);
    try testing.expect(a800Check(&.{ 0xFF, 0xFF, 0, 0x20 }, 0x10001) != null);
    try testing.expect(a800Check(&car, 16 + 0x4000) == null);
    try testing.expect(a800Check(&car, 16 + 0x2000) != null);
    car[7] = 1;
    try testing.expect(a800Check(&car, 16 + 0x2000) == null);
    car[7] = 9;
    try testing.expect(a800Check(&car, 16 + 0x2000) != null);
    try testing.expect(a800Check(&.{ 1, 2, 3 }, 0x2000) == null);
    try testing.expect(a800Check(&.{ 1, 2, 3 }, 1000) != null);
}

test "a800Check: XEGS types 12-14 at their sizes (the Arty's build and the tools); others refused" {
    var car = [_]u8{0} ** 16;
    @memcpy(car[0..4], "CART");
    car[7] = 12;
    try testing.expect(a800Check(&car, 16 + 0x8000) == null);
    car[7] = 13;
    try testing.expect(a800Check(&car, 16 + 0x10000) == null);
    car[7] = 14;
    try testing.expect(a800Check(&car, 16 + 0x20000) == null);
    try testing.expect(a800Check(&car, 16 + 0x10000) != null);
    car[7] = 15;
    try testing.expect(a800Check(&car, 16 + 0x8000) != null);
}

test "the 800XL's link parts: the first 64 KiB of the file" {
    const p = a800LinkParts(&.{ 0xFF, 0xFF });
    try testing.expectEqual(@as(u32, 0), p[0].off);
    try testing.expectEqual(@as(u32, 0x10000), p[0].len);
    try testing.expectEqual(@as(u32, 0), p[1].len);
}

test "a800Check: a tape (FUJI) with the Arty's DDR; its link part is the image's header" {
    try testing.expectEqual(@as(?[]const u8, null), a800Check("FUJI\x00\x00\x00\x00", 40000));
    try testing.expectEqual(@as(u32, 16), a800LinkParts("FUJI\x00\x00\x00\x00")[0].len);
    try testing.expectEqual(@as(u32, 0x10000), a800LinkParts("\xff\xff\x00\x20")[0].len);
}

test {
    _ = cas;
}
