//! The NES: the menu's row, its header check and the parts that go over the link (side track K, D4: one file per system;
//! its track owns it; platform/ps/systems.zig lists them).

const std = @import("std");
const menu = @import("../menu.zig");
const state_board = @import("state_board.zig");
const nes_state = @import("nes_state.zig");
const Range = menu.Range;
const testing = std.testing;

/// NES cartridges (M3 step 3.8; side track D for Kirby's Adventure):
/// the .nes file into DDR at Nes.Top.romBase (PRG ROM is read from
/// there), then its header and CHR ROM over the link at their file
/// offsets in Nes.Top's map (Nes.Loader routes them; 256 KiB CHR block
/// RAM). Up to 512 KiB PRG. Up+Down as the Game Boy (A is C).
pub const system: menu.System = .{
    .id = 3, .name = "NES", .dir = "nes", .exts = &.{".nes"}, .load_addr = 0, .max_size = 16 + 0x80000 + 0x40000, .check = nesCheck,
    .volume = true, // side track L: Platform.Audio.Volume in its tops
    .speed_top = 6, // side track L, L.4: 150 %, measured: the core tops out at 1.56x
    .ddr_addr = 0x0200_0000, .ddr_link = nesLinkParts,
    .menu_combo = .{ .c = false, .d = true, .l = false, .r = false, .u = true }, .menu_combo_name = "Up+Down",
    .state = state_board.spec(nes_state.spec), // side track N, N.9: the Arty's bitstream (Nes.StateHooks)
    .save = nes_state.save, // no battery saves: the state's read-back control only
};

/// The iNES / NES 2.0 header (NESdev wiki, "INES", "NES 2.0"): what
/// Nes.Loader.headerCfg (and Nes.Cartridge.checkSupported) accepts:
/// mappers 0, 1, 2, 3, 4, 5, 7; PRG ROM up to 512 KiB (MMC1 256 KiB), CHR
/// ROM up to 256 KiB, CHR RAM and PRG RAM up to 8 KiB (MMC5: PRG RAM 64
/// KiB); no trainer, no four-screen VRAM.
pub fn nesCheck(head: []const u8, size: u32) ?[]const u8 {
    if (head.len < 16 or !std.mem.eql(u8, head[0..4], "NES\x1A")) return "Not an NES ROM (no iNES header).";
    const nes20 = head[7] & 0x0C == 0x08;
    const junk = !nes20 and std.mem.readInt(u32, head[12..16], .little) != 0;
    var mapper: u32 = head[6] >> 4;
    if (nes20) mapper |= (@as(u32, head[7] & 0xF0)) | (@as(u32, head[8] & 0x0F) << 8) else if (!junk) mapper |= head[7] & 0xF0;
    switch (mapper) {
        0, 1, 2, 3, 4, 5, 7 => {},
        else => return "Mapper not supported (0-5, 7 only).",
    }
    if (head[6] & 0x0C != 0) return "Trainer or four-screen: unsupported.";
    const sz = nesSizes(head);
    const prg = sz.prg;
    const chr = sz.chr;
    if (prg > 0x80000 or chr > 0x40000) return "Too big (PRG 512 KiB, CHR 256 KiB).";
    if (mapper == 1 and prg > 0x40000) return "MMC1 above 256 KiB PRG: unsupported.";
    const chr_ram: u64 = if (nes20) shiftSize(head[11] & 0x0F) + shiftSize(head[11] >> 4) else 0;
    const prg_ram: u64 = if (nes20) shiftSize(head[10] & 0x0F) + shiftSize(head[10] >> 4) else if (junk) 0x2000 else 0x2000 * @as(u64, @max(1, head[8]));
    // (side track D) MMC5 boards have up to 64 KiB of PRG RAM
    if (chr_ram > 0x2000 or prg_ram > (if (mapper == 5) @as(u64, 0x10000) else 0x2000)) return "RAM above 8 KiB (MMC5: 64 KiB): not supported.";
    if (size < 16 + prg + chr) return "File shorter than its header says.";
    return null;
}

/// The PRG and CHR ROM sizes an iNES / NES 2.0 header (16 bytes) gives.
fn nesSizes(head: []const u8) struct { prg: u64, chr: u64 } {
    const nes20 = head[7] & 0x0C == 0x08;
    return .{
        .prg = if (nes20) romSize(head[4], head[9] & 0x0F, 0x4000) else @as(u64, head[4]) * 0x4000,
        .chr = if (nes20) romSize(head[5], head[9] >> 4, 0x2000) else @as(u64, head[5]) * 0x2000,
    };
}

/// What goes over the link once an NES file is in DDR (`System.ddr_link`):
/// the header, and CHR ROM after PRG ROM (none with CHR RAM). The header
/// passed `nesCheck`, so the sizes are within the limits.
pub fn nesLinkParts(head: []const u8) [2]Range {
    const sz = nesSizes(head);
    return .{ .{ .off = 0, .len = 16 }, .{ .off = @intCast(16 + sz.prg), .len = @intCast(sz.chr) } };
}

/// NES 2.0 ROM size: 12 bits of units, or 2^E * (2M + 1) (capped).
fn romSize(lsb: u8, msb: u8, unit: u64) u64 {
    if (msb == 0x0F) {
        const e: u6 = @intCast(lsb >> 2);
        if (e > 40) return 1 << 40;
        return (@as(u64, 1) << e) * (2 * @as(u64, lsb & 3) + 1);
    }
    return (@as(u64, msb) << 8 | lsb) * unit;
}

fn shiftSize(n: u8) u64 {
    return if (n == 0) 0 else @as(u64, 64) << @intCast(n);
}

test "NES header check: mappers, sizes, RAM, trainer, NES 2.0, short files" {
    var h = [_]u8{ 0x4E, 0x45, 0x53, 0x1A, 2, 1, 0x01, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    const full: u32 = 16 + 0x8000 + 0x2000;
    try testing.expect(nesCheck(&h, full) == null); // NROM 32K + 8K
    for ([_]u8{ 0x10, 0x20, 0x30, 0x40, 0x70 }) |m| {
        h[6] = m;
        try testing.expect(nesCheck(&h, full) == null);
    }
    h[6] = 0x90; // MMC2
    try testing.expectEqualStrings("Mapper not supported (0-5, 7 only).", nesCheck(&h, full).?);
    h[6] = 0x50; // MMC5 (side track D), with 64 KiB of PRG RAM (iNES: 8 units) but not more
    h[8] = 8;
    try testing.expect(nesCheck(&h, full) == null);
    h[8] = 9;
    try testing.expect(nesCheck(&h, full) != null);
    h[8] = 0;
    h[6] = 0x00;
    h[7] = 0x10; // mapper 16
    try testing.expect(nesCheck(&h, full) != null);
    @memcpy(h[12..16], "Dude"); // iNES junk: byte 7 ignored
    try testing.expect(nesCheck(&h, full) == null);
    @memset(h[7..16], 0);
    h[6] = 0x04; // trainer
    try testing.expect(nesCheck(&h, full) != null);
    h[6] = 0x08; // four-screen
    try testing.expect(nesCheck(&h, full) != null);
    h[6] = 0;
    h[4] = 33; // 528 KiB PRG
    try testing.expect(nesCheck(&h, 16 + 33 * 0x4000 + 0x2000) != null);
    h[4] = 32; // 512 KiB (in DDR, side track D)
    try testing.expect(nesCheck(&h, 16 + 0x80000 + 0x2000) == null);
    try testing.expect(nesCheck(&h, 16 + 0x80000) != null); // short
    h[6] = 0x10; // ... but not for MMC1 (SUROM)
    try testing.expectEqualStrings("MMC1 above 256 KiB PRG: unsupported.", nesCheck(&h, 16 + 0x80000 + 0x2000).?);
    h[4] = 16;
    try testing.expect(nesCheck(&h, 16 + 0x40000 + 0x2000) == null);
    h[6] = 0x40; // MMC3: 256 KiB CHR, not 264
    h[5] = 32;
    try testing.expect(nesCheck(&h, 16 + 0x40000 + 0x40000) == null);
    h[5] = 33;
    try testing.expect(nesCheck(&h, 16 + 0x40000 + 33 * 0x2000) != null);
    h[5] = 1;
    h[6] = 0;
    h[4] = 2;
    h[8] = 2; // iNES: 16 KiB PRG RAM
    try testing.expect(nesCheck(&h, full) != null);
    h[8] = 0;
    h[7] = 0x08; // NES 2.0: PRG RAM shift 7 (8 KiB), CHR RAM shift 7 with no CHR ROM
    h[5] = 0;
    h[10] = 0x07;
    h[11] = 0x07;
    try testing.expect(nesCheck(&h, 16 + 0x8000) == null);
    h[11] = 0x08; // 16 KiB CHR RAM
    try testing.expect(nesCheck(&h, 16 + 0x8000) != null);
    h[11] = 0x07;
    h[8] = 0x01; // mapper 256
    try testing.expect(nesCheck(&h, 16 + 0x8000) != null);
    h[8] = 0;
    h[9] = 0x01; // PRG size MSB: 258 units
    try testing.expect(nesCheck(&h, 16 + 258 * 0x4000) != null);
    h[9] = 0x0F; // exponent form: 2^14 * 1 = 16 KiB
    h[4] = 14 << 2;
    try testing.expect(nesCheck(&h, 16 + 0x4000) == null);
    try testing.expect(nesCheck(h[0..3], 3) != null);
}

test "NES: header and CHR over the link after the file is in DDR" {
    // Kirby's Adventure: MMC3, 512 KiB PRG, 256 KiB CHR
    const h = [_]u8{ 0x4E, 0x45, 0x53, 0x1A, 32, 32, 0x40, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    const p = nesLinkParts(&h);
    try testing.expectEqual(Range{ .off = 0, .len = 16 }, p[0]);
    try testing.expectEqual(Range{ .off = 16 + 0x80000, .len = 0x40000 }, p[1]);
    try testing.expect(nesCheck(&h, 16 + 0x80000 + 0x40000) == null);
    // CHR RAM: no CHR part
    const r = [_]u8{ 0x4E, 0x45, 0x53, 0x1A, 8, 0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    try testing.expectEqual(@as(u32, 0), nesLinkParts(&r)[1].len);
}
