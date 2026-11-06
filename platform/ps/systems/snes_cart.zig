//! The Super Nintendo's cartridge (M7 step 7.11, docs/systems/snes.md,
//! "The cartridge, DDR and saves"): the ROM file goes into DDR whole
//! (menu.zig's row, `ddr_addr`), and the PL needs what the header says,
//! found as Snes.Cart finds it (the same scores): the mapping, the ROM's
//! size as two powers of two (for its mirrors) and SRAM's size. They go
//! as one 16-bit word into the Super Nintendo's link map while the core is
//! held (Snes.Machine, `cartAddr`), low byte first: bits 1-0 the mapping
//! (0 LoROM, 1 HiROM, 2 ExHiROM), bits 5-2 the larger power of two's log
//! less 12, bits 10-6 the rest's log (0 none), bits 14-11 SRAM's size code
//! (1 KiB << n, 0 none), bit 15 a copier's 512-byte header before the ROM
//! (the PL skips it). The same word as `snes-image`'s build/snes/cart.bin
//! for a file without a copier's header.

const std = @import("std");

/// Where the word goes in the Super Nintendo's link map (Snes.Machine.cartAddr).
pub const link_addr: u24 = 0x000000;

/// The largest ROM (the media region's 8 MiB), with a copier's header.
pub const max_size: u32 = (8 << 20) + 512;

/// A copier's header: a file of 512 bytes more than a multiple of 1 KiB.
pub fn copier(size: usize) bool {
    return size % 1024 == 512;
}

/// The ROM's two powers of two, larger first (0 for no second), or null
/// if it isn't made of at most two.
pub fn parts(n: usize) ?[2]u32 {
    if (n == 0) return null;
    const p = std.math.floorPowerOfTwo(usize, n);
    const q = n - p;
    if (q != 0 and !std.math.isPowerOfTwo(q)) return null;
    return .{ @intCast(p), @intCast(q) };
}

/// A look at the file before loading (menu.zig `check`): its size only
/// (the header lies past the first block).
pub fn check(head: []const u8, size: u32) ?[]const u8 {
    _ = head;
    if (size > max_size) return "more than 8 MiB";
    const n = size - (if (copier(size)) @as(u32, 512) else 0);
    if (n < 0x8000) return "too small for a Super Nintendo ROM";
    if (parts(n) == null) return "not one or two powers of two in size";
    return null;
}

pub const Mapping = enum(u2) { lorom = 0, hirom = 1, exhirom = 2 };

/// A header's place: its offset in the ROM and the mapping it implies.
pub const Place = struct { off: usize, m: Mapping };

const places = [_]Place{
    .{ .off = 0x7FC0, .m = .lorom },
    .{ .off = 0xFFC0, .m = .hirom },
    .{ .off = 0x40FFC0, .m = .exhirom },
};

fn at(rom: []const u8, i: usize) u8 {
    return if (i < rom.len) rom[i] else 0;
}

fn word16(rom: []const u8, i: usize) u32 {
    return @as(u32, at(rom, i)) | @as(u32, at(rom, i + 1)) << 8;
}

/// The cartridge checksum (Snes.Cart.checksum): the bytes' sum modulo
/// 65536, a size that isn't a power of two counted as the largest below
/// it plus the rest repeated to fill the same again.
pub fn checksum(rom: []const u8) u16 {
    const total = struct {
        fn of(b: []const u8) u16 {
            var s: u16 = 0;
            for (b) |x| s +%= x;
            return s;
        }
    }.of;
    const pq = parts(rom.len) orelse {
        // not two powers of two (refused by check); as Snes.Cart would
        const p = std.math.floorPowerOfTwo(usize, @max(rom.len, 1));
        const rest = rom[@min(p, rom.len)..];
        return total(rom[0..@min(p, rom.len)]) +% if (rest.len == 0) 0 else @as(u16, @truncate(p / rest.len)) *% total(rest);
    };
    if (pq[1] == 0) return total(rom);
    return total(rom[0..pq[0]]) +% @as(u16, @truncate(pq[0] / pq[1])) *% total(rom[pq[0]..]);
}

/// How much a place looks like a header (Snes.Cart.score: the map mode
/// names its mapping 4, its fixed bits 2, the checksum the ROM's 4 or at
/// least matching its complement 2, a reset vector into ROM 2, plausible
/// ROM and SRAM sizes 1 each, a printable title 1).
fn score(rom: []const u8, off: usize, m: Mapping, sum: u16) u32 {
    const mode = at(rom, off + 0x15);
    const want: u8 = switch (m) {
        .lorom => 0,
        .hirom => 1,
        .exhirom => 5,
    };
    const csum = word16(rom, off + 0x1E);
    const cpl = word16(rom, off + 0x1C);
    var s: u32 = 0;
    if (mode & 0x0F == want) s += 4;
    if (mode & 0xE0 == 0x20) s += 2;
    if (csum + cpl == 0xFFFF and csum == sum) s += 4;
    if (csum + cpl == 0xFFFF) s += 2;
    if (word16(rom, off + 0x3C) >= 0x8000) s += 2;
    const rs = at(rom, off + 0x17);
    if (rs >= 0x05 and rs <= 0x0D) s += 1;
    if (at(rom, off + 0x18) <= 0x08) s += 1;
    var printable = true;
    for (0..21) |i| {
        const c = at(rom, off + i);
        printable = printable and c >= 0x20 and c < 0x7F;
    }
    if (printable) s += 1;
    return s;
}

/// The header's place: the best score (ties keep LoROM, HiROM, ExHiROM),
/// among the places that fit the ROM.
pub fn header(rom: []const u8) ?Place {
    const sum = checksum(rom);
    var best: ?Place = null;
    var best_s: u32 = 0;
    for (places) |p| {
        if (p.off + 0x40 > rom.len) continue;
        const s = score(rom, p.off, p.m, sum);
        if (best == null or s > best_s) {
            best = p;
            best_s = s;
        }
    }
    return best;
}

/// The word's two bytes, low first, for the whole file as loaded.
pub fn word(file: []const u8) [2]u8 {
    const skip: usize = if (copier(file.len)) 512 else 0;
    const rom = file[@min(skip, file.len)..];
    const h = header(rom) orelse return .{ 0, 0 };
    const pq: [2]u32 = parts(rom.len) orelse .{ 0x8000, 0 };
    const lg = struct {
        fn of(x: u32) u32 {
            return if (x == 0) 0 else std.math.log2_int(u32, x);
        }
    }.of;
    const n = at(rom, h.off + 0x18);
    const code: u32 = if (n == 0 or n > 0x0D) 0 else n;
    const w: u32 = @as(u32, @intFromEnum(h.m)) | (lg(pq[0]) -| 12) << 2 | lg(pq[1]) << 6 | (code & 0xF) << 11 |
        @as(u32, @intFromBool(skip != 0)) << 15;
    return .{ @truncate(w), @truncate(w >> 8) };
}

/// Battery saves (Snes.Save, the Game Boy's protocol): the link addresses
/// of SRAM (loaded while held), its read window, the save control and the
/// dirty flag's clear.
pub const save_ram_addr: u24 = 0x100000;
pub const save_read_addr: u24 = 0x120000;
pub const save_ctrl_addr: u24 = 0x140000;
pub const save_clear_addr: u24 = 0x140001;

/// A save file's size: SRAM's 32 KiB (the PL's) for every ROM. The header
/// lies past the file's first block (all `size` sees), so each ROM gets a
/// slot; a game without SRAM never sets the dirty flag, so its slot is
/// never written.
pub fn saveSize(head: []const u8) u32 {
    _ = head;
    return 0x8000;
}

/// The file's header (after a copier's 512 bytes), or null.
fn headerOf(file: []const u8) ?struct { rom: []const u8, off: usize } {
    const skip: usize = if (copier(file.len)) 512 else 0;
    const rom = file[@min(skip, file.len)..];
    const h = header(rom) orelse return null;
    return .{ .rom = rom, .off = h.off };
}

/// The whole ROM once in DDR (menu.zig `check_ddr`, side track K's hook):
/// a cartridge with a chip this bitstream lacks is refused, by name (the
/// type byte's low nibble 3 or more; its high nibble, or FFBFh for F, the
/// chip: Snes.Cart's rule).
pub fn checkDdr(file: []const u8) ?[]const u8 {
    const h = headerOf(file) orelse return "no Super Nintendo header";
    const ty = at(h.rom, h.off + 0x16);
    if (ty & 0x0F < 3) return null;
    return switch (ty >> 4) {
        0 => "a DSP chip (DSP-1 to DSP-4): not in this bitstream",
        1 => "a Super FX chip: not in this bitstream",
        2 => "an OBC1 chip: not in this bitstream",
        3 => "an SA-1 chip: not in this bitstream",
        4 => "an S-DD1 chip: not in this bitstream",
        5 => "an S-RTC chip: not in this bitstream",
        0xE => "a Super Game Boy or Satellaview cartridge: not supported",
        0xF => "a custom chip (Cx4, SPC7110, ST01x): not in this bitstream",
        else => "an unknown chip: not supported",
    };
}

/// A save's size from the whole ROM (menu.zig `size_of_rom`): a battery
/// cartridge's SRAM (the type byte's low nibble 2, 5 or 6; 1 KiB << the
/// header's code), at most the PL's 32 KiB; none without a battery.
pub fn saveSizeOfRom(file: []const u8) u32 {
    const h = headerOf(file) orelse return 0;
    const ty = at(h.rom, h.off + 0x16) & 0x0F;
    if (ty != 2 and ty != 5 and ty != 6) return 0;
    const n = at(h.rom, h.off + 0x18);
    if (n == 0 or n > 0x0D) return 0;
    return @min(@as(u32, 1024) << @intCast(n), 0x8000);
}

fn testRom(comptime n: usize, off: usize, mode: u8, sram: u8) [n]u8 {
    var rom = [_]u8{0} ** n;
    @memcpy(rom[off .. off + 21], "TEST ROM             ");
    rom[off + 0x15] = mode;
    rom[off + 0x17] = 0x08;
    rom[off + 0x18] = sram;
    rom[off + 0x3C] = 0x00; // reset vector 8000h
    rom[off + 0x3D] = 0x80;
    // the checksum and its complement
    const s = checksum(&rom);
    rom[off + 0x1C] = @truncate(~s);
    rom[off + 0x1D] = @truncate(~s >> 8);
    rom[off + 0x1E] = @truncate(s);
    rom[off + 0x1F] = @truncate(s >> 8);
    return rom;
}

test "the word: a LoROM of 256 KiB with 8 KiB of SRAM (as snes-image's)" {
    const rom = testRom(0x40000, 0x7FC0, 0x20, 0x03);
    // mapping 0, log 18 - 12 = 6, no rest, SRAM code 3
    try std.testing.expectEqual([2]u8{ 6 << 2, 3 << 3 }, word(&rom));
}

test "the word: a HiROM by its header, and a copier's header (bit 15)" {
    const rom = testRom(0x20000, 0xFFC0, 0x21, 0x00);
    try std.testing.expectEqual([2]u8{ 1 | 5 << 2, 0 }, word(&rom));
    var file = [_]u8{0xEE} ** (512 + 0x20000);
    @memcpy(file[512..], &rom);
    try std.testing.expectEqual([2]u8{ 1 | 5 << 2, 0x80 }, word(&file));
}

test "the word: 3 MiB as two powers of two" {
    const pq = parts(3 << 20).?;
    try std.testing.expectEqual([2]u32{ 2 << 20, 1 << 20 }, pq);
}

test "check_ddr: a chip refused by name, a plain cartridge let through" {
    var rom = testRom(0x40000, 0x7FC0, 0x20, 0x03);
    try std.testing.expect(checkDdr(&rom) == null);
    rom[0x7FC0 + 0x16] = 0x13; // ROM + chip: the Super FX
    try std.testing.expectEqualStrings("a Super FX chip: not in this bitstream", checkDdr(&rom).?);
    rom[0x7FC0 + 0x16] = 0x35; // SA-1 with RAM and battery
    try std.testing.expectEqualStrings("an SA-1 chip: not in this bitstream", checkDdr(&rom).?);
}

test "size_of_rom: the header's SRAM with a battery, none without" {
    var rom = testRom(0x40000, 0x7FC0, 0x20, 0x03);
    rom[0x7FC0 + 0x16] = 0x02; // ROM + RAM + battery, 8 KiB
    try std.testing.expectEqual(@as(u32, 0x2000), saveSizeOfRom(&rom));
    rom[0x7FC0 + 0x16] = 0x01; // RAM, no battery
    try std.testing.expectEqual(@as(u32, 0), saveSizeOfRom(&rom));
    rom[0x7FC0 + 0x16] = 0x02;
    rom[0x7FC0 + 0x18] = 0x07; // 128 KiB: the PL's 32 KiB at most
    try std.testing.expectEqual(@as(u32, 0x8000), saveSizeOfRom(&rom));
}

test "check: sizes" {
    try std.testing.expect(check(&.{}, 0x4000) != null);
    try std.testing.expect(check(&.{}, 0x8000) == null);
    try std.testing.expect(check(&.{}, 0x8000 + 512) == null);
    try std.testing.expect(check(&.{}, 0x30000) == null); // 128 + 64 KiB
    try std.testing.expect(check(&.{}, 0x38000) != null); // three parts
    try std.testing.expect(check(&.{}, max_size + 1024) != null);
}
