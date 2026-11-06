//! The NES's save states (side track N, N.9): its Spec for savestate.zig,
//! Nes.StateHooks's link map (docs/ARCHITECTURE.md), and the read-back
//! control the NES's row gives as its `save` (it has no battery saves: no
//! save file, size 0).

const std = @import("std");
const savestate = @import("../savestate.zig");
const menu = @import("../menu.zig");
const nes_restore = @import("nes_restore.zig");

/// Nes.StateHooks: the state mode as the others'; the 128 KiB windows
/// (write 0x150000, read 0x170000); after the table (0x100 bytes, read
/// only: the routine loads it) RAM, nametable RAM, OAM (read only: the
/// routine's DMA fills it from the overlay), MMC5's ExRAM, CHR RAM (8 KiB)
/// and PRG RAM (64 KiB); the ROM overlay at 0x6000 with its end and arm at
/// 0x1F10.
pub const spec: savestate.Spec = .{
    .mode_addr = 0x140050,
    .wbase = 0x150000,
    .rbase = 0x170000,
    .status = 0x1F00,
    .sections = &.{
        .{ .addr = 0x8000, .len = 0x800 },
        .{ .addr = 0x8800, .len = 0x800 },
        .{ .addr = 0x9000, .len = 0x100 },
        .{ .addr = 0x9400, .len = 0x400 },
        .{ .addr = 0xA000, .len = 0x2000 },
        .{ .addr = 0x10000, .len = 0x10000 },
    },
    .cram = noCram,
    .restore = .{
        .table_size = nes_restore.table_size,
        .overlay_size = nes_restore.overlay_size,
        .overlay_addr = 0x6000,
        .end_addr = 0x1F10,
        .build = build,
        .fills = &.{
            // OAM straight into the overlay's page 2 (the routine's DMA
            // reads it there)
            .{ .addr = 0x9000, .table = nes_restore.table_size + 0x200, .len = 0x100 },
            // what the routine puts back besides: the NMI's three pushes
            // ($01FB-$01FD), the nametable bytes its read buffer passes
            // through ($2000's and $2400's), stashed in the overlay's zero
            // page area until `build`
            .{ .addr = 0x8000 + 0x1FB, .table = nes_restore.table_size + stash, .len = 3 },
            .{ .addr = 0x8800, .table = nes_restore.table_size + stash + 4, .len = 1 },
            .{ .addr = 0x8C00, .table = nes_restore.table_size + stash + 5, .len = 1 },
        },
    },
};

/// Where the fills stash the RAM and nametable bytes in the overlay.
const stash = 0x0F0;

/// The read-back control (Nes.StateHooks: 0x140000 bit 0 puts the state's
/// reads on the byte channel); no battery-backed RAM.
pub const save: menu.Save = .{
    .size = noCram, .ram_addr = 0, .read_addr = 0, .ctrl_addr = 0x140000, .clear_addr = 0x140001,
};

fn noCram(_: []const u8) u32 {
    return 0;
}

/// The routine from the table, the fills' bytes taken out of the overlay
/// first (`build` writes all of it).
fn build(table: []const u8, overlay: []u8) ?u16 {
    var ram = [_]u8{0} ** nes_restore.ram_size;
    var ciram = [_]u8{0} ** nes_restore.ciram_size;
    var oam: [nes_restore.oam_size]u8 = undefined;
    @memcpy(&oam, overlay[0x200..][0..nes_restore.oam_size]);
    @memcpy(ram[0x1FB..0x1FE], overlay[stash..][0..3]);
    ciram[0] = overlay[stash + 4];
    ciram[0x400] = overlay[stash + 5];
    const r = nes_restore.build(table[0..nes_restore.table_size], &ram, &ciram, &oam, overlay[0..nes_restore.overlay_size]) catch return null;
    return r.end;
}

test "build: the fills' bytes reach the routine (OAM's copy, the NMI's pushes)" {
    var table = [_]u8{0} ** nes_restore.table_size;
    table[nes_restore.t.line] = 250;
    table[nes_restore.t.ctrl] = 0x80; // NMI on, with the flag set: its pushes put back
    table[nes_restore.t.status] = 0x80;
    table[nes_restore.t.dmcbits] = 8;
    var overlay = [_]u8{0} ** nes_restore.overlay_size;
    for (0..nes_restore.oam_size) |i| overlay[0x200 + i] = @truncate(i * 7);
    overlay[stash..][0..3].* = .{ 0x11, 0x22, 0x33 };
    const end = build(&table, &overlay) orelse return error.Refused;
    try std.testing.expect(end >= 0xFB00);
    for (0..nes_restore.oam_size) |i| try std.testing.expectEqual(@as(u8, @truncate(i * 7)), overlay[0x200 + i]);
    // LDA #$33; STA $01FD (and the others) somewhere in the code
    var found: u32 = 0;
    for (0x300..0x7F8) |i| {
        if (overlay[i] == 0xA9 and overlay[i + 2] == 0x8D and overlay[i + 4] == 0x01) {
            const v = overlay[i + 1];
            const a = overlay[i + 3];
            if ((a == 0xFD and v == 0x33) or (a == 0xFC and v == 0x22) or (a == 0xFB and v == 0x11)) found += 1;
        }
    }
    try std.testing.expectEqual(@as(u32, 3), found);
}
