//! The Game Gear's and Master System's save states (side track N, N.3):
//! their Spec for savestate.zig, Gg.Hooks's link map. One for both
//! systems: the same core, the same map.

const savestate = @import("../savestate.zig");
const gg_cart = @import("gg_cart.zig");
const gg_restore = @import("gg_restore.zig");

/// Gg.Hooks: the state mode and windows as the Game Boy's; after the table
/// (0x140 bytes, read only: the routine loads it), VRAM (16 KiB) and RAM
/// (8 KiB); cartridge RAM through the battery saves' windows; the ROM
/// overlay at 0x6000 with its end and arm at 0x1F10.
pub const spec: savestate.Spec = .{
    .mode_addr = 0x140050,
    .wbase = 0x150000,
    .rbase = 0x160000,
    .status = 0x1F00,
    .sections = &.{
        .{ .addr = 0x2000, .len = 0x4000 },
        .{ .addr = 0x8000, .len = 0x2000 },
    },
    .cram = gg_cart.saveSize,
    .restore = .{
        .table_size = gg_restore.table_size,
        .overlay_size = gg_restore.overlay_size,
        .overlay_addr = 0x6000,
        .end_addr = 0x1F10,
        .build = build,
        // RAM's DFFCh-DFFFh (state space 9FFCh), which the routine's mapper
        // writes overwrite: it writes them back (Gg.StateTable's RAMTAIL)
        .fills = &.{.{ .addr = 0x9FFC, .table = gg_restore.t.ramtail, .len = 4 }},
    },
};

fn build(table: []const u8, overlay: []u8) ?u16 {
    const r = gg_restore.build(table[0..gg_restore.table_size], overlay[0..gg_restore.overlay_size]) catch return null;
    return r.end;
}
