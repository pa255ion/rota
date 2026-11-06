//! The Atari 800XL's save states (side track N, N.6): its Spec for
//! savestate.zig, A800.Hooks's link map (docs/ARCHITECTURE.md), and the
//! read-back control the 800XL's row gives as its `save` (it has no
//! battery saves: no save file, size 0).

const savestate = @import("../savestate.zig");
const menu = @import("../menu.zig");
const a800_restore = @import("a800_restore.zig");

/// A800.Hooks: the state mode as the others'; the 128 KiB windows (write
/// 0x150000, read 0x170000); after the table (0x80 bytes, read only: the
/// routine loads it) RAM's 64 KiB at 0x10000; the ROM overlay at 0x6000
/// with its end and arm at 0x1F10.
pub const spec: savestate.Spec = .{
    .mode_addr = 0x140050,
    .wbase = 0x150000,
    .rbase = 0x170000,
    .status = 0x1F00,
    .sections = &.{.{ .addr = 0x10000, .len = 0x10000 }},
    .cram = noCram,
    .restore = .{
        .table_size = a800_restore.table_size,
        .overlay_size = a800_restore.overlay_size,
        .overlay_addr = 0x6000,
        .end_addr = 0x1F10,
        .build = build,
    },
};

/// The read-back control (A800.Hooks: 0x140000 bit 0 puts the state's
/// reads on the byte channel); no battery-backed RAM.
pub const save: menu.Save = .{
    .size = noCram, .ram_addr = 0, .read_addr = 0, .ctrl_addr = 0x140000, .clear_addr = 0x140001,
};

fn noCram(_: []const u8) u32 {
    return 0;
}

fn build(table: []const u8, overlay: []u8) ?u16 {
    const r = a800_restore.build(table[0..a800_restore.table_size], overlay[0..a800_restore.overlay_size]) catch return null;
    return r.end;
}
