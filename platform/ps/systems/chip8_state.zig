//! CHIP-8's save states (side track N, N.10): its Spec for savestate.zig,
//! Chip8.StateHooks's link map (docs/ARCHITECTURE.md), the direct path (no
//! restore routine: the record is the interpreter's table, written back
//! into the frozen core), and the read-back control its row gives as its
//! `save` (no battery saves).

const savestate = @import("../savestate.zig");
const menu = @import("../menu.zig");

/// Chip8.StateHooks: the state mode as the others'; the windows (write
/// 0x150000, read 0x170000); after the table (0x40 bytes, read and
/// written) RAM's 4 KiB at 0x2000 and the framebuffer's 256 bytes at
/// 0x3000.
pub const spec: savestate.Spec = .{
    .mode_addr = 0x140050,
    .wbase = 0x150000,
    .rbase = 0x170000,
    .status = 0x1F00,
    .sections = &.{
        .{ .addr = 0x2000, .len = 0x1000 },
        .{ .addr = 0x3000, .len = 0x100 },
    },
    .cram = noCram,
};

/// The read-back control (Chip8.StateHooks: 0x140000 bit 0 puts the
/// state's reads on the byte channel); no battery-backed RAM.
pub const save: menu.Save = .{
    .size = noCram, .ram_addr = 0, .read_addr = 0, .ctrl_addr = 0x140000, .clear_addr = 0x140001,
};

fn noCram(_: []const u8) u32 {
    return 0;
}
