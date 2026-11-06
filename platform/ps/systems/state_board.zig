//! Which PS app builds carry side track N's save states (GG/SMS, the
//! 800XL, the NES, CHIP-8): every one but the ZedBoard's. Their state
//! hooks are only in the Arty's tops (the ZedBoard's tops have none), and
//! the ZedBoard's app was 4 KiB over its 192 KiB of OCM with their code
//! (2026-10-18, after side K's FAT writes). The app's root says which
//! board it is built for (relay.zig's `ps_board`); the desktop's tools and
//! the tests have none and keep every spec.

const std = @import("std");
const root = @import("root");
const savestate = @import("../savestate.zig");

pub const on = !(@hasDecl(root, "ps_board") and std.mem.eql(u8, root.ps_board, "zedboard"));

/// @p s where the build has N's states, else none.
pub fn spec(comptime s: savestate.Spec) ?savestate.Spec {
    return if (on) s else null;
}
