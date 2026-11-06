//! The PS's own start-up (side track F): when the app is the boot image's
//! first-stage program, nobody has run `ps7_init` before it, so the app
//! runs it itself. `ps7_init` is the C file Vivado generates for the
//! ZedBoard sample (clocks and PLLs, MIO, DDR, peripherals as tables of
//! register writes), compiled with `zig cc` and linked into the app
//! (Makefile `PS7_INIT_C`; the thin C shim CLAUDE.md rule 9 allows; its
//! `xil_io.h` is our empty stand-in in platform/ps/c/).
//!
//! Loaded over JTAG (`make prog`), xsdb has already run the Tcl version
//! of the same tables, and the app leaves them alone; booted from the SD
//! card (or loaded over JTAG with `PS_INIT=app`), it runs them. The test
//! is the DDR controller's soft reset (DDRC ctrl bit 0,
//! `reg_ddrc_soft_rstb`): 0 from power-on until `ps7_init`'s DDR part
//! releases it as one of its last steps (UG585 appendix B; the
//! generated file, "UNLOCK DDR").

const hw = @import("hw.zig");

extern fn ps7_init() c_int;
extern fn ps7_post_config() c_int;

const ddrc_ctrl = 0xF800_6000;
const devcfg_int_sts = 0xF800_700C; // bit 2: PCFG_DONE_INT (UG585 B.16)

/// What `start` did, for the UART once it works.
pub const Start = union(enum) {
    /// xsdb (or an earlier run) had already set the PS up
    already,
    /// we ran ps7_init; its result code (0 = success, ps7_init.h)
    ran: c_int,
};

/// DDR is up, so `ps7_init` has run.
pub fn initialized() bool {
    return hw.reg(ddrc_ctrl).* & 1 != 0;
}

/// The PL is configured (its DONE has gone high since the last reset:
/// JTAG or, later, PCAP).
pub fn plConfigured() bool {
    return hw.reg(devcfg_int_sts).* & (1 << 2) != 0;
}

/// Runs `ps7_init` unless it has run already, then `ps7_post_config`
/// (PS-PL level shifters on, PL resets released) if the PL is configured,
/// as xsdb does after programming it. Called first thing in `main`,
/// before the UART (whose clock and pins `ps7_init` sets up) is used.
pub fn start() Start {
    if (initialized()) return .already;
    const r = ps7_init();
    if (r == 0 and plConfigured()) return .{ .ran = ps7_post_config() };
    return .{ .ran = r };
}

/// The line for the UART: "ps7_init: ..." (`ps7_init.h`'s codes).
pub fn describe(s: Start) []const u8 {
    return switch (s) {
        .already => "ps7_init: done before the app (xsdb)",
        .ran => |r| switch (r) {
            0 => "ps7_init: done by the app",
            1 => "ps7_init: FAILED, data corrupted",
            2 => "ps7_init: FAILED, a poll timed out",
            else => "ps7_init: FAILED (unknown code)",
        },
    };
}
