//! USB host for the PS app (side track E): the Zynq's usb0 controller, its
//! ULPI PHY and the one root port, bare metal and polled. Step 2 of PLAN.md
//! "Side track E": bring the controller up in host mode, turn on Vbus
//! through the PHY, then follow the port: connect, debounce, reset, speed,
//! disconnect. Step 3: enumerate what is plugged in (USB 9.1.2), find a
//! boot keyboard, configure it in boot protocol. Step 4: poll its
//! interrupt endpoint through the periodic schedule; `takeReport` hands
//! each new report to the caller (usb_keys.zig turns it into keys).
//!
//! `poll` never blocks: every call looks at the hardware once, maybe moves
//! the state machine on, and returns the new `Status` when it changed, so
//! the relay loop can call it between UART bytes. Timing is from the bus's
//! microsecond clock.
//!
//! Registers, bits and the rules behind each step are in usb_hw.zig, with
//! their sources (UG585 chapter 15, EHCI 1.0, USB 2.0). What the caller
//! must have set up (Vivado's ZedBoard `ps7_init` does): MIO 28 to 39 on
//! USB0 (ULPI), the USB0 AMBA clock (APER_CLK_CTRL bit 2), USB0 out of
//! reset (USB_RST_CTRL, 0 after a system reset), and the global timer.
//!
//! Generic over its register bus, `Host(Bus)`, so the tests at the end of
//! this file run it against usb_fake.zig. A Bus has `read32(*Bus, off:
//! u32) u32`, `write32(*Bus, off: u32, v: u32)`, `nowUs(*Bus) u64` and
//! `busAddr(*Bus, addr: usize) u32` (a pointer into `Dma` as the USB DMA
//! sees it).

const std = @import("std");
const hw = @import("usb_hw.zig");

/// A device's IDs, from its device descriptor.
pub const DeviceId = struct { vid: u16, pid: u16 };

pub const Status = union(enum) {
    /// before the first poll, and while the controller resets
    starting,
    /// the controller or the PHY doesn't answer: no 60 MHz ULPI clock
    /// (PHY held in reset, not powered) or no PHY on the pins
    no_phy,
    /// powered, nothing plugged in
    no_device,
    /// something is plugged in: debounce and port reset under way
    connecting,
    /// the port is enabled after its reset, at this speed; the device is
    /// being enumerated
    connected: hw.Speed,
    /// the port reset didn't enable the port (retried after a while)
    reset_failed,
    /// enumerated and configured: a boot keyboard in boot protocol
    keyboard: DeviceId,
    /// enumerated, but it has no boot keyboard interface (left alone)
    not_keyboard: DeviceId,
    /// enumeration failed three times (stalls, timeouts, bad
    /// descriptors); waits for the next plug-in
    enum_failed,

    /// The text for the menu and the UART, "USB: ..." (at most 40
    /// characters, the overlay's width).
    pub fn text(s: Status, buf: *[40]u8) []const u8 {
        return switch (s) {
            .starting => "USB: starting",
            .no_phy => "USB: no PHY (no ULPI clock?)",
            .no_device => "USB: no device",
            .connecting => "USB: device found, resetting",
            .connected => |sp| switch (sp) {
                .low => "USB: device connected (low speed)",
                .full => "USB: device connected (full speed)",
                .high => "USB: device connected (high speed)",
                .none => "USB: device connected",
            },
            .reset_failed => "USB: port reset failed",
            .keyboard => |id| std.fmt.bufPrint(buf, "USB: keyboard {X:0>4}:{X:0>4}", .{ id.vid, id.pid }) catch unreachable,
            .not_keyboard => |id| std.fmt.bufPrint(buf, "USB: {X:0>4}:{X:0>4} is not a keyboard", .{ id.vid, id.pid }) catch unreachable,
            .enum_failed => "USB: device not answering",
        };
    }
};

/// Time limits and waits, in microseconds.
const t_reset = 10_000; // controller reset (USBCMD.RST); TRM gives none
const t_halt = 10_000; // HCH after clearing RS (EHCI 2.3.1: 16 micro-frames)
const t_ulpi = 10_000; // one viewport access
const t_debounce = 100_000; // USB 7.1.7.3: connect to reset
const t_reset_hold = 50_000; // USB 7.1.7.5: TDRSTR, a root port's reset
const t_reset_end = 20_000; // PR = 0 to the port enabled (Linux waits 2 ms)
const t_recovery = 10_000; // USB 7.1.7.3: reset recovery
const t_set_address = 2_000; // USB 9.2.6.3: after SET_ADDRESS's status stage
const t_schedule = 10_000; // USBSTS.AS following USBCMD.ASE (EHCI 4.8)
const t_control = 500_000; // one control transfer (USB 9.2.6.4: 500 ms per data stage)
const t_retry = 1_000_000; // after a failure, before trying again
/// Vbus off at start-up, long enough for a plugged-in device to lose power
/// (JP3's >120 uF hold it up a few ms under a receiver's load)
const t_vbus_off = 250_000;
const enum_tries = 3;

/// PHY register accesses at bring-up: the IDs, Vbus off, then (after port
/// power and `t_vbus_off`) Vbus on. The ZedBoard's TPS2051 is enabled by
/// the TUSB1210's CPEN; Linux sets both bits for `drv-vbus` (phy-ulpi.c).
///
/// The power cycle is there because the PHY keeps its registers across a
/// `make prog` (its reset pin is a PL pin, which floats high while the PL
/// is configured), so a device can still be powered and attached from the
/// last run. The controller only hears of line-state *changes* (ULPI RX
/// CMD), so after its reset it never saw that device (board, 2026-10-04:
/// found only after a replug). Off and on again, with port power on, it
/// attaches anew.
const ulpi_ops = [_]u32{
    hw.ulpiRead(hw.ulpi_vendor_lo),
    hw.ulpiRead(hw.ulpi_vendor_hi),
    hw.ulpiRead(hw.ulpi_product_lo),
    hw.ulpiRead(hw.ulpi_product_hi),
    hw.ulpiWrite(hw.ulpi_otg_ctrl + hw.ulpi_clr, hw.otg_drv_vbus | hw.otg_drv_vbus_ext),
    hw.ulpiWrite(hw.ulpi_otg_ctrl + hw.ulpi_set, hw.otg_drv_vbus | hw.otg_drv_vbus_ext),
};
const ulpi_vbus_off_done = 5; // ops done when Vbus is off

/// A register snapshot: USBCMD, USBSTS, PORTSC1, ULPI_VIEWPORT, USBMODE.
pub const Regs = struct { cmd: u32 = 0, sts: u32 = 0, portsc: u32 = 0, ulpi: u32 = 0, mode: u32 = 0 };

/// The memory the controller reads and writes (DMA): the control
/// endpoint's queue head, the three stages of a control transfer, and
/// their buffers. Anywhere the USB DMA can reach (OCM or DDR); the driver
/// accesses it through volatile pointers only.
pub const Dma = extern struct {
    /// the periodic frame list (EHCI 3.1): 32 entries, 4 KiB aligned
    frame_list: [frame_list_len]u32 align(4096),
    qh: hw.Qh align(32),
    qtd: [3]hw.Qtd align(32),
    setup: [8]u8 align(32),
    buf: [256]u8 align(32),
    /// the keyboard's interrupt-IN endpoint
    int_qh: hw.Qh align(32),
    int_qtd: hw.Qtd align(32),
    report: [64]u8 align(32),
};

/// Where the PS app keeps `Dma`: the start of DDR above OCM, 64 KiB
/// reserved for the USB host (docs/ARCHITECTURE.md, "USB host"). DDR
/// rather than OCM because the frame list's 4 KiB alignment and the
/// buffers cost OCM the app needs; the USB DMA reaches DDR there (UG585
/// table 4-1) and, with the MMU off, the CPU's accesses are uncached.
pub const dma_ddr_addr: usize = 0x0010_0000;
comptime {
    std.debug.assert(@sizeOf(Dma) <= 0x10000);
}

/// Frame list entries: 32 ms of schedule, enough for any keyboard's
/// polling interval (powers of two up to 32 ms).
pub const frame_list_len = 32;
/// USBCMD's frame list size for 32 entries: FS2:FS1:FS0 = 101
const cmd_fs_32: u32 = (1 << 15) | (1 << 2);

/// The polling period, in frames, for an endpoint's bInterval (full and
/// low speed: ms; USB 9.6.6): the largest power of two not above it, at
/// most the frame list's length.
pub fn pollPeriod(interval: u8) u32 {
    var n: u32 = 1;
    while (n * 2 <= interval and n * 2 <= frame_list_len) n *= 2;
    return n;
}

/// The address the driver gives the device (one device, no hubs).
pub const dev_address: u7 = 1;

const CtlResult = union(enum) {
    /// done; bytes moved in the data stage
    ok: usize,
    stall,
    /// transaction error, babble or buffer error (the qTD's status bits)
    err: u32,
    timeout,
};

pub fn Host(comptime Bus: type) type {
    return struct {
        const Self = @This();
        const State = enum {
            start,
            wait_halt,
            wait_reset,
            ulpi_wake,
            ulpi,
            vbus_off,
            wait_connect,
            debounce,
            port_reset,
            port_reset_end,
            recovery,
            // enumeration (USB 9.1.2), one control transfer each
            async_off,
            async_on,
            get_dev8,
            set_address,
            address_wait,
            get_dev,
            get_cfg_head,
            get_cfg,
            set_config,
            set_protocol,
            set_idle,
            // the keyboard's interrupt endpoint into the periodic schedule
            periodic_off,
            periodic_on,
            /// enumerated (or given up): polling the keyboard if there is
            /// one, watching for the unplug
            ready,
            failed,
        };

        bus: *Bus,
        dma: *volatile Dma,
        state: State = .start,
        /// when the current state began (or the current ULPI access)
        since: u64 = 0,
        status: Status = .starting,
        ulpi_step: usize = 0,
        ulpi_busy: bool = false,
        /// what the PHY said its IDs are (ULPI registers 0 to 3)
        phy_id: [4]u8 = .{ 0, 0, 0, 0 },
        speed: hw.Speed = .none,
        /// how many times the port was reset and enabled since start-up
        connects: u32 = 0,
        /// where the last failure happened, with the registers then, for
        /// the UART (the board can't be debugged any other way)
        fail_state: State = .start,
        fail_regs: Regs = .{},
        /// what went wrong in the last failed enumeration step, and how
        /// many have failed since start-up (retried ones too)
        fail_why: []const u8 = "",
        enum_failures: u32 = 0,

        // the device being enumerated
        addr: u7 = 0,
        mps0: u11 = 8,
        id: DeviceId = .{ .vid = 0, .pid = 0 },
        config_value: u8 = 0,
        config_len: u16 = 0,
        keyboard: ?hw.BootKeyboard = null,
        tries: u8 = 0,
        /// the state after the async schedule is back on (`requeue`)
        after: State = .get_dev8,
        /// a control transfer is under way
        ctl_busy: bool = false,
        ctl_start: u64 = 0,
        ctl_len: u16 = 0,
        /// the last transfer's data, copied out of DMA memory
        data: [256]u8 = undefined,
        /// the keyboard's last report, and whether the caller has it yet
        report: [64]u8 = undefined,
        report_len: usize = 0,
        report_new: bool = false,
        /// reports received since start-up
        reports: u32 = 0,

        pub fn init(bus: *Bus, dma: *volatile Dma) Self {
            return .{ .bus = bus, .dma = dma };
        }

        /// The keyboard's newest report, once (null if none came since).
        pub fn takeReport(h: *Self) ?[]const u8 {
            if (!h.report_new) return null;
            h.report_new = false;
            return h.report[0..h.report_len];
        }

        pub fn phyVendor(h: *const Self) u16 {
            return @as(u16, h.phy_id[1]) << 8 | h.phy_id[0];
        }

        pub fn phyProduct(h: *const Self) u16 {
            return @as(u16, h.phy_id[3]) << 8 | h.phy_id[2];
        }

        fn rd(h: *Self, off: u32) u32 {
            return h.bus.read32(off);
        }

        fn wr(h: *Self, off: u32, v: u32) void {
            h.bus.write32(off, v);
        }

        fn busAddr(h: *Self, p: anytype) u32 {
            return h.bus.busAddr(@intFromPtr(p));
        }

        fn enter(h: *Self, s: State, now: u64) void {
            h.state = s;
            h.since = now;
        }

        /// PORTSC1 read-modify-write that never clears a change bit by
        /// accident (they are write-1-to-clear).
        fn port(h: *Self, set: u32, clear: u32) void {
            const v = h.rd(hw.reg_portsc1) & ~hw.port_w1c;
            h.wr(hw.reg_portsc1, (v | set) & ~clear);
        }

        /// One step. Returns the new status when it changed.
        pub fn poll(h: *Self) ?Status {
            const before = h.status;
            h.step();
            if (std.meta.eql(before, h.status)) return null;
            return h.status;
        }

        fn step(h: *Self) void {
            const now = h.bus.nowUs();
            const dt = now - h.since;
            // from the reset recovery on, an unplug ends whatever is going on
            if (@intFromEnum(h.state) >= @intFromEnum(State.recovery) and h.state != .failed) {
                const p = h.rd(hw.reg_portsc1);
                if (p & hw.port_ccs == 0 or p & hw.port_pe == 0 or p & hw.port_csc != 0) {
                    // unplugged (or replugged before we looked): the
                    // controller disabled the port (TRM PORTSC1.PEC)
                    return h.unplugged(now);
                }
            }
            switch (h.state) {
                .start => {
                    h.status = .starting;
                    h.ulpi_step = 0;
                    h.ulpi_busy = false;
                    h.speed = .none;
                    // left running (the app was loaded again)? Halt first:
                    // a reset while running is undefined (TRM 15.15.2).
                    // HCH only means something in host mode: after a
                    // power-on the controller is idle (USBMODE.CM = 00)
                    // and HCH reads 0 (seen on the board 2026-10-04)
                    const running = h.rd(hw.reg_usbmode) & hw.mode_cm_mask == hw.mode_cm_host and
                        h.rd(hw.reg_usbcmd) & hw.cmd_rs != 0;
                    if (running) {
                        h.wr(hw.reg_usbcmd, h.rd(hw.reg_usbcmd) & ~(hw.cmd_rs | hw.cmd_ase | hw.cmd_pse));
                        h.enter(.wait_halt, now);
                    } else {
                        h.wr(hw.reg_usbcmd, hw.cmd_rst);
                        h.enter(.wait_reset, now);
                    }
                },
                .wait_halt => {
                    if (h.rd(hw.reg_usbsts) & hw.sts_hch != 0) {
                        h.wr(hw.reg_usbcmd, hw.cmd_rst);
                        h.enter(.wait_reset, now);
                    } else if (dt > t_halt) h.fail(.no_phy, now);
                },
                .wait_reset => {
                    if (h.rd(hw.reg_usbcmd) & hw.cmd_rst == 0) {
                        h.wr(hw.reg_usbmode, hw.mode_cm_host);
                        h.enter(.ulpi_wake, now);
                    } else if (dt > t_reset) h.fail(.no_phy, now);
                },
                .ulpi_wake => {
                    // a wake-up before every access, as Linux's ChipIdea
                    // driver does (drivers/usb/chipidea/ulpi.c: WAKEUP,
                    // wait, then RUN; 10 ms each): a PHY in low-power
                    // mode answers nothing else (TRM ULPI_VIEWPORT)
                    if (!h.ulpi_busy) {
                        h.wr(hw.reg_ulpi_viewport, hw.ulpi_wu);
                        h.ulpi_busy = true;
                        h.since = now;
                    } else if (h.rd(hw.reg_ulpi_viewport) & hw.ulpi_wu == 0) {
                        h.ulpi_busy = false;
                        h.enter(.ulpi, now);
                    } else if (dt > t_ulpi) h.fail(.no_phy, now);
                },
                .ulpi => {
                    if (!h.ulpi_busy) {
                        h.wr(hw.reg_ulpi_viewport, ulpi_ops[h.ulpi_step]);
                        h.ulpi_busy = true;
                        h.since = now;
                        return;
                    }
                    const v = h.rd(hw.reg_ulpi_viewport);
                    if (v & hw.ulpi_run != 0) {
                        if (dt > t_ulpi) h.fail(.no_phy, now);
                        return;
                    }
                    h.ulpi_busy = false;
                    if (h.ulpi_step < 4) h.phy_id[h.ulpi_step] = hw.ulpiData(v);
                    h.ulpi_step += 1;
                    if (h.ulpi_step == 4 and (h.phyVendor() == 0 or h.phyVendor() == 0xFFFF)) {
                        // nothing answers on the ULPI data lines
                        h.fail(.no_phy, now);
                    } else if (h.ulpi_step == ulpi_vbus_off_done) {
                        // Vbus is off: run (SOFs once a device is enabled;
                        // the async schedule comes with a device), port
                        // power on, so the attach after Vbus on is heard
                        // the frame list, empty, is in place from here on;
                        // the periodic schedule runs only with a keyboard
                        for (&h.dma.frame_list) |*e| e.* = hw.link_t;
                        h.wr(hw.reg_periodiclistbase, h.busAddr(&h.dma.frame_list));
                        h.wr(hw.reg_usbcmd, hw.cmd_rs | cmd_fs_32);
                        h.port(hw.port_pp, 0);
                        h.enter(.vbus_off, now);
                    } else if (h.ulpi_step == ulpi_ops.len) {
                        h.status = .no_device;
                        h.enter(.wait_connect, now);
                    } else h.enter(.ulpi_wake, now);
                },
                .vbus_off => if (dt >= t_vbus_off) h.enter(.ulpi_wake, now),
                .wait_connect => {
                    const p = h.rd(hw.reg_portsc1);
                    if (p & hw.port_csc != 0) h.port(hw.port_csc, 0);
                    if (p & hw.port_ccs != 0) {
                        h.status = .connecting;
                        h.tries = 0;
                        h.enter(.debounce, now);
                    } else h.status = .no_device;
                },
                .debounce => {
                    const p = h.rd(hw.reg_portsc1);
                    if (p & hw.port_ccs == 0 or p & hw.port_csc != 0) {
                        // bounced or replugged: start over
                        h.port(hw.port_csc, 0);
                        h.enter(.wait_connect, now);
                    } else if (dt >= t_debounce) {
                        h.port(hw.port_pr, 0);
                        h.enter(.port_reset, now);
                    }
                },
                // UG585 15.11.4 says the controller ends the reset by itself
                // after 10 ms; on the board it didn't (PR still 1 after 50
                // ms, 2026-10-04). So, as EHCI 2.3.9 and Linux do: hold PR
                // for 50 ms, write PR = 0, wait for PR = 0 and PE = 1. A
                // controller that ends it early just gets there sooner.
                .port_reset => {
                    const p = h.rd(hw.reg_portsc1);
                    if (p & hw.port_ccs == 0) {
                        h.port(hw.port_csc, 0);
                        h.enter(.wait_connect, now);
                    } else if (p & hw.port_pr == 0 and p & hw.port_pe != 0) {
                        h.portEnabled(p, now);
                    } else if (dt >= t_reset_hold) {
                        h.port(0, hw.port_pr);
                        h.enter(.port_reset_end, now);
                    }
                },
                .port_reset_end => {
                    const p = h.rd(hw.reg_portsc1);
                    if (p & hw.port_ccs == 0) {
                        h.port(hw.port_csc, 0);
                        h.enter(.wait_connect, now);
                    } else if (p & hw.port_pr == 0 and p & hw.port_pe != 0) {
                        h.portEnabled(p, now);
                    } else if (dt > t_reset_end) h.fail(.reset_failed, now);
                },
                .recovery => if (dt >= t_recovery) {
                    h.connects += 1;
                    h.status = .{ .connected = h.speed };
                    // a device after a reset: address 0, and only 8 bytes
                    // of EP0 are safe until its descriptor says more
                    h.addr = 0;
                    h.mps0 = 8;
                    h.keyboard = null;
                    h.requeue(.get_dev8, now);
                },

                // The control QH changes (address, max packet) only while
                // the async schedule is off: EHCI 4.8.2 lets software touch
                // a QH's endpoint words only when the controller can't be
                // looking at it. The same path cleans up after a stall or
                // a timeout (halted QH, qTDs left active).
                .async_off => if (h.rd(hw.reg_usbsts) & hw.sts_as == 0) {
                    h.writeQh();
                    h.wr(hw.reg_asynclistaddr, h.busAddr(&h.dma.qh));
                    h.wr(hw.reg_usbcmd, h.rd(hw.reg_usbcmd) | hw.cmd_ase);
                    h.enter(.async_on, now);
                } else if (dt > t_schedule) h.enumFail("async schedule won't stop", now),
                .async_on => if (h.rd(hw.reg_usbsts) & hw.sts_as != 0) {
                    h.ctl_busy = false;
                    h.enter(h.after, now);
                } else if (dt > t_schedule) h.enumFail("async schedule won't start", now),

                .get_dev8 => if (h.control(now, hw.getDescriptor(hw.dt_device, 0, 0, 8))) |r| {
                    // USB 9.6.1 practice: the first 8 bytes give
                    // bMaxPacketSize0, which every later transfer needs
                    if (h.ok(r, now)) |n| {
                        if (n < 8) return h.enumFail("short device descriptor", now);
                        switch (h.data[7]) {
                            8, 16, 32, 64 => {},
                            else => return h.enumFail("bad bMaxPacketSize0", now),
                        }
                        h.mps0 = h.data[7];
                        h.requeue(.set_address, now);
                    }
                },
                .set_address => if (h.control(now, .{ .request_type = 0, .request = hw.req_set_address, .value = dev_address, .index = 0, .length = 0 })) |r| {
                    if (h.ok(r, now)) |_| h.enter(.address_wait, now);
                },
                .address_wait => if (dt >= t_set_address) {
                    h.addr = dev_address;
                    h.requeue(.get_dev, now);
                },
                .get_dev => if (h.control(now, hw.getDescriptor(hw.dt_device, 0, 0, 18))) |r| {
                    if (h.ok(r, now)) |n| {
                        const d = hw.parseDevice(h.data[0..n]) catch return h.enumFail("bad device descriptor", now);
                        h.id = .{ .vid = d.vendor, .pid = d.product };
                        h.enter(.get_cfg_head, now);
                    }
                },
                .get_cfg_head => if (h.control(now, hw.getDescriptor(hw.dt_config, 0, 0, 9))) |r| {
                    if (h.ok(r, now)) |n| {
                        const c = hw.parseConfigHead(h.data[0..n]) catch return h.enumFail("bad configuration", now);
                        if (c.total > h.data.len) return h.enumFail("configuration too long", now);
                        h.config_len = c.total;
                        h.enter(.get_cfg, now);
                    }
                },
                .get_cfg => if (h.control(now, hw.getDescriptor(hw.dt_config, 0, 0, h.config_len))) |r| {
                    if (h.ok(r, now)) |n| {
                        const cfg = h.data[0..n];
                        const c = hw.parseConfig(cfg) catch return h.enumFail("bad configuration", now);
                        h.config_value = c.value;
                        h.keyboard = hw.findBootKeyboard(cfg);
                        if (h.keyboard == null) {
                            // a mouse, a stick, a gamepad: not ours (yet)
                            h.status = .{ .not_keyboard = h.id };
                            h.enter(.ready, now);
                        } else h.enter(.set_config, now);
                    }
                },
                .set_config => if (h.control(now, .{ .request_type = 0, .request = hw.req_set_configuration, .value = h.config_value, .index = 0, .length = 0 })) |r| {
                    if (h.ok(r, now)) |_| h.enter(.set_protocol, now);
                },
                // HID 7.2.6: boot protocol, so every report is the 8-byte
                // boot format whatever the report descriptor says
                .set_protocol => if (h.control(now, .{ .request_type = hw.rt_class | hw.rt_interface, .request = hw.hid_set_protocol, .value = 0, .index = h.keyboard.?.interface, .length = 0 })) |r| {
                    if (h.ok(r, now)) |_| h.enter(.set_idle, now);
                },
                // HID 7.2.4: idle rate 0, a report only when something
                // changes. Optional for a device: a stall is fine (the QH
                // is halted then, so it is cleaned up through requeue).
                .set_idle => if (h.control(now, .{ .request_type = hw.rt_class | hw.rt_interface, .request = hw.hid_set_idle, .value = 0, .index = h.keyboard.?.interface, .length = 0 })) |r| {
                    if (r == .stall) {
                        h.requeue(.periodic_off, now);
                    } else if (h.ok(r, now)) |_| {
                        h.enter(.periodic_off, now);
                    }
                },
                // EHCI 4.6: the periodic schedule changes only while it is
                // off (as the async one); then the QH goes into every
                // `pollPeriod`th frame list entry
                .periodic_off => if (h.rd(hw.reg_usbsts) & hw.sts_ps == 0) {
                    h.linkInterrupt();
                    h.wr(hw.reg_usbcmd, h.rd(hw.reg_usbcmd) | hw.cmd_pse);
                    h.enter(.periodic_on, now);
                } else if (dt > t_schedule) h.enumFail("periodic schedule won't stop", now),
                .periodic_on => if (h.rd(hw.reg_usbsts) & hw.sts_ps != 0) {
                    h.status = .{ .keyboard = h.id };
                    h.enter(.ready, now);
                } else if (dt > t_schedule) h.enumFail("periodic schedule won't start", now),
                .ready => if (h.keyboard != null and h.status == .keyboard) {
                    const t = h.dma.int_qtd.token;
                    if (t & hw.tok_halted != 0) {
                        // a stall or three transaction errors: start over
                        h.enumFail("interrupt endpoint halted", now);
                    } else if (t & hw.tok_active == 0) {
                        const kb = h.keyboard.?;
                        const n = kb.max_packet - hw.tokenBytes(t);
                        for (h.report[0..n], 0..) |*b, k| b.* = h.dma.report[k];
                        h.report_len = n;
                        h.report_new = true;
                        h.reports += 1;
                        h.armInterrupt();
                    }
                },
                .failed => if (dt >= t_retry) {
                    // a port reset that didn't take: try the port again;
                    // a dead PHY: start from the controller reset
                    if (h.status == .reset_failed) {
                        h.port(hw.port_csc | hw.port_pec, hw.port_pe);
                        h.enter(.wait_connect, now);
                    } else h.enter(.start, now);
                },
            }
        }

        /// The state the last failure happened in, as text.
        pub fn failState(h: *const Self) []const u8 {
            return @tagName(h.fail_state);
        }

        /// The reset is over and the port enabled; PSPD has the speed
        /// (TRM 15.11.5).
        fn portEnabled(h: *Self, p: u32, now: u64) void {
            h.speed = hw.portSpeed(p);
            h.enter(.recovery, now);
        }

        fn unplugged(h: *Self, now: u64) void {
            h.port(hw.port_csc | hw.port_pec, 0);
            h.schedulesOff();
            h.speed = .none;
            h.keyboard = null;
            h.ctl_busy = false;
            h.status = .no_device;
            h.enter(.wait_connect, now);
        }

        fn asyncOff(h: *Self) void {
            const c = h.rd(hw.reg_usbcmd);
            if (c & hw.cmd_ase != 0) h.wr(hw.reg_usbcmd, c & ~hw.cmd_ase);
        }

        fn schedulesOff(h: *Self) void {
            const c = h.rd(hw.reg_usbcmd);
            if (c & (hw.cmd_ase | hw.cmd_pse) != 0) h.wr(hw.reg_usbcmd, c & ~(hw.cmd_ase | hw.cmd_pse));
            if (h.report_len != 0 or h.report_new) {
                // the keyboard is gone: whatever it held is released
                h.report = [_]u8{0} ** 64;
                h.report_len = 8;
                h.report_new = true;
            }
        }

        /// The keyboard's interrupt QH (DTC = 0: the QH keeps the data
        /// toggle, which starts at DATA0 after SET_CONFIGURATION, USB
        /// 9.4.5) in the frame list, with its first qTD. Full and low
        /// speed go through the root's TT as split transactions: start in
        /// micro-frame 0, complete in 2 to 4 (EHCI 4.12.2; TRM 15.11.7).
        fn linkInterrupt(h: *Self) void {
            const kb = h.keyboard.?;
            const q = &h.dma.int_qh;
            q.link = hw.link_t;
            q.chars = hw.qhChars(h.addr, @truncate(kb.endpoint), h.speed, @intCast(@min(kb.max_packet, 64)), false, false);
            q.caps = hw.qhCaps(1, 0x01, 0x1C);
            q.current = 0;
            q.overlay.next = hw.link_t;
            q.overlay.alt_next = hw.link_t;
            q.overlay.token = 0;
            for (&q.overlay.buf) |*b| b.* = 0;
            h.armInterrupt();
            const period = pollPeriod(kb.interval);
            for (&h.dma.frame_list, 0..) |*e, i| {
                e.* = if (i % period == 0) h.busAddr(q) | hw.typ_qh else hw.link_t;
            }
        }

        /// A fresh IN qTD for the next report, handed to the idle QH.
        fn armInterrupt(h: *Self) void {
            const kb = h.keyboard.?;
            const d = h.dma;
            const q = &d.int_qtd;
            q.next = hw.link_t;
            q.alt_next = hw.link_t;
            q.buf[0] = h.busAddr(&d.report);
            var k: u32 = 1;
            while (k < 5) : (k += 1) q.buf[k] = 0;
            q.token = hw.token(.in, @min(kb.max_packet, 64), 0, true);
            d.int_qh.overlay.next = h.busAddr(q);
        }

        /// Async schedule off, rewrite the control QH, back on, then `next`.
        fn requeue(h: *Self, next: State, now: u64) void {
            h.asyncOff();
            h.after = next;
            h.enter(.async_off, now);
        }

        /// The control endpoint's QH, alone in the async ring and its head
        /// (EHCI 4.8.3), idle: no qTD, not halted.
        fn writeQh(h: *Self) void {
            const q = &h.dma.qh;
            q.link = h.busAddr(q) | hw.typ_qh;
            q.chars = hw.qhChars(h.addr, 0, h.speed, h.mps0, true, true);
            // port 1 of the root's TT, as Linux sets it for a device on
            // the root port (ehci-q.c, qh_make: ttport)
            q.caps = hw.qhCaps(1, 0, 0);
            q.current = 0;
            q.overlay.next = hw.link_t;
            q.overlay.alt_next = hw.link_t;
            q.overlay.token = 0;
            for (&q.overlay.buf) |*b| b.* = 0;
        }

        /// Starts a control transfer, or, while one runs, polls it; null
        /// until it ends. The data stage uses `dma.buf`; the result's data
        /// is copied to `data`.
        fn control(h: *Self, now: u64, s: hw.Setup) ?CtlResult {
            const d = h.dma;
            if (!h.ctl_busy) {
                h.ctl_busy = true;
                h.ctl_start = now;
                h.ctl_len = s.length;
                const in = s.deviceToHost();
                d.setup = s.bytes();
                const status_i: usize = if (s.length > 0) 2 else 1;
                h.qtd(0, .setup, h.busAddr(&d.setup), 8, 0, false);
                if (s.length > 0) h.qtd(1, if (in) .in else .out, h.busAddr(&d.buf), s.length, 1, false);
                h.qtd(status_i, if (in and s.length > 0) .out else .in, h.busAddr(&d.buf), 0, 1, true);
                d.qtd[0].next = h.busAddr(&d.qtd[1]);
                if (s.length > 0) {
                    d.qtd[1].next = h.busAddr(&d.qtd[2]);
                    // a short IN data stage goes on to the status stage
                    d.qtd[1].alt_next = h.busAddr(&d.qtd[2]);
                }
                // hand the queue to the QH last: it is idle (inactive
                // overlay), so the controller picks the new qTD up on its
                // next visit (EHCI 4.10.2)
                d.qh.overlay.next = h.busAddr(&d.qtd[0]);
                d.qh.overlay.token = 0;
                return null;
            }
            const n_qtd: usize = if (h.ctl_len > 0) 3 else 2;
            var i: usize = 0;
            while (i < n_qtd) : (i += 1) {
                const t = d.qtd[i].token;
                if (t & hw.tok_halted != 0) {
                    h.ctl_busy = false;
                    const errs = t & (hw.tok_xact_err | hw.tok_babble | hw.tok_buf_err);
                    return if (errs == 0) .stall else .{ .err = t };
                }
            }
            if (d.qtd[n_qtd - 1].token & hw.tok_active == 0) {
                h.ctl_busy = false;
                const n: usize = if (h.ctl_len > 0) h.ctl_len - hw.tokenBytes(d.qtd[1].token) else 0;
                for (h.data[0..n], 0..) |*b, k| b.* = d.buf[k];
                return .{ .ok = n };
            }
            if (now - h.ctl_start > t_control) {
                h.ctl_busy = false;
                return .timeout;
            }
            return null;
        }

        fn qtd(h: *Self, i: usize, pid: hw.Pid, buf: u32, len: u32, toggle: u1, ioc: bool) void {
            const q = &h.dma.qtd[i];
            q.next = hw.link_t;
            q.alt_next = hw.link_t;
            q.buf[0] = buf;
            var k: u32 = 1;
            while (k < 5) : (k += 1) q.buf[k] = (buf & ~@as(u32, 0xFFF)) + 4096 * k;
            q.token = hw.token(pid, len, toggle, ioc);
        }

        /// The bytes moved, or null after recording the failure.
        fn ok(h: *Self, r: CtlResult, now: u64) ?usize {
            switch (r) {
                .ok => |n| return n,
                .stall => h.enumFail("stall", now),
                .err => h.enumFail("transaction error", now),
                .timeout => h.enumFail("timeout", now),
            }
            return null;
        }

        /// Enumeration went wrong: reset the port and start again, up to
        /// `enum_tries` times; then wait for the device to be replugged.
        fn enumFail(h: *Self, why: []const u8, now: u64) void {
            h.fail_why = why;
            h.enum_failures += 1;
            h.fail_state = h.state;
            h.fail_regs = h.snapshot();
            h.ctl_busy = false;
            h.schedulesOff();
            h.tries += 1;
            if (h.tries < enum_tries) {
                h.status = .connecting;
                h.port(hw.port_pr, 0);
                h.enter(.port_reset, now);
            } else {
                h.status = .enum_failed;
                h.enter(.ready, now);
            }
        }

        fn snapshot(h: *Self) Regs {
            return .{
                .cmd = h.rd(hw.reg_usbcmd),
                .sts = h.rd(hw.reg_usbsts),
                .portsc = h.rd(hw.reg_portsc1),
                .ulpi = h.rd(hw.reg_ulpi_viewport),
                .mode = h.rd(hw.reg_usbmode),
            };
        }

        fn fail(h: *Self, s: Status, now: u64) void {
            h.fail_state = h.state;
            h.fail_regs = h.snapshot();
            h.status = s;
            h.ulpi_busy = false;
            h.enter(.failed, now);
        }
    };
}

/// The real controller (usb0), through volatile pointers; time from the
/// Cortex-A9 global timer (333.333 MHz on the ZedBoard preset, so
/// microseconds = ticks * 3 / 1000), as sd.MmioBus.
pub const MmioBus = struct {
    const gtimer_lo = 0xF8F0_0200;
    const gtimer_hi = 0xF8F0_0204;

    pub fn read32(_: *MmioBus, off: u32) u32 {
        const p: *volatile u32 = @ptrFromInt(hw.usb0_base + off);
        return p.*;
    }

    pub fn write32(_: *MmioBus, off: u32, v: u32) void {
        const p: *volatile u32 = @ptrFromInt(hw.usb0_base + off);
        p.* = v;
    }

    pub fn nowUs(_: *MmioBus) u64 {
        const hi_p: *volatile u32 = @ptrFromInt(gtimer_hi);
        const lo_p: *volatile u32 = @ptrFromInt(gtimer_lo);
        while (true) {
            const hi = hi_p.*;
            const lo = lo_p.*;
            if (hi_p.* == hi) return (@as(u64, hi) << 32 | lo) * 3 / 1000;
        }
    }

    /// DMA addresses are CPU addresses: OCM mapped low and DDR look the
    /// same to the USB DMA (TRM table 4-1).
    pub fn busAddr(_: *MmioBus, addr: usize) u32 {
        return @intCast(addr);
    }
};

// --- Host tests: the driver against the fake controller ------------------------

const testing = std.testing;
const fake = @import("usb_fake.zig");
const fixtures = @import("usb_fixtures.zig");
const TestHost = Host(fake.Fake);

const mini_id: DeviceId = .{ .vid = 0x1997, .pid = 0x2433 };
const thumb_id: DeviceId = .{ .vid = 0x04B4, .pid = 0x0818 };

const Rig = struct {
    dma: Dma = undefined,
    f: fake.Fake = undefined,
    d: fake.Device = undefined,
    h: TestHost = undefined,

    fn init(r: *Rig, plugged: bool) !void {
        return r.initWith(&fixtures.mini_keyboard, plugged);
    }

    fn initWith(r: *Rig, fx: *const fake.Fixture, plugged: bool) !void {
        r.f = fake.Fake.init(std.mem.asBytes(&r.dma), 0x0010_0000);
        r.d = try fake.Device.init(fx);
        if (plugged) r.f.plug(&r.d);
        r.h = TestHost.init(&r.f, &r.dma);
    }

    /// Poll for `us` of fake time; returns the statuses seen, in order.
    fn run(r: *Rig, us: u64, seen: []Status) usize {
        var n: usize = 0;
        const t = r.f.time;
        while (r.f.time - t < us) {
            if (r.h.poll()) |s| {
                if (n < seen.len) seen[n] = s;
                n += 1;
            }
        }
        return n;
    }
};

test "bring-up and enumeration: PHY, Vbus, connect, reset, the keyboard configured in boot protocol" {
    var r: Rig = .{};
    try r.init(true);
    var seen: [8]Status = undefined;
    const n = r.run(600_000, &seen);
    try testing.expectEqual(@as(usize, 4), n);
    try testing.expectEqual(Status.no_device, seen[0]);
    try testing.expectEqual(Status.connecting, seen[1]);
    try testing.expectEqual(Status{ .connected = .full }, seen[2]);
    try testing.expectEqual(Status{ .keyboard = mini_id }, seen[3]);
    try testing.expectEqual(fake.tusb1210_vendor, r.h.phyVendor());
    try testing.expectEqual(fake.tusb1210_product, r.h.phyProduct());
    try testing.expectEqual(hw.otg_drv_vbus | hw.otg_drv_vbus_ext, r.f.phy[hw.ulpi_otg_ctrl] & (hw.otg_drv_vbus | hw.otg_drv_vbus_ext));
    // the device as the driver left it
    try testing.expectEqual(dev_address, r.d.address);
    try testing.expectEqual(@as(u8, 1), r.d.configuration);
    try testing.expectEqual(@as(u1, 0), r.d.protocol[0]);
    try testing.expectEqual(@as(u8, 0), r.d.idle[0]);
    try testing.expectEqual(hw.BootKeyboard{ .interface = 0, .endpoint = 0x81, .max_packet = 64, .interval = 10 }, r.h.keyboard.?);
    var buf: [40]u8 = undefined;
    try testing.expectEqualStrings("USB: keyboard 1997:2433", r.h.status.text(&buf));
    try testing.expect(r.f.read32(hw.reg_portsc1) & hw.port_w1c == 0);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

/// Polls until the host has a report (at most `us` of fake time).
fn nextReport(r: *Rig, us: u64) ![]const u8 {
    const t = r.f.time;
    while (r.f.time - t < us) {
        _ = r.h.poll();
        if (r.h.takeReport()) |rep| return rep;
    }
    return error.NoReport;
}

/// Enumerated, and the report a device may send right after
/// configuration (all keys up) taken: the next report is a key change.
fn readyRig(r: *Rig) !void {
    var seen: [8]Status = undefined;
    _ = r.run(600_000, &seen);
    if (r.h.status != .keyboard) return error.NotReady;
    _ = nextReport(r, 50_000) catch {};
}

test "poll period: the largest power of two up to bInterval, at most 32" {
    try testing.expectEqual(@as(u32, 1), pollPeriod(0));
    try testing.expectEqual(@as(u32, 1), pollPeriod(1));
    try testing.expectEqual(@as(u32, 8), pollPeriod(10));
    try testing.expectEqual(@as(u32, 16), pollPeriod(16));
    try testing.expectEqual(@as(u32, 32), pollPeriod(255));
}

test "key reports through the periodic schedule: press and release, within one poll period" {
    var r: Rig = .{};
    try r.init(true);
    try readyRig(&r);
    // SET_IDLE(0): nothing until a key changes
    try testing.expectError(error.NoReport, nextReport(&r, 100_000));
    r.d.press(0x0E); // 'k'
    const t = r.f.time;
    const a = try nextReport(&r, 20_000);
    try testing.expect(r.f.time - t <= 9_000); // every 8 frames, plus the frame itself
    try testing.expectEqualSlices(u8, &.{ 0, 0, 0x0E, 0, 0, 0, 0, 0 }, a);
    r.d.release(0x0E);
    const b = try nextReport(&r, 20_000);
    try testing.expectEqualSlices(u8, &[_]u8{0} ** 8, b);
    try testing.expectEqual(@as(u32, 2), r.d.reports_sent - 1);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "the recorded typing, from the emulated receiver through the driver to CHIP-8 keys" {
    const keymap = @import("keymap.zig");
    const usb_keys = @import("usb_keys.zig");
    var r: Rig = .{};
    try r.init(true);
    try readyRig(&r);
    const reports_before = r.h.reports;
    const m = keymap.defaults(.chip8);
    var kb = usb_keys.Keyboard.init(&m);
    var prev: u8 = 0;
    for (fixtures.mini_keyboard_typing) |want| {
        if (want[2] != 0) r.d.press(want[2]) else r.d.release(prev);
        prev = want[2];
        const got = try nextReport(&r, 20_000);
        try testing.expectEqualSlices(u8, &want, got);
        _ = kb.feed(got);
    }
    try testing.expectEqual(@as(u16, 0), kb.word);
    try testing.expectEqual(@as(u32, fixtures.mini_keyboard_typing.len), r.h.reports - reports_before);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "unplugged with a key held: a release report; replugged: reports again" {
    var r: Rig = .{};
    try r.init(true);
    try readyRig(&r);
    r.d.press(0x1A); // 'w'
    _ = try nextReport(&r, 20_000);
    r.f.unplug();
    const rel = try nextReport(&r, 20_000);
    try testing.expectEqualSlices(u8, &[_]u8{0} ** 8, rel);
    try testing.expectEqual(Status.no_device, r.h.status);
    r.d.release(0x1A);
    r.f.plug(&r.d);
    try readyRig(&r);
    r.d.press(0x04);
    const a = try nextReport(&r, 20_000);
    try testing.expectEqual(@as(u8, 0x04), a[2]);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "the thumb keyboard: 8-byte EP0, its 18-byte reports switched to boot protocol" {
    var r: Rig = .{};
    try r.initWith(&fixtures.thumb_keyboard, true);
    var seen: [8]Status = undefined;
    _ = r.run(600_000, &seen);
    try testing.expectEqual(Status{ .keyboard = thumb_id }, r.h.status);
    try testing.expectEqual(@as(u11, 8), r.h.mps0);
    try testing.expectEqual(@as(u1, 0), r.d.protocol[0]);
    try testing.expectEqual(@as(u16, 18), r.h.keyboard.?.max_packet);
    _ = nextReport(&r, 50_000) catch {};
    r.d.press(0x2C);
    const a = try nextReport(&r, 20_000);
    try testing.expectEqualSlices(u8, &.{ 0, 0, 0x2C, 0, 0, 0, 0, 0 }, a);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "a device that stalls SET_IDLE is still a keyboard" {
    var r: Rig = .{};
    try r.init(true);
    r.d.stall_set_idle = true;
    var seen: [8]Status = undefined;
    _ = r.run(600_000, &seen);
    try testing.expectEqual(Status{ .keyboard = mini_id }, r.h.status);
    try testing.expectEqual(@as(u8, 125), r.d.idle[0]); // unchanged: the default
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "a device without a boot keyboard is left alone, unconfigured" {
    var r: Rig = .{};
    try r.init(true);
    var cfg: [59]u8 = undefined;
    @memcpy(&cfg, fixtures.mini_keyboard.config);
    cfg[9 + 7] = 2; // interface 0: a boot mouse
    r.d.config_override = &cfg;
    var seen: [8]Status = undefined;
    _ = r.run(600_000, &seen);
    try testing.expectEqual(Status{ .not_keyboard = mini_id }, r.h.status);
    try testing.expectEqual(@as(u8, 0), r.d.configuration);
    var buf: [40]u8 = undefined;
    try testing.expectEqualStrings("USB: 1997:2433 is not a keyboard", r.h.status.text(&buf));
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "a device that never answers: three tries with port resets, then given up; replug works" {
    var r: Rig = .{};
    try r.init(true);
    r.d.mute = true;
    var seen: [16]Status = undefined;
    _ = r.run(3_000_000, &seen);
    try testing.expectEqual(Status.enum_failed, r.h.status);
    try testing.expectEqualStrings("timeout", r.h.fail_why);
    try testing.expectEqualStrings("get_dev8", r.h.failState());
    try testing.expectEqual(@as(u32, 3), r.h.connects);
    // the user replugs it, and this time it answers
    r.f.unplug();
    _ = r.run(10_000, &seen);
    try testing.expectEqual(Status.no_device, r.h.status);
    r.d.mute = false;
    r.f.plug(&r.d);
    _ = r.run(600_000, &seen);
    try testing.expectEqual(Status{ .keyboard = mini_id }, r.h.status);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "a malformed configuration fails enumeration, without hanging" {
    var r: Rig = .{};
    try r.init(true);
    var cfg: [59]u8 = undefined;
    @memcpy(&cfg, fixtures.mini_keyboard.config);
    cfg[9 + 4] = 2; // interface 0 claims two endpoints, has one
    r.d.config_override = &cfg;
    var seen: [16]Status = undefined;
    _ = r.run(3_000_000, &seen);
    try testing.expectEqual(Status.enum_failed, r.h.status);
    try testing.expectEqualStrings("bad configuration", r.h.fail_why);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "unplugged in the middle of enumeration, then replugged" {
    var r: Rig = .{};
    try r.init(true);
    var seen: [16]Status = undefined;
    // until the first transfers are under way
    const t = r.f.time;
    while (r.h.state != .get_cfg_head) {
        _ = r.h.poll();
        try testing.expect(r.f.time - t < 1_000_000);
    }
    r.f.unplug();
    _ = r.run(10_000, &seen);
    try testing.expectEqual(Status.no_device, r.h.status);
    try testing.expect(r.f.read32(hw.reg_usbcmd) & hw.cmd_ase == 0);
    r.f.plug(&r.d);
    _ = r.run(600_000, &seen);
    try testing.expectEqual(Status{ .keyboard = mini_id }, r.h.status);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "nothing plugged in: no device, and it stays that way quietly" {
    var r: Rig = .{};
    try r.init(false);
    var seen: [8]Status = undefined;
    try testing.expectEqual(@as(usize, 1), r.run(2_000_000, &seen));
    try testing.expectEqual(Status.no_device, seen[0]);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "unplug and replug: back to no device, then a keyboard again" {
    var r: Rig = .{};
    try r.init(true);
    var seen: [8]Status = undefined;
    _ = r.run(600_000, &seen);
    r.f.unplug();
    try testing.expectEqual(@as(usize, 1), r.run(50_000, &seen));
    try testing.expectEqual(Status.no_device, seen[0]);
    r.f.plug(&r.d);
    try testing.expectEqual(@as(usize, 3), r.run(600_000, &seen));
    try testing.expectEqual(Status{ .keyboard = mini_id }, seen[2]);
    try testing.expectEqual(@as(u32, 2), r.h.connects);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "a connect that bounces inside the debounce is debounced again" {
    var r: Rig = .{};
    try r.init(false);
    var seen: [8]Status = undefined;
    _ = r.run(10_000, &seen);
    r.f.plug(&r.d);
    _ = r.run(50_000, &seen);
    r.f.unplug();
    _ = r.run(1_000, &seen);
    r.f.plug(&r.d);
    // a reset inside 100 ms of the second connect would be a violation
    _ = r.run(600_000, &seen);
    try testing.expectEqual(Status{ .keyboard = mini_id }, r.h.status);
    try testing.expectEqual(@as(u32, 1), r.h.connects);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "a controller that ends the port reset by itself (UG585 15.11.4) works too" {
    var r: Rig = .{};
    try r.init(true);
    r.f.pr_auto = true;
    var seen: [8]Status = undefined;
    _ = r.run(600_000, &seen);
    try testing.expectEqual(Status{ .keyboard = mini_id }, r.h.status);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "a low-speed keyboard is enumerated at low speed" {
    var r: Rig = .{};
    try r.init(false);
    r.d.speed = .low;
    r.f.plug(&r.d);
    var seen: [8]Status = undefined;
    _ = r.run(600_000, &seen);
    try testing.expectEqual(Status{ .connected = .low }, seen[2]);
    try testing.expectEqual(Status{ .keyboard = mini_id }, r.h.status);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "no ULPI clock (PHY in reset): no_phy, retried, no hang" {
    var r: Rig = .{};
    try r.init(true);
    r.f.phy_clock = false;
    var seen: [8]Status = undefined;
    _ = r.run(100_000, &seen);
    try testing.expectEqual(Status.no_phy, r.h.status);
    var buf: [40]u8 = undefined;
    try testing.expectEqualStrings("USB: no PHY (no ULPI clock?)", r.h.status.text(&buf));
    // without the clock the controller reset never ends
    try testing.expectEqualStrings("wait_reset", r.h.failState());
    try testing.expect(r.h.fail_regs.cmd & hw.cmd_rst != 0);
    // the clock comes (the PHY leaves reset): the retry finds it
    r.f.phy_clock = true;
    _ = r.run(1_700_000, &seen);
    try testing.expectEqual(Status{ .keyboard = mini_id }, r.h.status);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "a PHY in low-power mode is woken before its registers are read" {
    var r: Rig = .{};
    try r.init(true);
    r.f.phy_suspended = true;
    var seen: [8]Status = undefined;
    _ = r.run(600_000, &seen);
    try testing.expectEqual(Status{ .keyboard = mini_id }, r.h.status);
    try testing.expectEqual(fake.tusb1210_vendor, r.h.phyVendor());
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "a receiver left powered and attached by the last run is found without a replug" {
    var r: Rig = .{};
    try r.init(true);
    var seen: [8]Status = undefined;
    _ = r.run(600_000, &seen);
    // `make prog`: the app restarts, the controller is reset, but the PHY
    // (and so Vbus, and the attached receiver) stays as it was
    r.f.write32(hw.reg_usbcmd, 0);
    while (r.f.read32(hw.reg_usbsts) & hw.sts_hch == 0) {}
    r.f.write32(hw.reg_usbcmd, hw.cmd_rst);
    while (r.f.read32(hw.reg_usbcmd) & hw.cmd_rst != 0) {}
    try testing.expect(r.f.line);
    r.h = TestHost.init(&r.f, &r.dma);
    _ = r.run(900_000, &seen);
    try testing.expectEqual(Status{ .keyboard = mini_id }, r.h.status);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}

test "the controller left running by an earlier app: halted before the reset" {
    var r: Rig = .{};
    try r.init(true);
    var seen: [8]Status = undefined;
    _ = r.run(600_000, &seen);
    try testing.expect(r.f.read32(hw.reg_usbsts) & hw.sts_hch == 0);
    r.h = TestHost.init(&r.f, &r.dma); // the app starts again; the fake keeps running
    _ = r.run(600_000, &seen);
    try testing.expectEqual(Status{ .keyboard = mini_id }, r.h.status);
    try testing.expectEqual(@as(u32, 0), r.f.allViolations());
}
