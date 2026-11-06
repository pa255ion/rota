//! A desktop model of the Zynq USB host controller with one emulated USB
//! device on its port, for testing the bare-metal host (side track E) the
//! way sd.zig's tests fake the SD controller.
//!
//! The controller (`Fake`) has the register interface of usb_hw.zig: it
//! resets, switches to host mode, talks to a ULPI PHY register file
//! through the viewport, powers the port when the PHY drives Vbus, reports
//! connects, times port resets, and, while running, walks the driver's
//! asynchronous ring and periodic frame list in memory, executing the qTDs
//! it finds against the device. Time advances by 1 us per register access
//! and per `nowUs` call; a micro-frame is 125 us, a frame 1 ms. Every
//! async QH gets one transaction per micro-frame, every periodic QH one
//! per frame in which the frame list names it.
//!
//! Memory: the driver's DMA structures live in one arena (a byte slice the
//! test hands to `Fake.init`), seen by the fake at bus addresses from
//! `bus_base` up. `busAddr` turns a pointer into the arena into its bus
//! address, as `MmioBus.busAddr` will on the board (where it is the
//! identity: OCM and DDR are at the same addresses for the CPU and the USB
//! DMA, TRM table 4-1).
//!
//! The device (`Device`) is built from a fixture (usb_fixtures.zig) and
//! refuses malformed ones. It follows USB 2.0 chapter 9 for the default,
//! address and configured states and the control pipe, and HID 1.11 for
//! the class requests, boot and report protocol, and idle rates.
//!
//! Whatever the real hardware or a real device would reject, or a rule the
//! driver must follow (TRM chapter 15, EHCI 1.0, USB 2.0), counts in
//! `violations` with a message. Behaviour where real hardware is
//! undefined is never relied on: it is a violation instead.

const std = @import("std");
const hw = @import("usb_hw.zig");
const fixtures = @import("usb_fixtures.zig");
pub const Fixture = fixtures.Fixture;

// --- The emulated device -----------------------------------------------------

pub const InResult = union(enum) {
    data: struct { bytes: []const u8, toggle: u1 },
    nak,
    stall,
};

pub const OutResult = enum { ack, nak, stall };

pub const DeviceError = hw.DescError || error{
    /// fewer report descriptors than interfaces, or one whose length
    /// differs from its HID descriptor's wDescriptorLength
    ReportLength,
    /// no boot keyboard interface
    NoBootKeyboard,
    /// report_keys out of range (6 to 32)
    BadReportKeys,
};

pub const Device = struct {
    const Stage = enum { idle, data_in, data_out, status_in, status_out, stalled };
    const max_ctrl = 512;

    fx: *const Fixture,
    desc: hw.DeviceDesc,
    config: hw.ConfigHead,
    keyboard: hw.BootKeyboard,
    speed: hw.Speed = .full,

    // USB 9.1: default (address 0), address, configured
    address: u7 = 0,
    configuration: u8 = 0,
    /// HID 7.2.6: devices start in report protocol (1); 0 = boot
    protocol: [8]u1 = [_]u1{1} ** 8,
    /// HID 7.2.4, in 4 ms units; 0 = report only on change. Keyboards
    /// should default to 500 ms (HID appendix B note).
    idle: [8]u8 = [_]u8{125} ** 8,
    leds: u8 = 0,

    // the control pipe
    stage: Stage = .idle,
    setup: hw.Setup = undefined,
    buf: [max_ctrl]u8 = undefined,
    len: usize = 0,
    pos: usize = 0,
    /// the last data packet was full-sized: the host may ask for a
    /// zero-length one (USB 5.5.3)
    zlp_due: bool = false,
    ep0_toggle: u1 = 0,
    /// SET_ADDRESS takes effect after its status stage (USB 9.4.6)
    new_address: ?u7 = null,

    // interrupt IN endpoints, by number
    toggle_in: [16]u1 = [_]u1{0} ** 16,
    report: [2 + 32]u8 = undefined,
    report_len: usize = 0,
    /// what the keyboard reports now, and whether that changed since the
    /// last report the host acknowledged
    mods: u8 = 0,
    keys: [32]u8 = [_]u8{0} ** 32,
    changed: bool = true,
    last_report_us: u64 = 0,
    /// counts of what happened, for tests
    reports_sent: u32 = 0,
    requests: u32 = 0,

    violations: u32 = 0,
    quiet: bool = false,

    // misbehaviour, for testing a host's error paths
    /// NAK everything on endpoint 0 after the SETUP (a hung device)
    mute: bool = false,
    /// STALL SET_IDLE (it is optional for keyboards, HID 7.2.4)
    stall_set_idle: bool = false,
    /// the configuration this device sends instead of the fixture's
    /// (which must still be well-formed for `init`)
    config_override: ?[]const u8 = null,

    pub fn init(fx: *const Fixture) DeviceError!Device {
        const desc = try hw.parseDevice(fx.device);
        const config = try hw.parseConfig(fx.config);
        if (fx.report_keys < 6 or fx.report_keys > 32) return error.BadReportKeys;
        var w = hw.Walker.init(fx.config);
        var iface: ?u8 = null;
        while (w.next()) |item| switch (item) {
            .interface => |i| iface = i.number,
            .hid => |len| {
                const n = iface orelse return error.BadHid;
                if (n >= fx.reports.len or fx.reports[n].len != len) return error.ReportLength;
            },
            else => {},
        };
        const kb = hw.findBootKeyboard(fx.config) orelse return error.NoBootKeyboard;
        return .{ .fx = fx, .desc = desc, .config = config, .keyboard = kb };
    }

    fn bad(d: *Device, comptime msg: []const u8, args: anytype) void {
        if (!d.quiet) std.debug.print("fake usb device: " ++ msg ++ "\n", args);
        d.violations += 1;
    }

    /// USB bus reset (port reset): back to the default state.
    pub fn busReset(d: *Device) void {
        d.address = 0;
        d.new_address = null;
        d.configuration = 0;
        d.stage = .idle;
        d.protocol = [_]u1{1} ** 8;
        d.idle = [_]u8{125} ** 8;
        d.toggle_in = [_]u1{0} ** 16;
        d.report_len = 0;
        d.changed = true;
    }

    // --- the keyboard, from the test's side
    pub fn press(d: *Device, usage: u8) void {
        if (usage >= 0xE0 and usage <= 0xE7) {
            d.mods |= @as(u8, 1) << @intCast(usage - 0xE0);
        } else {
            for (&d.keys) |*k| if (k.* == usage) return;
            for (&d.keys) |*k| if (k.* == 0) {
                k.* = usage;
                break;
            };
        }
        d.changed = true;
    }

    pub fn release(d: *Device, usage: u8) void {
        if (usage >= 0xE0 and usage <= 0xE7) {
            d.mods &= ~(@as(u8, 1) << @intCast(usage - 0xE0));
        } else {
            for (&d.keys) |*k| if (k.* == usage) {
                k.* = 0;
            };
        }
        d.changed = true;
    }

    /// The input report as the current protocol formats it: boot (HID
    /// appendix B.1) = modifiers, reserved, 6 keys, with all six slots
    /// ErrorRollOver (1) when more keys are down; report protocol = the
    /// same with the fixture's number of key slots.
    fn buildReport(d: *Device) []const u8 {
        const slots: usize = if (d.protocol[d.keyboard.interface] == 0) 6 else d.fx.report_keys;
        d.report[0] = d.mods;
        d.report[1] = 0;
        var n: usize = 0;
        for (d.keys) |k| if (k != 0) {
            n += 1;
        };
        var i: usize = 0;
        for (d.keys) |k| {
            if (k == 0) continue;
            if (i < slots) d.report[2 + i] = k;
            i += 1;
        }
        while (i < slots) : (i += 1) d.report[2 + i] = 0;
        if (n > slots) @memset(d.report[2 .. 2 + slots], 1);
        d.report_len = 2 + slots;
        return d.report[0..d.report_len];
    }

    // --- packets, from the controller's side

    /// A SETUP packet (always acknowledged, USB 8.5.3).
    pub fn setupPacket(d: *Device, bytes: *const [8]u8) void {
        d.requests += 1;
        d.setup = hw.Setup.parse(bytes);
        d.ep0_toggle = 1;
        d.pos = 0;
        d.len = 0;
        d.zlp_due = false;
        const s = d.setup;
        if (s.deviceToHost()) {
            if (d.answer(s)) |n| {
                d.len = @min(n, s.length);
                d.stage = if (s.length == 0) .status_out else .data_in;
            } else d.stage = .stalled;
        } else if (s.length > 0) {
            if (s.length > max_ctrl or !d.acceptsOut(s)) {
                d.stage = .stalled;
            } else d.stage = .data_out;
        } else {
            d.stage = if (d.acceptsNoData(s)) .status_in else .stalled;
        }
    }

    /// Fills `buf` with the answer to a device-to-host request; null =
    /// stall (request error, USB 9.2.7).
    fn answer(d: *Device, s: hw.Setup) ?usize {
        const dtype: u8 = @truncate(s.value >> 8);
        const index: u8 = @truncate(s.value);
        switch (s.request_type) {
            0x80 => switch (s.request) {
                hw.req_get_descriptor => switch (dtype) {
                    hw.dt_device => return d.copy(d.fx.device),
                    hw.dt_config => return if (index == 0) d.copy(d.config_override orelse d.fx.config) else null,
                    hw.dt_string => {
                        if (index == 0) return d.copy(&.{ 4, hw.dt_string, 0x09, 0x04 }); // English (US)
                        if (index >= d.fx.strings.len) return null;
                        const text = d.fx.strings[index] orelse return null;
                        d.buf[0] = @intCast(2 + 2 * text.len);
                        d.buf[1] = hw.dt_string;
                        for (text, 0..) |c, i| {
                            d.buf[2 + 2 * i] = c;
                            d.buf[3 + 2 * i] = 0;
                        }
                        return 2 + 2 * text.len;
                    },
                    else => return null,
                },
                hw.req_get_configuration => {
                    if (d.address == 0) return null;
                    d.buf[0] = d.configuration;
                    return 1;
                },
                hw.req_get_status => {
                    d.buf[0] = 0;
                    d.buf[1] = 0;
                    return 2;
                },
                else => return null,
            },
            0x81 => {
                // GET_DESCRIPTOR(report) to an interface (HID 7.1.1)
                if (s.request != hw.req_get_descriptor or dtype != hw.dt_report) return null;
                if (s.index >= d.fx.reports.len) return null;
                return d.copy(d.fx.reports[s.index]);
            },
            0xA1 => {
                if (d.configuration == 0 or s.index >= d.config.interfaces) return null;
                switch (s.request) {
                    hw.hid_get_protocol => {
                        d.buf[0] = d.protocol[s.index];
                        return 1;
                    },
                    hw.hid_get_idle => {
                        d.buf[0] = d.idle[s.index];
                        return 1;
                    },
                    hw.hid_get_report => {
                        if (s.index != d.keyboard.interface) return null;
                        const r = d.buildReport();
                        return d.copy(r);
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    fn copy(d: *Device, src: []const u8) usize {
        @memcpy(d.buf[0..src.len], src);
        return src.len;
    }

    fn acceptsOut(d: *Device, s: hw.Setup) bool {
        // SET_REPORT (output: the LEDs) to the keyboard, one byte
        return s.request_type == 0x21 and s.request == hw.hid_set_report and
            d.configuration != 0 and s.index == d.keyboard.interface and s.length == 1;
    }

    fn acceptsNoData(d: *Device, s: hw.Setup) bool {
        switch (s.request_type) {
            0x00 => switch (s.request) {
                hw.req_set_address => return s.value <= 127 and d.configuration == 0,
                hw.req_set_configuration => return d.address != 0 and (s.value == 0 or s.value == d.config.value),
                else => return false,
            },
            0x02 => return s.request == hw.req_clear_feature and s.value == 0 and d.configuration != 0,
            0x21 => {
                if (d.configuration == 0 or s.index >= d.config.interfaces) return false;
                return switch (s.request) {
                    hw.hid_set_idle => !d.stall_set_idle,
                    // HID 7.2.6: only boot devices support SET_PROTOCOL
                    hw.hid_set_protocol => s.value <= 1 and s.index == d.keyboard.interface,
                    else => false,
                };
            },
            else => return false,
        }
    }

    /// The status stage completed: the request takes effect.
    fn finish(d: *Device, now_us: u64) void {
        const s = d.setup;
        switch (s.request_type) {
            0x00 => switch (s.request) {
                hw.req_set_address => d.address = @intCast(s.value),
                hw.req_set_configuration => {
                    d.configuration = @truncate(s.value);
                    // USB 9.1.1.5, 9.4.5: endpoints restart at DATA0
                    d.toggle_in = [_]u1{0} ** 16;
                    d.changed = true;
                },
                else => {},
            },
            0x02 => d.toggle_in[s.index & 0xF] = 0, // CLEAR_FEATURE(ENDPOINT_HALT)
            0x21 => switch (s.request) {
                hw.hid_set_idle => {
                    d.idle[s.index] = @truncate(s.value >> 8);
                    d.last_report_us = now_us;
                },
                hw.hid_set_protocol => {
                    d.protocol[s.index] = @truncate(s.value);
                    d.changed = true;
                },
                hw.hid_set_report => d.leds = d.buf[0],
                else => {},
            },
            else => {},
        }
        d.stage = .idle;
    }

    /// An IN token. The caller sends `acked` back if it accepted the data.
    pub fn inPacket(d: *Device, ep: u4, max: usize, now_us: u64) InResult {
        if (ep == 0 and d.mute) return .nak;
        if (ep == 0) {
            switch (d.stage) {
                .data_in => {
                    const n = @min(d.len - d.pos, @as(usize, d.fx.device[7]));
                    if (n == 0 and !d.zlp_due) {
                        // the host asked past the end of a transfer that
                        // ended short: a real device would NAK forever
                        d.bad("IN after the end of the control data", .{});
                        return .nak;
                    }
                    return .{ .data = .{ .bytes = d.buf[d.pos .. d.pos + n], .toggle = d.ep0_toggle } };
                },
                .status_in => return .{ .data = .{ .bytes = &.{}, .toggle = 1 } },
                .stalled => return .stall,
                else => {
                    d.bad("IN on endpoint 0 in stage {s}", .{@tagName(d.stage)});
                    return .stall;
                },
            }
        }
        if (d.configuration == 0) {
            d.bad("IN to endpoint {d} before SET_CONFIGURATION", .{ep});
            return .stall;
        }
        if (ep == d.keyboard.endpoint & 0xF) {
            const idle_us = @as(u64, d.idle[d.keyboard.interface]) * 4000;
            if (d.report_len == 0) {
                if (!d.changed and (idle_us == 0 or now_us - d.last_report_us < idle_us)) return .nak;
                _ = d.buildReport();
            }
            if (d.report_len > max) {
                // a real device sends its packet anyway: babble for the host
            }
            return .{ .data = .{ .bytes = d.report[0..d.report_len], .toggle = d.toggle_in[ep] } };
        }
        // other interfaces (the mouse) have nothing to say
        return .nak;
    }

    pub fn inAcked(d: *Device, ep: u4, now_us: u64) void {
        if (ep == 0) {
            switch (d.stage) {
                .data_in => {
                    const n = @min(d.len - d.pos, @as(usize, d.fx.device[7]));
                    d.pos += n;
                    d.ep0_toggle ^= 1;
                    // a full packet that ends the data short of wLength
                    // is followed by a zero-length one (USB 5.5.3)
                    d.zlp_due = n == d.fx.device[7] and d.pos == d.len and d.len < d.setup.length;
                    if (n == 0) d.zlp_due = false;
                },
                .status_in => d.finish(now_us),
                else => {},
            }
            return;
        }
        d.toggle_in[ep] ^= 1;
        if (d.report_len != 0) {
            d.report_len = 0;
            d.changed = false;
            d.last_report_us = now_us;
            d.reports_sent += 1;
        }
    }

    /// An OUT (or the SETUP's data/status) packet with its toggle.
    pub fn outPacket(d: *Device, ep: u4, toggle: u1, data: []const u8, now_us: u64) OutResult {
        if (ep != 0) {
            d.bad("OUT to endpoint {d}: the keyboard has no OUT endpoint", .{ep});
            return .stall;
        }
        if (d.mute) return .nak;
        switch (d.stage) {
            .data_out => {
                if (toggle != d.ep0_toggle) {
                    // a retried packet: acknowledged and dropped (USB 8.6.4)
                    d.bad("OUT data with toggle {d}, expected {d}", .{ toggle, d.ep0_toggle });
                    return .ack;
                }
                if (d.pos + data.len > d.setup.length) {
                    d.bad("more OUT data than wLength", .{});
                    return .stall;
                }
                @memcpy(d.buf[d.pos .. d.pos + data.len], data);
                d.pos += data.len;
                d.ep0_toggle ^= 1;
                if (d.pos == d.setup.length) d.stage = .status_in;
                return .ack;
            },
            .data_in, .status_out => {
                // the status stage of an IN transfer, perhaps early (the
                // host may end the data stage at any time, USB 8.5.3.2)
                if (data.len != 0) d.bad("status OUT with {d} bytes", .{data.len});
                if (toggle != 1) d.bad("status OUT with toggle 0", .{});
                d.stage = .idle;
                _ = now_us;
                return .ack;
            },
            .stalled => return .stall,
            else => {
                d.bad("OUT on endpoint 0 in stage {s}", .{@tagName(d.stage)});
                return .stall;
            },
        }
    }
};

// --- The controller --------------------------------------------------------

/// TUSB1210 (ZedBoard J13): TI's ULPI vendor and product IDs.
pub const tusb1210_vendor: u16 = 0x0451;
pub const tusb1210_product: u16 = 0x1507;

pub const Fake = struct {
    arena: []u8,
    bus_base: u32,

    time: u64 = 0,
    /// time of the next micro-frame
    next_uframe: u64 = 125,

    usbcmd: u32 = 0x0008_0000, // reset value: ITC = 8
    sts: u32 = 0, // the write-1-to-clear bits
    usbintr: u32 = 0,
    frindex: u32 = 0,
    periodic_base: u32 = 0,
    async_addr: u32 = 0,
    ttctrl: u32 = 0,
    usbmode: u32 = 0,
    otgsc: u32 = 0,
    halted: bool = true,
    as_on: bool = false,
    ps_on: bool = false,
    reset_until: ?u64 = null,

    // the ULPI viewport and the PHY behind it
    phy: [0x40]u8 = [_]u8{0} ** 0x40,
    ulpi_until: ?u64 = null,
    ulpi_cmd: u32 = 0,
    ulpi_data: u8 = 0,

    // the port
    pp: bool = false,
    pfsc: bool = false,
    ccs: bool = false,
    csc: bool = false,
    pe: bool = false,
    pec: bool = false,
    /// a port reset is under way (PORTSC1.PR reads 1): since `pr_start`,
    /// ending at `pr_until` once that is known
    resetting: bool = false,
    pr_start: u64 = 0,
    pr_until: ?u64 = null,
    /// UG585 15.11.4 says the controller ends a port reset by itself after
    /// 10 ms and ignores an earlier PR = 0. The board's controller didn't
    /// (2026-10-04: PR still 1 after 50 ms), so by default the reset lasts
    /// until software writes PR = 0, as in EHCI 2.3.9; true = the TRM's
    /// timer, so a driver is tested against both
    pr_auto: bool = false,
    connected_at: u64 = 0,
    enabled_at: u64 = 0,
    /// the last SET_ADDRESS's status stage (USB 9.2.6.3: 2 ms recovery)
    address_set_at: ?u64 = null,
    /// a device on the other end of the cable; whether it gets power
    /// depends on the PHY's Vbus bits
    device: ?*Device = null,
    /// OTG control bits that must be set for Vbus on the port: on the
    /// ZedBoard the TUSB1210's CPEN enables the TPS2051 switch, which Linux
    /// drives with DrvVbus and DrvVbusExternal (phy-ulpi.c, "drv-vbus")
    vbus_bits: u8 = hw.otg_drv_vbus | hw.otg_drv_vbus_ext,
    /// the PHY's 60 MHz ULPI clock, which most of the controller runs on
    /// (TRM 15.15.1); without it (PHY held in reset or unpowered) the
    /// registers still answer, but a controller reset, a viewport access,
    /// the port and the schedules all stand still
    phy_clock: bool = true,
    /// the PHY in low-power mode (ULPISS = 0): register access needs a
    /// wake-up first (TRM ULPI_VIEWPORT)
    phy_suspended: bool = false,
    /// the cable: a device pulls D+ up `attach_us` after it gets Vbus
    /// (USB 7.1.7.3 allows it up to 100 ms); `line` is whether it does
    /// now. The controller only learns the line state from the PHY's
    /// reports of *changes* (ULPI RX CMD), and only while it is in host
    /// mode with port power on: a device that was already attached when
    /// the controller was reset stays unseen until the line changes again
    /// (seen on the board 2026-10-04: the PHY kept Vbus on across `make
    /// prog`, the receiver only appeared after a replug).
    attach_us: u64 = 2_000,
    line: bool = false,
    powered_at: u64 = 0,
    vbus_was: bool = false,

    violations: u32 = 0,
    quiet: bool = false,
    /// transactions answered, by kind, for tests
    setups: u32 = 0,
    naks: u32 = 0,

    pub fn init(arena: []u8, bus_base: u32) Fake {
        var f: Fake = .{ .arena = arena, .bus_base = bus_base };
        f.phyReset();
        return f;
    }

    fn phyReset(f: *Fake) void {
        f.phy = [_]u8{0} ** 0x40;
        f.phy[hw.ulpi_vendor_lo] = @truncate(tusb1210_vendor);
        f.phy[hw.ulpi_vendor_hi] = @truncate(tusb1210_vendor >> 8);
        f.phy[hw.ulpi_product_lo] = @truncate(tusb1210_product);
        f.phy[hw.ulpi_product_hi] = @truncate(tusb1210_product >> 8);
        f.phy[hw.ulpi_func_ctrl] = 0x41; // ULPI 1.1 reset values
        f.phy[hw.ulpi_otg_ctrl] = 0x06; // pull-downs on
    }

    fn bad(f: *Fake, comptime msg: []const u8, args: anytype) void {
        if (!f.quiet) std.debug.print("fake usb: " ++ msg ++ "\n", args);
        f.violations += 1;
    }

    /// Total of the controller's and the device's violations.
    pub fn allViolations(f: *const Fake) u32 {
        return f.violations + if (f.device) |d| d.violations else 0;
    }

    pub fn plug(f: *Fake, d: *Device) void {
        d.busReset();
        f.device = d;
        f.powered_at = f.time;
    }

    pub fn unplug(f: *Fake) void {
        f.device = null;
    }

    pub fn busAddr(f: *Fake, addr: usize) u32 {
        const start = @intFromPtr(f.arena.ptr);
        if (addr < start or addr > start + f.arena.len) {
            f.bad("bus address of a pointer outside the DMA arena", .{});
            return 0;
        }
        return f.bus_base + @as(u32, @intCast(addr - start));
    }

    // --- DMA
    fn mem(f: *Fake, bus: u32, len: usize) ?[]u8 {
        if (bus < f.bus_base or bus - f.bus_base + len > f.arena.len) {
            f.bad("DMA at 0x{X:0>8} (+{d}) outside the arena", .{ bus, len });
            return null;
        }
        const off = bus - f.bus_base;
        return f.arena[off .. off + len];
    }

    fn rd(f: *Fake, bus: u32) ?u32 {
        const m = f.mem(bus, 4) orelse return null;
        return std.mem.readInt(u32, m[0..4], .little);
    }

    fn wr(f: *Fake, bus: u32, v: u32) void {
        const m = f.mem(bus, 4) orelse return;
        std.mem.writeInt(u32, m[0..4], v, .little);
    }

    // --- Registers
    pub fn nowUs(f: *Fake) u64 {
        f.tick();
        return f.time;
    }

    pub fn read32(f: *Fake, off: u32) u32 {
        f.tick();
        return switch (off) {
            hw.reg_hcsparams => 0x0000_0011, // one port, port power control
            hw.reg_usbcmd => f.usbcmd | if (f.reset_until != null) hw.cmd_rst else 0,
            // HCH is a host-mode bit: an idle controller (after power-on or
            // RST) reads 0 (TRM "Register (usb) USBSTS"; on the board too)
            hw.reg_usbsts => f.sts | (if (f.halted and f.host()) hw.sts_hch else 0) |
                (if (f.as_on) hw.sts_as else 0) | (if (f.ps_on) hw.sts_ps else 0),
            hw.reg_usbintr => f.usbintr,
            hw.reg_frindex => f.frindex,
            hw.reg_periodiclistbase => f.periodic_base,
            hw.reg_asynclistaddr => f.async_addr,
            hw.reg_ttctrl => f.ttctrl,
            hw.reg_ulpi_viewport => (if (f.ulpi_until == null) 0 else if (f.ulpi_cmd & hw.ulpi_wu != 0) hw.ulpi_wu else hw.ulpi_run) |
                (if (f.phy_suspended) 0 else hw.ulpi_ss) |
                @as(u32, f.ulpi_data) << 8 | (f.ulpi_cmd & 0x00FF_00FF),
            hw.reg_portsc1 => f.portsc(),
            hw.reg_otgsc => f.otgsc,
            hw.reg_usbmode => f.usbmode,
            else => blk: {
                f.bad("read of unmodelled register 0x{X:0>3}", .{off});
                break :blk 0;
            },
        };
    }

    fn host(f: *const Fake) bool {
        return f.usbmode & hw.mode_cm_mask == hw.mode_cm_host;
    }

    fn portsc(f: *Fake) u32 {
        // reset value 0x8C000004 (TRM): PTS = ULPI, PSPD = not connected
        var v: u32 = 0x2 << 30;
        if (!f.host()) return v | 0x0C00_0004;
        const speed: u32 = if (f.pe) @intFromEnum(f.device.?.speed) else 3;
        v |= speed << hw.port_pspd_shift;
        if (f.pfsc) v |= hw.port_pfsc;
        if (f.pp) v |= hw.port_pp;
        if (f.ccs) v |= hw.port_ccs;
        if (f.csc) v |= hw.port_csc;
        if (f.pe) v |= hw.port_pe;
        if (f.pec) v |= hw.port_pec;
        if (f.resetting) v |= hw.port_pr;
        if (f.ccs and !f.resetting) v |= 0x1 << 10; // J state, idle FS
        return v;
    }

    pub fn write32(f: *Fake, off: u32, v: u32) void {
        f.tick();
        switch (off) {
            hw.reg_usbcmd => f.writeCmd(v),
            hw.reg_usbsts => f.sts &= ~(v & hw.sts_w1c),
            hw.reg_usbintr => f.usbintr = v,
            hw.reg_frindex => {
                if (!f.halted) f.bad("FRINDEX written while running", .{});
                f.frindex = v & 0x3FFF;
            },
            hw.reg_periodiclistbase => {
                if (v & 0xFFF != 0) f.bad("PERIODICLISTBASE 0x{X:0>8} not 4 KiB aligned", .{v});
                f.periodic_base = v & ~@as(u32, 0xFFF);
            },
            hw.reg_asynclistaddr => {
                if (v & 0x1F != 0) f.bad("ASYNCLISTADDR 0x{X:0>8} not 32-byte aligned", .{v});
                f.async_addr = v & ~@as(u32, 0x1F);
            },
            hw.reg_ttctrl => f.ttctrl = v & (0x7F << 24),
            hw.reg_ulpi_viewport => f.writeUlpi(v),
            hw.reg_portsc1 => f.writePort(v),
            hw.reg_otgsc => f.otgsc = v,
            hw.reg_usbmode => {
                // writable once after a reset (TRM "Register (usb) USBMODE")
                if (f.usbmode & hw.mode_cm_mask != 0 and (v ^ f.usbmode) & hw.mode_cm_mask != 0)
                    f.bad("USBMODE.CM changed without a controller reset", .{});
                if (v & hw.mode_cm_mask == 1) f.bad("USBMODE.CM = 01 (reserved)", .{});
                f.usbmode = v;
            },
            else => f.bad("write of unmodelled register 0x{X:0>3}", .{off}),
        }
    }

    fn writeCmd(f: *Fake, v: u32) void {
        if (v & hw.cmd_rst != 0) {
            // TRM 15.15.2: undefined while the host controller runs
            if (!f.halted) f.bad("USBCMD.RST while not halted", .{});
            f.reset_until = f.time + 5;
            f.usbcmd = 0x0008_0000;
            f.sts = 0;
            f.usbintr = 0;
            f.frindex = 0;
            f.periodic_base = 0;
            f.async_addr = 0;
            f.ttctrl = 0;
            f.usbmode = 0;
            f.halted = true;
            f.as_on = false;
            f.ps_on = false;
            f.pp = false;
            f.pe = false;
            f.ccs = false;
            f.csc = false;
            f.resetting = false;
            f.pr_until = null;
            return;
        }
        if (f.reset_until != null) {
            f.bad("USBCMD written during the controller reset", .{});
            return;
        }
        const was = f.usbcmd;
        if (v & hw.cmd_rs != 0 and was & hw.cmd_rs == 0) {
            if (!f.halted) f.bad("USBCMD.RS set before HCH (TRM 15.13.2)", .{});
            if (!f.host()) f.bad("USBCMD.RS set outside host mode", .{});
            f.halted = false;
        }
        if (v & hw.cmd_ase != 0 and was & hw.cmd_ase == 0 and f.async_addr == 0)
            f.bad("async schedule enabled with ASYNCLISTADDR = 0", .{});
        if (v & hw.cmd_pse != 0 and was & hw.cmd_pse == 0 and f.periodic_base == 0)
            f.bad("periodic schedule enabled with PERIODICLISTBASE = 0", .{});
        // EHCI 4.8: change ASE/PSE only when the status bits agree
        if ((v ^ was) & hw.cmd_ase != 0 and f.as_on != (was & hw.cmd_ase != 0))
            f.bad("USBCMD.ASE changed before USBSTS.AS followed the last change", .{});
        if ((v ^ was) & hw.cmd_pse != 0 and f.ps_on != (was & hw.cmd_pse != 0))
            f.bad("USBCMD.PSE changed before USBSTS.PS followed the last change", .{});
        f.usbcmd = v & ~hw.cmd_iaa;
        if (v & hw.cmd_iaa != 0) f.sts |= hw.sts_aai; // doorbell: answered at once
    }

    fn writeUlpi(f: *Fake, v: u32) void {
        if (f.ulpi_until != null) {
            f.bad("ULPI viewport written while busy", .{});
            return;
        }
        if (v & hw.ulpi_wu != 0 and v & hw.ulpi_run != 0)
            f.bad("ULPI wake-up and run in one write (TRM ULPI_VIEWPORT)", .{});
        if (v & hw.ulpi_wu != 0) {
            f.ulpi_cmd = v;
            f.ulpi_until = f.time + 2;
            return;
        }
        if (v & hw.ulpi_run == 0) return;
        if (f.phy_suspended) f.bad("ULPI access while the PHY is suspended (no wake-up)", .{});
        f.ulpi_cmd = v;
        f.ulpi_until = f.time + 2;
    }

    fn ulpiDone(f: *Fake) void {
        const v = f.ulpi_cmd;
        if (v & hw.ulpi_wu != 0) {
            f.phy_suspended = false;
            return;
        }
        const addr: u8 = @truncate(v >> 16);
        if (addr >= 0x40) {
            f.bad("ULPI access to register 0x{X:0>2}", .{addr});
            return;
        }
        if (v & hw.ulpi_rw == 0) {
            f.ulpi_data = f.phy[addr];
        } else {
            const data: u8 = @truncate(v);
            // registers 0x04 to 0x12 have set (+1) and clear (+2) aliases
            if (addr < hw.ulpi_func_ctrl) {
                f.bad("ULPI write to the read-only ID register 0x{X:0>2}", .{addr});
            } else if (addr < 0x13) {
                const base = addr - (addr - hw.ulpi_func_ctrl) % 3;
                switch ((addr - hw.ulpi_func_ctrl) % 3) {
                    0 => f.phy[base] = data,
                    1 => f.phy[base] |= data,
                    else => f.phy[base] &= ~data,
                }
            } else f.phy[addr] = data;
        }
        f.sts |= hw.sts_ulpii;
    }

    fn writePort(f: *Fake, v: u32) void {
        if (!f.host()) {
            f.bad("PORTSC1 written outside host mode", .{});
            return;
        }
        if (v & hw.port_pts_mask != 0x2 << 30) f.bad("PORTSC1.PTS changed from ULPI", .{});
        if (v & hw.port_csc != 0) f.csc = false;
        if (v & hw.port_pec != 0) f.pec = false;
        f.pfsc = v & hw.port_pfsc != 0;
        f.pp = v & hw.port_pp != 0;
        if (!f.pp) {
            f.pe = false;
            f.ccs = false;
            f.csc = false;
            f.resetting = false;
            f.pr_until = null;
        }
        // software can disable the port, never enable it (TRM PORTSC1.PE)
        if (v & hw.port_pe == 0 and f.pe) f.pe = false;
        if (v & hw.port_pr != 0 and !f.resetting and f.pp) {
            if (!f.ccs) f.bad("port reset with nothing connected", .{});
            // USB 7.1.7.3: 100 ms between connect and reset (debounce)
            if (f.ccs and f.time - f.connected_at < 100_000)
                f.bad("port reset {d} us after the connect, before the 100 ms debounce", .{f.time - f.connected_at});
            f.pe = false;
            f.resetting = true;
            f.pr_start = f.time;
            f.pr_until = if (f.pr_auto) f.time + 10_000 else null;
        } else if (v & hw.port_pr == 0 and f.resetting and !f.pr_auto and f.pr_until == null) {
            // software ends the reset; the port enables shortly after
            if (f.time - f.pr_start < 10_000)
                f.bad("port reset ended after {d} us (USB 7.1.7.5: at least 10 ms)", .{f.time - f.pr_start});
            f.pr_until = f.time + 100;
        }
    }

    // --- Time
    /// Vbus on the connector: the PHY drives it, whatever the controller
    /// does (PORTSC1.PP only drives an EMIO signal)
    fn vbus(f: *const Fake) bool {
        return f.phy[hw.ulpi_otg_ctrl] & f.vbus_bits == f.vbus_bits;
    }

    fn tick(f: *Fake) void {
        f.time += 1;
        if (!f.phy_clock) {
            if (f.reset_until) |*t| t.* += 1;
            if (f.ulpi_until) |*t| t.* += 1;
            if (f.pr_until) |*t| t.* += 1;
            f.pr_start += 1;
            f.next_uframe = f.time + 125;
            return;
        }
        if (f.reset_until) |t| if (f.time >= t) {
            f.reset_until = null;
        };
        if (f.ulpi_until) |t| if (f.time >= t) {
            f.ulpi_until = null;
            f.ulpiDone();
        };
        // the cable: Vbus, then the device's pull-up after `attach_us`
        const v = f.vbus();
        if (v and !f.vbus_was) f.powered_at = f.time;
        f.vbus_was = v;
        const line = f.device != null and v and f.time - f.powered_at >= f.attach_us;
        const changed = line != f.line;
        f.line = line;
        // connect and disconnect, as far as the controller hears of them
        const present = line;
        if (changed and f.host() and f.pp and present != f.ccs) {
            f.ccs = present;
            f.csc = true;
            f.sts |= hw.sts_pci;
            if (present) {
                f.connected_at = f.time;
                f.device.?.busReset();
            } else {
                if (f.pe) f.pec = true;
                f.pe = false;
                f.resetting = false;
                f.pr_until = null;
            }
        }
        if (f.pr_until) |t| if (f.time >= t) {
            f.pr_until = null;
            f.resetting = false;
            if (f.ccs) {
                f.device.?.busReset();
                f.pe = true;
                f.enabled_at = f.time;
                f.address_set_at = null;
                f.sts |= hw.sts_pci;
            }
        };
        while (f.time >= f.next_uframe) {
            f.next_uframe += 125;
            f.microframe();
        }
    }

    fn microframe(f: *Fake) void {
        if (f.halted) return;
        if (f.usbcmd & hw.cmd_rs == 0) {
            f.halted = true;
            f.as_on = false;
            f.ps_on = false;
            return;
        }
        f.frindex = (f.frindex + 1) & 0x3FFF;
        f.sts |= hw.sts_sri;
        const len = hw.frameListLen(f.usbcmd);
        if (f.frindex & 7 == 0 and (f.frindex >> 3) % len == 0) f.sts |= hw.sts_fri;
        f.as_on = f.usbcmd & hw.cmd_ase != 0;
        f.ps_on = f.usbcmd & hw.cmd_pse != 0;
        if (f.ps_on and f.frindex & 7 == 0) f.periodicFrame();
        if (f.as_on) f.asyncPass();
    }

    /// One frame of the periodic schedule: the frame list entry, then the
    /// QHs linked from it.
    fn periodicFrame(f: *Fake) void {
        const len = hw.frameListLen(f.usbcmd);
        const entry = f.periodic_base + 4 * ((f.frindex >> 3) % len);
        var link = f.rd(entry) orelse return;
        var steps: u32 = 0;
        while (link & hw.link_t == 0) : (steps += 1) {
            if (steps == 64) {
                f.bad("periodic list: more than 64 QHs (a loop?)", .{});
                return;
            }
            if (link & 0x6 != hw.typ_qh) {
                f.bad("periodic list: link type {d}, only QHs are modelled", .{(link >> 1) & 3});
                return;
            }
            const qh = hw.linkAddr(link);
            f.runQh(qh, true);
            link = f.rd(qh) orelse return;
        }
    }

    /// One pass around the async ring from ASYNCLISTADDR.
    fn asyncPass(f: *Fake) void {
        const start = f.async_addr;
        var qh = start;
        var heads: u32 = 0;
        var steps: u32 = 0;
        while (true) : (steps += 1) {
            if (steps == 32) {
                f.bad("async ring doesn't come back to ASYNCLISTADDR", .{});
                return;
            }
            const chars = f.rd(qh + 4) orelse return;
            if (chars & hw.ch_head != 0) heads += 1;
            f.runQh(qh, false);
            const link = f.rd(qh) orelse return;
            if (link & 0x6 != hw.typ_qh) {
                f.bad("async ring: link type {d} is not a QH", .{(link >> 1) & 3});
                return;
            }
            qh = hw.linkAddr(link);
            if (qh == start) break;
        }
        // EHCI 4.8.3: exactly one head of the reclamation list
        if (heads != 1) f.bad("async ring with {d} QHs marked H (EHCI 4.8.3: exactly one)", .{heads});
    }

    /// Static checks of a QH's endpoint words (TRM 15.11.6, table 15-42).
    fn checkQh(f: *Fake, qh: u32, chars: u32, caps: u32, periodic: bool) bool {
        var ok = true;
        if (qh & 0x1F != 0) {
            f.bad("QH at 0x{X:0>8} not 32-byte aligned", .{qh});
            ok = false;
        }
        const eps: u2 = @truncate(chars >> hw.ch_eps_shift);
        const mps = (chars >> hw.ch_mps_shift) & 0x7FF;
        const ep = (chars >> hw.ch_ep_shift) & 0xF;
        const smask: u8 = @truncate(caps);
        const cmask: u8 = @truncate(caps >> hw.cap_cmask_shift);
        if (caps >> hw.cap_mult_shift == 0) {
            f.bad("QH Mult = 0 (undefined)", .{});
            ok = false;
        }
        if (eps == 3) {
            f.bad("QH EPS = 11 (reserved)", .{});
            ok = false;
        }
        if (eps != 2) {
            if (mps > 64) {
                f.bad("FS/LS QH with max packet {d} > 64", .{mps});
                ok = false;
            }
            if ((caps >> hw.cap_hub_shift) & 0x7F != (f.ttctrl >> 24) & 0x7F) {
                f.bad("FS/LS QH hub address {d} != TTCTRL.TTHA", .{(caps >> hw.cap_hub_shift) & 0x7F});
                ok = false;
            }
            if (ep == 0 and chars & hw.ch_control == 0) {
                f.bad("FS/LS control QH without the C flag", .{});
                ok = false;
            }
        }
        if (ep != 0 and chars & hw.ch_control != 0) {
            f.bad("C flag on endpoint {d}", .{ep});
            ok = false;
        }
        if (periodic) {
            if (@popCount(smask) != 1) {
                f.bad("periodic QH S-mask 0x{X:0>2}: exactly one bit", .{smask});
                ok = false;
            }
            if (eps != 2 and (cmask == 0 or cmask & smask != 0)) {
                f.bad("periodic FS/LS QH C-mask 0x{X:0>2} with S-mask 0x{X:0>2}", .{ cmask, smask });
                ok = false;
            }
            if (chars & hw.ch_head != 0) f.bad("H flag on a periodic QH", .{});
        } else {
            if (smask != 0) {
                f.bad("async QH with S-mask 0x{X:0>2} (must be 0)", .{smask});
                ok = false;
            }
            if (chars & hw.ch_inactivate != 0) {
                f.bad("I flag on an async QH (undefined)", .{});
                ok = false;
            }
        }
        return ok;
    }

    /// Advance the QH's queue if the overlay is idle, then run one
    /// transaction (EHCI 4.10).
    fn runQh(f: *Fake, qh: u32, periodic: bool) void {
        const chars = f.rd(qh + 4) orelse return;
        const caps = f.rd(qh + 8) orelse return;
        if (!f.checkQh(qh, chars, caps, periodic)) return;
        var tok = f.rd(qh + 0x18) orelse return;
        if (tok & hw.tok_halted != 0) return;
        if (tok & hw.tok_active == 0) {
            const next = f.rd(qh + 0x10) orelse return;
            if (next & hw.link_t != 0) return;
            const qtd = hw.linkAddr(next);
            if (next & 0x1E != 0) f.bad("qTD pointer 0x{X:0>8} not 32-byte aligned", .{next});
            const t = f.rd(qtd + 8) orelse return;
            if (t & hw.tok_active == 0) return;
            // overlay the qTD (EHCI 4.10.2); DTC = 0 keeps the QH's toggle
            var i: u32 = 0;
            while (i < 8) : (i += 1) f.wr(qh + 0x10 + 4 * i, f.rd(qtd + 4 * i) orelse return);
            if (chars & hw.ch_dtc == 0) f.wr(qh + 0x18, (t & ~hw.tok_dt) | (tok & hw.tok_dt));
            f.wr(qh + 0xC, qtd);
            tok = f.rd(qh + 0x18).?;
        }
        f.transaction(qh, chars, periodic);
    }

    fn transaction(f: *Fake, qh: u32, chars: u32, periodic: bool) void {
        var tok = f.rd(qh + 0x18).?;
        const addr: u7 = @truncate(chars);
        const ep: u4 = @truncate(chars >> hw.ch_ep_shift);
        const eps: u2 = @truncate(chars >> hw.ch_eps_shift);
        const mps = (chars >> hw.ch_mps_shift) & 0x7FF;
        var bytes = hw.tokenBytes(tok);
        const toggle: u1 = @truncate(tok >> 31);
        const pid = hw.tokenPid(tok) orelse {
            f.bad("qTD with PID code 11 (reserved)", .{});
            return f.complete(qh, tok | hw.tok_halted, periodic, false);
        };

        // who answers: the device at this address, at the port's speed
        const dev: ?*Device = blk: {
            if (!f.pe) break :blk null;
            const d = f.device orelse break :blk null;
            if (d.address != addr) break :blk null;
            if (eps != @intFromEnum(d.speed)) {
                f.bad("QH EPS {d} for a device at port speed {d} (TRM 15.11.6)", .{ eps, @intFromEnum(d.speed) });
                break :blk null;
            }
            break :blk d;
        };
        const d = dev orelse {
            // no handshake: count down CERR (EHCI 4.15.1.1)
            const cerr = (tok >> hw.tok_cerr_shift) & 3;
            tok |= hw.tok_xact_err;
            if (cerr == 1) {
                tok &= ~(@as(u32, 3) << hw.tok_cerr_shift);
                return f.complete(qh, tok | hw.tok_halted, periodic, false);
            }
            if (cerr > 1) tok = (tok & ~(@as(u32, 3) << hw.tok_cerr_shift)) | (cerr - 1) << hw.tok_cerr_shift;
            f.wr(qh + 0x18, tok);
            return;
        };

        if (pid == .setup and d.speed != .high) {
            if (f.time - f.enabled_at < 10_000)
                f.bad("SETUP {d} us after the port reset (USB 7.1.7.3: 10 ms recovery)", .{f.time - f.enabled_at});
            if (f.address_set_at) |t| if (f.time - t < 2_000)
                f.bad("SETUP {d} us after SET_ADDRESS (USB 9.2.6.3: 2 ms)", .{f.time - t});
        }

        var buf: [1024]u8 = undefined;
        switch (pid) {
            .setup => {
                if (bytes != 8) f.bad("SETUP qTD with {d} bytes", .{bytes});
                if (toggle != 0) f.bad("SETUP with DATA1", .{});
                if (ep != 0) f.bad("SETUP to endpoint {d}", .{ep});
                if (!f.readBuf(qh, buf[0..8])) return;
                f.setups += 1;
                d.setupPacket(buf[0..8]);
                f.advance(qh, 8);
                bytes -|= 8;
                tok = f.rd(qh + 0x18).?;
                tok = (tok | hw.tok_dt);
                tok = setBytes(tok, bytes);
                return f.finishOrKeep(qh, tok, bytes == 0, periodic, false);
            },
            .out => {
                const n = @min(bytes, mps);
                if (!f.readBuf(qh, buf[0..n])) return;
                switch (d.outPacket(ep, toggle, buf[0..n], f.time)) {
                    .nak => {
                        f.naks += 1;
                        return;
                    },
                    .stall => return f.complete(qh, tok | hw.tok_halted, periodic, false),
                    .ack => {},
                }
                f.advance(qh, n);
                bytes -= n;
                tok = f.rd(qh + 0x18).?;
                tok = setBytes(tok ^ hw.tok_dt, bytes);
                return f.finishOrKeep(qh, tok, bytes == 0, periodic, false);
            },
            .in => {
                switch (d.inPacket(ep, mps, f.time)) {
                    .nak => {
                        f.naks += 1;
                        return;
                    },
                    .stall => return f.complete(qh, tok | hw.tok_halted, periodic, false),
                    .data => |p| {
                        if (p.toggle != toggle) {
                            // the host drops it as a repeat and ACKs: the
                            // device and the driver disagree on the toggle
                            f.bad("IN data with toggle {d}, qTD expects {d}", .{ p.toggle, toggle });
                            d.inAcked(ep, f.time);
                            return;
                        }
                        if (p.bytes.len > mps or p.bytes.len > bytes) {
                            // EHCI 4.15.1.3: babble halts the queue
                            return f.complete(qh, tok | hw.tok_babble | hw.tok_halted, periodic, false);
                        }
                        const was_status = ep == 0 and d.stage == .status_in;
                        const is_set_address = was_status and d.setup.request_type == 0 and
                            d.setup.request == hw.req_set_address;
                        if (!f.writeBuf(qh, p.bytes)) return;
                        d.inAcked(ep, f.time);
                        if (is_set_address) f.address_set_at = f.time;
                        f.advance(qh, @intCast(p.bytes.len));
                        bytes -= @intCast(p.bytes.len);
                        tok = f.rd(qh + 0x18).?;
                        tok = setBytes(tok ^ hw.tok_dt, bytes);
                        const short = p.bytes.len < mps;
                        return f.finishOrKeep(qh, tok, bytes == 0 or short, periodic, short);
                    },
                }
            },
        }
    }

    fn setBytes(tok: u32, bytes: u32) u32 {
        return (tok & ~(@as(u32, 0x7FFF) << hw.tok_bytes_shift)) | bytes << hw.tok_bytes_shift;
    }

    fn finishOrKeep(f: *Fake, qh: u32, tok: u32, done: bool, periodic: bool, short: bool) void {
        if (done) return f.complete(qh, tok, periodic, short);
        f.wr(qh + 0x18, tok);
    }

    /// The qTD is finished (or halted): clear Active, write the overlay's
    /// token and buffer pointer back to the qTD, follow the alternate next
    /// pointer after a short packet (EHCI 4.10.4, 4.10.5), interrupt.
    fn complete(f: *Fake, qh: u32, tok_in: u32, periodic: bool, short: bool) void {
        const tok = tok_in & ~hw.tok_active;
        f.wr(qh + 0x18, tok);
        const qtd = f.rd(qh + 0xC) orelse return;
        f.wr(qtd + 8, tok);
        f.wr(qtd + 12, f.rd(qh + 0x1C).?);
        if (tok & hw.tok_halted != 0) {
            f.sts |= hw.sts_uei;
            return;
        }
        if (short) {
            const alt = f.rd(qtd + 4).?;
            if (alt & hw.link_t == 0) f.wr(qh + 0x10, alt);
        }
        if (tok & hw.tok_ioc != 0) f.sts |= hw.sts_ui | (if (periodic) hw.sts_upi else hw.sts_uai);
    }

    /// The overlay's current buffer position: page `C_Page` plus the
    /// offset in page 0's low 12 bits (EHCI 3.5.4).
    fn bufAddr(f: *Fake, qh: u32, k: usize) ?u32 {
        const tok = f.rd(qh + 0x18).?;
        const cpage = (tok >> hw.tok_cpage_shift) & 7;
        const off = (f.rd(qh + 0x1C).? & 0xFFF) + k;
        const page = cpage + off / 4096;
        if (page > 4) {
            f.bad("qTD buffer runs past page 4", .{});
            return null;
        }
        const p = f.rd(qh + 0x1C + 4 * @as(u32, @intCast(page))).?;
        return (p & ~@as(u32, 0xFFF)) + @as(u32, @intCast(off % 4096));
    }

    fn readBuf(f: *Fake, qh: u32, out: []u8) bool {
        for (out, 0..) |*b, k| {
            const a = f.bufAddr(qh, k) orelse return false;
            b.* = (f.mem(a, 1) orelse return false)[0];
        }
        return true;
    }

    fn writeBuf(f: *Fake, qh: u32, in: []const u8) bool {
        for (in, 0..) |b, k| {
            const a = f.bufAddr(qh, k) orelse return false;
            (f.mem(a, 1) orelse return false)[0] = b;
        }
        return true;
    }

    /// Move the overlay's buffer position on by `n` bytes.
    fn advance(f: *Fake, qh: u32, n: u32) void {
        var tok = f.rd(qh + 0x18).?;
        const b0 = f.rd(qh + 0x1C).?;
        var cpage = (tok >> hw.tok_cpage_shift) & 7;
        var off = (b0 & 0xFFF) + n;
        while (off >= 4096) {
            off -= 4096;
            cpage += 1;
        }
        tok = (tok & ~(@as(u32, 7) << hw.tok_cpage_shift)) | (cpage & 7) << hw.tok_cpage_shift;
        f.wr(qh + 0x18, tok);
        f.wr(qh + 0x1C, (b0 & ~@as(u32, 0xFFF)) | off);
    }
};

// --- Tests ---------------------------------------------------------------------

const testing = std.testing;

test "both captured keyboards parse; boot keyboard on interface 0" {
    const want = [_]struct { mps0: u8, vid: u16, pid: u16, mps: u16 }{
        .{ .mps0 = 64, .vid = 0x1997, .pid = 0x2433, .mps = 64 },
        .{ .mps0 = 8, .vid = 0x04B4, .pid = 0x0818, .mps = 18 },
    };
    for (fixtures.all, want) |fx, w| {
        const d = try Device.init(fx);
        try testing.expectEqual(w.mps0, d.desc.max_packet0);
        try testing.expectEqual(w.vid, d.desc.vendor);
        try testing.expectEqual(w.pid, d.desc.product);
        try testing.expectEqual(@as(u8, 2), d.config.interfaces);
        try testing.expectEqual(@as(u16, 59), d.config.total);
        try testing.expectEqual(hw.BootKeyboard{ .interface = 0, .endpoint = 0x81, .max_packet = w.mps, .interval = 10 }, d.keyboard);
    }
}

/// A copy of a fixture with one byte of its device or configuration
/// descriptor changed (or the configuration cut short).
const Broken = struct {
    device: [18]u8,
    config: [59]u8,
    config_len: usize = 59,
    fx: Fixture,

    fn init(b: *Broken, base: *const Fixture) void {
        @memcpy(&b.device, base.device);
        @memcpy(&b.config, base.config);
        b.fx = base.*;
        b.fx.device = &b.device;
        b.fx.config = b.config[0..b.config_len];
    }

    fn cut(b: *Broken, len: usize) void {
        b.config_len = len;
        b.fx.config = b.config[0..len];
    }
};

test "the fake device refuses malformed descriptors" {
    const Case = struct { what: []const u8, area: enum { device, config, cut, report_keys }, at: usize, v: u8, err: DeviceError };
    // offsets into mini_keyboard's configuration: interface 0 at 9, its
    // HID descriptor at 18, endpoint 0x81 at 27, interface 1 at 34
    const cases = [_]Case{
        .{ .what = "device bLength 17", .area = .device, .at = 0, .v = 17, .err = error.BadLength },
        .{ .what = "device type 2", .area = .device, .at = 1, .v = 2, .err = error.BadType },
        .{ .what = "bMaxPacketSize0 7", .area = .device, .at = 7, .v = 7, .err = error.BadMaxPacket0 },
        .{ .what = "no configurations", .area = .device, .at = 17, .v = 0, .err = error.NoConfig },
        .{ .what = "config type 4", .area = .config, .at = 1, .v = 4, .err = error.BadType },
        .{ .what = "wTotalLength 58", .area = .config, .at = 2, .v = 58, .err = error.BadTotal },
        .{ .what = "bNumInterfaces 3", .area = .config, .at = 4, .v = 3, .err = error.CountMismatch },
        .{ .what = "interface 0 with 2 endpoints", .area = .config, .at = 9 + 4, .v = 2, .err = error.CountMismatch },
        .{ .what = "interface 0 with no endpoints", .area = .config, .at = 9 + 4, .v = 0, .err = error.CountMismatch },
        .{ .what = "a descriptor with bLength 0", .area = .config, .at = 18, .v = 0, .err = error.BadLength },
        .{ .what = "a descriptor running past the end", .area = .config, .at = 52, .v = 9, .err = error.Short },
        .{ .what = "endpoint address 0", .area = .config, .at = 27 + 2, .v = 0x80, .err = error.BadEndpoint },
        .{ .what = "HID descriptor without a report entry", .area = .config, .at = 18 + 5, .v = 0, .err = error.BadHid },
        .{ .what = "report descriptor length off by one", .area = .config, .at = 18 + 7, .v = 0x41, .err = error.ReportLength },
        .{ .what = "no boot keyboard (protocol 2)", .area = .config, .at = 9 + 7, .v = 2, .err = error.NoBootKeyboard },
        .{ .what = "configuration cut to 8 bytes", .area = .cut, .at = 8, .v = 0, .err = error.Short },
        .{ .what = "configuration cut mid-way", .area = .cut, .at = 30, .v = 0, .err = error.BadTotal },
        .{ .what = "report_keys 40", .area = .report_keys, .at = 0, .v = 40, .err = error.BadReportKeys },
    };
    for (cases) |c| {
        var b: Broken = undefined;
        b.config_len = 59;
        b.init(&fixtures.mini_keyboard);
        switch (c.area) {
            .device => b.device[c.at] = c.v,
            .config => b.config[c.at] = c.v,
            .cut => b.cut(c.at),
            .report_keys => b.fx.report_keys = c.v,
        }
        const got = Device.init(&b.fx);
        if (got) |_| {
            std.debug.print("accepted: {s}\n", .{c.what});
            return error.TestUnexpectedResult;
        } else |e| if (e != c.err) {
            std.debug.print("{s}: got {s}, want {s}\n", .{ c.what, @errorName(e), @errorName(c.err) });
            return error.TestUnexpectedResult;
        }
    }
    // an endpoint before any interface: move endpoint 0x81 to the front
    var b: Broken = undefined;
    b.config_len = 59;
    b.init(&fixtures.mini_keyboard);
    var moved: [59]u8 = undefined;
    @memcpy(moved[0..9], b.config[0..9]);
    @memcpy(moved[9..16], b.config[27..34]);
    @memcpy(moved[16..34], b.config[9..27]);
    @memcpy(moved[34..], b.config[34..]);
    b.config = moved;
    try testing.expectError(error.EndpointOutsideInterface, Device.init(&b.fx));
}

// --- A minimal test host: what the driver (usb.zig) will do, written out
// --- step by step, to drive the fake in these tests.

const Arena = extern struct {
    frame_list: [32]u32 align(4096),
    ctrl_qh: hw.Qh align(32),
    int_qh: hw.Qh align(32),
    qtd: [4]hw.Qtd align(32),
    setup: [8]u8 align(32),
    data: [256]u8 align(32),
    report: [64]u8 align(32),
};

const TestHost = struct {
    f: *Fake,
    a: *Arena,
    speed: hw.Speed = .none,
    addr: u7 = 0,
    mps0: u11 = 8,
    /// mistakes injected into the control QH by the tests
    mess_chars_clear: u32 = 0,
    mess_caps_clear: u32 = 0,
    mess_caps_set: u32 = 0,

    fn bus(h: *TestHost, p: anytype) u32 {
        return h.f.busAddr(@intFromPtr(p));
    }

    fn waitUs(h: *TestHost, us: u64) void {
        const t = h.f.nowUs();
        while (h.f.nowUs() - t < us) {}
    }

    fn waitFor(h: *TestHost, off: u32, mask: u32, want: u32, timeout_us: u64) !void {
        const t = h.f.nowUs();
        while (h.f.read32(off) & mask != want) {
            if (h.f.nowUs() - t > timeout_us) return error.Timeout;
        }
    }

    fn ulpi(h: *TestHost, cmd: u32) !u8 {
        h.f.write32(hw.reg_ulpi_viewport, cmd);
        try h.waitFor(hw.reg_ulpi_viewport, hw.ulpi_run, 0, 1000);
        return hw.ulpiData(h.f.read32(hw.reg_ulpi_viewport));
    }

    fn portWrite(h: *TestHost, set: u32, clear: u32) void {
        const v = h.f.read32(hw.reg_portsc1) & ~hw.port_w1c;
        h.f.write32(hw.reg_portsc1, (v | set) & ~clear);
    }

    /// Reset, host mode, Vbus through the PHY, port power, connect,
    /// debounce, port reset, schedules on.
    fn bringUp(h: *TestHost) !void {
        const f = h.f;
        f.write32(hw.reg_usbcmd, hw.cmd_rst);
        try h.waitFor(hw.reg_usbcmd, hw.cmd_rst, 0, 1000);
        f.write32(hw.reg_usbmode, hw.mode_cm_host);
        const vid = @as(u16, try h.ulpi(hw.ulpiRead(hw.ulpi_vendor_hi))) << 8 | try h.ulpi(hw.ulpiRead(hw.ulpi_vendor_lo));
        try testing.expectEqual(tusb1210_vendor, vid);
        _ = try h.ulpi(hw.ulpiWrite(hw.ulpi_otg_ctrl + hw.ulpi_set, hw.otg_drv_vbus | hw.otg_drv_vbus_ext));
        h.portWrite(hw.port_pp, 0);
        try h.waitFor(hw.reg_portsc1, hw.port_ccs, hw.port_ccs, 1_000_000);
        h.waitUs(100_000);
        h.portWrite(hw.port_pr, 0);
        h.waitUs(50_000);
        h.portWrite(0, hw.port_pr);
        try h.waitFor(hw.reg_portsc1, hw.port_pr | hw.port_pe, hw.port_pe, 20_000);
        h.speed = hw.portSpeed(f.read32(hw.reg_portsc1));
        h.waitUs(10_000);

        // async ring: the control QH alone, head of the list
        h.a.ctrl_qh = std.mem.zeroes(hw.Qh);
        h.a.ctrl_qh.link = h.bus(&h.a.ctrl_qh) | hw.typ_qh;
        h.setCtrlQh();
        h.a.ctrl_qh.overlay.next = hw.link_t;
        h.a.ctrl_qh.overlay.alt_next = hw.link_t;
        for (&h.a.frame_list) |*e| e.* = hw.link_t;
        f.write32(hw.reg_asynclistaddr, h.bus(&h.a.ctrl_qh));
        f.write32(hw.reg_periodiclistbase, h.bus(&h.a.frame_list));
        // 32-entry frame list: FS2:FS1:FS0 = 101
        f.write32(hw.reg_usbcmd, 0x0008_0000 | (1 << 15) | (1 << 2) | hw.cmd_rs | hw.cmd_ase);
        try h.waitFor(hw.reg_usbsts, hw.sts_as, hw.sts_as, 1000);
    }

    fn setCtrlQh(h: *TestHost) void {
        h.a.ctrl_qh.chars = hw.qhChars(h.addr, 0, h.speed, h.mps0, true, true) & ~h.mess_chars_clear;
        h.a.ctrl_qh.caps = (hw.qhCaps(1, 0, 0) & ~h.mess_caps_clear) | h.mess_caps_set;
    }

    fn qtd(h: *TestHost, i: usize, pid: hw.Pid, buf: []u8, toggle: u1, ioc: bool) void {
        const q = &h.a.qtd[i];
        q.next = hw.link_t;
        q.alt_next = hw.link_t;
        q.token = hw.token(pid, @intCast(buf.len), toggle, ioc);
        const start = h.bus(buf.ptr);
        for (&q.buf, 0..) |*p, k| p.* = if (k == 0) start else (start & ~@as(u32, 0xFFF)) + 4096 * @as(u32, @intCast(k));
    }

    /// A control transfer; returns the bytes moved in the data stage.
    fn control(h: *TestHost, s: hw.Setup, data: []u8) !usize {
        h.a.setup = s.bytes();
        h.qtd(0, .setup, &h.a.setup, 0, false);
        const in = s.deviceToHost();
        var last: usize = 1;
        if (data.len > 0) {
            h.qtd(1, if (in) .in else .out, data, 1, false);
            last = 2;
        }
        h.qtd(last, if (in and data.len > 0) .out else .in, h.a.data[0..0], 1, true);
        h.a.qtd[0].next = h.bus(&h.a.qtd[1]);
        if (last == 2) {
            h.a.qtd[1].next = h.bus(&h.a.qtd[2]);
            h.a.qtd[1].alt_next = h.bus(&h.a.qtd[2]); // short IN: on to the status stage
        }
        h.a.ctrl_qh.overlay.token = 0;
        h.a.ctrl_qh.overlay.next = h.bus(&h.a.qtd[0]);
        const t = h.f.nowUs();
        while (true) {
            for (h.a.qtd[0 .. last + 1]) |q| if (q.token & hw.tok_halted != 0) return error.Halted;
            if (h.a.qtd[last].token & hw.tok_active == 0) break;
            if (h.f.nowUs() - t > 100_000) return error.Timeout;
        }
        return if (last == 2) data.len - hw.tokenBytes(h.a.qtd[1].token) else 0;
    }

    fn enumerate(h: *TestHost, fx: *const Fixture) !void {
        var d: [256]u8 = undefined;
        // USB 9.6.1 practice: learn bMaxPacketSize0 from the first 8 bytes
        h.mps0 = 8;
        h.setCtrlQh();
        try testing.expectEqual(@as(usize, 8), try h.control(hw.getDescriptor(hw.dt_device, 0, 0, 8), h.a.data[0..8]));
        try testing.expectEqualSlices(u8, fx.device[0..8], h.a.data[0..8]);
        h.mps0 = h.a.data[7];
        try testing.expectEqual(@as(usize, 0), try h.control(.{ .request_type = 0, .request = hw.req_set_address, .value = 5, .index = 0, .length = 0 }, d[0..0]));
        h.waitUs(2_000);
        h.addr = 5;
        h.setCtrlQh();
        try testing.expectEqual(@as(usize, 18), try h.control(hw.getDescriptor(hw.dt_device, 0, 0, 18), h.a.data[0..18]));
        try testing.expectEqualSlices(u8, fx.device, h.a.data[0..18]);
        try testing.expectEqual(@as(usize, 9), try h.control(hw.getDescriptor(hw.dt_config, 0, 0, 9), h.a.data[0..9]));
        const head = try hw.parseConfigHead(h.a.data[0..9]);
        const n = try h.control(hw.getDescriptor(hw.dt_config, 0, 0, head.total), h.a.data[0..head.total]);
        try testing.expectEqualSlices(u8, fx.config, h.a.data[0..n]);
    }
};

fn setupTest(arena: *Arena, d: *Device) Fake {
    var f = Fake.init(std.mem.asBytes(arena), 0x0010_0000);
    f.plug(d);
    return f;
}

test "enumeration reproduces both captured devices" {
    for (fixtures.all) |fx| {
        const arena = try testing.allocator.create(Arena);
        defer testing.allocator.destroy(arena);
        var d = try Device.init(fx);
        var f = setupTest(arena, &d);
        var h: TestHost = .{ .f = &f, .a = arena };
        try h.bringUp();
        try testing.expectEqual(hw.Speed.full, h.speed);
        try h.enumerate(fx);

        // the report descriptor of each interface, and the strings
        for (fx.reports, 0..) |r, i| {
            const s: hw.Setup = .{ .request_type = 0x81, .request = hw.req_get_descriptor, .value = @as(u16, hw.dt_report) << 8, .index = @intCast(i), .length = @intCast(r.len) };
            const n = try h.control(s, h.a.data[0..r.len]);
            try testing.expectEqualSlices(u8, r, h.a.data[0..n]);
        }
        const dev = try hw.parseDevice(fx.device);
        const n = try h.control(hw.getDescriptor(hw.dt_string, dev.i_product, 0x0409, 255), h.a.data[0..255]);
        const text = fx.strings[dev.i_product].?;
        try testing.expectEqual(2 + 2 * text.len, n);
        for (text, 0..) |c, k| try testing.expectEqual(c, h.a.data[2 + 2 * k]);
        // a string the device doesn't have: STALL
        try testing.expectError(error.Halted, h.control(hw.getDescriptor(hw.dt_string, 9, 0x0409, 255), h.a.data[0..255]));
        try testing.expectEqual(@as(u32, 0), f.allViolations());
    }
}

/// Configure, then poll the keyboard's interrupt endpoint through the
/// periodic schedule; returns the first report's length.
fn firstReport(h: *TestHost, d: *Device, boot: bool) !usize {
    const kb = d.keyboard;
    _ = try h.control(.{ .request_type = 0, .request = hw.req_set_configuration, .value = d.config.value, .index = 0, .length = 0 }, h.a.data[0..0]);
    if (boot) _ = try h.control(.{ .request_type = 0x21, .request = hw.hid_set_protocol, .value = 0, .index = kb.interface, .length = 0 }, h.a.data[0..0]);
    _ = try h.control(.{ .request_type = 0x21, .request = hw.hid_set_idle, .value = 0, .index = kb.interface, .length = 0 }, h.a.data[0..0]);

    const q = &h.a.int_qh;
    q.* = std.mem.zeroes(hw.Qh);
    q.link = hw.link_t;
    q.chars = hw.qhChars(h.addr, @truncate(kb.endpoint), h.speed, @intCast(kb.max_packet), false, false);
    // start-split in micro-frame 0, complete-splits in 2 to 4 (EHCI 4.12.2)
    q.caps = hw.qhCaps(1, 0x01, 0x1C);
    q.overlay.next = hw.link_t;
    q.overlay.alt_next = hw.link_t;
    // every 8th frame (the endpoint asks for 10 ms; 8 is the power of two below)
    var i: usize = 0;
    while (i < h.a.frame_list.len) : (i += 8) h.a.frame_list[i] = h.bus(q) | hw.typ_qh;
    h.f.write32(hw.reg_usbcmd, h.f.read32(hw.reg_usbcmd) | hw.cmd_pse);
    try h.waitFor(hw.reg_usbsts, hw.sts_ps, hw.sts_ps, 1000);

    const qd = &h.a.qtd[3];
    qd.next = hw.link_t;
    qd.alt_next = hw.link_t;
    qd.token = hw.token(.in, kb.max_packet, 0, true);
    qd.buf = .{ h.bus(&h.a.report), 0, 0, 0, 0 };
    q.overlay.next = h.bus(qd);
    d.press(0x04); // 'a'
    const t = h.f.nowUs();
    while (qd.token & hw.tok_active != 0) {
        if (h.f.nowUs() - t > 100_000) return error.Timeout;
    }
    if (qd.token & hw.tok_halted != 0) return error.Halted;
    return kb.max_packet - hw.tokenBytes(qd.token);
}

test "boot protocol gives 8-byte reports on both keyboards; report protocol 18 on the thumb keyboard" {
    for (fixtures.all) |fx| for ([_]bool{ true, false }) |boot| {
        const arena = try testing.allocator.create(Arena);
        defer testing.allocator.destroy(arena);
        var d = try Device.init(fx);
        var f = setupTest(arena, &d);
        var h: TestHost = .{ .f = &f, .a = arena };
        try h.bringUp();
        try h.enumerate(fx);
        const n = try firstReport(&h, &d, boot);
        const want: usize = if (boot) 8 else 2 + fx.report_keys;
        try testing.expectEqual(want, n);
        try testing.expectEqual(@as(u8, 0x04), arena.report[2]);
        try testing.expectEqual(@as(u32, 1), d.reports_sent);
        try testing.expectEqual(@as(u32, 0), f.allViolations());
    };
}

test "typing on the emulated mini keyboard gives the reports recorded from the real one" {
    for ([_]bool{ true, false }) |boot| {
        const arena = try testing.allocator.create(Arena);
        defer testing.allocator.destroy(arena);
        var d = try Device.init(&fixtures.mini_keyboard);
        var f = setupTest(arena, &d);
        var h: TestHost = .{ .f = &f, .a = arena };
        try h.bringUp();
        try h.enumerate(&fixtures.mini_keyboard);
        _ = try firstReport(&h, &d, boot); // 'a' down
        d.release(0x04);
        const qd = &arena.qtd[3];
        var got: [fixtures.mini_keyboard_typing.len][8]u8 = undefined;
        var n: usize = 0;
        // drain the release of 'a', then type: each recorded press report
        // names the key, each zero report is its release
        var want_drain = true;
        var i: usize = 0;
        while (n < got.len) {
            if (!want_drain) {
                const r = fixtures.mini_keyboard_typing[i];
                if (r[2] != 0) d.press(r[2]) else d.release(fixtures.mini_keyboard_typing[i - 1][2]);
                i += 1;
            }
            qd.token = hw.token(.in, 8, 0, true);
            qd.buf = .{ h.bus(&arena.report), 0, 0, 0, 0 };
            arena.int_qh.overlay.next = h.bus(qd);
            const t = f.nowUs();
            while (qd.token & hw.tok_active != 0) {
                if (f.nowUs() - t > 100_000) return error.Timeout;
            }
            if (!want_drain) {
                @memcpy(&got[n], arena.report[0..8]);
                n += 1;
            }
            want_drain = false;
        }
        for (fixtures.mini_keyboard_typing, got) |w, g| try testing.expectEqualSlices(u8, &w, &g);
        try testing.expectEqual(@as(u32, 0), f.allViolations());
    }
}

test "a key held, idle rate 0: no repeats; released: one more report" {
    const arena = try testing.allocator.create(Arena);
    defer testing.allocator.destroy(arena);
    var d = try Device.init(&fixtures.mini_keyboard);
    var f = setupTest(arena, &d);
    var h: TestHost = .{ .f = &f, .a = arena };
    try h.bringUp();
    try h.enumerate(&fixtures.mini_keyboard);
    _ = try firstReport(&h, &d, true);
    const qd = &arena.qtd[3];
    // requeue: the QH keeps the toggle (DTC = 0)
    qd.token = hw.token(.in, 8, 0, true);
    qd.buf = .{ h.bus(&arena.report), 0, 0, 0, 0 };
    arena.int_qh.overlay.next = h.bus(qd);
    h.waitUs(200_000);
    try testing.expect(qd.token & hw.tok_active != 0); // NAKed all along
    d.release(0x04);
    h.waitUs(20_000);
    try testing.expect(qd.token & hw.tok_active == 0);
    try testing.expectEqualSlices(u8, &[_]u8{0} ** 8, arena.report[0..8]);
    try testing.expectEqual(@as(u32, 2), d.reports_sent);
    try testing.expectEqual(@as(u32, 0), f.allViolations());
}

test "unplugged mid-transfer: the qTD halts with a transaction error, the port reports it" {
    const arena = try testing.allocator.create(Arena);
    defer testing.allocator.destroy(arena);
    var d = try Device.init(&fixtures.mini_keyboard);
    var f = setupTest(arena, &d);
    var h: TestHost = .{ .f = &f, .a = arena };
    try h.bringUp();
    try h.enumerate(&fixtures.mini_keyboard);
    _ = f.read32(hw.reg_portsc1);
    h.portWrite(hw.port_csc, 0); // acknowledge the connect
    f.unplug();
    try testing.expectError(error.Halted, h.control(hw.getDescriptor(hw.dt_device, 0, 0, 18), arena.data[0..18]));
    try testing.expect(arena.qtd[0].token & hw.tok_xact_err != 0);
    const p = f.read32(hw.reg_portsc1);
    try testing.expect(p & hw.port_ccs == 0);
    try testing.expect(p & hw.port_csc != 0);
    try testing.expect(p & hw.port_pe == 0);
    try testing.expect(f.read32(hw.reg_usbsts) & hw.sts_uei != 0);
    // plugged back in: connect again, and a reset brings it back at address 0
    f.plug(&d);
    try h.waitFor(hw.reg_portsc1, hw.port_ccs, hw.port_ccs, 10_000);
    try testing.expectEqual(@as(u32, 0), f.allViolations());
}

test "no Vbus from the PHY: nothing connects" {
    const arena = try testing.allocator.create(Arena);
    defer testing.allocator.destroy(arena);
    var d = try Device.init(&fixtures.mini_keyboard);
    var f = setupTest(arena, &d);
    var h: TestHost = .{ .f = &f, .a = arena };
    f.write32(hw.reg_usbcmd, hw.cmd_rst);
    try h.waitFor(hw.reg_usbcmd, hw.cmd_rst, 0, 1000);
    f.write32(hw.reg_usbmode, hw.mode_cm_host);
    _ = try h.ulpi(hw.ulpiWrite(hw.ulpi_otg_ctrl + hw.ulpi_set, hw.otg_drv_vbus)); // external supply bit missing
    h.portWrite(hw.port_pp, 0);
    try testing.expectError(error.Timeout, h.waitFor(hw.reg_portsc1, hw.port_ccs, hw.port_ccs, 200_000));
}

/// Runs `bringUp` and `enumerate` with one mistake injected by `mess`
/// after the bring-up, and expects the fake to count violations.
fn expectCaught(comptime mess: fn (*TestHost) void) !void {
    const arena = try testing.allocator.create(Arena);
    defer testing.allocator.destroy(arena);
    var d = try Device.init(&fixtures.mini_keyboard);
    var f = setupTest(arena, &d);
    f.quiet = true;
    d.quiet = true;
    var h: TestHost = .{ .f = &f, .a = arena };
    try h.bringUp();
    mess(&h);
    h.enumerate(&fixtures.mini_keyboard) catch {};
    try testing.expect(f.allViolations() > 0);
}

test "the fake catches driver mistakes" {
    // control QH without the C flag
    try expectCaught(struct {
        fn f(h: *TestHost) void {
            h.mess_chars_clear = hw.ch_control;
        }
    }.f);
    // Mult = 0
    try expectCaught(struct {
        fn f(h: *TestHost) void {
            h.mess_caps_clear = @as(u32, 3) << hw.cap_mult_shift;
        }
    }.f);
    // an S-mask on an async QH
    try expectCaught(struct {
        fn f(h: *TestHost) void {
            h.mess_caps_set = 1;
        }
    }.f);
    // a high-speed EPS for a full-speed device
    try expectCaught(struct {
        fn f(h: *TestHost) void {
            h.speed = .high;
        }
    }.f);
    // no head of reclamation list
    try expectCaught(struct {
        fn f(h: *TestHost) void {
            h.mess_chars_clear = hw.ch_head;
        }
    }.f);
    // the async ring points outside the arena
    try expectCaught(struct {
        fn f(h: *TestHost) void {
            h.a.ctrl_qh.link = 0x0800_0000 | hw.typ_qh;
        }
    }.f);
    // RST while running
    try expectCaught(struct {
        fn f(h: *TestHost) void {
            h.f.write32(hw.reg_usbcmd, hw.cmd_rst);
        }
    }.f);
}

test "the fake catches a port reset inside the connect debounce and a SETUP too soon after it" {
    const arena = try testing.allocator.create(Arena);
    defer testing.allocator.destroy(arena);
    var d = try Device.init(&fixtures.mini_keyboard);
    var f = setupTest(arena, &d);
    f.quiet = true;
    var h: TestHost = .{ .f = &f, .a = arena };
    f.write32(hw.reg_usbcmd, hw.cmd_rst);
    try h.waitFor(hw.reg_usbcmd, hw.cmd_rst, 0, 1000);
    f.write32(hw.reg_usbmode, hw.mode_cm_host);
    _ = try h.ulpi(hw.ulpiWrite(hw.ulpi_otg_ctrl + hw.ulpi_set, hw.otg_drv_vbus | hw.otg_drv_vbus_ext));
    h.portWrite(hw.port_pp, 0);
    try h.waitFor(hw.reg_portsc1, hw.port_ccs, hw.port_ccs, 10_000);
    h.portWrite(hw.port_pr, 0);
    try testing.expectEqual(@as(u32, 1), f.violations);
    h.waitUs(50_000);
    h.portWrite(0, hw.port_pr);
    try h.waitFor(hw.reg_portsc1, hw.port_pr | hw.port_pe, hw.port_pe, 20_000);
    h.speed = .full;
    arena.ctrl_qh = std.mem.zeroes(hw.Qh);
    arena.ctrl_qh.link = h.bus(&arena.ctrl_qh) | hw.typ_qh;
    h.setCtrlQh();
    arena.ctrl_qh.overlay.next = hw.link_t;
    f.write32(hw.reg_asynclistaddr, h.bus(&arena.ctrl_qh));
    f.write32(hw.reg_usbcmd, 0x0008_0000 | hw.cmd_rs | hw.cmd_ase);
    _ = h.control(hw.getDescriptor(hw.dt_device, 0, 0, 8), arena.data[0..8]) catch {};
    try testing.expect(f.violations >= 2);
}

test "SET_ADDRESS takes effect after its status stage; the old address stops answering" {
    const arena = try testing.allocator.create(Arena);
    defer testing.allocator.destroy(arena);
    var d = try Device.init(&fixtures.thumb_keyboard);
    var f = setupTest(arena, &d);
    var h: TestHost = .{ .f = &f, .a = arena };
    try h.bringUp();
    try h.enumerate(&fixtures.thumb_keyboard);
    try testing.expectEqual(@as(u7, 5), d.address);
    h.addr = 0;
    h.setCtrlQh();
    try testing.expectError(error.Halted, h.control(hw.getDescriptor(hw.dt_device, 0, 0, 18), arena.data[0..18]));
    try testing.expect(arena.qtd[0].token & hw.tok_xact_err != 0);
}
