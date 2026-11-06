//! USB host definitions shared by the driver (usb.zig, side track E) and
//! its desktop fake (usb_fake.zig): the Zynq USB controller's registers,
//! the EHCI transfer structures it walks, and USB/HID descriptor parsing.
//!
//! Sources, cited below as:
//! * TRM: Zynq-7000 TRM (UG585 v1.8.1), chapter 15 and appendix B
//!   ("Register (usb) ..."). The controller is a ChipIdea/ARC core with an
//!   EHCI host interface and an *embedded transaction translator* (15.11.2):
//!   full- and low-speed devices on the root port use ordinary QHs and
//!   qTDs with Hub Addr = TTCTRL.TTHA (0), EPS = the port speed, and, for
//!   interrupt endpoints, S- and C-masks as for a hub's TT (15.11.6, 15.11.7).
//! * EHCI: Enhanced Host Controller Interface 1.0, section 3 (structures)
//!   and 4 (operation), for what the TRM leaves to it.
//! * USB: USB 2.0 specification, chapter 9 (requests, descriptors).
//! * HID: Device Class Definition for HID 1.11 (6.2.1 HID descriptor,
//!   7.2 class requests, appendix B boot protocol).
//!
//! All structures are little-endian 32-bit words; the PS runs little-endian
//! with the MMU and caches off, so the controller's AHB master (non-
//! cacheable, TRM 15.15.5) and the CPU see the same memory. OCM mapped low
//! is visible to the USB DMA at the same addresses (TRM table 4-1, "other
//! bus masters").

const std = @import("std");

// --- Controller registers (TRM appendix B, offsets from the base) ----------
/// usb0, on MIO 28 to 39 (the ZedBoard's J13 and the Arty's host port).
pub const usb0_base: usize = 0xE000_2000;

pub const reg_hcsparams = 0x104; // N_PORTS [3:0], N_TT, N_PTT
pub const reg_usbcmd = 0x140;
pub const reg_usbsts = 0x144;
pub const reg_usbintr = 0x148;
pub const reg_frindex = 0x14C;
pub const reg_periodiclistbase = 0x154; // 4 KiB aligned
pub const reg_asynclistaddr = 0x158; // 32-byte aligned
pub const reg_ttctrl = 0x15C; // TTHA [30:24]
pub const reg_ulpi_viewport = 0x170;
pub const reg_portsc1 = 0x184;
pub const reg_otgsc = 0x1A4;
pub const reg_usbmode = 0x1A8;

// USBCMD
pub const cmd_rs: u32 = 1 << 0; // run/stop
pub const cmd_rst: u32 = 1 << 1; // reset; reads 1 until done (15.15.2)
pub const cmd_fs_mask: u32 = (1 << 15) | (0x3 << 2); // frame list size FS2, FS1:0
pub const cmd_pse: u32 = 1 << 4; // periodic schedule enable
pub const cmd_ase: u32 = 1 << 5; // async schedule enable
pub const cmd_iaa: u32 = 1 << 6; // async advance doorbell
pub const cmd_itc_mask: u32 = 0xFF << 16; // interrupt threshold

/// Frame list length from USBCMD's FS2:FS1:FS0 (TRM "Register (usb)
/// USBCMD"): 000 = 1024 entries, 001 = 512, ..., 111 = 8.
pub fn frameListLen(usbcmd: u32) u32 {
    const code = ((usbcmd >> 13) & 0x4) | ((usbcmd >> 2) & 0x3);
    return @as(u32, 1024) >> @intCast(code);
}

// USBSTS (write 1 to clear the rw bits)
pub const sts_ui: u32 = 1 << 0; // a qTD with IOC completed
pub const sts_uei: u32 = 1 << 1; // a transaction ended in error
pub const sts_pci: u32 = 1 << 2; // port change
pub const sts_fri: u32 = 1 << 3; // frame list rollover
pub const sts_sei: u32 = 1 << 4; // system (AHB) error
pub const sts_aai: u32 = 1 << 5; // async advance
pub const sts_sri: u32 = 1 << 7; // SOF, every 125 us in host mode
pub const sts_ulpii: u32 = 1 << 10; // ULPI viewport transfer done
pub const sts_hch: u32 = 1 << 12; // halted
pub const sts_rcl: u32 = 1 << 13; // async schedule empty
pub const sts_ps: u32 = 1 << 14; // periodic schedule running
pub const sts_as: u32 = 1 << 15; // async schedule running
pub const sts_uai: u32 = 1 << 18; // async qTD with IOC completed
pub const sts_upi: u32 = 1 << 19; // periodic qTD with IOC completed
pub const sts_w1c: u32 = sts_ui | sts_uei | sts_pci | sts_fri | sts_sei | sts_aai |
    sts_sri | sts_ulpii | sts_uai | sts_upi;

// ULPI viewport (TRM "Register (usb) ULPI_VIEWPORT")
pub const ulpi_wu: u32 = 1 << 31; // wake-up; reads 1 while in progress
pub const ulpi_run: u32 = 1 << 30; // start; reads 1 while in progress
pub const ulpi_rw: u32 = 1 << 29; // 1 = write
pub const ulpi_ss: u32 = 1 << 27; // PHY in synchronous (normal) state
pub fn ulpiRead(addr: u8) u32 {
    return ulpi_run | @as(u32, addr) << 16;
}
pub fn ulpiWrite(addr: u8, v: u8) u32 {
    return ulpi_run | ulpi_rw | @as(u32, addr) << 16 | v;
}
pub fn ulpiData(viewport: u32) u8 {
    return @truncate(viewport >> 8);
}

// PORTSC1 (TRM "Register (usb) PORTSC1"; one port)
pub const port_ccs: u32 = 1 << 0; // device present
pub const port_csc: u32 = 1 << 1; // connect change (w1c)
pub const port_pe: u32 = 1 << 2; // enabled; software can only clear it
pub const port_pec: u32 = 1 << 3; // enable change (w1c)
pub const port_oca: u32 = 1 << 4;
pub const port_occ: u32 = 1 << 5; // over-current change (w1c)
pub const port_susp: u32 = 1 << 7;
pub const port_pr: u32 = 1 << 8; // reset; the controller times it (15.11.4)
pub const port_ls_mask: u32 = 0x3 << 10; // line state
pub const port_pp: u32 = 1 << 12; // power (drives the EMIO VBUSPWRSELECT only)
pub const port_pspd_shift = 26; // speed [27:26]
pub const port_pspd_mask: u32 = 0x3 << port_pspd_shift;
pub const port_pfsc: u32 = 1 << 24; // force full speed
pub const port_pts_mask: u32 = 0x3 << 30; // PHY type: 10 = ULPI
/// The write-1-to-clear bits: a read-modify-write must mask them out.
pub const port_w1c: u32 = port_csc | port_pec | port_occ;

pub const Speed = enum(u2) { full = 0, low = 1, high = 2, none = 3 };
pub fn portSpeed(portsc: u32) Speed {
    return @enumFromInt(@as(u2, @truncate(portsc >> port_pspd_shift)));
}

// USBMODE
pub const mode_cm_host: u32 = 0x3;
pub const mode_cm_mask: u32 = 0x3;

// ULPI PHY registers (ULPI 1.1, table 41; the TUSB1210 and the Arty's
// USB3320 both follow it). Each has a set (+1) and clear (+2) alias.
pub const ulpi_vendor_lo = 0x00;
pub const ulpi_vendor_hi = 0x01;
pub const ulpi_product_lo = 0x02;
pub const ulpi_product_hi = 0x03;
pub const ulpi_func_ctrl = 0x04;
pub const ulpi_iface_ctrl = 0x07;
pub const ulpi_otg_ctrl = 0x0A;
pub const ulpi_set = 1;
pub const ulpi_clr = 2;
// OTG control bits
pub const otg_dp_pulldown: u8 = 1 << 1;
pub const otg_dm_pulldown: u8 = 1 << 2;
pub const otg_drv_vbus: u8 = 1 << 5;
pub const otg_drv_vbus_ext: u8 = 1 << 6;

// --- Transfer structures (TRM 15.12; EHCI 3.5, 3.6) -------------------------
/// Link pointer terminate bit and type field (TRM 15.12.2).
pub const link_t: u32 = 1;
pub const typ_qh: u32 = 0b01 << 1;
pub fn linkAddr(w: u32) u32 {
    return w & ~@as(u32, 0x1F);
}

/// Queue element transfer descriptor (TRM 15.12.5): 32 bytes, 32-byte
/// aligned. Five page pointers, so up to 16 KiB from a page-aligned start.
pub const Qtd = extern struct {
    next: u32,
    alt_next: u32,
    token: u32,
    buf: [5]u32,
};

/// Queue head (TRM 15.12.6): 48 bytes, 32-byte aligned. Words 4 to 11 are
/// the transfer overlay, a copy of the current qTD.
pub const Qh = extern struct {
    link: u32,
    chars: u32, // DWord 1: endpoint characteristics
    caps: u32, // DWord 2: endpoint capabilities
    current: u32,
    overlay: Qtd,
};

comptime {
    std.debug.assert(@sizeOf(Qtd) == 32);
    std.debug.assert(@sizeOf(Qh) == 48);
}

// qTD token (TRM table 15-37; EHCI 3.5.3)
pub const tok_active: u32 = 1 << 7;
pub const tok_halted: u32 = 1 << 6;
pub const tok_buf_err: u32 = 1 << 5;
pub const tok_babble: u32 = 1 << 4;
pub const tok_xact_err: u32 = 1 << 3;
pub const tok_missed_uframe: u32 = 1 << 2;
pub const tok_split_xstate: u32 = 1 << 1;
pub const tok_status_mask: u32 = 0xFF;
pub const tok_pid_shift = 8;
pub const tok_cerr_shift = 10;
pub const tok_cpage_shift = 12;
pub const tok_ioc: u32 = 1 << 15;
pub const tok_bytes_shift = 16;
pub const tok_dt: u32 = 1 << 31;

pub const Pid = enum(u2) { out = 0, in = 1, setup = 2 };

pub fn tokenBytes(tok: u32) u32 {
    return (tok >> tok_bytes_shift) & 0x7FFF;
}
pub fn tokenPid(tok: u32) ?Pid {
    const p: u2 = @truncate(tok >> tok_pid_shift);
    return if (p == 3) null else @enumFromInt(p);
}

/// An active qTD token: `bytes` to move, 3 retries (CERR), the data toggle.
pub fn token(pid: Pid, bytes: u32, toggle: u1, ioc: bool) u32 {
    return @as(u32, toggle) << 31 | bytes << tok_bytes_shift |
        (if (ioc) tok_ioc else 0) | 3 << tok_cerr_shift |
        @as(u32, @intFromEnum(pid)) << tok_pid_shift | tok_active;
}

// QH DWord 1 (TRM table 15-42)
pub const ch_addr_mask: u32 = 0x7F;
pub const ch_inactivate: u32 = 1 << 7;
pub const ch_ep_shift = 8;
pub const ch_eps_shift = 12;
pub const ch_dtc: u32 = 1 << 14;
pub const ch_head: u32 = 1 << 15;
pub const ch_mps_shift = 16;
pub const ch_control: u32 = 1 << 27;
pub const ch_rl_shift = 28;

// QH DWord 2
pub const cap_smask_shift = 0;
pub const cap_cmask_shift = 8;
pub const cap_hub_shift = 16;
pub const cap_port_shift = 23;
pub const cap_mult_shift = 30;

/// DWord 1 for an endpoint of a device on the root port. `control` sets
/// the C flag, which a full- or low-speed control endpoint needs (TRM
/// table 15-42); DTC is set for control endpoints so each stage's qTD
/// brings its own toggle.
pub fn qhChars(addr: u7, ep: u4, speed: Speed, mps: u11, control: bool, head: bool) u32 {
    var v: u32 = @as(u32, addr) | @as(u32, ep) << ch_ep_shift |
        @as(u32, @intFromEnum(speed)) << ch_eps_shift | @as(u32, mps) << ch_mps_shift;
    if (control) v |= ch_dtc;
    if (control and speed != .high) v |= ch_control;
    if (head) v |= ch_head;
    return v;
}

/// DWord 2: one transaction per micro-frame (Mult = 1: 0 is undefined,
/// TRM table 15-42), hub address TTHA = 0, and the masks.
pub fn qhCaps(port: u7, smask: u8, cmask: u8) u32 {
    return @as(u32, 1) << cap_mult_shift | @as(u32, port) << cap_port_shift |
        @as(u32, cmask) << cap_cmask_shift | @as(u32, smask) << cap_smask_shift;
}

// --- USB requests and descriptors (USB 9.3, 9.4, 9.6) ----------------------
pub const Setup = extern struct {
    request_type: u8,
    request: u8,
    value: u16 align(1),
    index: u16 align(1),
    length: u16 align(1),

    pub fn bytes(s: Setup) [8]u8 {
        var b: [8]u8 = undefined;
        b[0] = s.request_type;
        b[1] = s.request;
        std.mem.writeInt(u16, b[2..4], s.value, .little);
        std.mem.writeInt(u16, b[4..6], s.index, .little);
        std.mem.writeInt(u16, b[6..8], s.length, .little);
        return b;
    }

    pub fn parse(b: *const [8]u8) Setup {
        return .{
            .request_type = b[0],
            .request = b[1],
            .value = std.mem.readInt(u16, b[2..4], .little),
            .index = std.mem.readInt(u16, b[4..6], .little),
            .length = std.mem.readInt(u16, b[6..8], .little),
        };
    }

    pub fn deviceToHost(s: Setup) bool {
        return s.request_type & 0x80 != 0;
    }
};

// bmRequestType
pub const rt_in: u8 = 0x80;
pub const rt_class: u8 = 0x20;
pub const rt_interface: u8 = 0x01;
// standard requests (USB table 9-4)
pub const req_get_status = 0;
pub const req_clear_feature = 1;
pub const req_set_address = 5;
pub const req_get_descriptor = 6;
pub const req_get_configuration = 8;
pub const req_set_configuration = 9;
// HID class requests (HID 7.2)
pub const hid_get_report = 0x01;
pub const hid_get_idle = 0x02;
pub const hid_get_protocol = 0x03;
pub const hid_set_report = 0x09;
pub const hid_set_idle = 0x0A;
pub const hid_set_protocol = 0x0B;

// descriptor types (USB table 9-5; HID 7.1)
pub const dt_device = 1;
pub const dt_config = 2;
pub const dt_string = 3;
pub const dt_interface = 4;
pub const dt_endpoint = 5;
pub const dt_hid = 0x21;
pub const dt_report = 0x22;

pub fn getDescriptor(dtype: u8, index: u8, lang: u16, length: u16) Setup {
    return .{ .request_type = rt_in, .request = req_get_descriptor, .value = @as(u16, dtype) << 8 | index, .index = lang, .length = length };
}

pub const DescError = error{
    /// fewer bytes than the descriptor claims, or than its type needs
    Short,
    /// bLength too small for the type, or zero
    BadLength,
    /// the first descriptor is not of the expected type
    BadType,
    /// wTotalLength differs from the configuration's bytes
    BadTotal,
    /// bMaxPacketSize0 not 8, 16, 32 or 64 (USB 9.6.1)
    BadMaxPacket0,
    /// no configurations
    NoConfig,
    /// an endpoint descriptor before any interface
    EndpointOutsideInterface,
    /// an interface with a different number of endpoint descriptors than
    /// its bNumEndpoints, or a different number of interfaces than
    /// bNumInterfaces
    CountMismatch,
    /// endpoint address 0, or wMaxPacketSize 0
    BadEndpoint,
    /// a HID descriptor without a report descriptor entry
    BadHid,
};

pub const DeviceDesc = struct {
    usb_version: u16,
    class: u8,
    max_packet0: u8,
    vendor: u16,
    product: u16,
    release: u16,
    i_manufacturer: u8,
    i_product: u8,
    i_serial: u8,
    configs: u8,
};

/// The 18-byte device descriptor (USB table 9-8). `b` may be longer.
pub fn parseDevice(b: []const u8) DescError!DeviceDesc {
    if (b.len < 18) return error.Short;
    if (b[0] != 18) return error.BadLength;
    if (b[1] != dt_device) return error.BadType;
    switch (b[7]) {
        8, 16, 32, 64 => {},
        else => return error.BadMaxPacket0,
    }
    if (b[17] == 0) return error.NoConfig;
    return .{
        .usb_version = std.mem.readInt(u16, b[2..4], .little),
        .class = b[4],
        .max_packet0 = b[7],
        .vendor = std.mem.readInt(u16, b[8..10], .little),
        .product = std.mem.readInt(u16, b[10..12], .little),
        .release = std.mem.readInt(u16, b[12..14], .little),
        .i_manufacturer = b[14],
        .i_product = b[15],
        .i_serial = b[16],
        .configs = b[17],
    };
}

/// The first 9 bytes of a configuration (USB table 9-10): enough to learn
/// wTotalLength before reading the whole thing.
pub const ConfigHead = struct {
    total: u16,
    interfaces: u8,
    value: u8,
    attributes: u8,
    max_power_2ma: u8,
};

pub fn parseConfigHead(b: []const u8) DescError!ConfigHead {
    if (b.len < 9) return error.Short;
    if (b[0] < 9) return error.BadLength;
    if (b[1] != dt_config) return error.BadType;
    const total = std.mem.readInt(u16, b[2..4], .little);
    if (total < b[0]) return error.BadTotal;
    return .{ .total = total, .interfaces = b[4], .value = b[5], .attributes = b[7], .max_power_2ma = b[8] };
}

pub const Interface = struct {
    number: u8,
    alt: u8,
    endpoints: u8,
    class: u8,
    subclass: u8,
    protocol: u8,
    /// wDescriptorLength of the report descriptor, from the HID descriptor
    report_len: ?u16 = null,
};

pub const Endpoint = struct {
    address: u8, // bit 7: IN
    attributes: u8, // [1:0]: 0 control, 1 iso, 2 bulk, 3 interrupt
    max_packet: u16,
    interval: u8,

    pub fn isInterruptIn(e: Endpoint) bool {
        return e.address & 0x80 != 0 and e.attributes & 3 == 3;
    }
};

pub const Item = union(enum) {
    interface: Interface,
    hid: u16, // report descriptor length, for the interface before it
    endpoint: Endpoint,
    other: u8, // a descriptor type this code doesn't use
};

/// Walks the descriptors after a configuration's first 9 bytes. Use only
/// on bytes `parseConfig` accepted.
pub const Walker = struct {
    b: []const u8,
    pos: usize,

    pub fn init(config: []const u8) Walker {
        return .{ .b = config, .pos = config[0] };
    }

    pub fn next(w: *Walker) ?Item {
        if (w.pos >= w.b.len) return null;
        const d = w.b[w.pos..][0..w.b[w.pos]];
        w.pos += d.len;
        return switch (d[1]) {
            dt_interface => .{ .interface = .{ .number = d[2], .alt = d[3], .endpoints = d[4], .class = d[5], .subclass = d[6], .protocol = d[7] } },
            dt_endpoint => .{ .endpoint = .{
                .address = d[2],
                .attributes = d[3],
                .max_packet = std.mem.readInt(u16, d[4..6], .little) & 0x7FF,
                .interval = d[6],
            } },
            dt_hid => .{ .hid = hidReportLen(d) orelse 0 },
            else => .{ .other = d[1] },
        };
    }
};

/// wDescriptorLength of the first report descriptor in a HID descriptor
/// (HID 6.2.1: bNumDescriptors at 5, then type/length pairs from 6).
fn hidReportLen(d: []const u8) ?u16 {
    if (d.len < 9 or d[5] == 0) return null;
    var i: usize = 6;
    while (i + 3 <= d.len) : (i += 3) {
        if (d[i] == dt_report) return std.mem.readInt(u16, d[i + 1 ..][0..2], .little);
    }
    return null;
}

/// Checks a whole configuration (wTotalLength bytes, USB 9.6.3): every
/// descriptor fits, endpoints follow an interface and match its
/// bNumEndpoints, the interface count matches bNumInterfaces (counting
/// alternate setting 0 only), HID descriptors name a report descriptor.
pub fn parseConfig(b: []const u8) DescError!ConfigHead {
    const head = try parseConfigHead(b);
    if (head.total != b.len) return error.BadTotal;
    var pos: usize = b[0];
    var in_interface = false;
    var eps_left: u8 = 0;
    var interfaces: u8 = 0;
    while (pos < b.len) {
        if (b.len - pos < 2) return error.Short;
        const len = b[pos];
        if (len < 2) return error.BadLength;
        if (len > b.len - pos) return error.Short;
        const d = b[pos..][0..len];
        switch (d[1]) {
            dt_interface => {
                if (len < 9) return error.BadLength;
                if (in_interface and eps_left != 0) return error.CountMismatch;
                in_interface = true;
                eps_left = d[4];
                if (d[3] == 0) interfaces += 1;
            },
            dt_endpoint => {
                if (len < 7) return error.BadLength;
                if (!in_interface) return error.EndpointOutsideInterface;
                if (eps_left == 0) return error.CountMismatch;
                eps_left -= 1;
                if (d[2] & 0x0F == 0) return error.BadEndpoint;
                if (std.mem.readInt(u16, d[4..6], .little) & 0x7FF == 0) return error.BadEndpoint;
            },
            dt_hid => {
                if (len < 9) return error.BadLength;
                if (hidReportLen(d) == null) return error.BadHid;
            },
            dt_config, dt_device => return error.BadType,
            else => {},
        }
        pos += len;
    }
    if (in_interface and eps_left != 0) return error.CountMismatch;
    if (interfaces != head.interfaces) return error.CountMismatch;
    return head;
}

/// A boot-protocol keyboard interface (HID 4.2, 4.3: class 3, subclass 1,
/// protocol 1) and its interrupt-IN endpoint.
pub const BootKeyboard = struct {
    interface: u8,
    endpoint: u8, // address, with the IN bit
    max_packet: u16,
    interval: u8, // frames (ms) at full and low speed
};

/// The first boot keyboard in a configuration `parseConfig` accepted
/// (alternate setting 0 only).
pub fn findBootKeyboard(config: []const u8) ?BootKeyboard {
    var w = Walker.init(config);
    var cur: ?Interface = null;
    while (w.next()) |item| switch (item) {
        .interface => |i| cur = i,
        .endpoint => |e| if (cur) |i| {
            if (i.alt == 0 and i.class == 3 and i.subclass == 1 and i.protocol == 1 and e.isInterruptIn())
                return .{ .interface = i.number, .endpoint = e.address, .max_packet = e.max_packet, .interval = e.interval };
        },
        else => {},
    };
    return null;
}
