//! Layer 4 harness (docs/TESTING.md): runs the Verilator model of a board
//! top (through the C ABI in vtop.cpp), decodes its VGA output like a
//! monitor (vga.zig), writes each frame as a PNG, and optionally compares
//! the frames with golden images.
//!
//!   vga-sim --out DIR [--frames N] [--expect DIR] [--expect-last FILE] [--sw X] [--btn X] [--ps-reset N]
//!
//! --out DIR      write DIR/frame_NNNN.png for decoded frames 0 .. N-1
//! --frames N     number of frames to decode (default 1)
//! --expect DIR   compare frame k with DIR/frame_NNNN.png where that file
//!                exists; fail on any difference, or if no file matched
//! --expect-last FILE
//!                compare the last decoded frame with FILE (for screens that
//!                only settle after a while, like a test ROM's result)
//! --sw X         switch levels (bit i = SWi), decimal or 0x hex
//! --btn X        buttons held (bit 0 = BTNC, 1 = BTND, 2 = BTNL,
//!                3 = BTNR, 4 = BTNU)
//! --expect-hdmi-last FILE
//!                (tops with HDMI) compare the last HDMI frame with FILE
//! --expect-i2c FILE
//!                (tops with HDMI) compare the I2C register writes with FILE
//! --expect-tone HZ
//!                (tops with HDMI audio) the left channel's frequency must
//!                be HZ within 1 %
//! --expect-quiet N
//!                (tops with HDMI audio) no tone: in the second half of the
//!                run, consecutive samples differ by at most N (a DC level
//!                settling passes, a hum or an aliased whine doesn't)
//! --ps FILE      (tops with the PS link) play FILE on PS_GPIO from clock 0,
//!                as the PS app would (ps.zig; format: Platform.Sim.Overlay)
//! --expect-ps-in X
//!                (tops with the PS link) PS_IN must read X at the end
//! --expect-ps-text TEXT
//!                (tops with the PS link) the bytes on PS_IN's byte
//!                channel (Platform.Ps.Link.psInWith: byte in bits 24 to
//!                31, a toggle in bit 5) must contain TEXT ("\n" for a
//!                newline); they also go to DIR/ps_text.txt
//! --expect-ps-hex FILE
//!                (tops with the PS link) the byte channel's bytes must be
//!                exactly those in FILE (hex, two digits each, any spacing)
//! --ddr-file ADDR=FILE
//!                (tops with the HP0 read channels, side track I) FILE in
//!                the simulated DDR from ADDR (a Game Boy cartridge at
//!                0x02000000); elsewhere the slave's pattern, as before
//! --ps-reset N  (tops with PS_RESETN, the PS's PL reset) hold it low for
//!               the first N pixel clocks, and fail on any HP0 read
//!               meanwhile (the reset race on the board: a read before
//!               the PS is ready). Without it PS_RESETN is high from the
//!               start. (side track K, as arty.zig's)
//! --axi-latency N, --axi-seed N
//!                (tops with the HP0 read channels, side track C) the AXI
//!                slave's longest first-beat latency (default 20) and seed
//!
//! Tops with the HP0 read channels (vtop_has_axi) get an AXI read slave
//! with random timing on them (axi.zig); it checks the master's side of
//! the protocol, and a violation fails the run.
//!
//! Tops with HDMI (vtop_has_hdmi) also get an HDMI sink (hdmi.zig) and,
//! on HD_SCL/HD_SDA, the simulated ADV7511's I2C slave (i2c.zig). They
//! write DIR/hdmi_NNNN.png (RGB as the chip would send it, using the CSC
//! registers the design wrote) and DIR/i2c_writes.txt ("0xRR 0xVV" per
//! write, in order). Tops with HDMI audio (vtop_has_spdif) get an S/PDIF
//! receiver on HD_SPDIF (spdif.zig) and write DIR/audio.wav; the stream
//! must decode without errors at 48 kHz.
//!
//! One tick is one pixel clock: the clock wizard's simulation model
//! (boards/sim/clk_wiz_sim.v.in) passes GCLK straight through. Frame
//! numbering is the decoder's (vga.zig): decoded frame 0 is the first one
//! after the first VSYNC pulse. Exit status 0 only if the sync timing,
//! blanking and every comparison are right.

const std = @import("std");
const png = @import("png.zig");
const vga = @import("vga.zig");
const hdmi = @import("hdmi.zig");
const i2c = @import("i2c.zig");
const ps = @import("ps.zig");
const spdif = @import("spdif.zig");
const axi = @import("axi.zig");

const vtop = @import("vtop.zig");
const vtop_new = vtop.vtop_new;
const vtop_free = vtop.vtop_free;
const vtop_set_inputs = vtop.vtop_set_inputs;
const vtop_tick = vtop.vtop_tick;
const vtop_tick_hdmi = vtop.vtop_tick_hdmi;

const Options = struct {
    out: []const u8,
    frames: u32 = 1,
    expect: ?[]const u8 = null,
    expect_last: ?[]const u8 = null,
    sw: u8 = 0,
    btn: u8 = 0,
    expect_hdmi_last: ?[]const u8 = null,
    expect_i2c: ?[]const u8 = null,
    ps: ?[]const u8 = null,
    expect_ps_in: ?u32 = null,
    expect_ps_text: ?[]const u8 = null,
    expect_tone: ?f64 = null,
    expect_quiet: ?u32 = null,
    expect_ps_hex: ?[]const u8 = null,
    axi_latency: ?u32 = null,
    axi_seed: ?u64 = null,
    ddr_file: ?[]const u8 = null,
    ddr_base: u32 = 0,
    ps_reset: u64 = 0,
};

fn usage() noreturn {
    std.debug.print("usage: vga-sim --out DIR [--frames N] [--expect DIR] [--expect-last FILE] [--sw X] [--btn X] [--expect-hdmi-last FILE] [--expect-i2c FILE] [--expect-tone HZ] [--expect-quiet N] [--ps FILE] [--expect-ps-in X] [--expect-ps-text TEXT] [--expect-ps-hex FILE] [--axi-latency N] [--axi-seed N] [--ddr-file ADDR=FILE]\n", .{});
    std.process.exit(2);
}

fn parseArgs(args: []const [:0]const u8) Options {
    var out: ?[]const u8 = null;
    var o: Options = .{ .out = "" };
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const a = args[i];
        if (i + 1 >= args.len) usage();
        const v = args[i + 1];
        i += 1;
        if (std.mem.eql(u8, a, "--out")) {
            out = v;
        } else if (std.mem.eql(u8, a, "--frames")) {
            o.frames = std.fmt.parseInt(u32, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--expect")) {
            o.expect = v;
        } else if (std.mem.eql(u8, a, "--expect-last")) {
            o.expect_last = v;
        } else if (std.mem.eql(u8, a, "--sw")) {
            o.sw = std.fmt.parseInt(u8, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--btn")) {
            o.btn = std.fmt.parseInt(u5, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--expect-hdmi-last")) {
            o.expect_hdmi_last = v;
        } else if (std.mem.eql(u8, a, "--expect-i2c")) {
            o.expect_i2c = v;
        } else if (std.mem.eql(u8, a, "--ps")) {
            o.ps = v;
        } else if (std.mem.eql(u8, a, "--expect-ps-text")) {
            o.expect_ps_text = v;
        } else if (std.mem.eql(u8, a, "--expect-ps-in")) {
            o.expect_ps_in = std.fmt.parseInt(u32, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--expect-tone")) {
            o.expect_tone = std.fmt.parseFloat(f64, v) catch usage();
        } else if (std.mem.eql(u8, a, "--expect-ps-hex")) {
            o.expect_ps_hex = v;
        } else if (std.mem.eql(u8, a, "--ps-reset")) {
            o.ps_reset = std.fmt.parseInt(u64, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--axi-latency")) {
            o.axi_latency = std.fmt.parseInt(u32, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--axi-seed")) {
            o.axi_seed = std.fmt.parseInt(u64, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--ddr-file")) {
            const eq = std.mem.indexOfScalar(u8, v, '=') orelse usage();
            o.ddr_base = std.fmt.parseInt(u32, v[0..eq], 0) catch usage();
            o.ddr_file = v[eq + 1 ..];
        } else if (std.mem.eql(u8, a, "--expect-quiet")) {
            o.expect_quiet = std.fmt.parseInt(u32, v, 0) catch usage();
        } else usage();
    }
    o.out = out orelse usage();
    return o;
}

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;
    const opts = parseArgs(try init.minimal.args.toSlice(init.arena.allocator()));
    const cwd = std.Io.Dir.cwd();
    try cwd.createDirPath(io, opts.out);

    const mode = vga.vga640x480;
    var dec = try vga.Decoder.init(gpa, mode);
    defer dec.deinit(gpa);

    const top = vtop_new();
    defer vtop_free(top);
    vtop_set_inputs(top, opts.btn, opts.sw);
    // the PS's PL reset: high from the start unless --ps-reset holds it
    const has_ps_reset = vtop.vtop_has_ps_reset() != 0;
    if (opts.ps_reset != 0 and !has_ps_reset) {
        std.debug.print("vga-sim: FAIL --ps-reset, but the top has no PS_RESETN\n", .{});
        std.process.exit(1);
    }
    vtop.vtop_set_ps_resetn(top, if (opts.ps_reset != 0) 0 else 1);
    var reads_in_reset: u64 = 0;

    const has_ps = vtop.vtop_has_ps() != 0;
    if ((opts.ps != null or opts.expect_ps_in != null or opts.expect_ps_text != null) and !has_ps) {
        std.debug.print("vga-sim: FAIL --ps/--expect-ps-in, but the top has no PS link\n", .{});
        std.process.exit(1);
    }
    var player: ?ps.Player = null;
    if (opts.ps) |path| {
        const script = try cwd.readFileAlloc(io, path, gpa, .limited(1 << 20));
        defer gpa.free(script);
        player = .{ .phases = try ps.compile(gpa, script) };
        std.debug.print("ps: {s}: {d} GPIO phase(s)\n", .{ path, player.?.phases.len });
    }
    defer if (player) |p| gpa.free(p.phases);
    // the byte channel, decoded like the relay does (pl.ByteIn)
    var ps_text: std.ArrayList(u8) = .empty;
    defer ps_text.deinit(gpa);
    var ps_toggle: bool = false;

    // the HP port (side track C)
    const has_axi = vtop.vtop_has_axi() != 0;
    if ((opts.axi_latency != null or opts.axi_seed != null or opts.ddr_file != null) and !has_axi) {
        std.debug.print("vga-sim: FAIL --axi-*, but the top has no HP0 port\n", .{});
        std.process.exit(1);
    }
    const ddr_file: []const u8 = if (opts.ddr_file) |path| blk: {
        const f = try cwd.readFileAlloc(io, path, gpa, .limited(64 << 20));
        std.debug.print("axi: {s}: {d} bytes in DDR from 0x{X:0>8}\n", .{ path, f.len, opts.ddr_base });
        break :blk f;
    } else &.{};
    defer if (opts.ddr_file != null) gpa.free(ddr_file);
    var hp0 = axi.Slave.init(.{ .latency = opts.axi_latency orelse 20, .seed = opts.axi_seed orelse 1, .file = ddr_file, .file_base = opts.ddr_base });
    var hp0_m: axi.MasterOut = undefined;

    // Generous bound: the first frame needs up to two frame times.
    const max_clocks: u64 = (@as(u64, opts.frames) + 2) * mode.h_total * mode.v_total;
    var frames: u32 = 0;
    var compared: u32 = 0;
    var mismatched: u32 = 0;
    var pins: vga.Pins = undefined;
    var clock: u64 = 0;

    const has_hdmi = vtop.vtop_has_hdmi() != 0;
    var sink = try hdmi.Sink.init(gpa, mode.width, mode.height);
    defer sink.deinit(gpa);
    var chip = i2c.Slave.init();
    var hp: vtop.Hdmi = undefined;
    var scl_in: u8 = 1;
    var sda_in: u8 = 1;
    var hdmi_frames: u32 = 0;
    const has_spdif = vtop.vtop_has_spdif() != 0;
    var rx: spdif.Receiver = .{};
    defer rx.deinit(gpa);

    var last_write_clock: u64 = 0;
    var last_hdmi_start: u64 = 0;
    // With HDMI, also run until the HDMI sink has as many frames (its
    // stream is a few clocks behind VGA).
    while ((frames < opts.frames or (has_hdmi and hdmi_frames < opts.frames)) and clock < max_clocks) : (clock += 1) {
        if (player) |*p| vtop.vtop_set_ps(top, p.tick());
        if (opts.expect_ps_text != null or opts.expect_ps_hex != null) {
            const w = vtop.vtop_ps_in(top);
            const t = w & (1 << 5) != 0;
            if (t != ps_toggle) {
                ps_toggle = t;
                try ps_text.append(gpa, @truncate(w >> 24));
            }
        }
        // the HP port: the slave's outputs go in before the clock, and it
        // steps with the master's outputs from before it (axi.zig)
        if (opts.ps_reset != 0 and clock == opts.ps_reset) vtop.vtop_set_ps_resetn(top, 1);
        if (has_axi) {
            vtop.vtop_axi_out(top, &hp0_m);
            vtop.vtop_set_axi(top, &hp0.out);
            if (clock < opts.ps_reset and hp0_m.arvalid != 0) reads_in_reset += 1;
        }
        defer if (has_axi) hp0.step(hp0_m);
        if (has_hdmi) {
            vtop_tick_hdmi(top, scl_in, sda_in, &pins, &hp);
            if (has_spdif) {
                try rx.feed(gpa, @intCast(hp.spdif_after_fall & 1));
                try rx.feed(gpa, @intCast(hp.spdif_after_rise & 1));
            }
            // Open drain: the wires are low if anyone pulls them low (the
            // slave never stretches SCL).
            const scl: u1 = if (hp.scl_low != 0) 0 else 1;
            const sda: u1 = if (hp.sda_low != 0 or chip.sda_low) 0 else 1;
            const before = chip.log_len;
            chip.step(scl, sda);
            if (chip.log_len != before) last_write_clock = clock;
            scl_in = scl;
            sda_in = if (hp.sda_low != 0 or chip.sda_low) 0 else 1;
            if (sink.feed(hp)) {
                last_hdmi_start = sink.frame_start;
                sink.toRgb(&chip.regs);
                var hname_buf: [32]u8 = undefined;
                const hname = try std.fmt.bufPrint(&hname_buf, "hdmi_{d:0>4}.png", .{hdmi_frames});
                const hbytes = try png.encode(gpa, mode.width, mode.height, sink.rgb);
                defer gpa.free(hbytes);
                const hsub = try std.fs.path.join(gpa, &.{ opts.out, hname });
                defer gpa.free(hsub);
                try cwd.writeFile(io, .{ .sub_path = hsub, .data = hbytes });
                hdmi_frames += 1;
            }
        } else vtop_tick(top, &pins);
        if (frames >= opts.frames) continue; // only waiting for HDMI
        if (!dec.feed(pins)) continue;

        var name_buf: [32]u8 = undefined;
        const name = try std.fmt.bufPrint(&name_buf, "frame_{d:0>4}.png", .{frames});
        const bytes = try png.encode(gpa, mode.width, mode.height, dec.pixels);
        defer gpa.free(bytes);
        const sub = try std.fs.path.join(gpa, &.{ opts.out, name });
        defer gpa.free(sub);
        try cwd.writeFile(io, .{ .sub_path = sub, .data = bytes });
        std.debug.print("vga-sim: wrote {s} (clock {d})\n", .{ sub, clock });

        if (opts.expect) |dir| {
            const gpath = try std.fs.path.join(gpa, &.{ dir, name });
            defer gpa.free(gpath);
            if (cwd.readFileAlloc(io, gpath, gpa, .limited(64 << 20))) |gbytes| {
                defer gpa.free(gbytes);
                compared += 1;
                if (!try sameAs(gpa, gbytes, mode, dec.pixels, gpath)) mismatched += 1;
            } else |err| switch (err) {
                error.FileNotFound => {},
                else => return err,
            }
        }
        frames += 1;
    }

    var ok = dec.report();
    if (has_hdmi) {
        ok = sink.report() and ok;
        std.debug.print("hdmi: {d} frame(s) written as hdmi_NNNN.png\n", .{hdmi_frames});
        // The I2C log.
        var text: std.ArrayList(u8) = .empty;
        defer text.deinit(gpa);
        for (chip.writes()) |w| {
            var line_buf: [16]u8 = undefined;
            try text.appendSlice(gpa, try std.fmt.bufPrint(&line_buf, "0x{X:0>2} 0x{X:0>2}\n", .{ w.reg, w.value }));
        }
        const lpath = try std.fs.path.join(gpa, &.{ opts.out, "i2c_writes.txt" });
        defer gpa.free(lpath);
        try cwd.writeFile(io, .{ .sub_path = lpath, .data = text.items });
        std.debug.print("i2c: {d} START(s), {d} register write(s) in {s}, the last at clock {d} (frame time {d}), {d} framing error(s)\n", .{ chip.starts, chip.writes().len, lpath, last_write_clock, last_write_clock / (mode.h_total * mode.v_total), chip.framing_errors });
        if (chip.framing_errors != 0) ok = false;
        if (opts.expect_i2c) |gpath| {
            const want = try cwd.readFileAlloc(io, gpath, gpa, .limited(1 << 20));
            defer gpa.free(want);
            if (std.mem.eql(u8, want, text.items)) {
                std.debug.print("i2c: {s}: identical\n", .{gpath});
            } else {
                std.debug.print("i2c: FAIL writes differ from {s}\n", .{gpath});
                ok = false;
            }
        }
        if (opts.expect_hdmi_last) |gpath| {
            if (hdmi_frames == 0) {
                ok = false;
            } else if (chip.log_len > 0 and last_write_clock >= last_hdmi_start) {
                // The sink converts a frame with the registers at its end;
                // a write during the frame would make that a guess.
                std.debug.print("hdmi: FAIL the compared frame started at clock {d}, before the last I2C write; run more frames\n", .{last_hdmi_start});
                ok = false;
            } else {
                const gbytes = try cwd.readFileAlloc(io, gpath, gpa, .limited(64 << 20));
                defer gpa.free(gbytes);
                if (!try sameAs(gpa, gbytes, mode, sink.rgb, gpath)) ok = false;
            }
        }
    }
    if (has_spdif) {
        ok = try audioReport(gpa, io, cwd, opts, &rx, clock) and ok;
    } else if (opts.expect_tone != null or opts.expect_quiet != null) {
        std.debug.print("vga-sim: FAIL --expect-tone, but the top has no HD_SPDIF\n", .{});
        ok = false;
    }
    if (has_hdmi) {} else if (opts.expect_hdmi_last != null or opts.expect_i2c != null) {
        std.debug.print("vga-sim: FAIL --expect-hdmi-last/--expect-i2c, but the top has no HDMI\n", .{});
        ok = false;
    }
    if (opts.ps_reset != 0) {
        if (reads_in_reset != 0) {
            std.debug.print("vga-sim: FAIL HP0 ARVALID high in {d} pixel clock(s) while PS_RESETN was held\n", .{reads_in_reset});
            ok = false;
        } else std.debug.print("vga-sim: PS_RESETN held for {d} pixel clocks: no HP0 read meanwhile\n", .{opts.ps_reset});
    }
    if (has_axi) {
        std.debug.print("axi: {d} burst(s), {d} beat(s), {d} violation(s){s}{s}\n", .{ hp0.bursts, hp0.beats, hp0.violations, if (hp0.first_violation != null) ", first: " else "", hp0.first_violation orelse "" });
        if (hp0.violations != 0) ok = false;
    }
    if (opts.expect_ps_hex) |hpath| {
        const htext = try cwd.readFileAlloc(io, hpath, gpa, .limited(1 << 20));
        defer gpa.free(htext);
        ok = try ps.hexReport(gpa, hpath, htext, ps_text.items) and ok;
    }
    if (player) |p| if (!p.done()) {
        std.debug.print("ps: FAIL the script hadn't finished after {d} clocks\n", .{clock});
        ok = false;
    };
    if (opts.expect_ps_text) |want_raw| {
        var want: std.ArrayList(u8) = .empty;
        defer want.deinit(gpa);
        var k: usize = 0;
        while (k < want_raw.len) : (k += 1) {
            if (want_raw[k] == '\\' and k + 1 < want_raw.len and want_raw[k + 1] == 'n') {
                try want.append(gpa, '\n');
                k += 1;
            } else try want.append(gpa, want_raw[k]);
        }
        const tpath = try std.fs.path.join(gpa, &.{ opts.out, "ps_text.txt" });
        defer gpa.free(tpath);
        try cwd.writeFile(io, .{ .sub_path = tpath, .data = ps_text.items });
        if (std.mem.indexOf(u8, ps_text.items, want.items) != null) {
            std.debug.print("ps: byte channel: {d} byte(s), contains the expected text\n", .{ps_text.items.len});
        } else {
            std.debug.print("ps: FAIL byte channel: {d} byte(s) (in {s}), expected text not among them\n", .{ ps_text.items.len, tpath });
            ok = false;
        }
    }
    if (opts.expect_ps_in) |want| {
        const got = vtop.vtop_ps_in(top);
        if (got == want) {
            std.debug.print("ps: PS_IN = 0x{X:0>8}\n", .{got});
        } else {
            std.debug.print("ps: FAIL PS_IN = 0x{X:0>8}, expected 0x{X:0>8}\n", .{ got, want });
            ok = false;
        }
    }
    if (frames < opts.frames) {
        std.debug.print("vga-sim: FAIL only {d} of {d} frame(s) decoded in {d} clocks\n", .{ frames, opts.frames, clock });
        ok = false;
    }
    if (opts.expect) |dir| {
        std.debug.print("vga-sim: {d} frame(s) compared with {s}, {d} differ\n", .{ compared, dir, mismatched });
        if (compared == 0 or mismatched != 0) ok = false;
    }
    // The decoder still holds the last complete frame: the loop stops
    // right after it.
    if (opts.expect_last) |gpath| {
        if (frames == 0) {
            ok = false;
        } else {
            const gbytes = try cwd.readFileAlloc(io, gpath, gpa, .limited(64 << 20));
            defer gpa.free(gbytes);
            if (!try sameAs(gpa, gbytes, mode, dec.pixels, gpath)) ok = false;
        }
    }
    if (!ok) {
        std.debug.print("vga-sim: FAIL\n", .{});
        std.process.exit(1);
    }
    std.debug.print("vga-sim: OK\n", .{});
}

/// Writes DIR/audio.wav and checks the S/PDIF stream: no decoding errors,
/// 48 kHz (frames against simulated time), and the tone if asked for.
fn audioReport(gpa: std.mem.Allocator, io: std.Io, cwd: std.Io.Dir, opts: Options, rx: *const spdif.Receiver, clocks: u64) !bool {
    var ok = true;
    const bytes = try rx.wav(gpa);
    defer gpa.free(bytes);
    const wpath = try std.fs.path.join(gpa, &.{ opts.out, "audio.wav" });
    defer gpa.free(wpath);
    try cwd.writeFile(io, .{ .sub_path = wpath, .data = bytes });
    const n = rx.frames.items.len;
    // counted from PS_RESETN's release with --ps-reset (the design,
    // S/PDIF included, is held until then; side I's finding)
    const running = clocks - @min(clocks, opts.ps_reset);
    const want: f64 = @as(f64, @floatFromInt(running)) * 39720e-12 * 48000.0;
    var peak: u32 = 0;
    for (rx.frames.items) |f| peak = @max(peak, @max(@abs(@as(i32, f[0])), @abs(@as(i32, f[1]))));
    std.debug.print("spdif: {d} frame(s) ({d:.1} expected at 48 kHz), {d} block(s), peak {d}, {d} error(s){s}{s}; {s}\n", .{ n, want, rx.blocks, peak, rx.errors, if (rx.first_error != null) ", first: " else "", rx.first_error orelse "", wpath });
    if (rx.errors != 0) ok = false;
    if (@abs(@as(f64, @floatFromInt(n)) - want) > 3) {
        std.debug.print("spdif: FAIL frame count is not 48 kHz\n", .{});
        ok = false;
    }
    if (opts.expect_tone) |hz| {
        const f = rx.frequency(480);
        if (@abs(f - hz) > hz * 0.01) {
            std.debug.print("spdif: FAIL tone {d:.1} Hz, expected {d:.1} Hz\n", .{ f, hz });
            ok = false;
        } else std.debug.print("spdif: tone {d:.1} Hz (expected {d:.1} Hz)\n", .{ f, hz });
    }
    if (opts.expect_quiet) |limit| {
        var step: u32 = 0;
        const half = rx.frames.items[rx.frames.items.len / 2 ..];
        for (half[1..], half[0 .. half.len - 1]) |a, b| {
            step = @max(step, @abs(@as(i32, a[0]) - @as(i32, b[0])));
        }
        if (step > limit) {
            std.debug.print("spdif: FAIL not quiet: consecutive samples differ by up to {d}, limit {d}\n", .{ step, limit });
            ok = false;
        } else std.debug.print("spdif: quiet (consecutive samples differ by up to {d}, limit {d})\n", .{ step, limit });
    }
    return ok;
}

fn sameAs(gpa: std.mem.Allocator, golden_png: []const u8, mode: vga.Mode, rgb: []const u8, gpath: []const u8) !bool {
    var g = try png.decode(gpa, golden_png);
    defer g.deinit(gpa);
    if (g.width != mode.width or g.height != mode.height) {
        std.debug.print("vga-sim: {s}: size {d}x{d}, frame is {d}x{d}\n", .{ gpath, g.width, g.height, mode.width, mode.height });
        return false;
    }
    var diff: u64 = 0;
    var first: usize = 0;
    var i: usize = 0;
    while (i < rgb.len) : (i += 3) {
        if (!std.mem.eql(u8, rgb[i..][0..3], g.rgb[i..][0..3])) {
            if (diff == 0) first = i / 3;
            diff += 1;
        }
    }
    if (diff == 0) {
        std.debug.print("vga-sim: {s}: identical\n", .{gpath});
        return true;
    }
    const x = first % mode.width;
    const y = first / mode.width;
    std.debug.print("vga-sim: {s}: {d} pixel(s) differ, first at ({d}, {d}): got {any}, golden {any}\n", .{ gpath, diff, x, y, rgb[first * 3 ..][0..3], g.rgb[first * 3 ..][0..3] });
    return false;
}

test {
    _ = png;
    _ = vga;
    _ = hdmi;
    _ = i2c;
}
