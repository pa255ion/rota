//! Layer 4 harness for Arty Z7-20 tops (side track G; docs/TESTING.md):
//! runs the Verilator model (through vtop_arty.cpp), receives its HDMI
//! output like a DVI monitor (tmds.zig, then vga.zig's decoder with 8-bit
//! colour), writes each frame as a PNG and compares frames with golden
//! images. The ZedBoard's harness is main.zig; this one shares its frame
//! decoding and PNG code.
//!
//!   arty-sim --out DIR [--frames N] [--expect DIR] [--expect-last FILE] [--sw X] [--btn X]
//!            [--ps FILE] [--expect-ps-in X] [--ddr-file ADDR=FILE]
//!            [--axi-latency N] [--axi-seed N] [--expect-ps-text TEXT]
//!            [--expect-ps-hex FILE] [--ps-bytes-out FILE]
//!            [--movie-record FILE] [--movie-play FILE]
//!
//! --out DIR      write DIR/frame_NNNN.png for decoded frames 0 .. N-1
//! --frames N     number of frames to decode (default 1)
//! --expect DIR   compare frame k with DIR/frame_NNNN.png where that file
//!                exists; fail on any difference, or if no file matched
//! --expect-last FILE
//!                compare the last decoded frame with FILE
//! --sw X         switch levels (bit i = SWi), decimal or 0x hex
//! --btn X        buttons held (bit i = BTNi)
//! --ps FILE      (tops with the PS link, phase 2) play FILE on PS_GPIO from
//!                pixel clock 0, as main.zig does (ps.zig; one step per
//!                pixel clock, i.e. every fifth tick here)
//! --expect-ps-in X
//!                (tops with the PS link) PS_IN must read X at the end
//! --expect-ps-text TEXT
//!                (tops with the PS link) the bytes on PS_IN's byte channel
//!                (byte in bits 24 to 31, a toggle in bit 5) must contain
//!                TEXT, as in main.zig
//! --expect-ps-hex FILE
//!                (tops with the PS link) the byte channel's bytes must be
//!                exactly those in FILE (hex, two digits each, any
//!                spacing), as in main.zig: a battery save read back
//! --expect-tone HZ
//!                (tops with aud_pwm, phase 2 step 6) the PWM line, averaged
//!                over 524 pixel clocks (about 48 kHz), must carry a tone of
//!                HZ within 1 % (zero crossings with hysteresis over the
//!                second half of the run; time from DomPix's nominal period,
//!                as main.zig's S/PDIF check)
//! --expect-hdmi-tone HZ
//!                (HDMI mode, phase 3) the audio samples the HDMI stream's
//!                data islands carry (tmds.zig) must hold a tone of HZ
//!                within 1 %, left channel; the stream must have carried
//!                ACR, the AVI and Audio InfoFrames, all with good ECC
//! --expect-quiet N
//!                (HDMI mode) no tone in the HDMI stream's audio samples, as
//!                main.zig's on S/PDIF: in the second half of the run,
//!                consecutive left samples differ by at most N; the stream
//!                must carry audio (M7, additively, side K's agreement)
//! --expect-led MASK=VALUE
//!                the LEDs at the end (led[3:0] in bits 0 to 3, led4_g in
//!                bit 4), masked with MASK, must equal VALUE: e.g. the
//!                Super Nintendo's LD1, rows shown late (M7, additively)
//! --ps-reset N
//!                (tops beside the PS) hold PS_RESETN (the PS's PL reset)
//!                low for the first N pixel clocks; HP0's ARVALID must stay
//!                low meanwhile (the Game Boy's first read once raced the
//!                level shifters on the board, side track G phase 3).
//!                Without it PS_RESETN is high from the start
//! --golden-4bit 1
//!                compare frames with the goldens on the top 4 bits of each
//!                channel, widened as the VGA DAC's (c x 17): for systems
//!                with 8-bit colour (the NES) whose goldens are the VGA
//!                ones. The TMDS stream itself is still checked exactly
//!                (every symbol re-encoded, tmds.zig); only the low bits of
//!                the colours go unchecked.
//! --ddr-file ADDR=FILE, --axi-latency N, --axi-seed N
//!                (tops with the HP0 read channels: the Game Boy) the AXI
//!                read slave of axi.zig on HP0, stepped once per pixel clock
//!                (HP0's clock), with FILE in its DDR from ADDR, as in
//!                main.zig; a protocol violation fails the run
//! --expect-ps-hex-differ FILE
//!                (tops with the PS link) the byte channel's bytes must NOT
//!                be those in FILE: a control run that shows a check can fail
//!                (side track O's determinism gate)
//! --expect-frame-toggles N
//!                (tops with frame-synchronous input, side track O) PS_IN
//!                bit 7 must flip exactly N times over the run: N frames
//!                (O.3's frame advance: N steps from a pause)
//! --expect-capture W,H,X0,Y0,SCALE
//!                (tops with HP0's write channels, side track O, O.4) the
//!                screenshot in DDR (Platform.Video.Capture, from 0x10000000:
//!                W x H pixels, R G B 0 each, then its trailer) must equal
//!                the last frame on screen, pixel (x, y) shown at
//!                (X0 + SCALE x, Y0 + SCALE y); its trailer complete (a
//!                sequence number, no pixel lost, W x H written). The
//!                script asks for it (`bytes 0xF01004 01`) on a still picture
//! --ps-bytes-out FILE
//!                (tops with the PS link) write the byte channel's bytes to
//!                FILE as --expect-ps-hex reads them (a golden's first run)
//! --movie-record FILE, --movie-play FILE
//!                (tops with frame-synchronous input, side track O) act as
//!                the PS app does with a movie (movie.zig): the input word
//!                (PS_GPIO bits 0 to 15) changes only at a flip of PS_IN
//!                bit 7. Recording, the script's input is taken at each
//!                flip and the word the core latched there goes to FILE, a
//!                movie (power-on start); playing, FILE's words replace the
//!                script's input, one per flip. The script sets the movie
//!                register's sync bit itself (`bytes 0xF01003 01`)
//!
//! One tick is one 5x clock (boards/sim/arty_mmcm.v), two samples of the
//! TMDS lines per tick, ten per pixel. TMDS is lossless, so frames must
//! match the goldens exactly. Exit status 0 only if the TMDS stream, the
//! sync timing, blanking and every comparison are right.

const std = @import("std");
const png = @import("png.zig");
const vga = @import("vga.zig");
const tmds = @import("tmds.zig");
const ps = @import("ps.zig");
const axi = @import("axi.zig");
const tone = @import("arty_tone.zig");
const movie = @import("movie");
const pwm_block = tone.pwm_block;
const toneHz = tone.toneHz;

const Vtop = opaque {};
/// struct arty_out in vtop_arty.cpp.
const Out = extern struct {
    p_after_fall: u8,
    n_after_fall: u8,
    p_after_rise: u8,
    n_after_rise: u8,
    led: u8,
    led4_g: u8,
    aud_pwm: u8,
};
extern fn arty_new() *Vtop;
extern fn arty_free(v: *Vtop) void;
extern fn arty_has_tmds() c_int;
extern fn arty_has_audio() c_int;
extern fn arty_set_inputs(v: *Vtop, btn: u8, sw: u8) void;
extern fn arty_tick(v: *Vtop, out: *Out) void;
extern fn arty_has_ps() c_int;
extern fn arty_set_ps(v: *Vtop, gpio: u32) void;
extern fn arty_ps_in(v: *Vtop) u32;
extern fn arty_has_axi() c_int;
extern fn arty_set_ps_resetn(v: *Vtop, level: u8) void;
extern fn arty_has_ps_reset() c_int;
extern fn arty_set_axi(v: *Vtop, s: *const axi.SlaveOut) void;
extern fn arty_axi_out(v: *Vtop, m: *axi.MasterOut) void;
extern fn arty_has_axi_w() c_int;
extern fn arty_set_axi_w(v: *Vtop, s: *const axi.WrSlaveOut) void;
extern fn arty_axi_w_out(v: *Vtop, m: *axi.WrMasterOut) void;

const Options = struct {
    out: []const u8,
    frames: u32 = 1,
    expect: ?[]const u8 = null,
    expect_last: ?[]const u8 = null,
    sw: u8 = 0,
    btn: u8 = 0,
    ps: ?[]const u8 = null,
    expect_ps_in: ?u32 = null,
    expect_ps_text: ?[]const u8 = null,
    expect_ps_hex: ?[]const u8 = null,
    axi_latency: ?u32 = null,
    axi_seed: ?u64 = null,
    ddr_file: ?[]const u8 = null,
    ddr_base: u32 = 0,
    golden_4bit: bool = false,
    expect_tone: ?f64 = null,
    expect_hdmi_tone: ?f64 = null,
    ps_reset: u64 = 0,
    expect_quiet: ?u32 = null,
    expect_led_mask: ?u8 = null,
    expect_led_value: u8 = 0,
    ps_bytes_out: ?[]const u8 = null,
    expect_frame_toggles: ?u32 = null,
    expect_capture: ?[5]u32 = null,
    expect_ps_hex_differ: ?[]const u8 = null,
    movie_record: ?[]const u8 = null,
    movie_play: ?[]const u8 = null,
};

fn usage() noreturn {
    std.debug.print("usage: arty-sim --out DIR [--frames N] [--expect DIR] [--expect-last FILE] [--sw X] [--btn X] [--ps FILE] [--expect-ps-in X] [--expect-ps-text TEXT] [--expect-ps-hex FILE] [--ddr-file ADDR=FILE] [--axi-latency N] [--axi-seed N] [--ps-bytes-out FILE] [--movie-record FILE] [--movie-play FILE] [--expect-quiet N] [--expect-led MASK=VALUE]\n", .{});
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
            o.sw = std.fmt.parseInt(u2, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--btn")) {
            o.btn = std.fmt.parseInt(u4, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--ps")) {
            o.ps = v;
        } else if (std.mem.eql(u8, a, "--expect-ps-in")) {
            o.expect_ps_in = std.fmt.parseInt(u32, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--ps-reset")) {
            o.ps_reset = std.fmt.parseInt(u64, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--expect-hdmi-tone")) {
            o.expect_hdmi_tone = std.fmt.parseFloat(f64, v) catch usage();
        } else if (std.mem.eql(u8, a, "--expect-tone")) {
            o.expect_tone = std.fmt.parseFloat(f64, v) catch usage();
        } else if (std.mem.eql(u8, a, "--golden-4bit")) {
            o.golden_4bit = !std.mem.eql(u8, v, "0");
        } else if (std.mem.eql(u8, a, "--expect-ps-text")) {
            o.expect_ps_text = v;
        } else if (std.mem.eql(u8, a, "--expect-ps-hex")) {
            o.expect_ps_hex = v;
        } else if (std.mem.eql(u8, a, "--axi-latency")) {
            o.axi_latency = std.fmt.parseInt(u32, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--axi-seed")) {
            o.axi_seed = std.fmt.parseInt(u64, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--expect-ps-hex-differ")) {
            o.expect_ps_hex_differ = v;
        } else if (std.mem.eql(u8, a, "--expect-capture")) {
            var it = std.mem.splitScalar(u8, v, ',');
            var c: [5]u32 = undefined;
            for (&c) |*x| x.* = std.fmt.parseInt(u32, it.next() orelse usage(), 0) catch usage();
            o.expect_capture = c;
        } else if (std.mem.eql(u8, a, "--expect-frame-toggles")) {
            o.expect_frame_toggles = std.fmt.parseInt(u32, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--ps-bytes-out")) {
            o.ps_bytes_out = v;
        } else if (std.mem.eql(u8, a, "--movie-record")) {
            o.movie_record = v;
        } else if (std.mem.eql(u8, a, "--movie-play")) {
            o.movie_play = v;
        } else if (std.mem.eql(u8, a, "--ddr-file")) {
            const eq = std.mem.indexOfScalar(u8, v, '=') orelse usage();
            o.ddr_base = std.fmt.parseInt(u32, v[0..eq], 0) catch usage();
            o.ddr_file = v[eq + 1 ..];
        } else if (std.mem.eql(u8, a, "--expect-quiet")) {
            o.expect_quiet = std.fmt.parseInt(u32, v, 0) catch usage();
        } else if (std.mem.eql(u8, a, "--expect-led")) {
            const eq = std.mem.indexOfScalar(u8, v, '=') orelse usage();
            o.expect_led_mask = std.fmt.parseInt(u8, v[0..eq], 0) catch usage();
            o.expect_led_value = std.fmt.parseInt(u8, v[eq + 1 ..], 0) catch usage();
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

    if (arty_has_tmds() == 0) {
        std.debug.print("arty-sim: FAIL the top has no HDMI TX ports (hdmi_tx_clk_p)\n", .{});
        std.process.exit(1);
    }

    const mode = vga.vga640x480;
    var dec = try vga.Decoder.init(gpa, mode);
    defer dec.deinit(gpa);
    dec.wide = true;
    var sink: tmds.Sink = .{};
    sink.hdmi.gpa = gpa;
    defer sink.hdmi.deinit();

    const top = arty_new();
    defer arty_free(top);
    arty_set_inputs(top, opts.btn, opts.sw);
    if (opts.ps_reset != 0 and arty_has_ps_reset() == 0) {
        std.debug.print("arty-sim: FAIL --ps-reset, but the top has no PS_RESETN\n", .{});
        std.process.exit(1);
    }
    arty_set_ps_resetn(top, if (opts.ps_reset != 0) 0 else 1);
    var reads_in_reset: u64 = 0;

    const has_ps = arty_has_ps() != 0;
    const movie_on = opts.movie_record != null or opts.movie_play != null;
    if ((opts.ps != null or opts.expect_ps_in != null or opts.expect_ps_text != null or opts.expect_ps_hex != null or opts.ps_bytes_out != null or opts.expect_ps_hex_differ != null or opts.expect_frame_toggles != null or movie_on) and !has_ps) {
        std.debug.print("arty-sim: FAIL --ps/--expect-ps-*, but the top has no PS link\n", .{});
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
    var ps_text: std.ArrayList(u8) = .empty;
    defer ps_text.deinit(gpa);
    var ps_toggle = false;
    const bytes_wanted = opts.expect_ps_text != null or opts.expect_ps_hex != null or opts.ps_bytes_out != null or opts.expect_ps_hex_differ != null;

    // input movies (side track O): the PS app's frame rule (movie.Sync)
    if (opts.movie_record != null and opts.movie_play != null) usage();
    var sync: ?movie.Sync = null;
    var frame_toggle = false;
    var flips: u32 = 0;
    // --expect-frame-toggles: PS_IN bit 7's flips, counted on their own
    var count_toggle = false;
    var counted: u32 = 0;
    var recorded: std.ArrayList(u16) = .empty;
    defer recorded.deinit(gpa);
    var play_words: []const u16 = &.{};
    defer if (opts.movie_play != null) gpa.free(play_words);
    if (opts.movie_play) |path| {
        const bytes = try cwd.readFileAlloc(io, path, gpa, .limited(64 << 20));
        defer gpa.free(bytes);
        const d = movie.decodeFile(gpa, bytes) catch |err| {
            std.debug.print("movie: FAIL {s}: {s}\n", .{ path, @errorName(err) });
            std.process.exit(1);
        };
        play_words = d.words;
        std.debug.print("movie: {s}: {d} frame(s)\n", .{ path, play_words.len });
    }

    // HP0 (the Game Boy's cartridge in DDR), as in main.zig
    const has_axi = arty_has_axi() != 0;
    if ((opts.axi_latency != null or opts.axi_seed != null or opts.ddr_file != null) and !has_axi) {
        std.debug.print("arty-sim: FAIL --axi-*/--ddr-file, but the top has no HP0 port\n", .{});
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
    // HP0's write channels (side track O, O.4: screenshots)
    const has_axi_w = arty_has_axi_w() != 0;
    if (opts.expect_capture != null and !has_axi_w) {
        std.debug.print("arty-sim: FAIL --expect-capture, but the top has no HP0 write channels\n", .{});
        std.process.exit(1);
    }
    var hp0w = axi.WrSlave.init(gpa, .{ .latency = opts.axi_latency orelse 20, .seed = (opts.axi_seed orelse 1) +% 1 });
    defer hp0w.deinit();
    var hp0w_m: axi.WrMasterOut = .{};

    // the PWM sound line, averaged per 524 pixel clocks
    const has_audio = arty_has_audio() != 0;
    if (opts.expect_tone != null and !has_audio) {
        std.debug.print("arty-sim: FAIL --expect-tone, but the top has no aud_pwm\n", .{});
        std.process.exit(1);
    }
    var pcm: std.ArrayList(u16) = .empty;
    defer pcm.deinit(gpa);
    var pwm_ones: u16 = 0;
    var pwm_n: u16 = 0;

    // Generous bound: the first frame needs up to two frame times; five
    // ticks per pixel.
    const max_ticks: u64 = (@as(u64, opts.frames) + 2) * mode.h_total * mode.v_total * 5;
    var frames: u32 = 0;
    var compared: u32 = 0;
    var mismatched: u32 = 0;
    var out: Out = undefined;
    var tick: u64 = 0;
    while (frames < opts.frames and tick < max_ticks) : (tick += 1) {
        // the PS script steps once per pixel clock (five ticks)
        if (tick % 5 == 0) {
            var gpio: u32 = if (player) |*p| p.tick() else 0;
            if (movie_on) {
                const live: u16 = @truncate(gpio);
                if (sync == null) sync = .{ .applied = if (opts.movie_play != null) (if (play_words.len > 0) play_words[0] else 0) else live };
                const s = &sync.?;
                const t = arty_ps_in(top) & (1 << 7) != 0;
                if (t != frame_toggle) {
                    frame_toggle = t;
                    flips += 1;
                    if (opts.movie_record != null) {
                        try recorded.append(gpa, s.record(live));
                    } else s.play(if (flips < play_words.len) play_words[flips] else 0);
                }
                gpio = gpio & 0xFFFF_0000 | s.applied;
            }
            if (player != null or movie_on) arty_set_ps(top, gpio);
        }
        if (opts.expect_frame_toggles != null and tick % 5 == 0) {
            const t = arty_ps_in(top) & (1 << 7) != 0;
            if (t != count_toggle) {
                count_toggle = t;
                counted += 1;
            }
        }
        if (bytes_wanted and tick % 5 == 0) {
            const w = arty_ps_in(top);
            const t = w & (1 << 5) != 0;
            if (t != ps_toggle) {
                ps_toggle = t;
                try ps_text.append(gpa, @truncate(w >> 24));
            }
        }
        // HP0 is clocked by the pixel clock, which rises in ticks 4, 9,
        // 14, ... (boards/sim/arty_mmcm.v): the slave's outputs go in
        // before such a tick, and it steps after it with the master's
        // outputs from before (axi.zig, as main.zig per clock)
        const pix_edge = tick % 5 == 4;
        if (opts.ps_reset != 0 and tick == opts.ps_reset * 5) arty_set_ps_resetn(top, 1);
        if (has_axi and pix_edge) {
            arty_axi_out(top, &hp0_m);
            arty_set_axi(top, &hp0.out);
        }
        arty_tick(top, &out);
        if (has_axi_w and pix_edge) {
            arty_axi_w_out(top, &hp0w_m);
            arty_set_axi_w(top, &hp0w.out);
        }
        if (has_axi and pix_edge) hp0.step(hp0_m);
        if (has_axi_w and pix_edge) try hp0w.step(hp0w_m);
        if (has_axi and pix_edge and tick < opts.ps_reset * 5 and hp0_m.arvalid != 0) reads_in_reset += 1;
        if (has_audio and pix_edge) {
            pwm_ones += out.aud_pwm & 1;
            pwm_n += 1;
            if (pwm_n == pwm_block) {
                try pcm.append(gpa, pwm_ones);
                pwm_ones = 0;
                pwm_n = 0;
            }
        }
        const samples = [2][2]u8{ .{ out.p_after_fall, out.n_after_fall }, .{ out.p_after_rise, out.n_after_rise } };
        for (samples) |s| {
            const pins = sink.feed(@truncate(s[0]), @truncate(s[1])) orelse continue;
            if (frames >= opts.frames or !dec.feed(pins)) continue;

            var name_buf: [32]u8 = undefined;
            const name = try std.fmt.bufPrint(&name_buf, "frame_{d:0>4}.png", .{frames});
            const bytes = try png.encode(gpa, mode.width, mode.height, dec.pixels);
            defer gpa.free(bytes);
            const sub = try std.fs.path.join(gpa, &.{ opts.out, name });
            defer gpa.free(sub);
            try cwd.writeFile(io, .{ .sub_path = sub, .data = bytes });
            std.debug.print("arty-sim: wrote {s} (tick {d})\n", .{ sub, tick });

            if (opts.expect) |dir| {
                const gpath = try std.fs.path.join(gpa, &.{ dir, name });
                defer gpa.free(gpath);
                if (cwd.readFileAlloc(io, gpath, gpa, .limited(64 << 20))) |gbytes| {
                    defer gpa.free(gbytes);
                    compared += 1;
                    if (!try sameAs(gpa, gbytes, mode, dec.pixels, gpath, opts.golden_4bit)) mismatched += 1;
                } else |err| switch (err) {
                    error.FileNotFound => {},
                    else => return err,
                }
            }
            frames += 1;
        }
    }

    var ok = sink.report();
    if (sink.hdmi.islands > 0 or opts.expect_hdmi_tone != null) ok = sink.hdmi.report() and ok;
    if (opts.expect_hdmi_tone) |hz| {
        const h = &sink.hdmi;
        if (h.packets[1] == 0 or h.packets[0x82] == 0 or h.packets[0x84] == 0) {
            std.debug.print("arty: FAIL HDMI: no ACR, AVI or Audio InfoFrame packets\n", .{});
            ok = false;
        }
        // the samples' spacing in pixel clocks: 128 x CTS / N
        const n = h.acr_n orelse 6144;
        const cts = h.acr_cts orelse 25174;
        const spacing = 128.0 * @as(f64, @floatFromInt(cts)) / @as(f64, @floatFromInt(n));
        var left = try gpa.alloc(u16, h.audio.items.len);
        defer gpa.free(left);
        for (h.audio.items, 0..) |smp, i| left[i] = @bitCast(smp[0] +% @as(i16, -32768));
        const f = tone.toneHzAt(left, spacing * 39720e-12);
        if (@abs(f - hz) > hz * 0.01) {
            std.debug.print("arty: FAIL tone {d:.1} Hz in the HDMI audio, expected {d:.1} Hz\n", .{ f, hz });
            ok = false;
        } else std.debug.print("arty: tone {d:.1} Hz in the HDMI audio (expected {d:.1} Hz)\n", .{ f, hz });
    }
    ok = dec.report() and ok;
    std.debug.print("arty: LEDs at the end: led = 0x{X}, led4_g = {d}\n", .{ out.led, out.led4_g });
    if (opts.expect_led_mask) |mask| {
        const leds: u8 = (out.led & 0xF) | (@as(u8, out.led4_g & 1) << 4);
        if (leds & mask != opts.expect_led_value) {
            std.debug.print("arty: FAIL LEDs 0x{X} under mask 0x{X}, expected 0x{X}\n", .{ leds & mask, mask, opts.expect_led_value });
            ok = false;
        } else std.debug.print("arty: LEDs 0x{X} under mask 0x{X} as expected\n", .{ leds & mask, mask });
    }
    if (opts.expect_quiet) |limit| {
        const items = sink.hdmi.audio.items;
        if (items.len < 4) {
            std.debug.print("arty: FAIL --expect-quiet, but the HDMI stream carried no audio\n", .{});
            ok = false;
        } else {
            var step: u32 = 0;
            const half = items[items.len / 2 ..];
            for (half[1..], half[0 .. half.len - 1]) |a, b| {
                step = @max(step, @abs(@as(i32, a[0]) - @as(i32, b[0])));
            }
            if (step > limit) {
                std.debug.print("arty: FAIL not quiet in the HDMI audio: consecutive samples differ by up to {d}, limit {d}\n", .{ step, limit });
                ok = false;
            } else std.debug.print("arty: quiet in the HDMI audio (consecutive samples differ by up to {d}, limit {d})\n", .{ step, limit });
        }
    }
    if (opts.ps_reset != 0) {
        if (reads_in_reset != 0) {
            std.debug.print("arty: FAIL HP0 ARVALID high in {d} pixel clock(s) while PS_RESETN was held\n", .{reads_in_reset});
            ok = false;
        } else std.debug.print("arty: PS_RESETN held for {d} pixel clocks: no HP0 read meanwhile\n", .{opts.ps_reset});
    }
    if (has_axi_w and hp0w.bursts > 0) {
        std.debug.print("axi: writes: {d} burst(s), {d} beat(s), {d} violation(s){s}{s}\n", .{ hp0w.bursts, hp0w.beats, hp0w.violations, if (hp0w.first_violation != null) ", first: " else "", hp0w.first_violation orelse "" });
        if (hp0w.violations != 0) ok = false;
    }
    if (opts.expect_capture) |c| ok = captureReport(&hp0w, c, mode, dec.pixels) and ok;
    if (has_axi) {
        std.debug.print("axi: {d} burst(s), {d} beat(s), {d} violation(s){s}{s}\n", .{ hp0.bursts, hp0.beats, hp0.violations, if (hp0.first_violation != null) ", first: " else "", hp0.first_violation orelse "" });
        if (hp0.violations != 0) ok = false;
    }
    if (opts.expect_tone) |hz| {
        const f = toneHz(pcm.items);
        if (@abs(f - hz) > hz * 0.01) {
            std.debug.print("arty: FAIL tone {d:.1} Hz on aud_pwm, expected {d:.1} Hz ({d} blocks)\n", .{ f, hz, pcm.items.len });
            ok = false;
        } else std.debug.print("arty: tone {d:.1} Hz on aud_pwm (expected {d:.1} Hz)\n", .{ f, hz });
    }
    if (opts.expect_ps_text) |want| {
        if (std.mem.indexOf(u8, ps_text.items, want) != null) {
            std.debug.print("ps: byte channel: {d} byte(s), contains the expected text\n", .{ps_text.items.len});
        } else {
            std.debug.print("ps: FAIL byte channel: {d} byte(s), expected text not among them: {s}\n", .{ ps_text.items.len, ps_text.items });
            ok = false;
        }
    }
    if (opts.expect_ps_hex) |hpath| {
        const htext = try cwd.readFileAlloc(io, hpath, gpa, .limited(1 << 20));
        defer gpa.free(htext);
        ok = try ps.hexReport(gpa, hpath, htext, ps_text.items) and ok;
    }
    if (opts.expect_ps_hex_differ) |hpath| {
        const htext = try cwd.readFileAlloc(io, hpath, gpa, .limited(1 << 20));
        defer gpa.free(htext);
        const want = ps.parseHex(gpa, htext) catch |err| switch (err) {
            error.BadHex => {
                std.debug.print("ps: FAIL {s}: not hex\n", .{hpath});
                std.process.exit(1);
            },
            else => |e| return e,
        };
        defer gpa.free(want);
        const first = std.mem.indexOfDiff(u8, want, ps_text.items);
        if (first) |i| {
            std.debug.print("ps: byte channel: {d} byte(s), differ from {s} from byte {d} (as the control run must)\n", .{ ps_text.items.len, hpath, i });
        } else {
            std.debug.print("ps: FAIL byte channel: {d} byte(s), the same as {s} (the control run must differ)\n", .{ ps_text.items.len, hpath });
            ok = false;
        }
    }
    if (opts.expect_frame_toggles) |want| {
        if (counted == want) {
            std.debug.print("ps: PS_IN bit 7 flipped {d} time(s), as expected\n", .{counted});
        } else {
            std.debug.print("ps: FAIL PS_IN bit 7 flipped {d} time(s), expected {d}\n", .{ counted, want });
            ok = false;
        }
    }
    if (opts.ps_bytes_out) |path| {
        var hex: std.ArrayList(u8) = .empty;
        defer hex.deinit(gpa);
        for (ps_text.items, 0..) |b, i| try hex.print(gpa, "{X:0>2}{s}", .{ b, if (i % 16 == 15 or i + 1 == ps_text.items.len) "\n" else " " });
        try cwd.writeFile(io, .{ .sub_path = path, .data = hex.items });
        std.debug.print("ps: byte channel: {d} byte(s) written to {s}\n", .{ ps_text.items.len, path });
    }
    if (movie_on) {
        std.debug.print("movie: {d} frame toggle(s)\n", .{flips});
        if (flips == 0) {
            std.debug.print("movie: FAIL PS_IN bit 7 never flipped (the sync bit set? MOVIES=0?)\n", .{});
            ok = false;
        }
    }
    if (opts.movie_record) |path| {
        const f = try movie.encodeFile(gpa, .{ .system = @truncate(arty_ps_in(top) >> 16), .frames = 0 }, "", recorded.items);
        defer gpa.free(f);
        try cwd.writeFile(io, .{ .sub_path = path, .data = f });
        std.debug.print("movie: {d} frame(s) recorded to {s}\n", .{ recorded.items.len, path });
    }
    if (opts.movie_play != null and flips + 1 < play_words.len)
        std.debug.print("movie: the run ended {d} frame(s) before the movie\n", .{play_words.len - flips - 1});
    if (player) |p| if (!p.done()) {
        std.debug.print("ps: FAIL the script hadn't finished after {d} ticks\n", .{tick});
        ok = false;
    };
    if (opts.expect_ps_in) |want| {
        const got = arty_ps_in(top);
        if (got == want) {
            std.debug.print("ps: PS_IN = 0x{X:0>8}\n", .{got});
        } else {
            std.debug.print("ps: FAIL PS_IN = 0x{X:0>8}, expected 0x{X:0>8}\n", .{ got, want });
            ok = false;
        }
    }
    if (frames < opts.frames) {
        std.debug.print("arty-sim: FAIL only {d} of {d} frame(s) decoded in {d} ticks\n", .{ frames, opts.frames, tick });
        ok = false;
    }
    if (opts.expect) |dir| {
        std.debug.print("arty-sim: {d} frame(s) compared with {s}, {d} differ\n", .{ compared, dir, mismatched });
        if (compared == 0 or mismatched != 0) ok = false;
    }
    if (opts.expect_last) |gpath| {
        if (frames == 0) {
            ok = false;
        } else {
            const gbytes = try cwd.readFileAlloc(io, gpath, gpa, .limited(64 << 20));
            defer gpa.free(gbytes);
            if (!try sameAs(gpa, gbytes, mode, dec.pixels, gpath, opts.golden_4bit)) ok = false;
        }
    }
    if (!ok) {
        std.debug.print("arty-sim: FAIL\n", .{});
        std.process.exit(1);
    }
    std.debug.print("arty-sim: OK\n", .{});
}

/// --expect-capture: the screenshot in the write slave's memory against the
/// last frame on screen, and its trailer.
fn captureReport(w: *const axi.WrSlave, c: [5]u32, mode: vga.Mode, rgb: []const u8) bool {
    const pw, const ph, const x0, const y0, const sc = c;
    const base: u32 = 0x1000_0000;
    // the trailer (Platform.Video.Capture.trailerWord)
    const taddr = base + ((4 * pw * ph + 7) & ~@as(u32, 7));
    var t: u64 = 0;
    for (0..8) |k| {
        const b = w.byte(taddr + @as(u32, @intCast(k))) orelse {
            std.debug.print("capture: FAIL no trailer at 0x{X:0>8} (no screenshot finished)\n", .{taddr});
            return false;
        };
        t |= @as(u64, b) << @intCast(8 * k);
    }
    const sq: u8 = @truncate(t);
    const status: u8 = @truncate(t >> 8);
    const tw: u32 = @truncate((t >> 16) & 0xFFF);
    const th: u32 = @truncate((t >> 28) & 0xFFF);
    const count: u32 = @truncate(t >> 40);
    var ok = true;
    if (sq == 0 or status != 0 or tw != pw or th != ph or count != pw * ph) {
        std.debug.print("capture: FAIL trailer: sequence {d}, status {d}, {d} x {d}, {d} pixel(s) written; expected {d} x {d}, all written\n", .{ sq, status, tw, th, count, pw, ph });
        ok = false;
    }
    var diff: u32 = 0;
    var first: ?[2]u32 = null;
    for (0..ph) |y| for (0..pw) |x| {
        const a = base + 4 * @as(u32, @intCast(y * pw + x));
        const sx = x0 + sc * @as(u32, @intCast(x));
        const sy = y0 + sc * @as(u32, @intCast(y));
        const i = (@as(usize, sy) * mode.width + sx) * 3;
        const same = for (0..3) |k| {
            if ((w.byte(a + @as(u32, @intCast(k))) orelse break false) != rgb[i + k]) break false;
        } else true;
        if (!same) {
            diff += 1;
            if (first == null) first = .{ @intCast(x), @intCast(y) };
        }
    };
    if (diff == 0) {
        std.debug.print("capture: {d} x {d} screenshot (sequence {d}) identical to the screen\n", .{ pw, ph, sq });
    } else {
        std.debug.print("capture: FAIL {d} pixel(s) differ from the screen, first at ({d}, {d})\n", .{ diff, first.?[0], first.?[1] });
        ok = false;
    }
    return ok;
}

/// c's top 4 bits widened (c x 17), as the VGA DAC shows it.
fn as444(c: u8) u8 {
    return (c >> 4) * 17;
}

fn sameAs(gpa: std.mem.Allocator, golden_png: []const u8, mode: vga.Mode, rgb: []const u8, gpath: []const u8, four_bit: bool) !bool {
    var g = try png.decode(gpa, golden_png);
    defer g.deinit(gpa);
    if (g.width != mode.width or g.height != mode.height) {
        std.debug.print("arty-sim: {s}: size {d}x{d}, frame is {d}x{d}\n", .{ gpath, g.width, g.height, mode.width, mode.height });
        return false;
    }
    var diff: u64 = 0;
    var first: usize = 0;
    var i: usize = 0;
    while (i < rgb.len) : (i += 3) {
        const same = if (four_bit)
            as444(rgb[i]) == g.rgb[i] and as444(rgb[i + 1]) == g.rgb[i + 1] and as444(rgb[i + 2]) == g.rgb[i + 2]
        else
            std.mem.eql(u8, rgb[i..][0..3], g.rgb[i..][0..3]);
        if (!same) {
            if (diff == 0) first = i / 3;
            diff += 1;
        }
    }
    if (diff == 0) {
        std.debug.print("arty-sim: {s}: identical{s}\n", .{ gpath, if (four_bit) " (top 4 bits per channel, --golden-4bit)" else "" });
        return true;
    }
    const x = first % mode.width;
    const y = first / mode.width;
    std.debug.print("arty-sim: {s}: {d} pixel(s) differ, first at ({d}, {d}): got {any}, golden {any}\n", .{ gpath, diff, x, y, rgb[first * 3 ..][0..3], g.rgb[first * 3 ..][0..3] });
    return false;
}
