//! The desktop emulator (side track Q, PLAN.md Q.2 to Q.4): a system's
//! reference model (gb_spec.zig, nes_spec.zig, chip8_spec.zig,
//! sms_spec.zig) in an SDL3 window, with sound and the board's key maps.
//! --reference runs the 800XL's and the SNES's reference models (their
//! original specs) instead of their fast versions, to compare by hand.
//! A `.sfc` or `.smc` file runs on the Super Nintendo (its fast version,
//! its pixels drawn by two threads on other cores (one on a 2-CPU machine;
//! --render-threads N,
//! --no-render-thread draws inline); the sound module's boot ROM from the repo's
//! build/snes/ipl.bin, `make snes-asm`, unless --ipl FILE), a `.xex`, a
//! `.car` or a raw `.rom` cartridge on the Atari 800XL (its fast version; the
//! OS, BASIC and the loader from the repo unless --os/--basic; the
//! keyboard as the board's USB keyboard, a800_keys.zig: F2 to F4 START
//! SELECT OPTION, F8 BREAK, ` ESC), a `.gg` file on the Game Gear, a `.sms` on the Master System II
//! (with its FM unit unless --no-fm; --bios FILE: your own BIOS dump), a
//! `.nes` on the NES, a `.ch8` on CHIP-8 (the board's slow speed, about 700 Hz,
//! and the COSMAC VIP's quirks; --fast its fast speed, --quirks N its
//! SW0 to SW4 bits), any other on the Game Boy.
//!
//!   desktop [--gbc] [--saves DIR | --no-save] [--scale N] [--keymap FILE]
//!           [--frames N] [--png OUT] [--expect-png GOLDEN] [--unpaced] ROM
//!
//! The NES has no battery saves (nor has the board: its system table has
//! no save hook for them), and no --gbc. A GBC cartridge (header byte 0x143 bit 7) runs on the Game Boy Color, a
//! DMG one on the DMG, or with --gbc on the GBC (its default palette, not
//! the boot ROM's per-title choice the board makes). Battery saves are the
//! board's own files (save.zig): read at the start, written every ten
//! seconds when they changed and at the end. For an MBC3 clock the file's
//! 48-byte clock footer is kept as it was (the spec's clock isn't saved).
//! --saves sets SAVES_DIR (make desktop passes the Makefile's);
//! --no-save neither reads nor writes one.
//!
//! Keys and gamepads come from a key map in `link`'s and the board's
//! format: --keymap FILE, else the desktop's own
//! $XDG_CONFIG_HOME/emulation_clash/desktop-keymap.conf when it exists
//! (`make keymap SYSTEM=gb KEYMAP=~/.config/emulation_clash/desktop-keymap.conf`
//! writes one), else the defaults. The board's keymap.conf isn't read: the
//! desktop's keyboard may differ from the board's (the user, 2026-10-20:
//! no keypad). SDL's scancodes are USB HID usages, translated to the
//! Linux codes the maps use as the board's USB keyboard does
//! (usb_keys.zig).
//! The `[global]` keys work as on the board: frame advance (F11) pauses
//! and steps a frame, frame run (F10) lets it run again; F12 takes a
//! screenshot (the data directory's screens/). F5 resets.
//!
//! The menu (Q.8, ui.zig; the user, 2026-10-25): Esc opens it, as on the
//! board, or the mouse at the window's top edge; the game waits while a
//! list is down. Game (pause, frame advance, reset, screenshot, Open
//! ROM... through SDL's file dialog: the program again on the new file,
//! quit), Speed (25% to 300%: the sound's frequency ratio, its pitch with
//! it; unlimited, without sound; holding TAB fast-forwards at --ff-speed,
//! 200% unless given, 0 unlimited), State (save F7 or [, load F9 or ]; on
//! the 800XL, whose keyboard is the PC's, TAB, [ and ] stay its keys; five slots:
//! the data directory's desktop-states/<ROM>.<slot>.state, "Platform.Snapshot";
//! the fast versions' only, so not with --reference), Movie (input movies,
//! side track O's board format: recorded from power-on with cartridge RAM
//! as a new machine has it and no battery save written from then on, into
//! the data directory's movies/, played back from a file; --movie FILE
//! plays one headless; the systems whose key word fits 16 bits: not yet
//! the SNES and the 800XL; gg-run --movie plays them on the reference
//! model, with its --png-at, --peek, --trace), View (1x to 6x, fullscreen, integer
//! scaling, smooth pixels, the speed shown), Audio (mute, volume). Closing
//! the window or Game, Quit quits.
//!
//! Pacing follows the sound: frames run while less than about three
//! frames of sound are queued, so the picture keeps the Game Boy's 59.73
//! Hz and the sound never runs dry. Without a sound device (or with
//! --unpaced, to measure the speed) the clock paces it, or nothing does.
//! --frames N quits after N frames and prints the speed; --png writes the
//! last frame (with SDL_VIDEODRIVER=dummy, a headless run); --expect-png
//! compares it with a PNG and fails if a pixel differs (make test-desktop).
//!
//! SDL3 is a C library called directly (CLAUDE.md rule 9); only what is
//! used here is declared, below, as in platform/sim/play.zig.

const std = @import("std");
const a800_keys = @import("ps_keys"); // platform/ps/a800_keys.zig
const usb_keys = a800_keys.usb;
const keymap = usb_keys.maps;
const png = @import("png");
const gb = @import("gb_spec.zig");
const nes = @import("nes_spec.zig");
const chip8 = @import("chip8_spec.zig");
const sms = @import("sms_spec.zig");
const debug = @import("debug.zig");
const a800 = @import("a800_spec.zig");
const snes = @import("snes_spec.zig");
const build_options = @import("build_options");
const save = @import("save.zig");
const ui_mod = @import("ui.zig");
const menu = usb_keys.board_menu; // the board's system table: the save sizes
const movie = menu.movie; // input movies: the board's file format (side track O)

// --- SDL3 (SDL3/SDL.h), only what is used -----------------------------
const SDL_Window = opaque {};
const SDL_Renderer = ui_mod.SDL_Renderer;
const SDL_Texture = opaque {};
const SDL_AudioStream = opaque {};
const SDL_Gamepad = opaque {};
const SDL_Event = extern struct { type: u32, reserved: u32, timestamp: u64, which: u32, pad: [108]u8 };
const SDL_AudioSpec = extern struct { format: c_int, channels: c_int, freq: c_int };

const SDL_INIT_AUDIO: u32 = 0x10;
const SDL_INIT_VIDEO: u32 = 0x20;
const SDL_INIT_GAMEPAD: u32 = 0x2000;
const SDL_PIXELFORMAT_ARGB8888: u32 = 0x16362004;
const SDL_TEXTUREACCESS_STREAMING: c_int = 1;
const SDL_SCALEMODE_NEAREST: c_int = 0;
const SDL_AUDIO_S16LE: c_int = 0x8010;
const SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK: u32 = 0xFFFFFFFF;
const SDL_WINDOW_RESIZABLE: u64 = 0x20;
const SDL_EVENT_QUIT: u32 = 0x100;
const SDL_EVENT_KEY_DOWN: u32 = 0x300;
const SDL_EVENT_MOUSE_MOTION: u32 = 0x400;
const SDL_EVENT_MOUSE_BUTTON_DOWN: u32 = 0x401;
const SDL_EVENT_GAMEPAD_ADDED: u32 = 0x653;
const SDL_EVENT_GAMEPAD_REMOVED: u32 = 0x654;

extern fn SDL_Init(flags: u32) bool;
extern fn SDL_Quit() void;
extern fn SDL_GetError() [*:0]const u8;
extern fn SDL_CreateWindow(title: [*:0]const u8, w: c_int, h: c_int, flags: u64) ?*SDL_Window;
extern fn SDL_DestroyWindow(w: *SDL_Window) void;
extern fn SDL_CreateRenderer(w: *SDL_Window, name: ?[*:0]const u8) ?*SDL_Renderer;
extern fn SDL_DestroyRenderer(r: *SDL_Renderer) void;
extern fn SDL_SetRenderLogicalPresentation(r: *SDL_Renderer, w: c_int, h: c_int, mode: c_int) bool;
extern fn SDL_CreateTexture(r: *SDL_Renderer, format: u32, access: c_int, w: c_int, h: c_int) ?*SDL_Texture;
extern fn SDL_DestroyTexture(t: *SDL_Texture) void;
extern fn SDL_SetTextureScaleMode(t: *SDL_Texture, mode: c_int) bool;
extern fn SDL_UpdateTexture(t: *SDL_Texture, rect: ?*const anyopaque, pixels: *const anyopaque, pitch: c_int) bool;
extern fn SDL_RenderClear(r: *SDL_Renderer) bool;
extern fn SDL_RenderTexture(r: *SDL_Renderer, t: *SDL_Texture, src: ?*const anyopaque, dst: ?*const anyopaque) bool;
extern fn SDL_RenderPresent(r: *SDL_Renderer) bool;
extern fn SDL_PollEvent(e: *SDL_Event) bool;
extern fn SDL_GetKeyboardState(numkeys: ?*c_int) [*]const bool;
extern fn SDL_GetTicksNS() u64;
extern fn SDL_DelayNS(ns: u64) void;
extern fn SDL_OpenAudioDeviceStream(dev: u32, spec: *const SDL_AudioSpec, cb: ?*const anyopaque, user: ?*anyopaque) ?*SDL_AudioStream;
extern fn SDL_ResumeAudioStreamDevice(s: *SDL_AudioStream) bool;
extern fn SDL_PutAudioStreamData(s: *SDL_AudioStream, buf: *const anyopaque, len: c_int) bool;
extern fn SDL_GetAudioStreamQueued(s: *SDL_AudioStream) c_int;
extern fn SDL_DestroyAudioStream(s: *SDL_AudioStream) void;
extern fn SDL_OpenGamepad(id: u32) ?*SDL_Gamepad;
extern fn SDL_CloseGamepad(g: *SDL_Gamepad) void;
extern fn SDL_GetGamepadID(g: *SDL_Gamepad) u32;
extern fn SDL_GetGamepadButton(g: *SDL_Gamepad, button: c_int) bool;
extern fn SDL_GetGamepadAxis(g: *SDL_Gamepad, axis: c_int) i16;
extern fn SDL_SetWindowSize(w: *SDL_Window, width: c_int, height: c_int) bool;
extern fn SDL_SetWindowFullscreen(w: *SDL_Window, on: bool) bool;
extern fn SDL_GetWindowSizeInPixels(w: *SDL_Window, width: *c_int, height: *c_int) bool;
extern fn SDL_GetWindowPixelDensity(w: *SDL_Window) f32;
extern fn SDL_SetAudioStreamFrequencyRatio(s: *SDL_AudioStream, ratio: f32) bool;
extern fn SDL_SetAudioStreamGain(s: *SDL_AudioStream, gain: f32) bool;
extern fn SDL_ClearAudioStream(s: *SDL_AudioStream) bool;
const SDL_DialogFileFilter = extern struct { name: [*:0]const u8, pattern: [*:0]const u8 };
extern fn SDL_ShowOpenFileDialog(cb: *const fn (?*anyopaque, ?[*]const ?[*:0]const u8, c_int) callconv(.c) void, user: ?*anyopaque, w: ?*SDL_Window, filters: [*]const SDL_DialogFileFilter, n: c_int, default_location: ?[*:0]const u8, allow_many: bool) void;
extern "c" fn execv(path: [*:0]const u8, argv: [*:null]const ?[*:0]const u8) c_int;
extern "c" fn time(t: ?*i64) i64;

const SCANCODE_ESCAPE = 41;
const SCANCODE_RETURN = 40;
const SCANCODE_RIGHT = 79;
const SCANCODE_LEFT = 80;
const SCANCODE_DOWN = 81;
const SCANCODE_UP = 82;
const SCANCODE_KP_ENTER = 88;
const SCANCODE_F5 = 62;
const SCANCODE_F7 = 64;
const SCANCODE_F9 = 66;
const SCANCODE_TAB = 43;
const SCANCODE_LEFTBRACKET = 47;
const SCANCODE_RIGHTBRACKET = 48;
/// Scancodes are HID usages; the ones past the keyboard page's 0xE7 aren't
/// keys the maps know.
const scancodes = 0xE8;

fn sdlFail(what: []const u8) noreturn {
    std.debug.print("desktop: {s}: {s}\n", .{ what, std.mem.span(SDL_GetError()) });
    std.process.exit(1);
}

fn fail(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print("desktop: " ++ fmt ++ "\n", args);
    std.process.exit(1);
}

const Opts = struct {
    rom: []const u8 = "",
    scale: c_int = 4,
    keymap: ?[]const u8 = null,
    frames: ?u64 = null,
    png_out: ?[]const u8 = null,
    expect_png: ?[]const u8 = null,
    unpaced: bool = false,
    gbc: bool = false,
    saves: ?[]const u8 = null,
    no_save: bool = false,
    fast: bool = false,
    /// an 800XL tape through the recorder itself (depth 2), not the
    /// instant answer to the OS's call
    real_tape: bool = false,
    /// the speed while TAB is held (percent; 0 unlimited)
    ff_speed: u16 = 200,
    no_fm: bool = false,
    os: ?[]const u8 = null,
    ipl: ?[]const u8 = null,
    /// null: the default, 'defaultRenderThreads'
    render_threads: ?u8 = null,
    frameskip: bool = true,
    frame_lag: bool = true,
    reference: bool = false,
    basic: ?[]const u8 = null,
    bios: ?[]const u8 = null,
    quirks: u8 = 0,
    scale_given: bool = false,
    /// an input movie played from power-on (Q.8: headless runs too)
    movie: ?[]const u8 = null,
    /// the debug socket (side track R; debug.zig): its path, or the default
    debug: bool = false,
    debug_socket: ?[]const u8 = null,
};

fn parseArgs(args: []const [:0]const u8) Opts {
    var o: Opts = .{};
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const a = args[i];
        const next = struct {
            fn f(as: []const [:0]const u8, j: *usize) []const u8 {
                j.* += 1;
                if (j.* >= as.len) fail("{s} needs a value", .{as[j.* - 1]});
                return as[j.*];
            }
        }.f;
        if (std.mem.eql(u8, a, "--scale")) {
            o.scale = std.fmt.parseInt(c_int, next(args, &i), 10) catch fail("--scale: a number", .{});
            o.scale_given = true;
        } else if (std.mem.eql(u8, a, "--reference")) {
            o.reference = true;
        } else if (std.mem.eql(u8, a, "--no-frame-lag")) {
            o.frame_lag = false;
        } else if (std.mem.eql(u8, a, "--no-frameskip")) {
            o.frameskip = false;
        } else if (std.mem.eql(u8, a, "--no-render-thread")) {
            o.render_threads = 0;
        } else if (std.mem.eql(u8, a, "--render-threads")) {
            o.render_threads = std.fmt.parseInt(u8, next(args, &i), 10) catch fail("--render-threads: a number, 0 to 8", .{});
        } else if (std.mem.eql(u8, a, "--ipl")) {
            o.ipl = next(args, &i);
        } else if (std.mem.eql(u8, a, "--os")) {
            o.os = next(args, &i);
        } else if (std.mem.eql(u8, a, "--basic")) {
            o.basic = next(args, &i);
        } else if (std.mem.eql(u8, a, "--no-fm")) {
            o.no_fm = true;
        } else if (std.mem.eql(u8, a, "--bios")) {
            o.bios = next(args, &i);
        } else if (std.mem.eql(u8, a, "--fast")) {
            o.fast = true;
        } else if (std.mem.eql(u8, a, "--real-tape")) {
            o.real_tape = true;
        } else if (std.mem.eql(u8, a, "--ff-speed")) {
            o.ff_speed = std.fmt.parseInt(u16, next(args, &i), 10) catch fail("--ff-speed: a percentage (200: twice real time; 0: unlimited)", .{});
        } else if (std.mem.eql(u8, a, "--quirks")) {
            o.quirks = std.fmt.parseInt(u8, next(args, &i), 0) catch fail("--quirks: a number, 0 to 31", .{});
        } else if (std.mem.eql(u8, a, "--keymap")) {
            o.keymap = next(args, &i);
        } else if (std.mem.eql(u8, a, "--frames")) {
            o.frames = std.fmt.parseInt(u64, next(args, &i), 10) catch fail("--frames: a number", .{});
        } else if (std.mem.eql(u8, a, "--png")) {
            o.png_out = next(args, &i);
        } else if (std.mem.eql(u8, a, "--expect-png")) {
            o.expect_png = next(args, &i);
        } else if (std.mem.eql(u8, a, "--gbc")) {
            o.gbc = true;
        } else if (std.mem.eql(u8, a, "--saves")) {
            o.saves = next(args, &i);
        } else if (std.mem.eql(u8, a, "--no-save")) {
            o.no_save = true;
        } else if (std.mem.eql(u8, a, "--unpaced")) {
            o.unpaced = true;
        } else if (std.mem.eql(u8, a, "--debug")) {
            o.debug = true;
        } else if (std.mem.eql(u8, a, "--debug-socket")) {
            o.debug = true;
            o.debug_socket = next(args, &i);
        } else if (std.mem.eql(u8, a, "--movie")) {
            o.movie = next(args, &i);
        } else if (a.len > 0 and a[0] == '-') {
            fail("unknown option {s} (desktop [--gbc] [--saves DIR | --no-save] [--no-fm] [--bios FILE] [--os FILE] [--basic FILE] [--ipl FILE] [--no-render-thread | --render-threads N] [--no-frameskip] [--no-frame-lag] [--reference] [--fast] [--real-tape] [--ff-speed PERCENT] [--debug | --debug-socket PATH] [--quirks N] [--scale N] [--keymap FILE] [--frames N] [--png OUT] [--expect-png GOLDEN] [--unpaced] ROM)", .{a});
        } else o.rom = a;
    }
    if (o.rom.len == 0) fail("no ROM (desktop [options] ROM)", .{});
    if (o.render_threads == null) o.render_threads = defaultRenderThreads();
    return o;
}

/// The SNES's render threads by default (Q.7): 2, at most one fewer than
/// the logical CPUs (the machine needs one). Measured (Rockman & Forte):
/// with the drawing as it is now, 2 beat 3 on every layout tried, 4 cores
/// with 8 threads included (2.36x real time against 2.23x); 3 were better
/// only while the drawing was slower (docs/desktop-speedups.md).
fn defaultRenderThreads() u8 {
    const n = std.Thread.getCpuCount() catch 4;
    return @intCast(@min(2, @max(1, n -| 1)));
}

/// The key map file: --keymap, else the desktop's own in the XDG location
/// (none is fine: the defaults then).
fn loadConfig(io: std.Io, gpa: std.mem.Allocator, env: *const std.process.Environ.Map, given: ?[]const u8, out: *keymap.Config) void {
    out.setDefaults();
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const path: []const u8 = given orelse blk: {
        if (env.get("XDG_CONFIG_HOME")) |x| if (x.len > 0 and x[0] == '/')
            break :blk std.fmt.bufPrint(&buf, "{s}/emulation_clash/desktop-keymap.conf", .{x}) catch return;
        const home = env.get("HOME") orelse return;
        break :blk std.fmt.bufPrint(&buf, "{s}/.config/emulation_clash/desktop-keymap.conf", .{home}) catch return;
    };
    const text = std.Io.Dir.cwd().readFileAlloc(io, path, gpa, .limited(64 * 1024)) catch |e| {
        if (e == error.FileNotFound and given == null) {
            std.debug.print("desktop: key map: the defaults (no {s})\n", .{path});
            return;
        }
        fail("can't read the key map {s} ({s})", .{ path, @errorName(e) });
    };
    defer gpa.free(text);
    var err: keymap.ParseError = undefined;
    keymap.parseInto(text, &err, out) catch fail("{s}:{d}: {s}", .{ path, err.line, err.msg });
    std.debug.print("desktop: key map {s}\n", .{path});
}

/// SDL's gamepad buttons (SDL_GamepadButton, 0 to 14) as Linux codes.
const pad_buttons = blk: {
    const names = [_][]const u8{
        "BTN_SOUTH",   "BTN_EAST",   "BTN_WEST", "BTN_NORTH", "BTN_SELECT",    "BTN_MODE",        "BTN_START",       "BTN_THUMBL",
        "BTN_THUMBR",  "BTN_TL",     "BTN_TR",   "BTN_DPAD_UP", "BTN_DPAD_DOWN", "BTN_DPAD_LEFT", "BTN_DPAD_RIGHT",
    };
    @setEvalBranchQuota(100_000);
    var t: [names.len]keymap.Input = undefined;
    for (names, 0..) |n, i| t[i] = keymap.parseInput(n) orelse @compileError("unknown input " ++ n);
    break :blk t;
};

/// The key word the keyboard and the gamepads hold now.
fn held(lookup: *const keymap.Lookup, pads: []const *SDL_Gamepad) keymap.Word {
    var w: keymap.Word = 0;
    const ks = SDL_GetKeyboardState(null);
    for (4..scancodes) |sc| if (ks[sc]) {
        if (usb_keys.usageCode(@intCast(sc))) |code| w |= lookup.mask[code];
    };
    for (pads) |p| {
        for (pad_buttons, 0..) |code, b| if (SDL_GetGamepadButton(p, @intCast(b))) {
            w |= lookup.mask[code];
        };
        // the left stick (SDL's axes 0, 1) as ABS_X, ABS_Y, past half way
        for (0..2) |axis| {
            const v = SDL_GetGamepadAxis(p, @intCast(axis));
            if (v < -16384) w |= lookup.mask[keymap.axisInput(@intCast(axis), false)];
            if (v > 16384) w |= lookup.mask[keymap.axisInput(@intCast(axis), true)];
        }
    }
    return w;
}

/// The keyboard as a USB boot report (the board's 800XL keyboard reads
/// one: a800_keys.zig): the modifier bits, then up to six usages (SDL's
/// scancodes are the usages).
fn bootReport() [8]u8 {
    var r = [_]u8{0} ** 8;
    const ks = SDL_GetKeyboardState(null);
    for (0..8) |b| {
        if (ks[0xE0 + b]) r[0] |= @as(u8, 1) << @intCast(b);
    }
    var n: usize = 2;
    for (4..0xE0) |sc| if (ks[sc] and n < 8) {
        r[n] = @intCast(sc);
        n += 1;
    };
    return r;
}

fn bindingHeld(b: *const keymap.Binding) bool {
    const ks = SDL_GetKeyboardState(null);
    for (4..scancodes) |sc| if (ks[sc]) {
        if (usb_keys.usageCode(@intCast(sc))) |code| if (b.has(code)) return true;
    };
    return false;
}

/// The cartridge's battery save: its file, the RAM's part of it, and what
/// was last written (so an unchanged RAM isn't written again).
const Battery = struct {
    path: []const u8,
    /// the file's size (the board's: menu.zig's Save.size)
    size: usize,
    /// cartridge RAM's bytes at its start; the rest is the clock footer
    ram: usize,
    file: []u8,
    now: []u8,

    /// Null if the cartridge has no battery RAM the board supports, or the
    /// file there has another size (then it is left alone).
    fn open(io: std.Io, gpa: std.mem.Allocator, m: anytype, sys: *const menu.System, rom: []const u8, path: []const u8) !?Battery {
        const sv = sys.save orelse return null;
        const size: usize = sv.size(rom[0..@min(rom.len, 512)]);
        if (size == 0) return null;
        const footer: usize = if (sv.clock) |c| (if (c(rom[0..@min(rom.len, 512)])) menu.clock_footer else 0) else 0;
        const ram = @min(size - footer, m.cramSize());
        var b: Battery = .{ .path = path, .size = size, .ram = ram, .file = try gpa.alloc(u8, size), .now = try gpa.alloc(u8, size) };
        @memset(b.file, 0);
        const old = std.Io.Dir.cwd().readFileAlloc(io, path, gpa, .limited(1 << 20)) catch |e| switch (e) {
            error.FileNotFound => {
                std.debug.print("desktop: no save yet, will write {s}\n", .{path});
                @memset(b.now, 0);
                m.cramGet(b.now[0..ram]);
                return b;
            },
            else => return e,
        };
        defer gpa.free(old);
        if (old.len != size) {
            std.debug.print("desktop: {s} is {d} bytes, not {d}: left alone, no saves this run\n", .{ path, old.len, size });
            b.deinit(gpa);
            return null;
        }
        @memcpy(b.file, old);
        @memcpy(b.now, old);
        m.cramSet(b.file[0..ram]);
        std.debug.print("desktop: save {s}\n", .{path});
        return b;
    }

    fn deinit(b: *Battery, gpa: std.mem.Allocator) void {
        gpa.free(b.file);
        gpa.free(b.now);
    }

    /// Writes the RAM if it changed since the last write (a temporary
    /// file, then renamed: a cut-off write leaves the old save).
    fn flush(b: *Battery, io: std.Io, m: anytype) void {
        m.cramGet(b.now[0..b.ram]);
        if (std.mem.eql(u8, b.now, b.file)) return;
        const cwd = std.Io.Dir.cwd();
        if (std.fs.path.dirname(b.path)) |d| cwd.createDirPath(io, d) catch {};
        var tmp_buf: [std.fs.max_path_bytes]u8 = undefined;
        const tmp = std.fmt.bufPrint(&tmp_buf, "{s}.tmp", .{b.path}) catch return;
        cwd.writeFile(io, .{ .sub_path = tmp, .data = b.now }) catch |e| {
            std.debug.print("desktop: can't write {s} ({s})\n", .{ tmp, @errorName(e) });
            return;
        };
        cwd.rename(tmp, cwd, b.path, io) catch |e| {
            std.debug.print("desktop: can't rename {s} ({s})\n", .{ tmp, @errorName(e) });
            return;
        };
        @memcpy(b.file, b.now);
    }
};

/// The machine being run, either system.
const Emu = union(enum) {
    gb: gb.Machine,
    nes: nes.Machine,
    chip8: chip8.Machine,
    sms: sms.Machine,
    a800: a800.Machine,
    snes: snes.Machine,

    fn width(e: *const Emu) c_int {
        return switch (e.*) { .gb => gb.width, .nes => nes.width, .chip8 => chip8.width, .sms => |*m| @constCast(m).width(), .a800 => a800.width, .snes => snes.width };
    }
    fn height(e: *const Emu) c_int {
        return switch (e.*) { .gb => gb.height, .nes => nes.height, .chip8 => chip8.height, .sms => |*m| @constCast(m).height(), .a800 => a800.height, .snes => snes.height };
    }
    fn frameNs(e: *const Emu) u64 {
        return switch (e.*) { .gb => gb.frame_ns, .nes => nes.frame_ns, .chip8 => chip8.frame_ns, .sms => sms.frame_ns, .a800 => a800.frame_ns, .snes => snes.frame_ns };
    }
    /// One frame with the key word `w` held (and, for the 800XL, the
    /// keyboard as a boot report); the stereo samples it made.
    fn frame(e: *Emu, w: keymap.Word, report: *const [8]u8) usize {
        return switch (e.*) {
            .gb => |*m| m.frame(@bitCast(@as(u8, @truncate(w)))),
            .nes => |*m| m.frame(nes.Pad.fromWord(w)),
            .chip8 => |*m| m.frame(@truncate(w)),
            .sms => |*m| m.frame(w),
            .a800 => |*m| m.frame(&a800_keys.bytes(report, @truncate(w))),
            .snes => |*m| m.frame(w),
        };
    }
    /// Bytes a movie keeps of a frame's input (the format's frame_bytes):
    /// the 800XL's key bytes, the SNES's two pads, everyone else's word.
    fn frameBytes(e: *const Emu) u16 {
        return switch (e.*) { .a800 => a800.key_bytes, .snes => 4, else => 2 };
    }
    /// A frame's input as a movie keeps it, in `out`.
    fn record(e: *const Emu, w: keymap.Word, report: *const [8]u8, out: *[16]u8) []const u8 {
        switch (e.*) {
            .a800 => out[0..a800.key_bytes].* = a800_keys.bytes(report, @truncate(w)),
            .snes => std.mem.writeInt(u32, out[0..4], w, .little),
            else => std.mem.writeInt(u16, out[0..2], @truncate(w), .little),
        }
        return out[0..e.frameBytes()];
    }
    /// One frame from a movie's record of its input (`record`'s).
    fn frameRec(e: *Emu, rec: []const u8, report: *const [8]u8) usize {
        return switch (e.*) {
            .a800 => |*m| m.frame(rec[0..a800.key_bytes]),
            .snes => |*m| m.frame(std.mem.readInt(u32, rec[0..4], .little)),
            else => e.frame(std.mem.readInt(u16, rec[0..2], .little), report),
        };
    }
    fn audio(e: *Emu, out: []i16) usize {
        return switch (e.*) { .gb => |*m| m.audio(out), .nes => |*m| m.audio(out), .chip8 => |*m| m.audio(out), .sms => |*m| m.audio(out), .a800 => |*m| m.audio(out), .snes => |*m| m.audio(out) };
    }
    /// The last frame into `out`, `width` × `height` pixels.
    fn pixels(e: *Emu, out: []u32) void {
        switch (e.*) {
            .gb => |*m| m.pixels(out[0 .. gb.width * gb.height]),
            .nes => |*m| m.pixels(out[0 .. nes.width * nes.height]),
            .chip8 => |*m| m.pixels(out[0 .. chip8.width * chip8.height]),
            .sms => |*m| m.pixels(out),
            .a800 => |*m| m.pixels(out[0 .. a800.width * a800.height]),
            .snes => |*m| m.pixels(out[0 .. snes.width * snes.height]),
        }
    }
    /// Frame skipping (Q.6): the next frame's picture not drawn, where a
    /// system can leave it out without the game seeing a difference (the
    /// 800XL's and the SNES's fast versions); elsewhere nothing.
    fn skip(e: *Emu, on: bool) void {
        switch (e.*) {
            .a800 => |*m| m.skip(on),
            .snes => |*m| m.skip(on),
            .nes => |*m| m.skip(on),
            else => {},
        }
    }

    /// Save states (Q.8): every system's library has them, the fast
    /// versions' (so not with --reference where a system has both).
    fn hasStates(e: *const Emu, reference: bool) bool {
        return switch (e.*) {
            .gb, .chip8 => true,
            .nes, .a800, .snes, .sms => !reference,
        };
    }
    fn stateSave(e: *Emu, gpa: std.mem.Allocator) ![]u8 {
        return switch (e.*) {
            inline else => |*m| m.stateSave(gpa),
        };
    }
    fn stateLoad(e: *Emu, blob: []const u8) bool {
        return switch (e.*) {
            inline else => |*m| m.stateLoad(blob),
        };
    }

    fn deinit(e: *Emu) void {
        switch (e.*) { .gb => |*m| m.deinit(), .nes => |*m| m.deinit(), .chip8 => |*m| m.deinit(), .sms => |*m| m.deinit(), .a800 => |*m| m.deinit(), .snes => |*m| m.deinit() }
    }
};

/// The battery save's RAM written back if it changed (the systems with
/// one: the Game Boy and GBC, the Game Gear and Master System).
fn flushBattery(b: *Battery, io: std.Io, e: *Emu) void {
    switch (e.*) {
        .gb => |*m| b.flush(io, m),
        .sms => |*m| b.flush(io, m),
        else => {},
    }
}

// --- the menu (ui.zig): its entries' ids and contents ------------------

const Act = enum(u32) {
    pause = 1,
    advance,
    reset,
    open_rom,
    screenshot,
    quit,
    state_save,
    state_load,
    fullscreen,
    integer,
    smooth,
    show_speed,
    mute,
    movie_record,
    movie_record_here,
    movie_stop,
    movie_play,
    _,
};
const act_speed: u32 = 100; // + an index into `speeds`
const act_slot: u32 = 200; // + the slot, 1 to `slots`
const act_scale: u32 = 300; // + the scale, 1 to 6
const act_volume: u32 = 400; // + an index into `volumes`

/// The speeds offered, in percent of real time (0: as fast as it goes).
const speeds = [_]u16{ 25, 50, 100, 150, 200, 300, 0 };
const speed_labels = [_][]const u8{ "25%", "50%", "100%", "150%", "200%", "300%", "Unlimited" };
const volumes = [_]u8{ 25, 50, 75, 100 };
const volume_labels = [_][]const u8{ "25%", "50%", "75%", "100%" };
const slots = 5;
const slot_labels = [_][]const u8{ "Slot 1", "Slot 2", "Slot 3", "Slot 4", "Slot 5" };
const scale_labels = [_][]const u8{ "1x", "2x", "3x", "4x", "5x", "6x" };

/// What the menu shows ticked or greyed out.
const View = struct {
    paused: bool = false,
    speed: usize = 2,
    slot: u8 = 1,
    scale: c_int = 1,
    fullscreen: bool = false,
    integer: bool = true,
    smooth: bool = false,
    show_speed: bool = false,
    /// [ and ] save and load (not on the 800XL)
    brackets: bool = true,
    mute: bool = false,
    volume: usize = 3,
    states: bool = false,
    has_audio: bool = false,
    /// input movies: the system's ID for them, null where its input
    /// doesn't fit the format's 16-bit words (the SNES's two pads, the
    /// 800XL's keyboard)
    movie_system: ?u8 = null,
    movie_mode: MovieMode = .off,
};

const MovieMode = enum { off, record, play };

const MenuBuf = struct {
    game: [7]ui_mod.Entry = undefined,
    speed: [speeds.len]ui_mod.Entry = undefined,
    state: [2 + slots]ui_mod.Entry = undefined,
    view: [scale_labels.len + 4]ui_mod.Entry = undefined,
    audio: [1 + volumes.len]ui_mod.Entry = undefined,
    movie: [4]ui_mod.Entry = undefined,
    menus: [6]ui_mod.Menu = undefined,

    fn build(mb: *MenuBuf, v: View) []const ui_mod.Menu {
        mb.game = .{
            .{ .label = if (v.paused) "Resume" else "Pause", .shortcut = "F10/F11", .id = @intFromEnum(Act.pause) },
            .{ .label = "Frame advance", .shortcut = "F11", .id = @intFromEnum(Act.advance) },
            .{ .label = "Reset", .shortcut = "F5", .id = @intFromEnum(Act.reset) },
            .{ .label = "Screenshot", .shortcut = "F12", .id = @intFromEnum(Act.screenshot) },
            .{ .label = "Open ROM...", .id = @intFromEnum(Act.open_rom), .sep = true },
            .{ .label = "Quit", .id = @intFromEnum(Act.quit), .sep = true },
            .{ .label = "Close menu", .shortcut = "Esc", .id = 0 },
        };
        for (&mb.speed, 0..) |*e, i| e.* = .{ .label = speed_labels[i], .id = act_speed + @as(u32, @intCast(i)), .checked = v.speed == i, .sep = speeds[i] == 0 };
        mb.state[0] = .{ .label = "Save state", .shortcut = if (v.brackets) "F7 [" else "F7", .id = @intFromEnum(Act.state_save), .enabled = v.states };
        mb.state[1] = .{ .label = "Load state", .shortcut = if (v.brackets) "F9 ]" else "F9", .id = @intFromEnum(Act.state_load), .enabled = v.states };
        for (0..slots) |i| mb.state[2 + i] = .{ .label = slot_labels[i], .id = act_slot + @as(u32, @intCast(i + 1)), .checked = v.slot == i + 1, .enabled = v.states, .sep = i == 0 };
        for (0..scale_labels.len) |i| mb.view[i] = .{ .label = scale_labels[i], .id = act_scale + @as(u32, @intCast(i + 1)), .checked = !v.fullscreen and v.scale == i + 1, .enabled = !v.fullscreen };
        const k = scale_labels.len;
        mb.view[k] = .{ .label = "Fullscreen", .id = @intFromEnum(Act.fullscreen), .checked = v.fullscreen, .sep = true };
        mb.view[k + 1] = .{ .label = "Integer scaling", .id = @intFromEnum(Act.integer), .checked = v.integer };
        mb.view[k + 2] = .{ .label = "Smooth pixels", .id = @intFromEnum(Act.smooth), .checked = v.smooth };
        mb.view[k + 3] = .{ .label = "Show speed", .id = @intFromEnum(Act.show_speed), .checked = v.show_speed };
        mb.audio[0] = .{ .label = "Mute", .id = @intFromEnum(Act.mute), .checked = v.mute, .enabled = v.has_audio };
        for (0..volumes.len) |i| mb.audio[1 + i] = .{ .label = volume_labels[i], .id = act_volume + @as(u32, @intCast(i)), .checked = v.volume == i, .enabled = v.has_audio, .sep = i == 0 };
        const mv_ok = v.movie_system != null;
        mb.movie = .{
            .{ .label = "Record from power-on", .id = @intFromEnum(Act.movie_record), .enabled = mv_ok and v.movie_mode == .off },
            .{ .label = "Record from here (with a state)", .id = @intFromEnum(Act.movie_record_here), .enabled = mv_ok and v.states and v.movie_mode == .off },
            .{ .label = if (v.movie_mode == .record) "Stop and save" else "Stop", .id = @intFromEnum(Act.movie_stop), .enabled = v.movie_mode != .off },
            .{ .label = "Play movie...", .id = @intFromEnum(Act.movie_play), .enabled = mv_ok and v.movie_mode == .off },
        };
        mb.menus = .{
            .{ .title = "Game", .entries = &mb.game },
            .{ .title = "Speed", .entries = &mb.speed },
            .{ .title = "State", .entries = &mb.state },
            .{ .title = "Movie", .entries = &mb.movie },
            .{ .title = "View", .entries = &mb.view },
            .{ .title = "Audio", .entries = &mb.audio },
        };
        return &mb.menus;
    }
};

/// The file picked in "Open ROM..." (SDL's dialog calls back, maybe from
/// another thread): its path, its length once there.
var picked_buf: [std.fs.max_path_bytes]u8 = undefined;
var picked_len: std.atomic.Value(usize) = .init(0);
/// the dialog was for a movie, not a ROM
var picked_movie = false;

fn pickedCallback(_: ?*anyopaque, files: ?[*]const ?[*:0]const u8, _: c_int) callconv(.c) void {
    const list = files orelse return;
    const f = list[0] orelse return;
    const path = std.mem.span(f);
    if (path.len == 0 or path.len > picked_buf.len) return;
    @memcpy(picked_buf[0..path.len], path);
    picked_len.store(path.len, .release);
}

/// The data directory's folder `sub` ($XDG_DATA_HOME/emulation_clash/sub,
/// else ~/.local/share/emulation_clash/sub: the board's pulls go there).
fn dataDir(buf: []u8, env: *const std.process.Environ.Map, sub: []const u8) ?[]const u8 {
    if (env.get("XDG_DATA_HOME")) |x| if (x.len > 0 and x[0] == '/')
        return std.fmt.bufPrint(buf, "{s}/emulation_clash/{s}", .{ x, sub }) catch null;
    const home = env.get("HOME") orelse return null;
    return std.fmt.bufPrint(buf, "{s}/.local/share/emulation_clash/{s}", .{ home, sub }) catch null;
}

/// The picture as a PNG in the data directory's screens/ (where the
/// board's `make screenshot` puts its own): the ROM's name and the time.
fn screenshot(io: std.Io, gpa: std.mem.Allocator, env: *const std.process.Environ.Map, rom: []const u8, px: []const u32, w: c_int, h: c_int, ui: *ui_mod.Ui, now: u64) void {
    var dbuf: [std.fs.max_path_bytes]u8 = undefined;
    const dir = dataDir(&dbuf, env, "screens") orelse return ui.say(now, "Screenshot: no HOME", .{});
    const stem = std.fs.path.stem(rom);
    var pbuf: [std.fs.max_path_bytes]u8 = undefined;
    const path = std.fmt.bufPrint(&pbuf, "{s}/{s}-{d}.png", .{ dir, stem, time(null) }) catch return;
    const rgb = gpa.alloc(u8, px.len * 3) catch return;
    defer gpa.free(rgb);
    for (px, 0..) |p, i| {
        rgb[3 * i] = @truncate(p >> 16);
        rgb[3 * i + 1] = @truncate(p >> 8);
        rgb[3 * i + 2] = @truncate(p);
    }
    const bytes = png.encode(gpa, @intCast(w), @intCast(h), rgb) catch return ui.say(now, "Screenshot failed", .{});
    defer gpa.free(bytes);
    const cwd = std.Io.Dir.cwd();
    cwd.createDirPath(io, dir) catch {};
    cwd.writeFile(io, .{ .sub_path = path, .data = bytes }) catch |e| return ui.say(now, "Screenshot: {s}", .{@errorName(e)});
    ui.say(now, "Screenshot {s}", .{std.fs.path.basename(path)});
    std.debug.print("desktop: screenshot {s}\n", .{path});
}

/// A save state's file: the data directory's desktop-states/, the ROM's
/// name, the slot (the board's states are other files: its own format).
fn statePath(buf: []u8, env: *const std.process.Environ.Map, rom: []const u8, slot: u8) ?[]const u8 {
    var dbuf: [std.fs.max_path_bytes]u8 = undefined;
    const dir = dataDir(&dbuf, env, "desktop-states") orelse return null;
    return std.fmt.bufPrint(buf, "{s}/{s}.{d}.state", .{ dir, std.fs.path.basename(rom), slot }) catch null;
}

fn saveState(io: std.Io, gpa: std.mem.Allocator, env: *const std.process.Environ.Map, rom: []const u8, slot: u8, emu: *Emu, ui: *ui_mod.Ui, now: u64) void {
    var pbuf: [std.fs.max_path_bytes]u8 = undefined;
    const path = statePath(&pbuf, env, rom, slot) orelse return ui.say(now, "Save state: no HOME", .{});
    const blob = emu.stateSave(gpa) catch |e| return ui.say(now, "Save state: {s}", .{@errorName(e)});
    defer gpa.free(blob);
    const cwd = std.Io.Dir.cwd();
    if (std.fs.path.dirname(path)) |d| cwd.createDirPath(io, d) catch {};
    cwd.writeFile(io, .{ .sub_path = path, .data = blob }) catch |e| return ui.say(now, "Save state: {s}", .{@errorName(e)});
    ui.say(now, "State {d} saved", .{slot});
}

fn loadState(io: std.Io, gpa: std.mem.Allocator, env: *const std.process.Environ.Map, rom: []const u8, slot: u8, emu: *Emu, ui: *ui_mod.Ui, now: u64) bool {
    var pbuf: [std.fs.max_path_bytes]u8 = undefined;
    const path = statePath(&pbuf, env, rom, slot) orelse return false;
    const blob = std.Io.Dir.cwd().readFileAlloc(io, path, gpa, .limited(64 << 20)) catch {
        ui.say(now, "No state in slot {d}", .{slot});
        return false;
    };
    defer gpa.free(blob);
    if (!emu.stateLoad(blob)) {
        ui.say(now, "State {d} doesn't fit this game (or this build)", .{slot});
        return false;
    }
    ui.say(now, "State {d} loaded", .{slot});
    return true;
}

/// Input movies (Q.8): the board's system IDs (Platform.Ps.Link, menu.zig),
/// for the systems whose key word fits the format's 16 bits.
fn movieSystem(e: *const Emu, cgb: bool, gg: bool) ?u8 {
    return switch (e.*) {
        .chip8 => 1,
        .gb => if (cgb) 6 else 2,
        .nes => 3,
        .sms => if (gg) 7 else 8,
        .a800 => 5,
        .snes => 9,
    };
}

/// A recording into the data directory's movies/ (where `make movie-pull`
/// puts the board's): the ROM's name, the time. The header names the game
/// as the board's states do where it can (the Game Boy's header), so the
/// board's `make movie-play` takes it too; elsewhere its size only.
fn saveMovie(io: std.Io, gpa: std.mem.Allocator, env: *const std.process.Environ.Map, rom_path: []const u8, rom: []const u8, system: u8, state: []const u8, frame_bytes: u16, data: []const u8, ui: *ui_mod.Ui, now: u64) void {
    var dbuf: [std.fs.max_path_bytes]u8 = undefined;
    const dir = dataDir(&dbuf, env, "movies") orelse return ui.say(now, "Movie: no HOME", .{});
    var pbuf: [std.fs.max_path_bytes]u8 = undefined;
    const path = std.fmt.bufPrint(&pbuf, "{s}/{s}.{d}.mv1", .{ dir, std.fs.path.stem(rom_path), time(null) }) catch return;
    const id: menu.savestate.RomId = if (system == 2 or system == 6) menu.savestate.RomId.ofGb(rom[0..@min(rom.len, 0x150)], @intCast(rom.len)) else .{ .size = @intCast(rom.len) };
    // (a movie from a state: the desktop's save state in its state area,
    // the format's start = state; the Game Gear's and Master System's are
    // the reference model's layout too, so gg-run plays them)
    // (2 bytes a frame: the board's format exactly; the SNES's and 800XL's
    // wider, the header's frame_bytes)
    const bytes = movie.encodeFileRaw(gpa, .{ .system = system, .frames = 0, .title = id.title, .sum = id.sum, .rom_size = id.size }, state, frame_bytes, data) catch return ui.say(now, "Movie: out of memory", .{});
    const frames = data.len / frame_bytes;
    defer gpa.free(bytes);
    const cwd = std.Io.Dir.cwd();
    cwd.createDirPath(io, dir) catch {};
    cwd.writeFile(io, .{ .sub_path = path, .data = bytes }) catch |e| return ui.say(now, "Movie: {s}", .{@errorName(e)});
    ui.say(now, "Movie saved: {s} ({d} frames)", .{ std.fs.path.basename(path), frames });
    std.debug.print("desktop: movie {s}, {d} frames\n", .{ path, frames });
}

/// A movie file of this system's: its frames' inputs (`frame_bytes`
/// each), and the save state it starts from (empty: from power-on); the
/// caller frees both.
const LoadedMovie = struct { data: []u8, state: []u8 };

fn loadMovie(io: std.Io, gpa: std.mem.Allocator, path: []const u8, system: u8, frame_bytes: u16) !LoadedMovie {
    const bytes = try std.Io.Dir.cwd().readFileAlloc(io, path, gpa, .limited(64 << 20));
    defer gpa.free(bytes);
    const d = try movie.decodeFileRaw(bytes);
    if (d.header.system != system) return error.OtherSystem;
    if (d.header.frame_bytes != frame_bytes) return error.OtherFrameSize;
    const data = try gpa.dupe(u8, d.data);
    errdefer gpa.free(data);
    const state = try gpa.dupe(u8, if (d.header.start == .state) d.state else &.{});
    return .{ .data = data, .state = state };
}

/// The largest frame any system makes.
const max_pixels = @max(snes.width * snes.height, nes.width * nes.height, a800.width * a800.height, sms.max_width * sms.max_height, gb.width * gb.height, chip8.width * chip8.height);

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    const o = parseArgs(args);

    const config = try gpa.create(keymap.Config);
    defer gpa.destroy(config);
    loadConfig(io, gpa, init.environ_map, o.keymap, config);
    const lookup = try gpa.create(keymap.Lookup);
    defer gpa.destroy(lookup);
    const is_nes = std.ascii.endsWithIgnoreCase(o.rom, ".nes");
    const is_chip8 = std.ascii.endsWithIgnoreCase(o.rom, ".ch8");
    const is_gg = std.ascii.endsWithIgnoreCase(o.rom, ".gg");
    const is_sms = std.ascii.endsWithIgnoreCase(o.rom, ".sms");
    const is_xex = std.ascii.endsWithIgnoreCase(o.rom, ".xex");
    // (.rom: a raw 8 or 16 KiB 800XL cartridge; no other system here uses it)
    const is_a800 = is_xex or std.ascii.endsWithIgnoreCase(o.rom, ".car") or std.ascii.endsWithIgnoreCase(o.rom, ".rom") or std.ascii.endsWithIgnoreCase(o.rom, ".cas");
    const is_snes = std.ascii.endsWithIgnoreCase(o.rom, ".sfc") or std.ascii.endsWithIgnoreCase(o.rom, ".smc");
    lookup.fill(config.map(if (is_nes) .nes else if (is_chip8) .chip8 else if (is_gg) .gg else if (is_sms) .sms else if (is_a800) .a800xl else if (is_snes) .snes else .gb));
    const ipf: u16 = if (o.fast) chip8.ipf_fast else chip8.ipf_slow;

    const rom = std.Io.Dir.cwd().readFileAlloc(io, o.rom, gpa, .limited(8 << 20)) catch |e| fail("can't read {s} ({s})", .{ o.rom, @errorName(e) });
    defer gpa.free(rom);
    const is_gb = !is_nes and !is_chip8 and !is_gg and !is_sms and !is_a800 and !is_snes;
    // the SNES's sound module boot ROM (the repo's, from make snes-asm, unless given)
    var ipl_buf: [std.fs.max_path_bytes]u8 = undefined;
    const ipl: ?[]u8 = if (!is_snes) null else blk: {
        const ip = o.ipl orelse try std.fmt.bufPrint(&ipl_buf, "{s}/build/snes/ipl.bin", .{build_options.repo});
        break :blk std.Io.Dir.cwd().readFileAlloc(io, ip, gpa, .limited(1 << 16)) catch |e| fail("can't read the boot ROM {s} ({s}; make snes-asm, or --ipl)", .{ ip, @errorName(e) });
    };
    defer if (ipl) |b| gpa.free(b);
    // the 800XL's OS, BASIC and loader (from the repo unless given)
    var a800_files: [3]?[]u8 = .{ null, null, null };
    defer for (a800_files) |f| if (f) |x| gpa.free(x);
    if (is_a800) {
        var pb: [std.fs.max_path_bytes]u8 = undefined;
        const paths = [3][]const u8{
            o.os orelse try std.fmt.bufPrint(&pb, "{s}/roms/a800xl/os/altirraos_xl.rom", .{build_options.repo}),
            "",
            "",
        };
        a800_files[0] = std.Io.Dir.cwd().readFileAlloc(io, paths[0], gpa, .limited(1 << 20)) catch |e| fail("can't read the OS ROM {s} ({s}; tools/fetch-roms.sh, or --os)", .{ paths[0], @errorName(e) });
        const bp = o.basic orelse try std.fmt.bufPrint(&pb, "{s}/roms/a800xl/os/altirra_basic.rom", .{build_options.repo});
        a800_files[1] = std.Io.Dir.cwd().readFileAlloc(io, bp, gpa, .limited(1 << 20)) catch |e| fail("can't read BASIC {s} ({s}; tools/fetch-roms.sh, or --basic)", .{ bp, @errorName(e) });
        const lp = try std.fmt.bufPrint(&pb, "{s}/build/a800xl/xexload.bin", .{build_options.repo});
        a800_files[2] = std.Io.Dir.cwd().readFileAlloc(io, lp, gpa, .limited(1 << 20)) catch |e| fail("can't read the loader {s} ({s}; make a800-asm)", .{ lp, @errorName(e) });
    }
    // A file named .xex is taken by what it holds (Q.7, the user's games):
    // a binary load file starts with FF FF; a cartridge under that name (a
    // .car's CART header, or a raw 8 or 16 KiB image) runs as one; a
    // cassette image (FUJI) is refused with a message, as no cassette is
    // emulated.
    // a cassette image (FUJI): booted through the OS's SIO call (side
    // track Q's depth 1; the library holds START, then presses Space)
    const is_cas = is_a800 and std.mem.startsWith(u8, rom, "FUJI");
    if (is_cas) std.debug.print("desktop: {s} is a cassette: booting it (START held, then Space){s}\n", .{ o.rom, if (o.real_tape) "; the recorder at its speed (--real-tape)" else "" });
    const xex_is_cart = is_xex and !is_cas and !std.mem.startsWith(u8, rom, "\xff\xff") and
        (std.mem.startsWith(u8, rom, "CART") or rom.len == 8192 or rom.len == 16384);
    if (xex_is_cart) std.debug.print("desktop: {s} holds a cartridge, not an executable: running it as one\n", .{o.rom});
    const a800_program: a800.Program = if (is_cas and o.real_tape) .{ .cas_real = rom } else if (is_cas) .{ .cas = rom } else if (is_xex and !xex_is_cart) .{ .xex = .{ .loader = a800_files[2] orelse &.{}, .file = rom } } else .{ .cart = rom };
    const bios: ?[]u8 = if (o.bios) |b| (std.Io.Dir.cwd().readFileAlloc(io, b, gpa, .limited(1 << 20)) catch |e| fail("can't read {s} ({s})", .{ b, @errorName(e) })) else null;
    defer if (bios) |b| gpa.free(b);
    const sms_mode: sms.Mode = if (is_gg) .gg else .sms;
    if (is_gb and rom.len < 0x150) fail("{s}: too short for a Game Boy ROM", .{o.rom});
    const cgb_cart = is_gb and rom[0x143] & 0x80 != 0;
    const model: gb.Model = if (cgb_cart or o.gbc) .cgb else .dmg;
    var emu: Emu = if (is_nes)
        .{ .nes = nes.Machine.initWith(rom, o.reference) catch |e| fail("{s}: {s}", .{ o.rom, @errorName(e) }) }
    else if (is_chip8)
        .{ .chip8 = chip8.Machine.init(rom, o.quirks, ipf) catch |e| fail("{s}: {s}", .{ o.rom, @errorName(e) }) }
    else if (is_gg or is_sms)
        .{ .sms = sms.Machine.initWith(rom, sms_mode, !o.no_fm, bios, o.reference) catch |e| fail("{s}: {s}", .{ o.rom, @errorName(e) }) }
    else if (is_a800)
        .{ .a800 = a800.Machine.initWith(a800_files[0].?, a800_files[1].?, a800_program, o.reference) catch |e| {
            // a .car's type in its header (bytes 4-7, big-endian): say which
            if (e == error.CartRefused and std.mem.startsWith(u8, rom, "CART") and rom.len >= 8)
                fail("{s}: a cartridge of type {d}, which the 800XL doesn't support yet (types 1 and 2, the standard 8 and 16 KiB ones, and 12 to 14, XEGS of 32 to 128 KiB)", .{ o.rom, std.mem.readInt(u32, rom[4..8], .big) });
            fail("{s}: {s}", .{ o.rom, @errorName(e) });
        } }
    else if (is_snes)
        .{ .snes = snes.Machine.initFull(ipl.?, rom, o.render_threads.?, o.reference) catch |e| fail("{s}: {s}", .{ o.rom, @errorName(e) }) }
    else
        .{ .gb = gb.Machine.init(rom, model) catch |e| fail("{s}: {s}", .{ o.rom, @errorName(e) }) };
    defer emu.deinit();
    // the SNES's picture one frame late (its render threads run on)
    if (o.frame_lag and o.render_threads.? > 0) switch (emu) {
        .snes => |*m| m.lag(true),
        else => {},
    };

    // the battery save, the board's file (the GBC's DMG games below gbc/gb)
    const sys_dir: []const u8 = if (is_gg) "gg" else if (is_sms) "sms" else if (cgb_cart) "gbc" else if (o.gbc) "gbc/gb" else "gb";
    const sys = blk: {
        const want: []const u8 = if (is_gg) "gg" else if (is_sms) "sms" else if (model == .cgb) "gbc" else "gb";
        for (&menu.systems) |*x| if (std.mem.eql(u8, x.dir, want)) break :blk x;
        unreachable;
    };
    var saves_buf: [std.fs.max_path_bytes]u8 = undefined;
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    var battery: ?Battery = null;
    if (!o.no_save and (is_gb or is_gg or is_sms)) {
        if (o.saves orelse save.defaultDir(&saves_buf, init.environ_map)) |dir| {
            if (save.cardPath(o.rom)) |cp| {
                const path = save.savePath(&path_buf, dir, sys_dir, cp) catch fail("save path too long", .{});
                battery = (if (is_gb) Battery.open(io, gpa, &emu.gb, sys, rom, path) else Battery.open(io, gpa, &emu.sms, sys, rom, path)) catch |e| fail("can't read {s} ({s})", .{ path, @errorName(e) });
            }
        }
    }
    defer if (battery) |*b| {
        flushBattery(b, io, &emu);
        b.deinit(gpa);
    };

    if (!SDL_Init(SDL_INIT_VIDEO | SDL_INIT_GAMEPAD)) sdlFail("SDL_Init");
    defer SDL_Quit();
    const title: [*:0]const u8 = if (is_nes) "NES (spec)" else if (is_chip8) "CHIP-8 (spec)" else if (is_gg) "Game Gear (spec)" else if (is_sms) "Master System (spec)" else if (is_a800) "Atari 800XL (spec, fast version)" else if (is_snes) "Super Nintendo (spec, fast version)" else if (model == .cgb) "Game Boy Color (spec)" else "Game Boy (spec)";
    const w = emu.width();
    const h = emu.height();
    // CHIP-8's 64 × 32 at 10 unless --scale says otherwise
    const scale: c_int = if (o.scale_given) o.scale else if (is_chip8) 10 else if (is_a800) 3 else if (is_snes) 2 else o.scale;
    const win = SDL_CreateWindow(title, w * scale, h * scale, SDL_WINDOW_RESIZABLE) orelse sdlFail("SDL_CreateWindow");
    defer SDL_DestroyWindow(win);
    const ren = SDL_CreateRenderer(win, null) orelse sdlFail("SDL_CreateRenderer");
    defer SDL_DestroyRenderer(ren);
    // integer scaling, letterboxed (SDL_LOGICAL_PRESENTATION_INTEGER_SCALE)
    _ = SDL_SetRenderLogicalPresentation(ren, w, h, 4);
    const tex = SDL_CreateTexture(ren, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, w, h) orelse sdlFail("SDL_CreateTexture");
    defer SDL_DestroyTexture(tex);
    _ = SDL_SetTextureScaleMode(tex, SDL_SCALEMODE_NEAREST);

    // sound: optional (no device, or --unpaced: the clock paces, or nothing)
    var audio: ?*SDL_AudioStream = null;
    if (!o.unpaced and SDL_Init(SDL_INIT_AUDIO)) {
        const spec: SDL_AudioSpec = .{ .format = SDL_AUDIO_S16LE, .channels = 2, .freq = 48000 };
        audio = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &spec, null, null);
        if (audio) |a| _ = SDL_ResumeAudioStreamDevice(a) else std.debug.print("desktop: no sound ({s})\n", .{std.mem.span(SDL_GetError())});
    }
    defer if (audio) |a| SDL_DestroyAudioStream(a);
    // about three frames of sound queued at most (4 bytes a stereo sample)
    const queue_max: c_int = 3 * 804 * 4;

    var view: View = .{ .scale = scale, .has_audio = audio != null, .states = emu.hasStates(o.reference), .movie_system = movieSystem(&emu, model == .cgb, is_gg), .brackets = !is_a800 };
    // a movie's frames, `fb` bytes each
    var mv_data: std.ArrayList(u8) = .empty;
    defer mv_data.deinit(gpa);
    const fb: usize = emu.frameBytes();
    var mv_pos: usize = 0;
    var movie_start: MovieMode = .off;
    // the state a movie starts from (empty: power-on)
    var mv_state: []u8 = &.{};
    defer gpa.free(mv_state);
    var first_pass = true;
    // --movie: played from power-on
    if (o.movie) |path| {
        const sys_id = view.movie_system orelse fail("--movie: no movies for this system yet", .{});
        const lm = loadMovie(io, gpa, path, sys_id, emu.frameBytes()) catch |e| fail("--movie {s}: {s}", .{ path, @errorName(e) });
        defer gpa.free(lm.data);
        try mv_data.appendSlice(gpa, lm.data);
        mv_state = lm.state;
        movie_start = .play;
    }
    var ui: ui_mod.Ui = .{};
    var mb: MenuBuf = .{};
    var speed_frames: u64 = 0;
    var speed_t0 = SDL_GetTicksNS();
    var ff_was = false;
    var speed_text_buf: [32]u8 = undefined;
    var speed_text: []const u8 = "";

    var pads: [8]*SDL_Gamepad = undefined;
    var npads: usize = 0;
    defer for (pads[0..npads]) |p| SDL_CloseGamepad(p);

    var px: [max_pixels]u32 = undefined;
    var snd: [2 * gb.audio_cap]i16 = undefined;
    const npx: usize = @intCast(w * h);
    var paused = false;
    var step_once = false;
    var adv_was = false;
    var run_was = false;
    var reset_was = false;
    var count: u64 = 0;
    // the debugger's socket (side track R): rota-dbg connects to it
    var dbg: ?debug.Server = null;
    defer if (dbg) |*d| d.close();
    if (o.debug) {
        var pbuf: [256]u8 = undefined;
        const path = o.debug_socket orelse debug.defaultPath(&pbuf, init.environ_map);
        dbg = debug.Server.open(path) catch |e| fail("--debug: {s}: {s}", .{ path, @errorName(e) });
        std.debug.print("desktop: the debugger listens on {s} (rota-dbg connects to it)\n", .{path});
    }
    // a frame a breakpoint stopped part way: its key word, for the rest
    var dbg_mid: ?keymap.Word = null;
    var skipped: u32 = 0;
    var skips_total: u64 = 0;
    const start = SDL_GetTicksNS();
    var due = start;

    outer: while (true) {
        var ev: SDL_Event = undefined;
        // the menu's contents as things are now, and what it was asked
        const menus = mb.build(view);
        var act: ?u32 = null;
        // (--movie: the first pass starts it from power-on)
        var want_reset = first_pass and movie_start != .off;
        first_pass = false;
        // the debugger's commands (one pass of the socket, non-blocking)
        if (dbg) |*d| {
            d.service(switch (emu) {
                .sms => |*m| m,
                else => null,
            }, count);
            // held: the picture as the steps left it
            if (d.paused) {
                emu.pixels(&px);
                _ = SDL_UpdateTexture(tex, null, &px, w * 4);
            }
        }
        const dbg_holds = if (dbg) |d| d.paused else false;
        while (SDL_PollEvent(&ev)) switch (ev.type) {
            SDL_EVENT_QUIT => break :outer,
            // SDL_KeyboardEvent: the scancode at byte 24 (pad[4..8])
            SDL_EVENT_KEY_DOWN => {
                const sc = std.mem.readInt(u32, ev.pad[4..8], .little);
                if (ui.state == .open) {
                    const k: ?ui_mod.Key = switch (sc) {
                        SCANCODE_LEFT => .left,
                        SCANCODE_RIGHT => .right,
                        SCANCODE_UP => .up,
                        SCANCODE_DOWN => .down,
                        SCANCODE_RETURN, SCANCODE_KP_ENTER => .enter,
                        SCANCODE_ESCAPE => .escape,
                        else => null,
                    };
                    if (k) |kk| act = ui.key(menus, kk) orelse act;
                } else if (sc == SCANCODE_ESCAPE) {
                    ui.toggle(menus);
                } else if (ev.pad[17] == 0) {
                    // the shortcuts (not on a key's repeats)
                    if (sc == SCANCODE_F7) act = @intFromEnum(Act.state_save);
                    if (sc == SCANCODE_F9) act = @intFromEnum(Act.state_load);
                    // [ and ] as F7 and F9 (the user, 2026-10-31), but the
                    // 800XL's: its keyboard is the PC's, and both are its keys
                    if (!is_a800 and sc == SCANCODE_LEFTBRACKET) act = @intFromEnum(Act.state_save);
                    if (!is_a800 and sc == SCANCODE_RIGHTBRACKET) act = @intFromEnum(Act.state_load);
                    if (usb_keys.usageCode(@intCast(@min(sc, 0xFF)))) |code| if (config.screenshot.has(code)) {
                        act = @intFromEnum(Act.screenshot);
                    };
                }
            },
            // SDL_MouseMotionEvent, SDL_MouseButtonEvent: x and y at bytes
            // 28 and 32 (window points), the button at byte 24
            SDL_EVENT_MOUSE_MOTION, SDL_EVENT_MOUSE_BUTTON_DOWN => {
                const d = SDL_GetWindowPixelDensity(win);
                const x = @as(f32, @bitCast(std.mem.readInt(u32, ev.pad[8..12], .little))) * d;
                const y = @as(f32, @bitCast(std.mem.readInt(u32, ev.pad[12..16], .little))) * d;
                if (ev.type == SDL_EVENT_MOUSE_MOTION) ui.motion(menus, x, y) else if (ev.pad[4] == 1) act = ui.click(menus, x, y) orelse act;
            },
            SDL_EVENT_GAMEPAD_ADDED => if (npads < pads.len) {
                if (SDL_OpenGamepad(ev.which)) |p| {
                    pads[npads] = p;
                    npads += 1;
                }
            },
            SDL_EVENT_GAMEPAD_REMOVED => for (pads[0..npads], 0..) |p, i| if (SDL_GetGamepadID(p) == ev.which) {
                SDL_CloseGamepad(p);
                pads[i] = pads[npads - 1];
                npads -= 1;
                break;
            },
            else => {},
        };

        // the menu's entries and the shortcuts
        const now = SDL_GetTicksNS();
        if (act) |id| switch (id) {
            @intFromEnum(Act.pause) => paused = !paused,
            @intFromEnum(Act.advance) => if (paused) {
                step_once = true;
            } else {
                paused = true;
            },
            @intFromEnum(Act.reset) => want_reset = true,
            @intFromEnum(Act.quit) => break :outer,
            @intFromEnum(Act.open_rom) => {
                const filters = [_]SDL_DialogFileFilter{
                    .{ .name = "ROMs", .pattern = "gb;gbc;nes;gg;sms;ch8;xex;car;rom;cas;sfc;smc" },
                    .{ .name = "All files", .pattern = "*" },
                };
                SDL_ShowOpenFileDialog(pickedCallback, null, win, &filters, filters.len, null, false);
            },
            @intFromEnum(Act.movie_record) => {
                movie_start = .record;
                gpa.free(mv_state);
                mv_state = &.{};
                want_reset = true;
            },
            @intFromEnum(Act.movie_record_here) => if (emu.stateSave(gpa)) |blob| {
                // the state now, then the input from the next frame on; no
                // battery save written from here (as from power-on)
                gpa.free(mv_state);
                mv_state = blob;
                if (battery) |*b| {
                    flushBattery(b, io, &emu);
                    b.deinit(gpa);
                    battery = null;
                }
                mv_data.clearRetainingCapacity();
                mv_pos = 0;
                view.movie_mode = .record;
                ui.say(now, "Recording from here (with a state; no battery saves now)", .{});
            } else |e| ui.say(now, "Movie: {s}", .{@errorName(e)}),
            @intFromEnum(Act.movie_stop) => {
                if (view.movie_mode == .record) saveMovie(io, gpa, init.environ_map, o.rom, rom, view.movie_system.?, mv_state, emu.frameBytes(), mv_data.items, &ui, now) else ui.say(now, "Movie stopped at frame {d}", .{mv_pos});
                view.movie_mode = .off;
            },
            @intFromEnum(Act.movie_play) => {
                const filters = [_]SDL_DialogFileFilter{
                    .{ .name = "Movies", .pattern = "mv1" },
                    .{ .name = "All files", .pattern = "*" },
                };
                picked_movie = true;
                SDL_ShowOpenFileDialog(pickedCallback, null, win, &filters, filters.len, null, false);
            },
            @intFromEnum(Act.screenshot) => screenshot(io, gpa, init.environ_map, o.rom, px[0..npx], w, h, &ui, now),
            @intFromEnum(Act.state_save) => if (view.states) saveState(io, gpa, init.environ_map, o.rom, view.slot, &emu, &ui, now) else ui.say(now, "No save states for this system yet", .{}),
            @intFromEnum(Act.state_load) => if (!view.states) {
                ui.say(now, "No save states for this system yet", .{});
            } else if (loadState(io, gpa, init.environ_map, o.rom, view.slot, &emu, &ui, now)) {
                // the picture as the state left it, no old sound queued
                if (audio) |a| _ = SDL_ClearAudioStream(a);
                emu.pixels(&px);
                _ = SDL_UpdateTexture(tex, null, &px, w * 4);
            },
            @intFromEnum(Act.fullscreen) => {
                view.fullscreen = !view.fullscreen;
                _ = SDL_SetWindowFullscreen(win, view.fullscreen);
            },
            @intFromEnum(Act.integer) => {
                view.integer = !view.integer;
                // SDL_LOGICAL_PRESENTATION_INTEGER_SCALE, else LETTERBOX
                _ = SDL_SetRenderLogicalPresentation(ren, w, h, if (view.integer) 4 else 2);
            },
            @intFromEnum(Act.smooth) => {
                view.smooth = !view.smooth;
                _ = SDL_SetTextureScaleMode(tex, if (view.smooth) 1 else SDL_SCALEMODE_NEAREST);
            },
            @intFromEnum(Act.show_speed) => view.show_speed = !view.show_speed,
            @intFromEnum(Act.mute) => {
                view.mute = !view.mute;
                if (audio) |a| _ = SDL_SetAudioStreamGain(a, if (view.mute) 0 else @as(f32, @floatFromInt(volumes[view.volume])) / 100);
            },
            else => if (id >= act_speed and id < act_speed + speeds.len) {
                view.speed = id - act_speed;
                if (audio) |a| {
                    // the sound plays faster or slower with the game (its
                    // pitch with it); unlimited: none queued
                    const sp = speeds[view.speed];
                    _ = SDL_SetAudioStreamFrequencyRatio(a, if (sp == 0) 1 else @as(f32, @floatFromInt(sp)) / 100);
                    _ = SDL_ClearAudioStream(a);
                }
                due = SDL_GetTicksNS();
                ui.say(now, "Speed {s}", .{speed_labels[view.speed]});
            } else if (id > act_slot and id <= act_slot + slots) {
                view.slot = @intCast(id - act_slot);
                ui.say(now, "State slot {d}", .{view.slot});
            } else if (id > act_scale and id <= act_scale + scale_labels.len) {
                view.scale = @intCast(id - act_scale);
                _ = SDL_SetWindowSize(win, w * view.scale, h * view.scale);
            } else if (id >= act_volume and id < act_volume + volumes.len) {
                view.volume = id - act_volume;
                view.mute = false;
                if (audio) |a| _ = SDL_SetAudioStreamGain(a, @as(f32, @floatFromInt(volumes[view.volume])) / 100);
            },
        };
        view.paused = paused;
        // Open ROM...: the program again on the picked file (the same
        // options), the battery save written first
        const pl = picked_len.load(.acquire);
        if (pl > 0 and picked_movie) {
            picked_movie = false;
            picked_len.store(0, .release);
            if (loadMovie(io, gpa, picked_buf[0..pl], view.movie_system.?, emu.frameBytes())) |lm| {
                mv_data.clearRetainingCapacity();
                mv_data.appendSlice(gpa, lm.data) catch {};
                gpa.free(lm.data);
                gpa.free(mv_state);
                mv_state = lm.state;
                movie_start = .play;
                want_reset = true;
            } else |e| ui.say(now, "Movie: {s}", .{@errorName(e)});
        } else if (pl > 0) {
            if (battery) |*b| flushBattery(b, io, &emu);
            const argv = try init.arena.allocator().allocSentinel(?[*:0]const u8, args.len + 1, null);
            const pz = try init.arena.allocator().dupeZ(u8, picked_buf[0..pl]);
            var have_rom = false;
            for (args, 0..) |a, i| {
                const is_rom = a.ptr == o.rom.ptr;
                have_rom = have_rom or is_rom;
                argv[i] = if (is_rom) pz.ptr else a.ptr;
            }
            argv[args.len] = null;
            if (have_rom) {
                SDL_Quit();
                _ = execv("/proc/self/exe", argv);
                fail("can't start again on {s}", .{pz});
            }
            picked_len.store(0, .release);
        }

        // frame advance and run (the [global] keys), reset: on the press
        const adv = bindingHeld(&config.frame_advance);
        const run = bindingHeld(&config.frame_run);
        const reset = SDL_GetKeyboardState(null)[SCANCODE_F5];
        if (ui.state != .open) {
            if (adv and !adv_was) {
                if (paused) step_once = true else paused = true;
            }
            if (run and !run_was) paused = false;
            if (reset and !reset_was) want_reset = true;
        }
        if (want_reset) {
            if (battery) |*b| flushBattery(b, io, &emu);
            // a movie starts at power-on with cartridge RAM as a new
            // machine has it, and no battery save is written from then on
            // (as the board's: a movie's game never overwrites the save)
            if (movie_start != .off) if (battery) |*b| {
                b.deinit(gpa);
                battery = null;
            };
            emu.deinit();
            emu = if (is_nes) .{ .nes = nes.Machine.initWith(rom, o.reference) catch unreachable } else if (is_chip8) .{ .chip8 = chip8.Machine.init(rom, o.quirks, ipf) catch unreachable } else if (is_gg or is_sms) .{ .sms = sms.Machine.initWith(rom, sms_mode, !o.no_fm, bios, o.reference) catch unreachable } else if (is_a800) .{ .a800 = a800.Machine.initWith(a800_files[0].?, a800_files[1].?, a800_program, o.reference) catch unreachable } else if (is_snes) .{ .snes = snes.Machine.initFull(ipl.?, rom, o.render_threads.?, o.reference) catch unreachable } else .{ .gb = gb.Machine.init(rom, model) catch unreachable };
            if (battery) |*b| switch (emu) {
                .gb => |*m| m.cramSet(b.file[0..b.ram]),
                .sms => |*m| m.cramSet(b.file[0..b.ram]),
                else => {},
            };
            switch (movie_start) {
                .off => ui.say(now, "Reset", .{}),
                .record => {
                    mv_data.clearRetainingCapacity();
                    ui.say(now, "Recording from power-on (no battery saves now)", .{});
                },
                .play => if (mv_state.len == 0) {
                    ui.say(now, "Playing a movie of {d} frames (no battery saves now)", .{mv_data.items.len / fb});
                } else if (emu.stateLoad(mv_state)) {
                    // from its state: the machine as the movie's start
                    ui.say(now, "Playing a movie of {d} frames from its state (no battery saves now)", .{mv_data.items.len / fb});
                } else {
                    ui.say(now, "Movie: its state doesn't fit this game (or this build)", .{});
                    movie_start = .off;
                },
            }
            if (movie_start != .off) {
                view.movie_mode = movie_start;
                mv_pos = 0;
                count = 0;
            }
            movie_start = .off;
        }
        adv_was = adv;
        run_was = run;
        reset_was = reset;

        // fast forward while TAB is held (the user, 2026-10-31: at
        // --ff-speed, 200% unless given; not the 800XL's, whose TAB it is):
        // the speed as the Speed menu sets it, sound and all, until released
        const ff = !is_a800 and ui.state != .open and SDL_GetKeyboardState(null)[SCANCODE_TAB];
        const speed_now: u16 = if (ff) o.ff_speed else speeds[view.speed];
        if (ff != ff_was) {
            if (audio) |a| {
                _ = SDL_SetAudioStreamFrequencyRatio(a, if (speed_now == 0) 1 else @as(f32, @floatFromInt(speed_now)) / 100);
                _ = SDL_ClearAudioStream(a);
            }
            due = SDL_GetTicksNS();
            ff_was = ff;
        }
        const unlimited = o.unpaced or speed_now == 0;
        // a frame's time at this speed
        const frame_ns = if (unlimited) emu.frameNs() else emu.frameNs() * 100 / speed_now;
        if ((!paused and !ui.holdsGame() and !dbg_holds) or step_once) {
            step_once = false;
            const report = bootReport();
            // frame skipping: behind real time (less than a frame of sound
            // queued, or the clock past the frame's time), the next frame
            // isn't drawn; at most three in a row, so the picture moves.
            // Unlimited speed: three in four not drawn
            const behind = if (unlimited) !o.unpaced else if (audio) |a| SDL_GetAudioStreamQueued(a) < 804 * 4 else SDL_GetTicksNS() > due + frame_ns;
            const skip_now = o.frameskip and behind and skipped < 3;
            emu.skip(skip_now);
            skipped = if (skip_now) skipped + 1 else 0;
            if (skip_now) skips_total += 1;
            // (the rest of a frame a breakpoint stopped: its word again)
            const word = dbg_mid orelse held(lookup, pads[0..npads]);
            var rec_buf: [16]u8 = undefined;
            // the frame's input as a movie has it (null: no movie)
            var rec: ?[]const u8 = null;
            if (dbg_mid == null) switch (view.movie_mode) {
                .play => if (mv_pos < mv_data.items.len / fb) {
                    rec = mv_data.items[mv_pos * fb ..][0..fb];
                    mv_pos += 1;
                } else {
                    view.movie_mode = .off;
                    ui.say(now, "Movie ended at frame {d}", .{mv_pos});
                    if (o.movie != null and o.frames == null) break :outer;
                },
                .record => {
                    rec = emu.record(word, &report, &rec_buf);
                    mv_data.appendSlice(gpa, rec.?) catch {};
                },
                .off => {},
            };
            // the debugger's run (breakpoints set, or a "frame" stop asked;
            // the Game Gear and Master System; not while a movie plays or
            // records, whose words are per frame): to a breakpoint or the
            // frame's end
            const dbg_m: ?*sms.Machine = if (dbg) |d| (if (rec == null and (d.n_breaks > 0 or d.pause_at_frame)) switch (emu) {
                .sms => |*m| m,
                else => null,
            } else null) else null;
            var frame_done = true;
            const n = if (dbg_m) |m| blk: {
                const stop = m.run(word, dbg.?.breakList()) catch .frame_end;
                const k = m.audio(&snd);
                if (stop == .breakpoint) {
                    frame_done = false;
                    dbg_mid = word;
                    dbg.?.stopped(m, "break");
                } else {
                    dbg_mid = null;
                    if (dbg.?.pause_at_frame) {
                        dbg.?.pause_at_frame = false;
                        dbg.?.stopped(m, "frame");
                    }
                }
                break :blk k;
            } else if (rec) |r| emu.frameRec(r, &report) else emu.frame(word, &report);
            if (frame_done) count += 1;
            // the battery save every ten seconds or so, when it changed
            if (count % 600 == 0) if (battery) |*b| flushBattery(b, io, &emu);
            if (audio) |a| {
                _ = emu.audio(&snd);
                if (!unlimited) _ = SDL_PutAudioStreamData(a, &snd, @intCast(4 * n));
            }
            speed_frames += 1;
            emu.pixels(&px);
            _ = SDL_UpdateTexture(tex, null, &px, w * 4);
        }
        _ = SDL_RenderClear(ren);
        _ = SDL_RenderTexture(ren, tex, null, null);
        // the speed (once a second, in percent of real time), the menu
        if (now - speed_t0 >= 1_000_000_000) {
            const pct = @as(f64, @floatFromInt(speed_frames)) * @as(f64, @floatFromInt(emu.frameNs())) / @as(f64, @floatFromInt(now - speed_t0)) * 100;
            speed_text = std.fmt.bufPrint(&speed_text_buf, "{d:.0}%", .{pct}) catch "";
            speed_frames = 0;
            speed_t0 = now;
        }
        var mv_text_buf: [32]u8 = undefined;
        const mv_text: ?[]const u8 = switch (view.movie_mode) {
            .record => std.fmt.bufPrint(&mv_text_buf, "REC {d}", .{mv_data.items.len / fb}) catch null,
            .play => std.fmt.bufPrint(&mv_text_buf, "PLAY {d}/{d}", .{ mv_pos, mv_data.items.len / fb }) catch null,
            .off => null,
        };
        var ff_text_buf: [16]u8 = undefined;
        const ff_text: ?[]const u8 = if (!ff) null else if (o.ff_speed == 0) "FF" else std.fmt.bufPrint(&ff_text_buf, "FF {d}%", .{o.ff_speed}) catch null;
        const corner: ?[]const u8 = if (paused) "Paused" else if (mv_text) |t| t else if (ff_text) |t| t else if (view.show_speed) speed_text else null;
        ui_mod.draw(&ui, ren, mb.build(view), now, corner);
        _ = SDL_RenderPresent(ren);
        if (o.frames) |f| if (count >= f) break;

        // pacing: by the sound queue, else by the clock
        const waiting = paused or ui.holdsGame();
        if (unlimited and !waiting) continue;
        if (audio) |a| {
            if (!waiting) while (SDL_GetAudioStreamQueued(a) > queue_max) SDL_DelayNS(1_000_000);
            if (waiting) SDL_DelayNS(emu.frameNs());
        } else {
            due += frame_ns;
            const t = SDL_GetTicksNS();
            if (due > t) SDL_DelayNS(due - t) else due = t;
        }
    }

    const secs = @as(f64, @floatFromInt(SDL_GetTicksNS() - start)) / 1e9;
    const real = @as(f64, @floatFromInt(count)) * @as(f64, @floatFromInt(emu.frameNs())) / 1e9;
    std.debug.print("desktop: {d} frames in {d:.2} s ({d:.2}x real time)\n", .{ count, secs, if (secs > 0) real / secs else 0 });
    if (skips_total > 0) std.debug.print("desktop: {d} frames not drawn (frame skipping; --no-frameskip draws every one)\n", .{skips_total});
    if (o.expect_png) |path| {
        const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, gpa, .limited(16 << 20)) catch |e| fail("can't read {s} ({s})", .{ path, @errorName(e) });
        defer gpa.free(bytes);
        var img = png.decode(gpa, bytes) catch |e| fail("{s}: {s}", .{ path, @errorName(e) });
        defer img.deinit(gpa);
        if (img.width != w or img.height != h) fail("FAIL {s} is {d}x{d}", .{ path, img.width, img.height });
        var diff: usize = 0;
        for (px[0..npx], 0..) |p, i| {
            const want = 0xFF000000 | (@as(u32, img.rgb[3 * i]) << 16) | (@as(u32, img.rgb[3 * i + 1]) << 8) | img.rgb[3 * i + 2];
            if (p != want) diff += 1;
        }
        if (diff != 0) fail("FAIL {d} pixels differ from {s}", .{ diff, path });
        std.debug.print("desktop: the last frame equals {s}\n", .{path});
    }
    if (o.png_out) |path| {
        var rgb: [max_pixels * 3]u8 = undefined;
        for (px[0..npx], 0..) |p, i| {
            rgb[3 * i] = @truncate(p >> 16);
            rgb[3 * i + 1] = @truncate(p >> 8);
            rgb[3 * i + 2] = @truncate(p);
        }
        const bytes = try png.encode(gpa, @intCast(w), @intCast(h), rgb[0 .. 3 * npx]);
        defer gpa.free(bytes);
        try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = path, .data = bytes });
    }
}
