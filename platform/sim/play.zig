//! Layer 4, interactive (docs/TESTING.md, `make play`): runs the Verilator
//! model of a board top and shows its VGA output in an SDL3 window, about
//! in real time, with the keyboard standing in for the board's buttons and
//! switches.
//!
//!   vga-play [--sw X] [--frames N]
//!
//! Keys: arrows = BTNU/BTND/BTNL/BTNR, space = BTNC, F1 to F8 toggle SW0 to
//! SW7 (printed when they change), Esc or closing the window quits.
//! --sw sets the switches at start; --frames N quits after N frames (for a
//! quick smoke test, e.g. with SDL_VIDEODRIVER=dummy).
//!
//! SDL3 is a C library called directly (CLAUDE.md rule 9); only the dozen
//! functions used here are declared, below. (SDL3 rather than SDL2: nixpkgs'
//! SDL2 is sdl2-compat, a layer over SDL3 that loads it at run time.)

const std = @import("std");
const vga = @import("vga.zig");
const vtop = @import("vtop.zig");

// --- SDL3 (SDL3/SDL.h), only what is used -----------------------------
const SDL_Window = opaque {};
const SDL_Renderer = opaque {};
const SDL_Texture = opaque {};
const SDL_Event = extern struct { type: u32, pad: [124]u8 };

const SDL_INIT_VIDEO: u32 = 0x20;
const SDL_PIXELFORMAT_RGB24: u32 = 0x17101803;
const SDL_TEXTUREACCESS_STREAMING: c_int = 1;
const SDL_EVENT_QUIT: u32 = 0x100;

const SCANCODE_RIGHT = 79;
const SCANCODE_LEFT = 80;
const SCANCODE_DOWN = 81;
const SCANCODE_UP = 82;
const SCANCODE_SPACE = 44;
const SCANCODE_ESCAPE = 41;
const SCANCODE_F1 = 58;

extern fn SDL_Init(flags: u32) bool;
extern fn SDL_Quit() void;
extern fn SDL_GetError() [*:0]const u8;
extern fn SDL_CreateWindow(title: [*:0]const u8, w: c_int, h: c_int, flags: u64) ?*SDL_Window;
extern fn SDL_DestroyWindow(w: *SDL_Window) void;
extern fn SDL_CreateRenderer(w: *SDL_Window, name: ?[*:0]const u8) ?*SDL_Renderer;
extern fn SDL_DestroyRenderer(r: *SDL_Renderer) void;
extern fn SDL_CreateTexture(r: *SDL_Renderer, format: u32, access: c_int, w: c_int, h: c_int) ?*SDL_Texture;
extern fn SDL_DestroyTexture(t: *SDL_Texture) void;
extern fn SDL_UpdateTexture(t: *SDL_Texture, rect: ?*const anyopaque, pixels: *const anyopaque, pitch: c_int) bool;
extern fn SDL_RenderClear(r: *SDL_Renderer) bool;
extern fn SDL_RenderTexture(r: *SDL_Renderer, t: *SDL_Texture, src: ?*const anyopaque, dst: ?*const anyopaque) bool;
extern fn SDL_RenderPresent(r: *SDL_Renderer) bool;
extern fn SDL_PollEvent(e: *SDL_Event) bool;
extern fn SDL_GetKeyboardState(numkeys: ?*c_int) [*]const bool;
extern fn SDL_GetTicks() u64;
extern fn SDL_Delay(ms: u32) void;

fn sdlFail(what: []const u8) noreturn {
    std.debug.print("vga-play: {s}: {s}\n", .{ what, std.mem.span(SDL_GetError()) });
    std.process.exit(1);
}

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    var sw: u8 = 0;
    var max_frames: ?u64 = null;
    var i: usize = 1;
    while (i + 1 < args.len) : (i += 2) {
        if (std.mem.eql(u8, args[i], "--sw")) {
            sw = try std.fmt.parseInt(u8, args[i + 1], 0);
        } else if (std.mem.eql(u8, args[i], "--frames")) {
            max_frames = try std.fmt.parseInt(u64, args[i + 1], 0);
        } else {
            std.debug.print("usage: vga-play [--sw X] [--frames N]\n", .{});
            std.process.exit(2);
        }
    }

    const mode = vga.vga640x480;
    var dec = try vga.Decoder.init(gpa, mode);
    defer dec.deinit(gpa);

    if (!SDL_Init(SDL_INIT_VIDEO)) sdlFail("SDL_Init");
    defer SDL_Quit();
    const win = SDL_CreateWindow("vga-play", 640, 480, 0) orelse sdlFail("SDL_CreateWindow");
    defer SDL_DestroyWindow(win);
    const ren = SDL_CreateRenderer(win, null) orelse sdlFail("SDL_CreateRenderer");
    defer SDL_DestroyRenderer(ren);
    const tex = SDL_CreateTexture(ren, SDL_PIXELFORMAT_RGB24, SDL_TEXTUREACCESS_STREAMING, 640, 480) orelse sdlFail("SDL_CreateTexture");
    defer SDL_DestroyTexture(tex);

    const top = vtop.vtop_new();
    defer vtop.vtop_free(top);

    std.debug.print("vga-play: arrows = BTNU/D/L/R, space = BTNC, F1..F8 toggle SW0..SW7, Esc quits\n", .{});
    std.debug.print("vga-play: SW = 0x{x:0>2}\n", .{sw});

    var prev_f = [_]bool{false} ** 8;
    var frames: u64 = 0;
    const start = SDL_GetTicks();
    var pins: vga.Pins = undefined;
    var btn: u8 = 0;
    vtop.vtop_set_inputs(top, btn, sw);
    outer: while (true) {
        vtop.vtop_tick(top, &pins);
        if (!dec.feed(pins)) continue;
        frames += 1;

        _ = SDL_UpdateTexture(tex, null, dec.pixels.ptr, 640 * 3);
        _ = SDL_RenderClear(ren);
        _ = SDL_RenderTexture(ren, tex, null, null);
        _ = SDL_RenderPresent(ren);

        var ev: SDL_Event = undefined;
        while (SDL_PollEvent(&ev)) {
            if (ev.type == SDL_EVENT_QUIT) break :outer;
        }
        const keys = SDL_GetKeyboardState(null);
        if (keys[SCANCODE_ESCAPE]) break;
        btn = 0;
        if (keys[SCANCODE_SPACE]) btn |= 1 << 0;
        if (keys[SCANCODE_DOWN]) btn |= 1 << 1;
        if (keys[SCANCODE_LEFT]) btn |= 1 << 2;
        if (keys[SCANCODE_RIGHT]) btn |= 1 << 3;
        if (keys[SCANCODE_UP]) btn |= 1 << 4;
        for (0..8) |k| {
            const down = keys[SCANCODE_F1 + k];
            if (down and !prev_f[k]) {
                sw ^= @as(u8, 1) << @intCast(k);
                std.debug.print("vga-play: SW = 0x{x:0>2}\n", .{sw});
            }
            prev_f[k] = down;
        }
        vtop.vtop_set_inputs(top, btn, sw);

        // Hold to about 60 frames per second when the model is faster.
        const due = start + frames * 1000 / 60;
        const now = SDL_GetTicks();
        if (due > now) SDL_Delay(@intCast(due - now));

        if (max_frames) |m| if (frames >= m) break;
    }
    const secs = @as(f64, @floatFromInt(SDL_GetTicks() - start)) / 1000.0;
    std.debug.print("vga-play: {d} frames in {d:.1} s\n", .{ frames, secs });
}
