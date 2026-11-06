//! The desktop emulator's menu (side track Q, the user 2026-10-25: a drop
//! down menu for the scale, the speed, pause, save states and the like):
//! a bar of titles at the top of the window, each dropping a list of
//! entries, drawn by the front end itself over the game's picture with
//! SDL's built-in 8 × 8 font (SDL has no native menus). Esc opens it, as on
//! the board (the menu key from a game); the mouse shows the bar when it
//! comes to the window's top edge. While a list is dropped the game waits
//! (main.zig). The arrow keys, Enter and Esc work it too.
//!
//! The menus' contents come from main.zig each frame (`Menu`, `Entry`: a
//! label, a shortcut shown on the right, a tick, an id main.zig acts on);
//! this file lays them out, hit-tests the mouse, moves the selection and
//! draws them. Units are the window's pixels divided by `scale` (1 to 4,
//! by the window's width), so the text stays readable at any size.

const std = @import("std");

pub const Entry = struct {
    label: []const u8,
    shortcut: []const u8 = "",
    id: u32 = 0,
    checked: bool = false,
    enabled: bool = true,
    /// a line before this entry
    sep: bool = false,
};

pub const Menu = struct {
    title: []const u8,
    entries: []const Entry,
};

/// The font's glyphs (SDL_DEBUG_TEXT_FONT_CHARACTER_SIZE).
pub const glyph: f32 = 8;
pub const bar_h: f32 = 12;
const row_h: f32 = 11;
const title_pad: f32 = 8;

pub const State = enum { closed, bar, open };

pub const Ui = struct {
    state: State = .closed,
    /// the menu whose list is dropped (or highlighted on the bar)
    cur: usize = 0,
    /// the highlighted entry of the dropped list
    sel: ?usize = null,
    scale: f32 = 1,
    msg_buf: [96]u8 = undefined,
    msg_len: usize = 0,
    msg_until: u64 = 0,

    /// A message at the bottom of the picture for about two seconds.
    pub fn say(ui: *Ui, now: u64, comptime fmt: []const u8, args: anytype) void {
        const s = std.fmt.bufPrint(&ui.msg_buf, fmt, args) catch ui.msg_buf[0..0];
        ui.msg_len = s.len;
        ui.msg_until = now + 2_000_000_000;
    }

    pub fn message(ui: *const Ui, now: u64) ?[]const u8 {
        return if (ui.msg_len > 0 and now < ui.msg_until) ui.msg_buf[0..ui.msg_len] else null;
    }

    /// The game waits while a list is dropped.
    pub fn holdsGame(ui: *const Ui) bool {
        return ui.state == .open;
    }

    pub fn openMenu(ui: *Ui, menus: []const Menu, i: usize) void {
        ui.state = .open;
        ui.cur = i;
        ui.sel = firstEnabled(menus[i], 0, 1);
    }

    pub fn close(ui: *Ui) void {
        ui.state = .closed;
        ui.sel = null;
    }

    /// Esc: the first menu's list, or closed again.
    pub fn toggle(ui: *Ui, menus: []const Menu) void {
        if (ui.state == .open) ui.close() else ui.openMenu(menus, ui.cur);
    }

    /// A key while the list is dropped: left, right, up, down, Enter
    /// (the entry's id, to act on), Esc.
    pub fn key(ui: *Ui, menus: []const Menu, k: Key) ?u32 {
        if (ui.state != .open) return null;
        const m = menus[ui.cur];
        switch (k) {
            .left => ui.openMenu(menus, (ui.cur + menus.len - 1) % menus.len),
            .right => ui.openMenu(menus, (ui.cur + 1) % menus.len),
            .down => {
                const from = if (ui.sel) |s| s + 1 else 0;
                if (firstEnabled(m, from, 1)) |t| ui.sel = t;
            },
            .up => if (ui.sel) |s| {
                if (s > 0) if (firstEnabled(m, s - 1, -1)) |t| {
                    ui.sel = t;
                };
            } else {
                ui.sel = firstEnabled(m, m.entries.len -| 1, -1);
            },
            .enter => if (ui.sel) |s| {
                const e = m.entries[s];
                ui.close();
                if (e.enabled) return e.id;
            },
            .escape => ui.close(),
        }
        return null;
    }

    /// The mouse moved to (x, y), in the window's pixels.
    pub fn motion(ui: *Ui, menus: []const Menu, x: f32, y: f32) void {
        const u = x / ui.scale;
        const v = y / ui.scale;
        switch (ui.state) {
            .closed => if (v < bar_h) {
                ui.state = .bar;
            },
            .bar => if (v >= bar_h) {
                ui.state = .closed;
            },
            .open => {
                if (v < bar_h) {
                    if (titleAt(menus, u)) |i| if (i != ui.cur) ui.openMenu(menus, i);
                    ui.sel = null;
                } else ui.sel = entryAt(menus, ui.cur, u, v);
            },
        }
    }

    /// A click at (x, y): opens a title's list, acts on an entry (its id
    /// returned), or closes the menu when outside it.
    pub fn click(ui: *Ui, menus: []const Menu, x: f32, y: f32) ?u32 {
        const u = x / ui.scale;
        const v = y / ui.scale;
        if (v < bar_h) {
            if (titleAt(menus, u)) |i| {
                if (ui.state == .open and ui.cur == i) ui.state = .bar else ui.openMenu(menus, i);
            } else if (ui.state == .open) ui.close();
            return null;
        }
        if (ui.state != .open) return null;
        if (entryAt(menus, ui.cur, u, v)) |i| {
            const e = menus[ui.cur].entries[i];
            if (!e.enabled) return null;
            ui.close();
            return e.id;
        }
        ui.close();
        return null;
    }
};

pub const Key = enum { left, right, up, down, enter, escape };

fn firstEnabled(m: Menu, from: usize, dir: isize) ?usize {
    var i: isize = @intCast(from);
    while (i >= 0 and i < m.entries.len) : (i += dir) {
        if (m.entries[@intCast(i)].enabled) return @intCast(i);
    }
    return null;
}

/// A title's left edge and width on the bar.
pub fn titleSpan(menus: []const Menu, i: usize) struct { x: f32, w: f32 } {
    var x: f32 = 4;
    for (menus[0..i]) |m| x += textW(m.title) + 2 * title_pad;
    return .{ .x = x, .w = textW(menus[i].title) + 2 * title_pad };
}

fn titleAt(menus: []const Menu, u: f32) ?usize {
    for (0..menus.len) |i| {
        const s = titleSpan(menus, i);
        if (u >= s.x and u < s.x + s.w) return i;
    }
    return null;
}

/// The dropped list's box: under its title, as wide as its longest label
/// and shortcut.
pub fn listBox(menus: []const Menu, i: usize) struct { x: f32, y: f32, w: f32, h: f32 } {
    const m = menus[i];
    var lw: f32 = 0;
    var sw: f32 = 0;
    for (m.entries) |e| {
        lw = @max(lw, textW(e.label));
        sw = @max(sw, textW(e.shortcut));
    }
    // a tick column, the label, a gap, the shortcut
    const w = 2 * glyph + lw + (if (sw > 0) 3 * glyph + sw else glyph);
    return .{ .x = titleSpan(menus, i).x, .y = bar_h, .w = w, .h = row_h * @as(f32, @floatFromInt(m.entries.len)) + 4 };
}

fn entryAt(menus: []const Menu, i: usize, u: f32, v: f32) ?usize {
    const b = listBox(menus, i);
    if (u < b.x or u >= b.x + b.w or v < b.y + 2) return null;
    const k: usize = @intFromFloat((v - b.y - 2) / row_h);
    return if (k < menus[i].entries.len) k else null;
}

fn textW(s: []const u8) f32 {
    return glyph * @as(f32, @floatFromInt(s.len));
}

// --- drawing (SDL3, only what is used) ---------------------------------
pub const SDL_Renderer = opaque {};
const SDL_FRect = extern struct { x: f32, y: f32, w: f32, h: f32 };
extern fn SDL_SetRenderDrawColor(r: *SDL_Renderer, red: u8, g: u8, b: u8, a: u8) bool;
extern fn SDL_SetRenderDrawBlendMode(r: *SDL_Renderer, mode: u32) bool;
extern fn SDL_RenderFillRect(r: *SDL_Renderer, rect: *const SDL_FRect) bool;
extern fn SDL_RenderDebugText(r: *SDL_Renderer, x: f32, y: f32, s: [*:0]const u8) bool;
extern fn SDL_SetRenderScale(r: *SDL_Renderer, x: f32, y: f32) bool;
extern fn SDL_SetRenderLogicalPresentation(r: *SDL_Renderer, w: c_int, h: c_int, mode: c_int) bool;
extern fn SDL_GetRenderLogicalPresentation(r: *SDL_Renderer, w: *c_int, h: *c_int, mode: *c_int) bool;
extern fn SDL_GetRenderOutputSize(r: *SDL_Renderer, w: *c_int, h: *c_int) bool;
const SDL_BLENDMODE_BLEND: u32 = 1;

/// The scale for a window this wide (in pixels): 1 to 4.
pub fn scaleFor(width: c_int) f32 {
    return @floatFromInt(std.math.clamp(@divTrunc(width, 320), 1, 4));
}

fn text(r: *SDL_Renderer, x: f32, y: f32, s: []const u8) void {
    var buf: [128:0]u8 = undefined;
    const n = @min(s.len, buf.len);
    @memcpy(buf[0..n], s[0..n]);
    buf[n] = 0;
    _ = SDL_RenderDebugText(r, x, y, &buf);
}

fn fill(r: *SDL_Renderer, x: f32, y: f32, w: f32, h: f32, c: [4]u8) void {
    _ = SDL_SetRenderDrawColor(r, c[0], c[1], c[2], c[3]);
    _ = SDL_RenderFillRect(r, &.{ .x = x, .y = y, .w = w, .h = h });
}

const bg = [4]u8{ 24, 24, 32, 230 };
const hi = [4]u8{ 70, 90, 150, 255 };
const fg = [4]u8{ 230, 230, 230, 255 };
const dim = [4]u8{ 120, 120, 130, 255 };

/// Draws the bar, the dropped list, the message and the `corner` text
/// (the speed, when shown) over the picture, in the window's pixels: the
/// renderer's logical presentation is set aside meanwhile.
pub fn draw(ui: *Ui, r: *SDL_Renderer, menus: []const Menu, now: u64, corner: ?[]const u8) void {
    var lw: c_int = 0;
    var lh: c_int = 0;
    var mode: c_int = 0;
    _ = SDL_GetRenderLogicalPresentation(r, &lw, &lh, &mode);
    _ = SDL_SetRenderLogicalPresentation(r, 0, 0, 0);
    defer _ = SDL_SetRenderLogicalPresentation(r, lw, lh, mode);
    var ow: c_int = 0;
    var oh: c_int = 0;
    _ = SDL_GetRenderOutputSize(r, &ow, &oh);
    ui.scale = scaleFor(ow);
    _ = SDL_SetRenderScale(r, ui.scale, ui.scale);
    defer _ = SDL_SetRenderScale(r, 1, 1);
    _ = SDL_SetRenderDrawBlendMode(r, SDL_BLENDMODE_BLEND);
    const W = @as(f32, @floatFromInt(ow)) / ui.scale;
    const H = @as(f32, @floatFromInt(oh)) / ui.scale;

    if (ui.state != .closed) {
        fill(r, 0, 0, W, bar_h, bg);
        for (menus, 0..) |m, i| {
            const s = titleSpan(menus, i);
            if (ui.state == .open and i == ui.cur) fill(r, s.x, 0, s.w, bar_h, hi);
            _ = SDL_SetRenderDrawColor(r, fg[0], fg[1], fg[2], fg[3]);
            text(r, s.x + title_pad, 2, m.title);
        }
    }
    if (ui.state == .open) {
        const b = listBox(menus, ui.cur);
        fill(r, b.x, b.y, b.w, b.h, bg);
        for (menus[ui.cur].entries, 0..) |e, k| {
            const y = b.y + 2 + row_h * @as(f32, @floatFromInt(k));
            if (e.sep) fill(r, b.x + 2, y - 1, b.w - 4, 1, dim);
            if (ui.sel == k and e.enabled) fill(r, b.x, y, b.w, row_h, hi);
            const c = if (e.enabled) fg else dim;
            _ = SDL_SetRenderDrawColor(r, c[0], c[1], c[2], c[3]);
            if (e.checked) text(r, b.x + 4, y + 1.5, "*");
            text(r, b.x + 2 * glyph, y + 1.5, e.label);
            if (e.shortcut.len > 0) text(r, b.x + b.w - glyph - textW(e.shortcut), y + 1.5, e.shortcut);
        }
    }
    if (ui.message(now)) |m| {
        fill(r, 2, H - bar_h - 2, textW(m) + 8, bar_h, bg);
        _ = SDL_SetRenderDrawColor(r, fg[0], fg[1], fg[2], fg[3]);
        text(r, 6, H - bar_h, m);
    }
    if (corner) |c| if (ui.state == .closed) {
        fill(r, W - textW(c) - 8, 2, textW(c) + 6, bar_h, bg);
        _ = SDL_SetRenderDrawColor(r, fg[0], fg[1], fg[2], fg[3]);
        text(r, W - textW(c) - 5, 4, c);
    };
}

// --- tests (make test-desktop) ------------------------------------------

const testing = std.testing;

const test_menus = [_]Menu{
    .{ .title = "Game", .entries = &.{ .{ .label = "Pause", .shortcut = "F11", .id = 1 }, .{ .label = "Reset", .shortcut = "F5", .id = 2 } } },
    .{ .title = "Speed", .entries = &.{ .{ .label = "50%", .id = 10 }, .{ .label = "off", .id = 11, .enabled = false }, .{ .label = "200%", .id = 12 } } },
};

test "the titles side by side, the list under its title" {
    try testing.expectEqual(@as(f32, 4), titleSpan(&test_menus, 0).x);
    try testing.expectEqual(4 + 4 * glyph + 16, titleSpan(&test_menus, 1).x);
    const b = listBox(&test_menus, 1);
    try testing.expectEqual(titleSpan(&test_menus, 1).x, b.x);
    try testing.expectEqual(bar_h, b.y);
    // a tick column, "200%", a glyph's gap (no shortcuts)
    try testing.expectEqual(2 * glyph + 4 * glyph + glyph, b.w);
}

test "the mouse: the bar at the top, a click opens a list and picks an entry" {
    var ui: Ui = .{};
    ui.motion(&test_menus, 100, 5);
    try testing.expectEqual(State.bar, ui.state);
    try testing.expectEqual(@as(?u32, null), ui.click(&test_menus, titleSpan(&test_menus, 0).x + 2, 5));
    try testing.expectEqual(State.open, ui.state);
    try testing.expect(ui.holdsGame());
    // the second entry, Reset
    const b = listBox(&test_menus, 0);
    try testing.expectEqual(@as(?u32, 2), ui.click(&test_menus, b.x + 4, b.y + 2 + row_h + 3));
    try testing.expectEqual(State.closed, ui.state);
}

test "a disabled entry isn't picked; a click outside closes" {
    var ui: Ui = .{};
    ui.openMenu(&test_menus, 1);
    const b = listBox(&test_menus, 1);
    try testing.expectEqual(@as(?u32, null), ui.click(&test_menus, b.x + 4, b.y + 2 + row_h + 3));
    try testing.expectEqual(State.open, ui.state);
    try testing.expectEqual(@as(?u32, null), ui.click(&test_menus, 300, 200));
    try testing.expectEqual(State.closed, ui.state);
}

test "the keys: down skips a disabled entry, right moves to the next menu, Enter acts" {
    var ui: Ui = .{};
    ui.openMenu(&test_menus, 1);
    try testing.expectEqual(@as(?usize, 0), ui.sel);
    _ = ui.key(&test_menus, .down);
    try testing.expectEqual(@as(?usize, 2), ui.sel);
    try testing.expectEqual(@as(?u32, 12), ui.key(&test_menus, .enter));
    ui.toggle(&test_menus);
    try testing.expectEqual(@as(usize, 1), ui.cur);
    _ = ui.key(&test_menus, .right);
    try testing.expectEqual(@as(usize, 0), ui.cur);
    _ = ui.key(&test_menus, .escape);
    try testing.expectEqual(State.closed, ui.state);
}

test "the scale by the window's width" {
    try testing.expectEqual(@as(f32, 1), scaleFor(320));
    try testing.expectEqual(@as(f32, 2), scaleFor(640));
    try testing.expectEqual(@as(f32, 3), scaleFor(1024));
    try testing.expectEqual(@as(f32, 4), scaleFor(4000));
}
