//! Battery saves on the desktop (side track Q, PLAN.md Q.3): the same
//! files the board keeps, so a game continues on either side.
//!
//! The board's save for a ROM at `/<dir>/<folder>/<path>/<name>.<ext>` on
//! the card is `/saves/<dir>/<folder>/<path>/<name>.sav` (menu.zig's
//! `Save`); `make saves-pull` and `make sd-sync` copy them into
//! SAVES_DIR with the same paths, and `make sd` puts them back. The card's
//! folders are the basenames of the SD_*_ROMS folders (roms/gb/games ->
//! games, build/gb/asm -> asm), so a ROM's place on the card follows from
//! its path here: the folder after `roms/<system>/`, or `asm` for
//! build/gb/asm. The Game Boy's DMG games run on the GBC are the GBC
//! system's folder `gb` (`/gbc/gb/<folder>/...`).

const std = @import("std");

/// Where a ROM sits on the card, below its system's folder.
pub const CardPath = struct {
    /// the card folder (games, asm, blargg, ...)
    folder: []const u8,
    /// the path below it without the extension (local/Some Game)
    stem: []const u8,
};

/// A ROM path's place on the card: the component after `roms/<system>/`
/// (the last such), or `asm` for `build/gb/asm/`; otherwise the file's own
/// folder. Null for a path without a folder.
pub fn cardPath(rom: []const u8) ?CardPath {
    const no_ext = if (std.mem.lastIndexOfScalar(u8, rom, '.')) |dot|
        (if (std.mem.lastIndexOfScalar(u8, rom, '/')) |sl| (if (dot > sl) rom[0..dot] else rom) else rom[0..dot])
    else
        rom;
    // roms/<system>/<folder>/<stem>
    var best: ?usize = null;
    var i: usize = 0;
    while (std.mem.indexOfPos(u8, no_ext, i, "roms/")) |at| : (i = at + 1) {
        if (at == 0 or no_ext[at - 1] == '/') best = at;
    }
    if (best) |at| {
        const rest = no_ext[at + "roms/".len ..];
        if (std.mem.indexOfScalar(u8, rest, '/')) |s1| {
            const below = rest[s1 + 1 ..];
            if (std.mem.indexOfScalar(u8, below, '/')) |s2|
                return .{ .folder = below[0..s2], .stem = below[s2 + 1 ..] };
        }
    }
    if (std.mem.indexOf(u8, no_ext, "build/gb/asm/")) |at|
        if (at == 0 or no_ext[at - 1] == '/')
            return .{ .folder = "asm", .stem = no_ext[at + "build/gb/asm/".len ..] };
    const sl = std.mem.lastIndexOfScalar(u8, no_ext, '/') orelse return null;
    const dir = no_ext[0..sl];
    const folder = if (std.mem.lastIndexOfScalar(u8, dir, '/')) |s| dir[s + 1 ..] else dir;
    if (folder.len == 0) return null;
    return .{ .folder = folder, .stem = no_ext[sl + 1 ..] };
}

/// The save file's path: `<saves>/<system dir>/<folder>/<stem>.sav`.
pub fn savePath(buf: []u8, saves: []const u8, system_dir: []const u8, p: CardPath) ![]const u8 {
    return std.fmt.bufPrint(buf, "{s}/{s}/{s}/{s}.sav", .{ saves, system_dir, p.folder, p.stem });
}

/// SAVES_DIR's default, as the Makefile's: $XDG_DATA_HOME (or
/// ~/.local/share)/emulation_clash/saves.
pub fn defaultDir(buf: []u8, env: *const std.process.Environ.Map) ?[]const u8 {
    if (env.get("XDG_DATA_HOME")) |x| if (x.len > 0 and x[0] == '/')
        return std.fmt.bufPrint(buf, "{s}/emulation_clash/saves", .{x}) catch null;
    const home = env.get("HOME") orelse return null;
    return std.fmt.bufPrint(buf, "{s}/.local/share/emulation_clash/saves", .{home}) catch null;
}

const testing = std.testing;

test "cardPath: the folder after roms/<system>/, a subfolder kept in the stem" {
    const p = cardPath("/home/u/emulation_clash/roms/gb/games/local/Some Game - Long Title (Rev 1) [a][b].gb").?;
    try testing.expectEqualStrings("games", p.folder);
    try testing.expectEqualStrings("local/Some Game - Long Title (Rev 1) [a][b]", p.stem);
    const q = cardPath("roms/gbc/games/ucity.gbc").?;
    try testing.expectEqualStrings("games", q.folder);
    try testing.expectEqualStrings("ucity", q.stem);
    // the last roms/ counts (a checkout below a folder called roms)
    const r = cardPath("/x/roms/y/roms/gb/blargg/dmg_sound.gb").?;
    try testing.expectEqualStrings("blargg", r.folder);
    try testing.expectEqualStrings("dmg_sound", r.stem);
}

test "cardPath: build/gb/asm is the card's asm; elsewhere the file's own folder" {
    const a = cardPath("build/gb/asm/save.gb").?;
    try testing.expectEqualStrings("asm", a.folder);
    try testing.expectEqualStrings("save", a.stem);
    const b = cardPath("/tmp/stuff/My.Game.gb").?;
    try testing.expectEqualStrings("stuff", b.folder);
    try testing.expectEqualStrings("My.Game", b.stem);
    try testing.expectEqual(@as(?CardPath, null), cardPath("game.gb"));
}

test "savePath: the board's layout, the GBC's DMG games below gbc/gb" {
    var buf: [256]u8 = undefined;
    const p = cardPath("roms/gb/games/local/Some Game.gb").?;
    try testing.expectEqualStrings("/s/gb/games/local/Some Game.sav", try savePath(&buf, "/s", "gb", p));
    try testing.expectEqualStrings("/s/gbc/gb/games/local/Some Game.sav", try savePath(&buf, "/s", "gbc/gb", p));
}
