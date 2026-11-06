//! The boot splash (side track J, step 5; docs/brand.md): after a system's
//! bitstream is in the PL, `/systems/boot/<name><ext>` on the card
//! (`<name>` as in `<name>.bit`, the system table's first extension) is
//! loaded as a ROM and plays with the overlay hidden until any input; once
//! that input is released the menu opens over the splash's last frame.
//! Without the file, or with one the system's check refuses, the menu
//! opens as before (the reason goes to the UART). relay.zig does the
//! loading (load.zig, as for the menu's ROMs, but with no save); this file
//! holds the parts it can test on the desktop.

const std = @import("std");
const menu = @import("menu.zig");

/// The folder in `/systems` that holds the boot files.
pub const dir_name = "boot";

/// `<dir><first extension>`: chip8.ch8, gb.gb, nes.nes.
pub fn fileName(buf: []u8, s: *const menu.System) ?[]const u8 {
    if (s.exts.len == 0) return null;
    return std.fmt.bufPrint(buf, "{s}{s}", .{ s.dir, s.exts[0] }) catch null;
}

pub fn Lookup(comptime Entry: type) type {
    return union(enum) {
        /// the boot file, to load
        file: Entry,
        /// no splash, and why (for the UART)
        none: []const u8,
    };
}

/// The system's boot file under `systems` (the card's `/systems`, if it
/// has one). `fs` has `find(dir, name) !?Entry` and `openDir(Entry)`, as
/// fat.zig's Fs; `Entry` has `is_dir` and `size`.
pub fn find(comptime Entry: type, fs: anytype, systems: anytype, s: *const menu.System) !Lookup(Entry) {
    const sys_dir = systems orelse return .{ .none = "no /systems folder" };
    const d = (try fs.find(sys_dir, dir_name)) orelse return .{ .none = "no /systems/boot folder" };
    if (!d.is_dir) return .{ .none = "/systems/boot is not a folder" };
    var buf: [64]u8 = undefined;
    const name = fileName(&buf, s) orelse return .{ .none = "the system has no file extension" };
    const e = (try fs.find(fs.openDir(d), name)) orelse return .{ .none = "no boot file for this system" };
    if (e.is_dir) return .{ .none = "the boot file is a folder" };
    if (e.size == 0) return .{ .none = "the boot file is empty" };
    return .{ .file = e };
}

/// While the splash plays: any input (the board buttons, a USB key, the
/// desktop's keys) ends it, but the menu opens only once everything is
/// released again, so the key that ended it doesn't act in the menu too.
/// Input already down when the splash starts (the button that picked the
/// system on the Systems menu) is let go of first and doesn't count.
pub const Wait = struct {
    state: enum { settle, ready, pressed } = .settle,

    /// One poll: `any` if some input is down now. True when the menu
    /// should open (the wait is over).
    pub fn poll(w: *Wait, any: bool) bool {
        switch (w.state) {
            .settle => if (!any) {
                w.state = .ready;
            },
            .ready => if (any) {
                w.state = .pressed;
            },
            .pressed => return !any,
        }
        return false;
    }
};

// --- Tests -------------------------------------------------------------------
const testing = std.testing;

test "file names: the system's dir and first extension" {
    var buf: [64]u8 = undefined;
    try testing.expectEqualStrings("chip8.ch8", fileName(&buf, &menu.systems[0]).?);
    try testing.expectEqualStrings("gb.gb", fileName(&buf, &menu.systems[1]).?);
    try testing.expectEqualStrings("nes.nes", fileName(&buf, &menu.systems[2]).?);
    const none: menu.System = .{ .id = 99, .name = "t", .dir = "t", .exts = &.{}, .load_addr = 0, .max_size = 1 };
    try testing.expect(fileName(&buf, &none) == null);
}

/// A file system of named entries, folders by a path prefix.
const FakeFs = struct {
    const Entry = struct { path: []const u8, is_dir: bool, size: u32 };
    const Dir = []const u8;
    entries: []const Entry,

    fn find(f: *const FakeFs, d: Dir, name: []const u8) !?Entry {
        for (f.entries) |e| {
            const slash = std.mem.lastIndexOfScalar(u8, e.path, '/') orelse continue;
            if (std.mem.eql(u8, e.path[0..slash], d) and std.ascii.eqlIgnoreCase(e.path[slash + 1 ..], name)) return e;
        }
        return null;
    }
    fn openDir(_: *const FakeFs, e: Entry) Dir {
        return e.path;
    }
};

test "the boot file: present (any case), or why there is none" {
    const nes = &menu.systems[2];
    const full: FakeFs = .{ .entries = &.{
        .{ .path = "/systems/boot", .is_dir = true, .size = 0 },
        .{ .path = "/systems/boot/NES.NES", .is_dir = false, .size = 40976 },
        .{ .path = "/systems/boot/gb.gb", .is_dir = true, .size = 0 },
        .{ .path = "/systems/boot/chip8.ch8", .is_dir = false, .size = 0 },
    } };
    const sys: ?FakeFs.Dir = "/systems";
    const found = try find(FakeFs.Entry, &full, sys, nes);
    try testing.expectEqualStrings("/systems/boot/NES.NES", found.file.path);
    try testing.expectEqualStrings("the boot file is a folder", (try find(FakeFs.Entry, &full, sys, &menu.systems[1])).none);
    try testing.expectEqualStrings("the boot file is empty", (try find(FakeFs.Entry, &full, sys, &menu.systems[0])).none);
    try testing.expectEqualStrings("no /systems folder", (try find(FakeFs.Entry, &full, @as(?FakeFs.Dir, null), nes)).none);
    const no_boot: FakeFs = .{ .entries = &.{.{ .path = "/systems/nes.bit", .is_dir = false, .size = 100 }} };
    try testing.expectEqualStrings("no /systems/boot folder", (try find(FakeFs.Entry, &no_boot, sys, nes)).none);
    const boot_file: FakeFs = .{ .entries = &.{.{ .path = "/systems/boot", .is_dir = false, .size = 10 }} };
    try testing.expectEqualStrings("/systems/boot is not a folder", (try find(FakeFs.Entry, &boot_file, sys, nes)).none);
    const other: FakeFs = .{ .entries = &.{
        .{ .path = "/systems/boot", .is_dir = true, .size = 0 },
        .{ .path = "/systems/boot/gb.gb", .is_dir = false, .size = 32768 },
    } };
    try testing.expectEqualStrings("no boot file for this system", (try find(FakeFs.Entry, &other, sys, nes)).none);
}

test "a boot file the system's check refuses is not loaded" {
    const load = @import("load.zig");
    // a Game Boy "cartridge" with no Nintendo logo / an unknown MBC
    const Reader = struct {
        block: [512]u8,
        given: bool = false,
        pub fn next(r: *@This()) !?[]const u8 {
            if (r.given) return null;
            r.given = true;
            return &r.block;
        }
    };
    var r: Reader = .{ .block = [_]u8{0xF4} ** 512 };
    const p = try load.prepare(&menu.systems[1], 32768, &r);
    try testing.expect(p == .refused);
}

test "the wait: nothing until a press, the menu only after its release" {
    var w: Wait = .{};
    for (0..5) |_| try testing.expect(!w.poll(false));
    try testing.expect(!w.poll(true));
    try testing.expect(!w.poll(true)); // held: still the splash
    try testing.expect(w.poll(false)); // released: the menu
    // a key already down when the splash starts doesn't count
    var held: Wait = .{};
    try testing.expect(!held.poll(true));
    try testing.expect(!held.poll(true));
    try testing.expect(!held.poll(false)); // let go of: now it waits
    try testing.expect(!held.poll(false));
    try testing.expect(!held.poll(true));
    try testing.expect(held.poll(false));
}
