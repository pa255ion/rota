//! Battery saves on the desktop (side track I, phase 1; the board's side
//! is platform/ps/save.zig):
//!
//! * `slots`: which ROMs of a folder get a save file on the card, and how
//!   big (`link --save-slots SYSTEM DIR`, for tools/mksd.sh). The sizes
//!   come from the PS app's own system table (menu.zig's `save` hook), so
//!   the card and the board always agree.
//! * `pull`: every file below `/saves` on a card into a folder on this PC
//!   (`make saves-pull`, and `make sd` before it erases the card), read a
//!   block at a time through the relay's R command (or from an image, in
//!   the tests), with the PS app's own FAT reader.

const std = @import("std");
const menu = @import("menu"); // platform/ps/menu.zig, the board's system table
const fat = @import("fat"); // platform/ps/fat.zig

/// The system whose card folder is `dir` ("gb"), if it has saves.
pub fn systemByDir(dir: []const u8) ?*const menu.System {
    for (&menu.systems) |*s| if (std.ascii.eqlIgnoreCase(s.dir, dir)) return s;
    return null;
}

/// The save file's size for a ROM image, 0 if none.
pub fn slotSize(s: *const menu.System, name: []const u8, rom: []const u8) u32 {
    const sv = s.save orelse return 0;
    if (!menu.hasExt(name, s.exts)) return 0;
    // the whole ROM when the system says so (M7's request), as the board
    // does once the ROM is in DDR
    if (sv.size_of_rom) |size| return size(rom);
    return sv.size(rom[0..@min(rom.len, 512)]);
}

/// A save-state slot's size for a ROM image (side track L, L.7;
/// menu.savestate.fileSize), 0 if the system has no save states.
pub fn stateSize(s: *const menu.System, name: []const u8, rom: []const u8) u32 {
    const st = s.state orelse return 0;
    if (!menu.hasExt(name, s.exts)) return 0;
    return menu.savestate.fileSize(st, st.cram(rom[0..@min(rom.len, 512)]));
}

/// The frames a movie slot holds (`link --movie-frames`; 30 minutes at 60
/// frames a second unless set).
pub var movie_frames: u32 = 30 * 60 * 60;

/// A movie slot's size for a ROM image (side track O, O.2b;
/// menu.movie.fileSize, the state area from menu.savestate.movieArea), 0 if
/// the system has no movies.
pub fn movieSize(s: *const menu.System, name: []const u8, rom: []const u8) u32 {
    if (!s.movies or !menu.hasExt(name, s.exts)) return 0;
    const cram: u32 = if (s.state) |st| st.cram(rom[0..@min(rom.len, 512)]) else 0;
    return menu.movie.fileSize(menu.savestate.movieArea(s.state, cram), movie_frames);
}

/// Every ROM below `root` with a save: "<path below root>\t<size>\n",
/// sorted by path, into `out`. Files the board would refuse (too big)
/// are skipped.
pub fn slots(io: std.Io, gpa: std.mem.Allocator, s: *const menu.System, root: []const u8, out: *std.ArrayList(u8)) !void {
    return slotsOf(io, gpa, s, root, out, slotSize);
}

/// The same with each ROM's movie slot size (`link --movie-slots`).
pub fn movieSlots(io: std.Io, gpa: std.mem.Allocator, s: *const menu.System, root: []const u8, out: *std.ArrayList(u8)) !void {
    return slotsOf(io, gpa, s, root, out, movieSize);
}

/// The same with each ROM's save-state slot size (`link --state-slots`).
pub fn stateSlots(io: std.Io, gpa: std.mem.Allocator, s: *const menu.System, root: []const u8, out: *std.ArrayList(u8)) !void {
    return slotsOf(io, gpa, s, root, out, stateSize);
}

fn slotsOf(io: std.Io, gpa: std.mem.Allocator, s: *const menu.System, root: []const u8, out: *std.ArrayList(u8), comptime sizeOf: fn (*const menu.System, []const u8, []const u8) u32) !void {
    var dir = try std.Io.Dir.cwd().openDir(io, root, .{ .iterate = true });
    defer dir.close(io);
    var walker = try dir.walk(gpa);
    defer walker.deinit();
    var lines: std.ArrayList([]u8) = .empty;
    defer {
        for (lines.items) |l| gpa.free(l);
        lines.deinit(gpa);
    }
    while (try walker.next(io)) |e| {
        if (e.kind != .file and e.kind != .sym_link) continue;
        if (!menu.hasExt(e.basename, s.exts)) continue;
        const rom = dir.readFileAlloc(io, e.path, gpa, .limited(s.max_size + 1)) catch |err| switch (err) {
            error.StreamTooLong => continue,
            else => return err,
        };
        defer gpa.free(rom);
        const size = sizeOf(s, e.basename, rom);
        if (size == 0) continue;
        try lines.append(gpa, try std.fmt.allocPrint(gpa, "{s}\t{d}\n", .{ e.path, size }));
    }
    std.mem.sort([]u8, lines.items, {}, struct {
        fn lt(_: void, a: []u8, b: []u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.lt);
    for (lines.items) |l| try out.appendSlice(gpa, l);
}

pub const Pulled = struct {
    files: u32 = 0,
    bytes: u64 = 0,
    /// the card has no /saves folder (nothing to pull, not an error)
    no_folder: bool = false,
};

const max_depth = 8;
const max_save = 1 << 20;

/// Every file below `/saves` on the card behind `dev` (a block device:
/// `readBlock(lba, *[512]u8) !void`) into `dst` (made if missing), with
/// the card's names. Each file is written next to its place and renamed
/// over it, so an interrupted pull never leaves half a save. `log`, if
/// set, gets one line per file.
/// A save with a cartridge clock's footer (menu.clock_footer bytes after
/// RAM whose size is a multiple of 512, MBC2's included): its time stamp
/// set to `now` (seconds since 1970), so another emulator counts the time
/// from the pull. The board writes 0 there: it has no clock of its own.
pub fn stampClock(data: []u8, now: u64) bool {
    if (data.len < menu.clock_footer or data.len % 512 != menu.clock_footer) return false;
    std.mem.writeInt(u64, data[data.len - 8 ..][0..8], now, .little);
    return true;
}

/// Every save below `dir` (`*.sav`, any depth) with a clock footer stamped
/// with `now`, as `pull` stamps them (`link --stamp-clocks`: saves copied
/// off a card mounted on the desktop, make sd-sync). Returns how many.
pub fn stampDir(io: std.Io, gpa: std.mem.Allocator, dir: []const u8, now: u64) !u32 {
    var d = std.Io.Dir.cwd().openDir(io, dir, .{ .iterate = true }) catch |e| switch (e) {
        error.FileNotFound => return 0,
        else => return e,
    };
    defer d.close(io);
    var walker = try d.walk(gpa);
    defer walker.deinit();
    var n: u32 = 0;
    while (try walker.next(io)) |e| {
        if (e.kind != .file or !std.ascii.endsWithIgnoreCase(e.basename, ".sav")) continue;
        const data = try d.readFileAlloc(io, e.path, gpa, .limited(1 << 20));
        defer gpa.free(data);
        if (!stampClock(data, now)) continue;
        try d.writeFile(io, .{ .sub_path = e.path, .data = data });
        n += 1;
    }
    return n;
}

pub fn pull(dev: anytype, io: std.Io, gpa: std.mem.Allocator, dst: []const u8, log: bool, now: u64) !Pulled {
    return pullFolder(dev, io, gpa, menu.saves_dir, dst, log, now);
}

/// The same for `/states`, the save-state slots (side track L): no clock
/// footers there, nothing stamped.
pub fn pullStates(dev: anytype, io: std.Io, gpa: std.mem.Allocator, dst: []const u8, log: bool) !Pulled {
    return pullFolder(dev, io, gpa, menu.savestate.states_dir, dst, log, null);
}

/// The card's /movies (side track O) into `dst`, as the states.
pub fn pullMovies(dev: anytype, io: std.Io, gpa: std.mem.Allocator, dst: []const u8, log: bool) !Pulled {
    return pullFolder(dev, io, gpa, menu.movie.movies_dir, dst, log, null);
}

/// The card's /screens (side track O, O.4: screenshots) into `dst`.
pub fn pullScreens(dev: anytype, io: std.Io, gpa: std.mem.Allocator, dst: []const u8, log: bool) !Pulled {
    return pullFolder(dev, io, gpa, "screens", dst, log, null);
}

/// One file of the card (`path`, folders from the root) into folder `dst`
/// under its own name (`link --screenshot`); its size.
pub fn pullFile(dev: anytype, io: std.Io, gpa: std.mem.Allocator, path: []const u8, dst: []const u8) !u32 {
    const Fs = fat.Fs(@TypeOf(dev.*));
    var fs = try Fs.mount(dev);
    var dir = fs.root();
    var it = std.mem.tokenizeScalar(u8, path, '/');
    const e = while (it.next()) |part| {
        const f = (try fs.find(dir, part)) orelse return error.NotFound;
        if (it.peek() == null) break f;
        if (!f.is_dir) return error.NotFound;
        dir = fs.openDir(f);
    } else return error.NotFound;
    const data = try gpa.alloc(u8, e.size);
    defer gpa.free(data);
    _ = try fs.readFile(e, data);
    try std.Io.Dir.cwd().createDirPath(io, dst);
    var out = try std.Io.Dir.cwd().openDir(io, dst, .{});
    defer out.close(io);
    const base = path[(std.mem.lastIndexOfScalar(u8, path, '/') orelse 0) + 1 ..];
    try out.writeFile(io, .{ .sub_path = base, .data = data });
    return e.size;
}

/// A movie file (menu.movie's format) into the card's slot at `path`
/// (`link --movie-put`, `make movie-play`): its blocks written in place,
/// the header block first marked unused and last as it is, so a transfer
/// cut off leaves an empty slot. A slot that isn't there is made (with its
/// folders, the movie's size; fat.zig's create, side track K, F1). The
/// board plays it from the menu.
pub fn putMovie(dev: anytype, path: []const u8, bytes: []const u8) !u32 {
    const mv = menu.movie;
    if (bytes.len < mv.header_bytes) return error.NotAMovie;
    const h = mv.Header.decode(bytes[0..mv.header_bytes]) orelse return error.NotAMovie;
    if (!h.used or h.frames == 0) return error.EmptyMovie;
    const end = h.words_at + h.frames * 2;
    if (h.words_at < mv.header_bytes or bytes.len < end) return error.ShortMovie;
    const Fs = fat.Fs(@TypeOf(dev.*));
    var fs = try Fs.mount(dev);
    var dir = fs.root();
    var it = std.mem.tokenizeScalar(u8, path, '/');
    const e = while (it.next()) |part| {
        const found = try fs.find(dir, part);
        if (it.peek() == null) {
            if (found) |f| {
                if (f.is_dir) return error.NoSlot;
                break f;
            }
            break try fs.create(dir, part, std.mem.alignForward(u32, end, 512));
        }
        dir = if (found) |f| (if (f.is_dir) fs.openDir(f) else return error.NoSlot) else try fs.mkdir(dir, part);
    } else return error.NoSlot;
    if (end > e.size) return error.NoRoom;
    var blocks: mv.Blocks = .{};
    var runs = fs.runs(e, 0xFFFF);
    while (try runs.next()) |r| try blocks.add(r.lba, r.blocks);
    var b: [512]u8 = undefined;
    // the header, unused, until the rest is in
    var off_h = h;
    off_h.used = false;
    off_h.encode(&b);
    try dev.writeBlock(try blocks.at(0), &b);
    var k: u32 = 1;
    while (k * 512 < end) : (k += 1) {
        const from = k * 512;
        const n = @min(512, end - from);
        @memset(&b, 0);
        @memcpy(b[0..n], bytes[from..][0..n]);
        try dev.writeBlock(try blocks.at(k), &b);
    }
    @memset(&b, 0);
    @memcpy(b[0..mv.header_bytes], bytes[0..mv.header_bytes]);
    try dev.writeBlock(try blocks.at(0), &b);
    return h.frames;
}

fn pullFolder(dev: anytype, io: std.Io, gpa: std.mem.Allocator, folder: []const u8, dst: []const u8, log: bool, now: ?u64) !Pulled {
    const Fs = fat.Fs(@TypeOf(dev.*));
    var fs = try Fs.mount(dev);
    var p: Pulled = .{};
    const top = (try fs.find(fs.root(), folder)) orelse {
        p.no_folder = true;
        return p;
    };
    if (!top.is_dir) {
        p.no_folder = true;
        return p;
    }
    try std.Io.Dir.cwd().createDirPath(io, dst);
    var out = try std.Io.Dir.cwd().openDir(io, dst, .{});
    defer out.close(io);
    var path: std.ArrayList(u8) = .empty;
    defer path.deinit(gpa);
    try pullDir(Fs, &fs, fs.openDir(top), io, gpa, out, &path, 0, &p, log, now);
    return p;
}

fn pullDir(comptime Fs: type, fs: *Fs, d: fat.Dir, io: std.Io, gpa: std.mem.Allocator, out: std.Io.Dir, path: *std.ArrayList(u8), depth: u32, p: *Pulled, log: bool, now: ?u64) !void {
    if (depth == max_depth) return error.TooDeep;
    // the listing first: reading files moves the reader's cached block
    var entries: std.ArrayList(fat.Entry) = .empty;
    defer entries.deinit(gpa);
    var it = fs.iterate(d);
    while (try it.next()) |e| try entries.append(gpa, e);
    const base = path.items.len;
    defer path.shrinkRetainingCapacity(base);
    for (entries.items) |e| {
        path.shrinkRetainingCapacity(base);
        if (base > 0) try path.append(gpa, '/');
        try path.appendSlice(gpa, e.name());
        if (e.is_dir) {
            try out.createDirPath(io, path.items);
            try pullDir(Fs, fs, fs.openDir(e), io, gpa, out, path, depth + 1, p, log, now);
            continue;
        }
        if (e.size > max_save) return error.SaveTooBig;
        const data = try gpa.alloc(u8, e.size);
        defer gpa.free(data);
        if (try fs.readFile(e, data) != e.size) return error.ShortFile;
        if (now) |t| _ = stampClock(data, t);
        const tmp = try std.fmt.allocPrint(gpa, "{s}.part", .{path.items});
        defer gpa.free(tmp);
        try out.writeFile(io, .{ .sub_path = tmp, .data = data });
        try std.Io.Dir.rename(out, tmp, out, path.items, io);
        p.files += 1;
        p.bytes += e.size;
        if (log) std.debug.print("link: saves: {s} ({d} bytes)\n", .{ path.items, e.size });
    }
}

// --- Tests: the card image of make test-zig (tools/mksd.sh with saves) -------
const testing = std.testing;

const ImgDev = struct {
    data: []const u8,
    pub fn readBlock(d: *ImgDev, lba: u32, buf: *[512]u8) !void {
        const off = @as(usize, lba) * 512;
        if (off + 512 > d.data.len) return error.OutOfRange;
        @memcpy(buf, d.data[off..][0..512]);
    }
};

test "slots: battery cartridges only, with their sizes (the test ROMs of make test-zig)" {
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(testing.allocator);
    try slots(testing.io, testing.allocator, systemByDir("gb").?, "build/ps-test/gbroms", &out);
    try testing.expectEqualStrings("games/blank.gb\t32768\ngames/clock.gb\t32816\ngames/count.gb\t8192\n", out.items);
    try testing.expect(systemByDir("GB") != null);
    // (CHIP-8's save is its save states' read-back control: no battery RAM)
    try testing.expectEqual(@as(u32, 0), systemByDir("chip8").?.save.?.size(&.{}));
    try testing.expect(systemByDir("atari") == null);
}

/// The same, writable (a copy of the image in memory).
const ImgDevRw = struct {
    data: []u8,
    pub fn readBlock(d: *ImgDevRw, lba: u32, buf: *[512]u8) !void {
        const off = @as(usize, lba) * 512;
        if (off + 512 > d.data.len) return error.OutOfRange;
        @memcpy(buf, d.data[off..][0..512]);
    }
    pub fn writeBlock(d: *ImgDevRw, lba: u32, buf: *const [512]u8) !void {
        const off = @as(usize, lba) * 512;
        if (off + 512 > d.data.len) return error.OutOfRange;
        @memcpy(d.data[off..][0..512], buf);
    }
};

test "movie slots: Game Boy ROMs, a whole state slot and the frames each" {
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(testing.allocator);
    const keep = movie_frames;
    defer movie_frames = keep;
    movie_frames = 3600;
    try movieSlots(testing.io, testing.allocator, systemByDir("gb").?, "build/ps-test/gbroms/games", &out);
    var lines = std.mem.tokenizeScalar(u8, out.items, '\n');
    var n: u32 = 0;
    while (lines.next()) |l| : (n += 1) {
        const tab = std.mem.indexOfScalar(u8, l, '\t').?;
        const size = try std.fmt.parseInt(u32, l[tab + 1 ..], 10);
        try testing.expectEqual(@as(u32, 0), size % 512);
        try testing.expect(size >= 512 + 3600 * 2 + menu.savestate.fileSize(menu.savestate.gb, 0));
    }
    try testing.expectEqual(@as(u32, 4), n);
    out.clearRetainingCapacity();
    try movieSlots(testing.io, testing.allocator, systemByDir("nes").?, "build/ps-test/gbroms/games", &out);
    try testing.expectEqualStrings("", out.items);
}

test "pullFile: one file of a card image into a folder, its own name" {
    const gpa = testing.allocator;
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/sd.img", gpa, .limited(1 << 27));
    defer gpa.free(img);
    var dev: ImgDev = .{ .data = img };
    const dst = "build/ps-test/pulled-one";
    std.Io.Dir.cwd().deleteTree(testing.io, dst) catch {};
    try testing.expectEqual(@as(u32, 0x2000), try pullFile(&dev, testing.io, gpa, "/saves/gb/games/count.sav", dst));
    const got = try std.Io.Dir.cwd().readFileAlloc(testing.io, dst ++ "/count.sav", gpa, .limited(1 << 20));
    defer gpa.free(got);
    for (got, 0..) |b, i| try testing.expectEqual(@as(u8, @truncate(i * 5 + 1)), b);
    try testing.expectError(error.NotFound, pullFile(&dev, testing.io, gpa, "/saves/gb/games/none.sav", dst));
}

test "put a movie: into its slot on a card image, header last; refused when it doesn't fit" {
    const gpa = testing.allocator;
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/sd.img", gpa, .limited(1 << 27));
    defer gpa.free(img);
    var dev: ImgDevRw = .{ .data = img };
    const mv = menu.movie;
    var words: [1000]u16 = undefined;
    for (&words, 0..) |*w, i| w.* = @truncate(i *% 7919);
    const file = try mv.encodeFile(gpa, .{ .system = 2, .frames = 0, .sum = 9 }, "", &words);
    defer gpa.free(file);
    // into a slot make sd made (none any more: one put there first)
    try testing.expectEqual(@as(u32, 1000), try putMovie(&dev, "/movies/gb/games/count.mv1", file));
    // read back through the file system, as the board finds it
    const Fs = fat.Fs(ImgDevRw);
    var fs = try Fs.mount(&dev);
    const e = (try fs.find(fs.openDir((try fs.find(fs.openDir((try fs.find(fs.openDir((try fs.find(fs.root(), "movies")).?), "gb")).?), "games")).?), "count.mv1")).?;
    const back = try gpa.alloc(u8, e.size);
    defer gpa.free(back);
    var r = fs.reader(e);
    var at: usize = 0;
    while (try r.next()) |chunk| : (at += chunk.len) @memcpy(back[at..][0..chunk.len], chunk);
    const d = try mv.decodeFile(gpa, back);
    defer gpa.free(d.words);
    try testing.expectEqualSlices(u16, &words, d.words);
    try testing.expectEqual(@as(u16, 9), d.header.sum);
    // too long for the slot (about 61 KiB: 3600 frames, make test-zig's
    // MOVIE_MINUTES=1, and the state area; a movie without a state may use
    // that room, its header says where its words are)
    const long = try gpa.alloc(u16, 40000);
    defer gpa.free(long);
    @memset(long, 1);
    const big = try mv.encodeFile(gpa, .{ .system = 2, .frames = 0 }, "", long);
    defer gpa.free(big);
    try testing.expectError(error.NoRoom, putMovie(&dev, "/movies/gb/games/count.mv1", big));
    // a slot that isn't there: made, the movie's size, and read back
    try testing.expectEqual(@as(u32, 1000), try putMovie(&dev, "/movies/gb/new folder/none.mv1", file));
    var fs2 = try fat.Fs(ImgDevRw).mount(&dev);
    const made = (try fs2.find(fs2.openDir((try fs2.find(fs2.openDir((try fs2.find(fs2.root(), "movies")).?), "gb")).?), "new folder")).?;
    const mv1 = (try fs2.find(fs2.openDir(made), "none.mv1")).?;
    try testing.expectEqual(std.mem.alignForward(u32, 512 + 2000, 512), mv1.size);
    try testing.expectError(error.NoSlot, putMovie(&dev, "/movies/gb/games/count.mv1/x", file));
    try testing.expectError(error.NotAMovie, putMovie(&dev, "/movies/gb/games/count.mv1", img[0..512]));
}

test "slotSize: a system's size_of_rom reads the whole ROM (a header past the first block)" {
    const S = struct {
        fn head(_: []const u8) u32 {
            return 1;
        }
        fn whole(rom: []const u8) u32 {
            return if (rom.len > 0x8000 and rom[0x7FD8] == 3) 0x2000 else 0;
        }
    };
    var sys: menu.System = systemByDir("gb").?.*;
    sys.save.?.size = S.head;
    sys.save.?.size_of_rom = S.whole;
    var rom = [_]u8{0} ** 0x10000;
    rom[0x7FD8] = 3;
    try testing.expectEqual(@as(u32, 0x2000), slotSize(&sys, "x.gb", &rom));
    sys.save.?.size_of_rom = null;
    try testing.expectEqual(@as(u32, 1), slotSize(&sys, "x.gb", &rom));
}

test "stampDir: clock saves below a folder get the time; others untouched" {
    const gpa = testing.allocator;
    const io = testing.io;
    const root = "build/ps-test/stamp";
    std.Io.Dir.cwd().deleteTree(io, root) catch {};
    try std.Io.Dir.cwd().createDirPath(io, root ++ "/gb/games");
    var clock = [_]u8{0} ** (0x8000 + menu.clock_footer);
    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = root ++ "/gb/games/clock.sav", .data = &clock });
    var plain = [_]u8{7} ** 0x2000;
    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = root ++ "/gb/games/plain.sav", .data = &plain });
    try testing.expectEqual(@as(u32, 1), try stampDir(io, gpa, root, 1_790_000_123));
    const c = try std.Io.Dir.cwd().readFileAlloc(io, root ++ "/gb/games/clock.sav", gpa, .limited(1 << 20));
    defer gpa.free(c);
    try testing.expectEqual(@as(u64, 1_790_000_123), std.mem.readInt(u64, c[c.len - 8 ..][0..8], .little));
    const p = try std.Io.Dir.cwd().readFileAlloc(io, root ++ "/gb/games/plain.sav", gpa, .limited(1 << 20));
    defer gpa.free(p);
    try testing.expect(std.mem.allEqual(u8, p, 7));
    try testing.expectEqual(@as(u32, 0), try stampDir(io, gpa, root ++ "/none", 1));
}

test "pull: /saves from a card image into a folder, names and bytes as on the card" {
    const gpa = testing.allocator;
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/sd.img", gpa, .limited(1 << 27));
    defer gpa.free(img);
    var dev: ImgDev = .{ .data = img };
    const dst = "build/ps-test/pulled";
    std.Io.Dir.cwd().deleteTree(testing.io, dst) catch {};
    const p = try pull(&dev, testing.io, gpa, dst, false, 1_790_000_000);
    try testing.expectEqual(Pulled{ .files = 3, .bytes = 0x2000 + 0x8000 + 0x8030 }, p);
    // the clock's save: the time stamp of the pull, the rest as on the card
    const clock = try std.Io.Dir.cwd().readFileAlloc(testing.io, dst ++ "/gb/games/clock.sav", gpa, .limited(1 << 20));
    defer gpa.free(clock);
    try testing.expectEqual(@as(u64, 1_790_000_000), std.mem.readInt(u64, clock[0x8028..][0..8], .little));
    try testing.expect(std.mem.allEqual(u8, clock[0..0x8028], 0));
    const count = try std.Io.Dir.cwd().readFileAlloc(testing.io, dst ++ "/gb/games/count.sav", gpa, .limited(1 << 20));
    defer gpa.free(count);
    try testing.expectEqual(@as(usize, 0x2000), count.len);
    for (count, 0..) |b, i| try testing.expectEqual(@as(u8, @truncate(i * 5 + 1)), b);
    // a card without /saves: nothing to pull, not an error
    const plain = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/fat32.img", gpa, .limited(1 << 27));
    defer gpa.free(plain);
    var dev2: ImgDev = .{ .data = plain };
    try testing.expect((try pull(&dev2, testing.io, gpa, dst, false, 0)).no_folder);
    // which files get a stamp: RAM (and MBC2's 512) + 48 only
    var b = [_]u8{0} ** (512 + 48);
    try testing.expect(stampClock(&b, 7) and b[512 + 40] == 7);
    try testing.expect(stampClock(b[0..48], 7));
    try testing.expect(!stampClock(b[0..512], 7));
    try testing.expect(!stampClock(b[0..47], 7));
}
