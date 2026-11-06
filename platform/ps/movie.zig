//! Input movies (side track O, O.2; PLAN.md "Side track O"): a game's
//! input, one word per frame, recorded while it runs and played back
//! frame by frame, as a tool-assisted speedrun does.
//!
//! The PL side is O.1's frame-synchronous input (Platform.Ps.FrameInput):
//! with the movie register's sync bit set, the core sees the input word
//! latched at each of its frame starts, and PS_IN bit 7 flips there. The PS
//! changes the word only right after a flip (`Sync`), so the word the core
//! latched at the next flip is the one the PS knows it set: a recording is
//! exact, and a playback hands the core the same word at the same frame.
//!
//! A movie file: a 512-byte header (`Header`), then a state area (a copy
//! of the save state it starts from, when it doesn't start at power-on:
//! the movie stays good if the slot is overwritten), then the input words,
//! two bytes each (little endian), frame 0 first, from `words_at` (the
//! header says where: a card slot keeps a state area of the game's state
//! size even for a power-on movie). On the card the file has a fixed size,
//! made when the game first records (side track K, F1; `fileSize`,
//! `/movies/<system folder>/<path>/<name>.mv1`);
//! the PS rewrites its blocks in place, as the state slots (`Engine`).
//!
//! Shared by the PS app, the desktop's link and the Verilator harness
//! (arty-sim's --movie-record/--movie-play): std only.
const std = @import("std");

pub const magic = "ECMOVIE1";
pub const version = 1;
pub const header_bytes = 512;
pub const block_words = 256; // 512-byte blocks of words

/// How a movie starts.
pub const Start = enum(u8) { power_on = 0, state = 1 };

pub const Header = struct {
    system: u8,
    start: Start = .power_on,
    /// frames recorded (words in the file)
    frames: u32,
    /// bytes of the save state that follows the header (0 at power-on)
    state_len: u32 = 0,
    /// where the words start in the file (`wordsOffset`; 0 = right after
    /// the state, `encodeFile` fills it in)
    words_at: u32 = 0,
    /// the game, as the save states take it (savestate.RomId)
    title: [16]u8 = [_]u8{0} ** 16,
    sum: u16 = 0,
    rom_size: u32 = 0,
    /// written last, as a state slot's: a recording cut off reads as empty
    used: bool = true,
    /// bytes a frame's input takes (side track Q, 2026-10-29: the desktop's
    /// SNES (4) and 800XL (11) movies; the PS's systems all 2); on disk 0
    /// means 2, so every file before it reads the same
    frame_bytes: u16 = 2,

    pub fn encode(h: Header, b: *[header_bytes]u8) void {
        @memset(b, 0);
        @memcpy(b[0..8], magic);
        b[8] = h.system;
        b[9] = version;
        b[10] = @intFromEnum(h.start);
        b[11] = @intFromBool(h.used);
        std.mem.writeInt(u32, b[12..16], h.frames, .little);
        std.mem.writeInt(u32, b[16..20], h.state_len, .little);
        std.mem.writeInt(u32, b[20..24], h.words_at, .little);
        @memcpy(b[32..48], &h.title);
        std.mem.writeInt(u16, b[48..50], h.sum, .little);
        std.mem.writeInt(u32, b[50..54], h.rom_size, .little);
        std.mem.writeInt(u16, b[24..26], if (h.frame_bytes == 2) 0 else h.frame_bytes, .little);
    }

    /// Null if the block is no movie header (a slot `make sd` left empty,
    /// another version).
    pub fn decode(b: *const [header_bytes]u8) ?Header {
        if (!std.mem.eql(u8, b[0..8], magic) or b[9] != version) return null;
        if (std.mem.readInt(u32, b[20..24], .little) % 512 != 0) return null;
        var h: Header = .{
            .system = b[8],
            .start = std.enums.fromInt(Start, b[10]) orelse return null,
            .used = b[11] == 1,
            .frames = std.mem.readInt(u32, b[12..16], .little),
            .state_len = std.mem.readInt(u32, b[16..20], .little),
            .words_at = std.mem.readInt(u32, b[20..24], .little),
            .sum = std.mem.readInt(u16, b[48..50], .little),
            .rom_size = std.mem.readInt(u32, b[50..54], .little),
            .frame_bytes = switch (std.mem.readInt(u16, b[24..26], .little)) {
                0 => 2,
                else => |n| n,
            },
        };
        @memcpy(&h.title, b[32..48]);
        return h;
    }
};

/// The state area's size for a save state of `state_bytes` (0: the system
/// has no save states), whole blocks.
pub fn stateArea(state_bytes: u32) u32 {
    return std.mem.alignForward(u32, state_bytes, 512);
}

/// Where the words start.
pub fn wordsOffset(state_area: u32) u32 {
    return header_bytes + state_area;
}

/// A slot file's size: header, state area, `max_frames` words.
pub fn fileSize(state_area: u32, max_frames: u32) u32 {
    return wordsOffset(state_area) + std.mem.alignForward(u32, max_frames * 2, 512);
}

/// Frames a slot file of `size` holds.
pub fn maxFrames(size: u32, state_area: u32) u32 {
    return if (size <= wordsOffset(state_area)) 0 else (size - wordsOffset(state_area)) / 2;
}

/// The frame rule, the same for the PS app and the harness. `applied` is
/// the word on PS_GPIO; it changes only at a flip of PS_IN bit 7, right
/// after the core latched the one before.
pub const Sync = struct {
    applied: u16,

    /// Recording: at a flip, the core latched `applied`, the frame's word
    /// for the file; the live input (keyboard, desktop) becomes the next.
    pub fn record(s: *Sync, live: u16) u16 {
        const w = s.applied;
        s.applied = live;
        return w;
    }

    /// Playing: at a flip, the next frame's word from the file.
    pub fn play(s: *Sync, next: u16) void {
        s.applied = next;
    }
};

/// Words packed into 512-byte blocks, as they go to the card.
pub const BlockWriter = struct {
    block: [512]u8 = undefined,
    n: u16 = 0,

    /// Adds a word; the full block when this one filled it.
    pub fn push(w: *BlockWriter, x: u16) ?*const [512]u8 {
        std.mem.writeInt(u16, w.block[w.n * 2 ..][0..2], x, .little);
        w.n += 1;
        if (w.n < block_words) return null;
        w.n = 0;
        return &w.block;
    }

    /// The last, partial block (zeros after its words), or null if empty.
    pub fn flush(w: *BlockWriter) ?*const [512]u8 {
        if (w.n == 0) return null;
        @memset(w.block[w.n * 2 ..], 0);
        w.n = 0;
        return &w.block;
    }
};

/// A block's `i`th word.
pub fn word(block: *const [512]u8, i: usize) u16 {
    return std.mem.readInt(u16, block[i * 2 ..][0..2], .little);
}

/// A whole movie in memory (the harness, the desktop): header, state, words.
pub fn encodeFile(gpa: std.mem.Allocator, h: Header, state: []const u8, words: []const u16) ![]u8 {
    const area = stateArea(@intCast(state.len));
    const out = try gpa.alloc(u8, fileSize(area, @intCast(words.len)));
    @memset(out, 0);
    var hh = h;
    hh.frames = @intCast(words.len);
    hh.state_len = @intCast(state.len);
    hh.start = if (state.len == 0) .power_on else .state;
    hh.words_at = wordsOffset(area);
    hh.encode(out[0..header_bytes]);
    @memcpy(out[header_bytes..][0..state.len], state);
    for (words, 0..) |w, i| std.mem.writeInt(u16, out[wordsOffset(area) + i * 2 ..][0..2], w, .little);
    return out;
}

pub const Decoded = struct { header: Header, state: []const u8, words: []const u16 };

/// A movie file's parts (the words copied into `gpa`'s memory); one of
/// more than 2 bytes a frame is `decodeFileRaw`'s.
pub fn decodeFile(gpa: std.mem.Allocator, bytes: []const u8) !Decoded {
    if (bytes.len < header_bytes) return error.NotAMovie;
    const h = Header.decode(bytes[0..header_bytes]) orelse return error.NotAMovie;
    if (!h.used) return error.EmptyMovie;
    if (h.frame_bytes != 2) return error.WideFrames;
    if (h.words_at < header_bytes + h.state_len or bytes.len < h.words_at + @as(usize, h.frames) * 2) return error.ShortMovie;
    const words = try gpa.alloc(u16, h.frames);
    for (words, 0..) |*w, i| w.* = std.mem.readInt(u16, bytes[h.words_at + i * 2 ..][0..2], .little);
    return .{ .header = h, .state = bytes[header_bytes..][0..h.state_len], .words = words };
}

/// A movie of `frame_bytes` a frame (the desktop's SNES and 800XL ones):
/// `data` the frames' inputs, `frame_bytes` each, frame 0 first, from
/// `words_at` as the words; 2 makes the same file as `encodeFile`.
pub fn encodeFileRaw(gpa: std.mem.Allocator, h: Header, state: []const u8, frame_bytes: u16, data: []const u8) ![]u8 {
    if (frame_bytes == 0 or data.len % frame_bytes != 0) return error.BadFrames;
    const area = stateArea(@intCast(state.len));
    const at = wordsOffset(area);
    const out = try gpa.alloc(u8, at + std.mem.alignForward(usize, data.len, 512));
    @memset(out, 0);
    var hh = h;
    hh.frames = @intCast(data.len / frame_bytes);
    hh.frame_bytes = frame_bytes;
    hh.state_len = @intCast(state.len);
    hh.start = if (state.len == 0) .power_on else .state;
    hh.words_at = at;
    hh.encode(out[0..header_bytes]);
    @memcpy(out[header_bytes..][0..state.len], state);
    @memcpy(out[at..][0..data.len], data);
    return out;
}

pub const DecodedRaw = struct { header: Header, state: []const u8, data: []const u8 };

/// Any movie file's parts, whatever its frame size (slices of `bytes`).
pub fn decodeFileRaw(bytes: []const u8) !DecodedRaw {
    if (bytes.len < header_bytes) return error.NotAMovie;
    const h = Header.decode(bytes[0..header_bytes]) orelse return error.NotAMovie;
    if (!h.used) return error.EmptyMovie;
    const len = @as(usize, h.frames) * h.frame_bytes;
    if (h.words_at < header_bytes + h.state_len or bytes.len < h.words_at + len) return error.ShortMovie;
    return .{ .header = h, .state = bytes[header_bytes..][0..h.state_len], .data = bytes[h.words_at..][0..len] };
}

pub const movies_dir = "movies";

/// The slot's path: "/movies" in front, ".mv<slot>" for the ending, as
/// savestate.statePath. Null if it doesn't fit.
pub fn moviePath(buf: []u8, dir_path: []const u8, rom_name: []const u8, slot: u8) ?[]const u8 {
    const dot = std.mem.lastIndexOfScalar(u8, rom_name, '.') orelse rom_name.len;
    const sep: []const u8 = if (dir_path.len > 0 and dir_path[dir_path.len - 1] == '/') "" else "/";
    const lead: []const u8 = if (dir_path.len > 0 and dir_path[0] == '/') "" else "/";
    return std.fmt.bufPrint(buf, "/{s}{s}{s}{s}{s}.mv{d}", .{ movies_dir, lead, dir_path, sep, rom_name[0..dot], slot }) catch null;
}

/// A slot file's blocks on the card: its runs of consecutive blocks (from
/// fat.Fs.runs). `make sd` copies each file in one piece, so one run is
/// usual; more than `max_runs` are refused.
pub const Blocks = struct {
    pub const max_runs = 16;
    lba: [max_runs]u32 = undefined,
    len: [max_runs]u32 = undefined,
    n: u8 = 0,

    pub fn add(b: *Blocks, lba: u32, count: u32) error{Fragmented}!void {
        if (b.n > 0 and b.lba[b.n - 1] + b.len[b.n - 1] == lba) {
            b.len[b.n - 1] += count;
            return;
        }
        if (b.n == max_runs) return error.Fragmented;
        b.lba[b.n] = lba;
        b.len[b.n] = count;
        b.n += 1;
    }

    /// The card block of the file's block k.
    pub fn at(b: *const Blocks, k: u32) error{PastEnd}!u32 {
        var left = k;
        for (b.lba[0..b.n], b.len[0..b.n]) |lba, len| {
            if (left < len) return lba + left;
            left -= len;
        }
        return error.PastEnd;
    }
};

pub const Mode = enum { off, record, play };

/// What a frame toggle brought.
pub const Step = union(enum) {
    /// no new frame
    none,
    /// a new frame: this word goes to PS_GPIO now (`Sync.applied`)
    word: u16,
    /// the movie stopped by itself (the input is live again): played to its
    /// end, or the slot is full
    ended: enum { done, full },
};

/// A movie being recorded into, or played from, a slot file on the card
/// (`card.readBlock`, `card.writeBlock`). The caller sets the sync bit and
/// starts the game first (power-on: the core held, its RAM cleared, let
/// go), then calls `poll` with PS_IN bit 7 and the live word as often as it
/// can (at least once a frame), and puts `Step.word` on PS_GPIO.
pub const Engine = struct {
    mode: Mode = .off,
    blocks: Blocks = .{},
    header: Header = .{ .system = 0, .frames = 0 },
    sync: Sync = .{ .applied = 0 },
    toggle: bool = false,
    /// frames recorded or played so far
    frame: u32 = 0,
    /// recording: the slot's room; playing: the movie's frames
    limit: u32 = 0,
    out: BlockWriter = .{},
    /// playing: the block in `in` (file block number)
    in: [512]u8 = undefined,
    in_k: ?u32 = null,

    /// Off, as `.{}`, in place: the PS app's engine is a global, and a
    /// struct literal of all of it (its two 512-byte blocks too) is kept
    /// whole in the OCM as the value to copy from (side K, 2026-10-18).
    pub fn init(e: *Engine) void {
        e.begin(.off, .{ .system = 0, .frames = 0 }, false);
        e.blocks.n = 0;
    }

    /// Every field but the blocks' list and the two buffers: a new field
    /// must be set here too (the count below says so).
    fn begin(e: *Engine, mode: Mode, h: Header, toggle: bool) void {
        comptime std.debug.assert(@typeInfo(Engine).@"struct".fields.len == 10);
        e.mode = mode;
        e.header = h;
        e.sync = .{ .applied = 0 };
        e.toggle = toggle;
        e.frame = 0;
        e.limit = 0;
        e.out.n = 0;
        e.in_k = null;
    }

    /// Record from now on into a slot of `size` bytes whose words start at
    /// `h.words_at`: the header first, unused (a recording cut off before
    /// its first block of words reads as empty).
    pub fn startRecord(e: *Engine, card: anytype, blocks: Blocks, size: u32, h: Header, live: u16, toggle: bool) !void {
        e.begin(.record, h, toggle);
        e.blocks = blocks;
        e.sync = .{ .applied = live };
        e.limit = maxFrames(size, h.words_at - header_bytes);
        if (e.limit == 0) return error.NoRoom;
        e.header.frames = 0;
        e.header.used = false;
        try e.writeHeader(card);
    }

    /// Play the movie in a slot: its header (already checked by the
    /// caller, `check`); the first word goes to PS_GPIO before the game
    /// starts (`firstWord`).
    pub fn startPlay(e: *Engine, card: anytype, blocks: Blocks, h: Header, toggle: bool) !void {
        e.begin(.play, h, toggle);
        e.blocks = blocks;
        e.limit = h.frames;
        e.sync = .{ .applied = if (h.frames > 0) try e.readWord(card, 0) else 0 };
    }

    pub fn firstWord(e: *const Engine) u16 {
        return e.sync.applied;
    }

    /// A poll: `toggle` is PS_IN bit 7, `live` the keyboard's and the
    /// desktop's word (recording).
    pub fn poll(e: *Engine, card: anytype, toggle: bool, live: u16) !Step {
        if (e.mode == .off or toggle == e.toggle) return .none;
        e.toggle = toggle;
        switch (e.mode) {
            .off => unreachable,
            .record => {
                // the core latched `applied` at this flip: frame `frame`'s word
                const w = e.sync.record(live);
                if (e.out.push(w)) |full| try e.writeWords(card, full);
                e.frame += 1;
                if (e.frame == e.limit) {
                    try e.stop(card);
                    return .{ .ended = .full };
                }
                return .{ .word = e.sync.applied };
            },
            .play => {
                e.frame += 1;
                if (e.frame >= e.limit) {
                    e.mode = .off;
                    return .{ .ended = .done };
                }
                e.sync.play(try e.readWord(card, e.frame));
                return .{ .word = e.sync.applied };
            },
        }
    }

    /// Stop: a recording's last words and its header (used, its frames).
    pub fn stop(e: *Engine, card: anytype) !void {
        const was = e.mode;
        e.mode = .off;
        if (was != .record) return;
        if (e.out.n > 0) {
            const at = e.frame - e.out.n;
            const last = e.out.flush().?;
            try card.writeBlock(try e.blocks.at((e.header.words_at + at * 2) / 512), last);
        }
        e.header.frames = e.frame;
        e.header.used = e.frame > 0;
        try e.writeHeader(card);
    }

    /// A full block of words; the header then says it holds them, so a
    /// recording cut off (the power) keeps all but its last few seconds.
    fn writeWords(e: *Engine, card: anytype, block: *const [512]u8) !void {
        const first = e.frame + 1 - block_words; // the block's first frame
        try card.writeBlock(try e.blocks.at((e.header.words_at + first * 2) / 512), block);
        e.header.frames = e.frame + 1;
        e.header.used = true;
        try e.writeHeader(card);
    }

    fn writeHeader(e: *Engine, card: anytype) !void {
        var b: [512]u8 = undefined;
        e.header.encode(&b);
        try card.writeBlock(try e.blocks.at(0), &b);
    }

    fn readWord(e: *Engine, card: anytype, i: u32) !u16 {
        const off = e.header.words_at + i * 2;
        const k = off / 512;
        if (e.in_k != k) {
            try card.readBlock(try e.blocks.at(k), &e.in);
            e.in_k = k;
        }
        return word(&e.in, (off % 512) / 2);
    }
};

/// Why a slot's movie isn't played.
pub const Refusal = enum { empty, other_system, other_game, from_state };

pub fn refusalText(r: Refusal) []const u8 {
    return switch (r) {
        .empty => "The movie slot is empty.",
        .other_system => "That movie is another system's.",
        .other_game => "That movie is another game's.",
        .from_state => "That movie starts from a state (not yet).",
    };
}

/// Whether a slot's movie plays on this game.
pub fn check(h: ?Header, system: u8, title: *const [16]u8, sum: u16, rom_size: u32) ?Refusal {
    const m = h orelse return .empty;
    if (!m.used or m.frames == 0) return .empty;
    if (m.system != system) return .other_system;
    if (!std.mem.eql(u8, &m.title, title) or m.sum != sum or m.rom_size != rom_size) return .other_game;
    if (m.start != .power_on) return .from_state;
    return null;
}

/// A card for the tests: blocks in memory.
const TestCard = struct {
    mem: [64][512]u8 = [_][512]u8{[_]u8{0xEE} ** 512} ** 64,
    writes: u32 = 0,
    pub fn readBlock(c: *TestCard, lba: u32, b: *[512]u8) !void {
        b.* = c.mem[lba];
    }
    pub fn writeBlock(c: *TestCard, lba: u32, b: *const [512]u8) !void {
        c.mem[lba] = b.*;
        c.writes += 1;
    }
    /// The file at blocks `blocks` as one byte slice.
    fn file(c: *const TestCard, blocks: Blocks, n: u32, out: []u8) void {
        for (0..n) |k| @memcpy(out[k * 512 ..][0..512], &c.mem[blocks.at(@intCast(k)) catch unreachable]);
    }
};

test "blocks: runs joined when they touch, block k found, the end and fragments refused" {
    var b: Blocks = .{};
    try b.add(10, 2);
    try b.add(12, 3);
    try b.add(40, 1);
    try std.testing.expectEqual(@as(u8, 2), b.n);
    try std.testing.expectEqual(@as(u32, 10), try b.at(0));
    try std.testing.expectEqual(@as(u32, 14), try b.at(4));
    try std.testing.expectEqual(@as(u32, 40), try b.at(5));
    try std.testing.expectError(error.PastEnd, b.at(6));
    var f: Blocks = .{};
    for (0..Blocks.max_runs) |i| try f.add(@intCast(i * 2), 1);
    try std.testing.expectError(error.Fragmented, f.add(100, 1));
}

test "slot path: under /movies, .mv<n>" {
    var buf: [64]u8 = undefined;
    try std.testing.expectEqualStrings("/movies/gb/games/tetris.mv1", moviePath(&buf, "/gb/games", "tetris.gb", 1).?);
    try std.testing.expectEqualStrings("/movies/gb/x.mv2", moviePath(&buf, "gb/", "x", 2).?);
    var small: [8]u8 = undefined;
    try std.testing.expect(moviePath(&small, "/gb/games", "tetris.gb", 1) == null);
}

test "engine: a recording across blocks, stopped, plays back the words the core latched" {
    var card: TestCard = .{};
    // a slot: header, a 1-block state area, 3 blocks of words (768 frames),
    // in two runs
    var blocks: Blocks = .{};
    try blocks.add(5, 3);
    try blocks.add(20, 2);
    const size: u32 = 5 * 512;
    const h: Header = .{ .system = 2, .frames = 0, .words_at = 1024, .sum = 7 };
    var e: Engine = .{};
    try e.startRecord(&card, blocks, size, h, 0x10, false);
    try std.testing.expectEqual(@as(u32, 768), e.limit);
    try std.testing.expect(!Header.decode(card.mem[5][0..header_bytes]).?.used);
    // 600 frames: the live word changes at odd moments, several polls a frame
    var toggle = false;
    var latched: [600]u16 = undefined;
    var live: u16 = 0x10;
    for (0..600) |f| {
        for (0..3) |p| {
            if (p == 1) live = @truncate(f *% 37 +% 5);
            try std.testing.expectEqual(Step.none, try e.poll(&card, toggle, live));
        }
        latched[f] = e.sync.applied; // the core latches at the flip
        toggle = !toggle;
        const st = try e.poll(&card, toggle, live);
        try std.testing.expectEqual(Step{ .word = live }, st);
    }
    // two full blocks in: the header says 512 frames so far
    try std.testing.expectEqual(@as(u32, 512), Header.decode(card.mem[5][0..header_bytes]).?.frames);
    try e.stop(&card);
    const done = Header.decode(card.mem[5][0..header_bytes]).?;
    try std.testing.expect(done.used);
    try std.testing.expectEqual(@as(u32, 600), done.frames);
    // the file as the desktop sees it
    var bytes: [5 * 512]u8 = undefined;
    card.file(blocks, 5, &bytes);
    const d = try decodeFile(std.testing.allocator, &bytes);
    defer std.testing.allocator.free(d.words);
    try std.testing.expectEqualSlices(u16, &latched, d.words);
    // played back: the same word at each flip as recorded
    var p: Engine = .{};
    try p.startPlay(&card, blocks, done, false);
    try std.testing.expectEqual(latched[0], p.firstWord());
    toggle = false;
    for (1..600) |f| {
        try std.testing.expectEqual(Step.none, try p.poll(&card, toggle, 0xFFFF));
        toggle = !toggle;
        try std.testing.expectEqual(Step{ .word = latched[f] }, try p.poll(&card, toggle, 0xFFFF));
    }
    toggle = !toggle;
    try std.testing.expectEqual(Step{ .ended = .done }, try p.poll(&card, toggle, 0));
    try std.testing.expectEqual(Mode.off, p.mode);
}

test "engine: a full slot stops the recording by itself, used" {
    var card: TestCard = .{};
    var blocks: Blocks = .{};
    try blocks.add(0, 2); // header + 256 frames
    var e: Engine = .{};
    try e.startRecord(&card, blocks, 1024, .{ .system = 2, .frames = 0, .words_at = 512 }, 1, false);
    var toggle = false;
    for (0..255) |_| {
        toggle = !toggle;
        _ = try e.poll(&card, toggle, 1);
    }
    toggle = !toggle;
    try std.testing.expectEqual(Step{ .ended = .full }, try e.poll(&card, toggle, 1));
    try std.testing.expectEqual(@as(u32, 256), Header.decode(card.mem[0][0..header_bytes]).?.frames);
    try std.testing.expectError(error.NoRoom, e.startRecord(&card, blocks, 512, .{ .system = 2, .frames = 0, .words_at = 512 }, 1, false));
}

test "check: empty, another system or game, from a state; this game's plays" {
    const t = "TETRIS\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00".*;
    const h: Header = .{ .system = 2, .frames = 10, .title = t, .sum = 3, .rom_size = 32768 };
    try std.testing.expectEqual(@as(?Refusal, .empty), check(null, 2, &t, 3, 32768));
    var u = h;
    u.used = false;
    try std.testing.expectEqual(@as(?Refusal, .empty), check(u, 2, &t, 3, 32768));
    try std.testing.expectEqual(@as(?Refusal, .other_system), check(h, 6, &t, 3, 32768));
    try std.testing.expectEqual(@as(?Refusal, .other_game), check(h, 2, &t, 4, 32768));
    var s = h;
    s.start = .state;
    try std.testing.expectEqual(@as(?Refusal, .from_state), check(s, 2, &t, 3, 32768));
    try std.testing.expectEqual(@as(?Refusal, null), check(h, 2, &t, 3, 32768));
}

test "header round trip; an empty or foreign block is no header" {
    var b: [header_bytes]u8 = undefined;
    const h: Header = .{ .system = 2, .start = .state, .frames = 1234, .state_len = 54321, .words_at = 55296, .title = "JOYBANK\x00\x00\x00\x00\x00\x00\x00\x00\x00".*, .sum = 0xBEEF, .rom_size = 131072 };
    h.encode(&b);
    try std.testing.expectEqualDeep(h, Header.decode(&b).?);
    b[9] = version + 1;
    try std.testing.expect(Header.decode(&b) == null);
    @memset(&b, 0);
    try std.testing.expect(Header.decode(&b) == null);
}

test "sizes: whole blocks, the words after the state" {
    try std.testing.expectEqual(@as(u32, 0), stateArea(0));
    try std.testing.expectEqual(@as(u32, 1024), stateArea(513));
    try std.testing.expectEqual(@as(u32, 512 + 1024 + 1024), fileSize(1024, 300));
    try std.testing.expectEqual(@as(u32, 512), maxFrames(fileSize(1024, 300), 1024));
    try std.testing.expectEqual(@as(u32, 0), maxFrames(100, 0));
}

test "sync: a recording plays back to the words the core latched" {
    // the core latches `applied` at each flip; live input changes at any time
    const live = [_]u16{ 0, 0, 1, 1, 0x11, 0, 0x80, 0x80, 0 };
    var rec: Sync = .{ .applied = live[0] };
    var latched_rec: [live.len]u16 = undefined;
    var file: [live.len]u16 = undefined;
    for (live, 0..) |l, i| {
        latched_rec[i] = rec.applied; // the flip: the core latches
        file[i] = rec.record(l);
    }
    try std.testing.expectEqualSlices(u16, &latched_rec, &file);
    var pl: Sync = .{ .applied = file[0] };
    for (0..file.len) |i| {
        try std.testing.expectEqual(file[i], pl.applied); // latched at flip i
        if (i + 1 < file.len) pl.play(file[i + 1]);
    }
}

test "blocks: 256 words a block, the last one padded" {
    var w: BlockWriter = .{};
    var full: u32 = 0;
    for (0..600) |i| if (w.push(@intCast(i))) |b| {
        try std.testing.expectEqual(@as(u16, @intCast(full * 256)), word(b, 0));
        try std.testing.expectEqual(@as(u16, @intCast(full * 256 + 255)), word(b, 255));
        full += 1;
    };
    try std.testing.expectEqual(@as(u32, 2), full);
    const last = w.flush().?;
    try std.testing.expectEqual(@as(u16, 599), word(last, 87));
    try std.testing.expectEqual(@as(u16, 0), word(last, 88));
    try std.testing.expect(w.flush() == null);
}

test "file round trip, with and without a state" {
    const gpa = std.testing.allocator;
    const words = [_]u16{ 1, 2, 3, 0xFFFF };
    for ([_][]const u8{ "", "a state" }) |state| {
        const f = try encodeFile(gpa, .{ .system = 2, .frames = 0 }, state, &words);
        defer gpa.free(f);
        const d = try decodeFile(gpa, f);
        defer gpa.free(d.words);
        try std.testing.expectEqualSlices(u16, &words, d.words);
        try std.testing.expectEqualSlices(u8, state, d.state);
        try std.testing.expectEqual(if (state.len == 0) Start.power_on else Start.state, d.header.start);
    }
    var empty = [_]u8{0} ** 1024;
    try std.testing.expectError(error.NotAMovie, decodeFile(gpa, &empty));
}

test "wide frames: a raw round trip (4 and 11 bytes a frame), refused by decodeFile; 2 bytes the same file as encodeFile" {
    const gpa = std.testing.allocator;
    inline for (.{ 4, 11 }) |n| {
        var data: [n * 300]u8 = undefined;
        for (&data, 0..) |*x, i| x.* = @truncate(i * 7 + 3);
        const f = try encodeFileRaw(gpa, .{ .system = 9, .frames = 0 }, "abc", n, &data);
        defer gpa.free(f);
        const d = try decodeFileRaw(f);
        try std.testing.expectEqual(@as(u16, n), d.header.frame_bytes);
        try std.testing.expectEqual(@as(u32, 300), d.header.frames);
        try std.testing.expectEqualStrings("abc", d.state);
        try std.testing.expectEqualSlices(u8, &data, d.data);
        try std.testing.expectError(error.WideFrames, decodeFile(gpa, f));
    }
    const words = [_]u16{ 1, 0x8000, 0xABCD };
    const old = try encodeFile(gpa, .{ .system = 2, .frames = 0 }, "", &words);
    defer gpa.free(old);
    const raw = try encodeFileRaw(gpa, .{ .system = 2, .frames = 0 }, "", 2, std.mem.sliceAsBytes(&words));
    defer gpa.free(raw);
    try std.testing.expectEqualSlices(u8, old, raw);
    // (a file before frame_bytes: byte 24 zero, read as 2)
    try std.testing.expectEqual(@as(u8, 0), old[24]);
    const d = try decodeFileRaw(old);
    try std.testing.expectEqual(@as(u16, 2), d.header.frame_bytes);
    try std.testing.expectError(error.BadFrames, encodeFileRaw(gpa, .{ .system = 9, .frames = 0 }, "", 4, "abc"));
}
