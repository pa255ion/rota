//! Save states (side track L, PLAN.md L.7): the running game's whole
//! state, read out of the PL into a slot file on the card and written back
//! from it. The PL side is the system's save-state hooks ("Platform.SaveState",
//! the Game Boy's in Gb.Core.gbCoreHooked): with the game paused (the menu
//! open), the PS sets the state mode, the core freezes once at rest, and its
//! state's bytes are read through a read window (onto PS_IN's byte channel,
//! as the battery saves' reads) and written through a write window;
//! cartridge RAM goes through the battery saves' own windows.
//!
//! A slot is `/states/<system folder>/<path below it>/<name>.st1` to
//! `.st4`, made at its full size when a state is first saved into it (side
//! track K, F1: save.makeFile; then the PS rewrites its blocks in place,
//! never growing it): a 512-byte header, the core's
//! record (`record_max` bytes kept for it), each section of the system's
//! state space, then cartridge RAM. The header says whose state it is
//! (system, record size and layout version from the PL, the ROM's title,
//! checksum and size); a slot whose header doesn't match is refused.
//!
//! Loading through the CPU (L.6c, the Game Boy): the record is the
//! system's state table, read only. `prepare` builds the restore routine
//! from the slot's table (`Spec.restore`), before anything changes, so a
//! slot it can't restore leaves the game as it was; then the caller holds
//! the core (its reset), `load` writes the memories and the routine into
//! the core's ROM overlay, and the caller lets the core go: the routine
//! puts the rest back.
//!
//! Generic over the file system (fat.Fs), the card and the link, as
//! save.zig, so the tests run it on a card image and a model of the PL.
const std = @import("std");
const menu = @import("menu.zig");
const gb_restore = @import("systems/gb_restore.zig");
// no fat.zig here, not even for the tests: this file is also part of the
// "menu" module (menu.System.state), which the desktop's link imports
// beside "fat"; a slot file is anything with a `size` (a fat.Entry)

pub const slots = 4;
pub const header_bytes = 512;
/// bytes kept in the file for the core's record (its real size, from the
/// PL, is in the header)
pub const record_max = 4096;
pub const magic = "ECSTATE1";

/// A part of the system's state space, after the record (which starts at
/// 0 and whose size the PL reports). Its address may pass 64 KiB (side
/// track N: the 800XL's RAM is at 0x10000 in its 128 KiB space).
pub const Section = struct { addr: u32, len: u32 };

/// A system's save states (menu.System.state).
pub const Spec = struct {
    /// the state mode: 1 freezes the core (once paused and at rest), 0
    /// lets it go
    mode_addr: u24,
    /// a byte written at wbase + n writes byte n of the state (frozen)
    wbase: u24,
    /// a byte written at rbase + n reads byte n onto the byte channel
    rbase: u24,
    /// the status bytes: frozen (1), the record's size (2 bytes, low
    /// first), the layout version
    status: u16,
    sections: []const Section,
    /// bytes of cartridge RAM a ROM has (from its first block), battery or
    /// not: they go through the battery saves' windows (`menu.Save`)
    cram: *const fn (head: []const u8) u32,
    /// loading through the CPU (L.6c): the record is a table the PL only
    /// reads, turned into a routine for the core's ROM overlay
    restore: ?Restore = null,
};

/// A system whose states load through its CPU (Spec.restore).
pub const Restore = struct {
    table_size: u16,
    overlay_size: u16,
    /// the overlay's bytes in the state space
    overlay_addr: u16,
    /// its end address (low, high), then 1 to arm it
    end_addr: u16,
    /// the routine for a table into the overlay; its end address, or null
    /// (a table it can't restore)
    build: *const fn (table: []const u8, overlay: []u8) ?u16,
    /// slot bytes copied into the work buffer before `build` (state-space
    /// address, offset in the table and the overlay after it, length):
    /// what the PL's table can't hold (side track N: the Game Gear's RAM
    /// under its mapper registers; the NES's OAM, into the overlay page its
    /// routine's DMA reads, and the few RAM bytes its routine puts back)
    fills: []const Fill = &.{},
    /// bytes `load` needs as its work buffer
    pub fn work(r: Restore) usize {
        return @as(usize, r.table_size) + r.overlay_size;
    }
};

/// A piece of a slot's sections copied into the work buffer ('Restore.fills';
/// `table` counts from the table's start, on into the overlay).
pub const Fill = struct { addr: u32, table: u16, len: u16 };

/// Where state-space address `addr` (in a section) is in a slot file.
fn fileOffset(spec: Spec, addr: u32) ?u32 {
    var base: u32 = header_bytes + record_max;
    for (spec.sections) |sec| {
        if (addr >= sec.addr and addr - sec.addr < sec.len) return base + (addr - sec.addr);
        base += sec.len;
    }
    return null;
}

/// Which game a state belongs to: the header's title bytes, its global
/// checksum, the ROM's size (Game Boy: 0x134 to 0x143, 0x14E).
pub const RomId = struct {
    title: [16]u8 = [_]u8{0} ** 16,
    sum: u16 = 0,
    size: u32 = 0,

    pub fn ofGb(head: []const u8, size: u32) RomId {
        var r: RomId = .{ .size = size };
        if (head.len >= 0x150) {
            @memcpy(&r.title, head[0x134..0x144]);
            r.sum = @as(u16, head[0x14E]) << 8 | head[0x14F];
        }
        return r;
    }
};

/// What the PL reports of its state's layout.
pub const Layout = struct { record: u16, version: u8 };

pub const Header = struct {
    system: u8,
    layout: Layout,
    cram: u32,
    rom: RomId,
    used: bool,

    pub fn encode(h: Header, b: *[header_bytes]u8) void {
        @memset(b, 0);
        @memcpy(b[0..8], magic);
        b[8] = h.system;
        b[9] = h.layout.version;
        std.mem.writeInt(u16, b[10..12], h.layout.record, .little);
        std.mem.writeInt(u32, b[12..16], h.cram, .little);
        @memcpy(b[16..32], &h.rom.title);
        std.mem.writeInt(u16, b[32..34], h.rom.sum, .little);
        std.mem.writeInt(u32, b[34..38], h.rom.size, .little);
        b[38] = @intFromBool(h.used);
    }

    /// Null if the block is no state header (a slot `make sd` left empty).
    pub fn decode(b: *const [header_bytes]u8) ?Header {
        if (!std.mem.eql(u8, b[0..8], magic)) return null;
        var h: Header = .{
            .system = b[8],
            .layout = .{ .version = b[9], .record = std.mem.readInt(u16, b[10..12], .little) },
            .cram = std.mem.readInt(u32, b[12..16], .little),
            .rom = .{ .sum = std.mem.readInt(u16, b[32..34], .little), .size = std.mem.readInt(u32, b[34..38], .little) },
            .used = b[38] == 1,
        };
        @memcpy(&h.rom.title, b[16..32]);
        return h;
    }
};

/// A slot file's size for a ROM with `cram` bytes of cartridge RAM.
pub fn fileSize(spec: Spec, cram: u32) u32 {
    var n: u32 = header_bytes + record_max;
    for (spec.sections) |s| n += s.len;
    return n + cram;
}

/// The state area a movie slot keeps (side track O, movie.zig): room for a
/// whole state slot's bytes of this game, in whole blocks, so a movie may
/// start from a state; 0 for a system without save states. `make sd`
/// (link --movie-slots) and the PS app both size a slot with it.
pub fn movieArea(spec: ?Spec, cram: u32) u32 {
    const s = spec orelse return 0;
    return std.mem.alignForward(u32, fileSize(s, cram), 512);
}

/// The slot's path: "/states" in front, ".st<slot>" for the ending (slot
/// 1 to 4), as save.savePath. Null if it doesn't fit.
pub fn statePath(buf: []u8, dir_path: []const u8, rom_name: []const u8, slot: u8) ?[]const u8 {
    const dot = std.mem.lastIndexOfScalar(u8, rom_name, '.') orelse rom_name.len;
    const sep: []const u8 = if (dir_path.len > 0 and dir_path[dir_path.len - 1] == '/') "" else "/";
    const lead: []const u8 = if (dir_path.len > 0 and dir_path[0] == '/') "" else "/";
    return std.fmt.bufPrint(buf, "/{s}{s}{s}{s}{s}.st{d}", .{ states_dir, lead, dir_path, sep, rom_name[0..dot], slot }) catch null;
}
pub const states_dir = "states";

/// Why a slot isn't loaded.
pub const Refusal = enum { empty, other_system, other_game, other_bitstream, wrong_size };

pub fn refusalText(r: Refusal) []const u8 {
    return switch (r) {
        .empty => "That slot is empty.",
        .other_system => "That state is another system's.",
        .other_game => "That state is another game's.",
        .other_bitstream => "That state is from another bitstream.",
        .wrong_size => "The slot file has the wrong size.",
    };
}

/// Whether a slot's header lets it load into this game and bitstream.
pub fn check(h: ?Header, system: u8, layout: Layout, rom: RomId, cram: u32) ?Refusal {
    const s = h orelse return .empty;
    if (!s.used) return .empty;
    if (s.system != system) return .other_system;
    if (!std.mem.eql(u8, &s.rom.title, &rom.title) or s.rom.sum != rom.sum or s.rom.size != rom.size or s.cram != cram) return .other_game;
    if (s.layout.record != layout.record or s.layout.version != layout.version) return .other_bitstream;
    return null;
}

// --- the PL -------------------------------------------------------------------

/// The next byte of the byte channel (as save.nextByte).
fn nextByte(link: anytype, toggle: *bool) !u8 {
    var i: u32 = 0;
    while (i < 1000) : (i += 1) {
        const in = link.inputs();
        if (in.toggle != toggle.*) {
            toggle.* = in.toggle;
            return in.byte;
        }
        link.port.delayUs(1);
    }
    return error.NoAnswer;
}

/// `n` bytes of the state space from `addr`, read through the window.
fn readBytes(link: anytype, spec: Spec, addr: u32, out: []u8, toggle: *bool) !void {
    link.setAddr(spec.rbase + @as(u24, @intCast(addr)));
    for (out) |*b| {
        link.data(&.{0});
        b.* = try nextByte(link, toggle);
    }
}

/// Freeze the core (the game is paused): the state mode on, then the
/// status read until it says frozen (the core comes to rest within a few
/// clocks; 100 ms without it is an error). The byte channel carries the
/// reads (the battery saves' control). The PL's layout comes back.
pub fn freeze(link: anytype, spec: Spec, sv: menu.Save) !Layout {
    link.write(sv.ctrl_addr, &.{1});
    link.write(spec.mode_addr, &.{1});
    var toggle = link.inputs().toggle;
    var st: [4]u8 = undefined;
    var tries: u32 = 0;
    while (true) : (tries += 1) {
        try readBytes(link, spec, spec.status, &st, &toggle);
        if (st[0] == 1) break;
        if (tries == 100) return error.NotFrozen;
        link.port.delayUs(1000);
    }
    return .{ .record = @as(u16, st[1]) | @as(u16, st[2]) << 8, .version = st[3] };
}

/// The core goes on (after `freeze`).
pub fn thaw(link: anytype, spec: Spec, sv: menu.Save) void {
    link.write(spec.mode_addr, &.{0});
    link.write(sv.ctrl_addr, &.{0});
}

// --- a slot -----------------------------------------------------------------

/// The bytes of a slot file in order, from the PL (frozen): the header,
/// the record (padded to record_max), the sections, cartridge RAM.
fn Source(comptime Link: type) type {
    return struct {
        link: Link,
        spec: Spec,
        save: menu.Save,
        header: [header_bytes]u8,
        layout: Layout,
        cram: u32,
        toggle: bool,
        /// where the next byte is in the file
        at: u32 = 0,

        const Self = @This();
        /// Fills `out` with the file's next bytes.
        fn fill(s: *Self, out: []u8) !void {
            var i: usize = 0;
            while (i < out.len) {
                const at = s.at;
                if (at < header_bytes) {
                    const n = @min(out.len - i, header_bytes - at);
                    @memcpy(out[i..][0..n], s.header[at..][0..n]);
                    i += n;
                    s.at += @intCast(n);
                    continue;
                }
                var off: u32 = at - header_bytes;
                if (off < record_max) {
                    const n: u32 = @intCast(@min(out.len - i, record_max - off));
                    const real = @min(n, @as(u32, s.layout.record) -| off);
                    if (real > 0) try readBytes(s.link, s.spec, off, out[i..][0..real], &s.toggle);
                    @memset(out[i + real ..][0 .. n - real], 0);
                    i += n;
                    s.at += n;
                    continue;
                }
                off -= record_max;
                var done = false;
                for (s.spec.sections) |sec| {
                    if (off < sec.len) {
                        const n: u32 = @intCast(@min(out.len - i, sec.len - off));
                        try readBytes(s.link, s.spec, sec.addr + off, out[i..][0..n], &s.toggle);
                        i += n;
                        s.at += n;
                        done = true;
                        break;
                    }
                    off -= sec.len;
                }
                if (done) continue;
                // cartridge RAM, through its read window
                const n: u32 = @intCast(@min(out.len - i, s.cram -| off));
                if (n == 0) return error.PastEnd;
                s.link.setAddr(s.save.read_addr + @as(u24, @intCast(off)));
                for (out[i..][0..n]) |*b| {
                    s.link.data(&.{0});
                    b.* = try nextByte(s.link, &s.toggle);
                }
                i += n;
                s.at += n;
            }
        }
    };
}

/// The frozen core's state into slot file `e` (its blocks rewritten in
/// place where they changed; the header last, marked used, so a slot cut
/// off half way reads as empty).
pub fn save(fs: anytype, card: anytype, e: anytype, link: anytype, spec: Spec, sv: menu.Save, layout: Layout, h: Header, buf: *[512]u8, old: *[512]u8) !void {
    if (e.size != fileSize(spec, h.cram)) return error.WrongSize;
    defer fs.buf_lba = null;
    var src: Source(@TypeOf(link)) = .{ .link = link, .spec = spec, .save = sv, .header = undefined, .layout = layout, .cram = h.cram, .toggle = link.inputs().toggle };
    var unused = h;
    unused.used = false;
    unused.encode(&src.header);
    var first: ?u32 = null;
    var left = e.size;
    var runs = fs.runs(e, 0xFFFF);
    while (try runs.next()) |run| {
        var k: u32 = 0;
        while (k < run.blocks and left > 0) : (k += 1) {
            const n = @min(left, 512);
            try src.fill(buf[0..n]);
            if (first == null) first = run.lba + k;
            try card.readBlock(run.lba + k, old);
            if (!std.mem.eql(u8, old[0..n], buf[0..n])) {
                @memcpy(old[0..n], buf[0..n]);
                try card.writeBlock(run.lba + k, old);
            }
            left -= n;
        }
    }
    // the header, now used
    try card.readBlock(first.?, old);
    var hb: [header_bytes]u8 = undefined;
    h.encode(&hb);
    @memcpy(old[0..header_bytes], &hb);
    try card.writeBlock(first.?, old);
}

/// Every section of the state space zeroed through the write window (the
/// core frozen and held): a movie's power-on start (side track O). The
/// Game Boy's RAMs keep their bytes over a reset (`NoClearOnReset`), so
/// without this a recording and its playback would start from whatever the
/// game before left there.
pub fn clearSections(link: anytype, spec: Spec) void {
    // (a section's address is u32: the 800XL's and the NES's pass 64 KiB)
    for (spec.sections) |sec| link.fill(@intCast(@as(u32, spec.wbase) + sec.addr), 0, sec.len);
}

/// A slot's header (its first block), null if it holds none.
pub fn readHeader(fs: anytype, card: anytype, e: anytype, buf: *[512]u8) !?Header {
    defer fs.buf_lba = null;
    try card.readBlock(try fs.firstBlock(e), buf);
    return Header.decode(buf[0..header_bytes]);
}

/// A load through the CPU (`spec.restore`) with the core held: held (its
/// reset), `load`, let go again once it all went in. If the load fails
/// half way the core stays held: part of the memories are the slot's,
/// and a game run on from there is neither the old nor the saved one
/// (side K's review, 2026-10-13). The caller says so, and starting a game
/// lets go of the hold as always.
pub fn loadHeld(fs: anytype, e: anytype, link: anytype, spec: Spec, sv: menu.Save, layout: Layout, cram: u32, work: []const u8, end: u16) !void {
    link.setHold(true);
    try load(fs, e, link, spec, sv, layout, cram, work, end);
    link.setHold(false);
}

/// With `spec.restore`: the routine for slot file `e`'s table, into
/// `work` (the table, then the overlay); its end address. Nothing goes to
/// the PL. error.CantRestore for a table the routine can't put back.
pub fn prepare(fs: anytype, e: anytype, spec: Spec, layout: Layout, work: []u8) !u16 {
    const rs = spec.restore orelse return error.CantRestore;
    if (work.len < rs.work() or layout.record != rs.table_size) return error.WrongSize;
    var r = fs.reader(e);
    var at: u32 = 0;
    const tbl = work[0..rs.table_size];
    // how far to read: the table, and the fills' bytes
    var need: u32 = header_bytes + rs.table_size;
    for (rs.fills) |f| {
        const o = fileOffset(spec, f.addr) orelse return error.WrongSize;
        if (@as(usize, f.table) + f.len > rs.work()) return error.WrongSize;
        need = @max(need, o + f.len);
    }
    while (try r.next()) |chunk| {
        for (chunk, 0..) |b, i| {
            const pos = at + @as(u32, @intCast(i));
            if (pos >= header_bytes and pos < header_bytes + rs.table_size) tbl[pos - header_bytes] = b;
            for (rs.fills) |f| {
                const o = fileOffset(spec, f.addr).?;
                if (pos >= o and pos < o + f.len) work[f.table + (pos - o)] = b;
            }
        }
        at += @intCast(chunk.len);
        if (at >= need) break;
    }
    if (at < need) return error.WrongSize;
    return rs.build(tbl, work[rs.table_size..][0..rs.overlay_size]) orelse error.CantRestore;
}

/// Slot file `e` into the frozen core: the record, the sections and
/// cartridge RAM written through the windows (the caller checked the
/// header: `check`). With `spec.restore` the record (the table) isn't
/// written: after the rest, the routine `prepare` put into `work` goes
/// into the overlay with its end address `end`, armed; the caller has
/// held the core.
pub fn load(fs: anytype, e: anytype, link: anytype, spec: Spec, sv: menu.Save, layout: Layout, cram: u32, work: []const u8, end: u16) !void {
    if (e.size != fileSize(spec, cram)) return error.WrongSize;
    if (spec.restore) |rs| {
        if (work.len < rs.work() or layout.record != rs.table_size) return error.WrongSize;
    }
    var r = fs.reader(e);
    var at: u32 = 0;
    while (try r.next()) |chunk| {
        var i: usize = 0;
        while (i < chunk.len) {
            const pos = at + @as(u32, @intCast(i));
            if (pos < header_bytes) {
                i += @min(chunk.len - i, header_bytes - pos);
                continue;
            }
            var off = pos - header_bytes;
            if (off < record_max) {
                const n: u32 = @intCast(@min(chunk.len - i, record_max - off));
                const real = @min(n, @as(u32, layout.record) -| off);
                if (real > 0 and spec.restore == null) link.write(spec.wbase + @as(u24, @intCast(off)), chunk[i..][0..real]);
                i += n;
                continue;
            }
            off -= record_max;
            var done = false;
            for (spec.sections) |sec| {
                if (off < sec.len) {
                    const n: u32 = @intCast(@min(chunk.len - i, sec.len - off));
                    link.write(spec.wbase + @as(u24, @intCast(sec.addr + off)), chunk[i..][0..n]);
                    i += n;
                    done = true;
                    break;
                }
                off -= sec.len;
            }
            if (done) continue;
            const n: u32 = @intCast(@min(chunk.len - i, cram -| off));
            if (n == 0) break;
            link.write(sv.ram_addr + @as(u24, @intCast(off)), chunk[i..][0..n]);
            i += n;
        }
        at += @intCast(chunk.len);
    }
    if (spec.restore) |rs| {
        link.write(spec.wbase + rs.overlay_addr, work[rs.table_size..][0..rs.overlay_size]);
        link.write(spec.wbase + rs.end_addr, &.{ @truncate(end), @truncate(end >> 8), 1 });
    }
}

// --- the Game Boy -------------------------------------------------------------

/// Cartridge RAM a Game Boy ROM has, from its header (0x147 the type, 0x149
/// the RAM size; MBC2's 512 half-bytes as bytes), battery or not.
pub fn gbCram(head: []const u8) u32 {
    if (head.len < 0x150) return 0;
    const t = head[0x147];
    if (t == 0x05 or t == 0x06) return 512;
    return switch (head[0x149]) {
        2 => 0x2000,
        3 => 0x8000,
        4 => 0x20000,
        5 => 0x10000,
        else => 0,
    };
}

/// Gb.Loader's state mode and windows; Gb.Core's state space after the
/// record: HRAM, VRAM's two banks, WRAM's eight.
pub const gb: Spec = .{
    .mode_addr = 0x140050,
    .wbase = 0x150000,
    .rbase = 0x160000,
    .status = 0x1F00,
    .sections = &.{
        .{ .addr = 0x1000, .len = 0x80 },
        .{ .addr = 0x2000, .len = 0x4000 },
        .{ .addr = 0x8000, .len = 0x8000 },
    },
    .cram = gbCram,
    .restore = .{
        .table_size = gb_restore.table_size,
        .overlay_size = gb_restore.overlay_size,
        .overlay_addr = 0x6000,
        .end_addr = 0x1F10,
        .build = gbBuild,
    },
};

fn gbBuild(table: []const u8, overlay: []u8) ?u16 {
    const r = gb_restore.build(table[0..gb_restore.table_size], overlay[0..gb_restore.overlay_size]) catch return null;
    return r.end;
}

// --- tests: a model of the Game Boy's PL side, a card image in memory ---------

const testing = std.testing;
const pl = @import("pl.zig");

/// The Game Boy's link map with save states (Gb.Loader, Gb.Core): the
/// state space and cartridge RAM, the windows, the mode, the byte channel.
const Model = struct {
    space: [0x10000]u8 = [_]u8{0} ** 0x10000,
    cram: [0x20000]u8 = [_]u8{0} ** 0x20000,
    record: u16 = gb_restore.table_size,
    version: u8 = 3,
    strobe: bool = false,
    addr: u24 = 0,
    mode: bool = false,
    read_back: bool = false,
    byte: u8 = 0,
    toggle: bool = false,
    /// writes refused (frozen-only, while not frozen)
    refused: u32 = 0,
    /// reads still answered (null: all); 0: the PL has gone quiet
    answers: ?u32 = null,
    /// the hold line
    held: bool = false,

    fn status(m: *const Model, i: u16) u8 {
        return switch (i) {
            0 => @intFromBool(m.mode),
            1 => @truncate(m.record),
            2 => @truncate(m.record >> 8),
            3 => m.version,
            else => 0,
        };
    }
    fn answer(m: *Model, b: u8) void {
        if (m.answers) |*n| {
            if (n.* == 0) return;
            n.* -= 1;
        }
        m.byte = b;
        m.toggle = !m.toggle;
    }
    pub fn write(m: *Model, v: u32) void {
        m.held = v & (1 << 26) != 0;
        const st = v & (1 << 24) != 0;
        if (st == m.strobe) return;
        m.strobe = st;
        const d: u8 = @truncate(v >> 16);
        if (v & (1 << 25) != 0) {
            m.addr = m.addr << 8 | d;
            return;
        }
        const a = m.addr;
        m.addr +%= 1;
        if (a >= 0x150000 and a < 0x160000) {
            if (m.mode) m.space[a - 0x150000] = d else m.refused += 1;
        } else if (a >= 0x160000 and a < 0x170000) {
            const n: u16 = @intCast(a - 0x160000);
            if (m.read_back) m.answer(if (n >= 0x1F00 and n < 0x1F04) m.status(n - 0x1F00) else if (m.mode) m.space[n] else 0);
        } else if (a >= 0x100000 and a < 0x120000) {
            if (m.mode) m.cram[a - 0x100000] = d else m.refused += 1;
        } else if (a >= 0x120000 and a < 0x140000) {
            if (m.read_back) m.answer(m.cram[a - 0x120000]);
        } else if (a == 0x140000) {
            m.read_back = d & 1 != 0;
        } else if (a == 0x140050) {
            m.mode = d & 1 != 0;
        }
    }
    pub fn read(m: *Model) u32 {
        return @as(u32, m.byte) << 24 | 0x02 << 16 | @as(u32, @intFromBool(m.toggle)) << 5;
    }
    pub fn delayUs(_: *Model, _: u32) void {}
};

const gbSave = menu.systemById(2).?.save.?;

test "the header: encoded and decoded; an empty block is none" {
    var b: [header_bytes]u8 = undefined;
    var rom: RomId = .{ .sum = 0xBEEF, .size = 0x8000 };
    @memcpy(rom.title[0..5], "TETRI");
    const h: Header = .{ .system = 2, .layout = .{ .record = 540, .version = 1 }, .cram = 0x2000, .rom = rom, .used = true };
    h.encode(&b);
    try testing.expectEqual(h, Header.decode(&b).?);
    @memset(&b, 0);
    try testing.expect(Header.decode(&b) == null);
}

test "check: empty, another system, game or bitstream" {
    const rom: RomId = .{ .sum = 1, .size = 2 };
    const lay: Layout = .{ .record = 540, .version = 1 };
    const h: Header = .{ .system = 2, .layout = lay, .cram = 0, .rom = rom, .used = true };
    try testing.expectEqual(@as(?Refusal, null), check(h, 2, lay, rom, 0));
    try testing.expectEqual(@as(?Refusal, .empty), check(null, 2, lay, rom, 0));
    var u = h;
    u.used = false;
    try testing.expectEqual(@as(?Refusal, .empty), check(u, 2, lay, rom, 0));
    try testing.expectEqual(@as(?Refusal, .other_system), check(h, 6, lay, rom, 0));
    try testing.expectEqual(@as(?Refusal, .other_game), check(h, 2, lay, .{ .sum = 9, .size = 2 }, 0));
    try testing.expectEqual(@as(?Refusal, .other_game), check(h, 2, lay, rom, 0x2000));
    try testing.expectEqual(@as(?Refusal, .other_bitstream), check(h, 2, .{ .record = 541, .version = 1 }, rom, 0));
    try testing.expectEqual(@as(?Refusal, .other_bitstream), check(h, 2, .{ .record = 540, .version = 2 }, rom, 0));
}

test "paths and sizes" {
    var b: [128]u8 = undefined;
    try testing.expectEqualStrings("/states/gb/games/x.st1", statePath(&b, "/gb/games", "x.gb", 1).?);
    try testing.expectEqualStrings("/states/GB/Some Game (Rev 1).st4", statePath(&b, "/GB", "Some Game (Rev 1).gb", 4).?);
    try testing.expectEqual(@as(u32, 512 + 4096 + 0x80 + 0x4000 + 0x8000 + 0x2000), fileSize(gb, 0x2000));
    var head = [_]u8{0} ** 0x150;
    head[0x149] = 3;
    try testing.expectEqual(@as(u32, 0x8000), gbCram(&head));
    head[0x147] = 0x06;
    try testing.expectEqual(@as(u32, 512), gbCram(&head));
    head[0x147] = 0x00;
    head[0x149] = 0;
    try testing.expectEqual(@as(u32, 0), gbCram(&head));
}

/// A card image with one slot file (made by the test as `make sd` would).
const Card = struct {
    data: []u8,
    pub fn readBlock(d: *Card, lba: u32, buf: *[512]u8) !void {
        const off = @as(usize, lba) * 512;
        if (off + 512 > d.data.len) return error.OutOfRange;
        @memcpy(buf, d.data[off..][0..512]);
    }
    pub fn writeBlock(d: *Card, lba: u32, buf: *const [512]u8) !void {
        const off = @as(usize, lba) * 512;
        if (off + 512 > d.data.len) return error.OutOfRange;
        @memcpy(d.data[off..][0..512], buf);
    }
};

/// A minimal file system over one contiguous file at block 1: what save
/// and load use of fat.Fs (runs, reader, firstBlock, buf_lba).
const OneFile = struct {
    card: *Card,
    buf_lba: ?u32 = null,
    /// the card fails after this many blocks read (null: never)
    fail_after: ?u32 = null,
    const Run = struct { lba: u32, blocks: u32 };
    const Runs = struct {
        e: Entry,
        done: bool = false,
        pub fn next(r: *Runs) !?Run {
            if (r.done) return null;
            r.done = true;
            return .{ .lba = 1, .blocks = (r.e.size + 511) / 512 };
        }
    };
    pub fn runs(_: *OneFile, e: Entry, _: u32) Runs {
        return .{ .e = e };
    }
    pub fn firstBlock(_: *OneFile, _: Entry) !u32 {
        return 1;
    }
    const Reader = struct {
        f: *OneFile,
        size: u32,
        at: u32 = 0,
        pub fn next(r: *Reader) !?[]const u8 {
            if (r.at >= r.size) return null;
            if (r.f.fail_after) |n| {
                if (r.at / 512 >= n) return error.ReadFailed;
            }
            const n = @min(r.size - r.at, 512);
            const s = r.f.card.data[512 + r.at ..][0..n];
            r.at += n;
            return s;
        }
    };
    pub fn reader(f: *OneFile, e: Entry) Reader {
        return .{ .f = f, .size = e.size };
    }
};

/// A slot file, as far as save and load look at it.
const Entry = struct { size: u32 };

fn entryOf(size: u32) Entry {
    return .{ .size = size };
}

test "a state saved from one core and loaded into another: every byte, the header last" {
    const cram: u32 = 0x2000;
    const size = fileSize(gb, cram);
    const img = try testing.allocator.alloc(u8, 512 + size + 512);
    defer testing.allocator.free(img);
    @memset(img, 0);
    var card: Card = .{ .data = img };
    var fs: OneFile = .{ .card = &card };
    const e = entryOf(size);

    // core A: a pattern everywhere, a DMG's state table at 0 (in vertical
    // blank, MBC1, the LCD on)
    var a: Model = .{};
    for (&a.space, 0..) |*b, i| b.* = @truncate(i *% 7 +% 3);
    for (&a.cram, 0..) |*b, i| b.* = @truncate(i *% 13 +% 1);
    @memset(a.space[0..gb_restore.table_size], 0);
    a.space[gb_restore.t.mbc] = 1;
    a.space[gb_restore.t.ly] = 145;
    a.space[gb_restore.t.dot] = 100;
    a.space[gb_restore.t.lcdc] = 0x91;
    a.space[gb_restore.t.pc] = 0x50;
    a.space[gb_restore.t.pc + 1] = 0x01;
    var la: pl.Link(Model) = .{ .port = &a };
    const lay = try freeze(&la, gb, gbSave);
    try testing.expectEqual(Layout{ .record = gb_restore.table_size, .version = 3 }, lay);
    var rom: RomId = .{ .sum = 0x1234, .size = 0x8000 };
    @memcpy(rom.title[0..4], "FILL");
    const h: Header = .{ .system = 2, .layout = lay, .cram = cram, .rom = rom, .used = true };
    var buf: [512]u8 = undefined;
    var old: [512]u8 = undefined;
    try save(&fs, &card, e, &la, gb, gbSave, lay, h, &buf, &old);
    thaw(&la, gb, gbSave);
    try testing.expect(!a.mode and !a.read_back);
    try testing.expectEqual(h, (try readHeader(&fs, &card, e, &buf)).?);

    // core B: something else in it, then the slot
    var b: Model = .{};
    @memset(&b.space, 0xA5);
    @memset(&b.cram, 0x5A);
    var lb: pl.Link(Model) = .{ .port = &b };
    const lay2 = try freeze(&lb, gb, gbSave);
    try testing.expectEqual(@as(?Refusal, null), check(try readHeader(&fs, &card, e, &buf), 2, lay2, rom, cram));
    var work: [gb_restore.table_size + gb_restore.overlay_size]u8 = undefined;
    const end = try prepare(&fs, e, gb, lay2, &work);
    try testing.expectEqual(@as(u32, 0), b.refused);
    try testing.expectEqual(@as(u8, 0xA5), b.space[0x6000]); // nothing written yet
    try load(&fs, e, &lb, gb, gbSave, lay2, cram, &work, end);
    thaw(&lb, gb, gbSave);
    // the table isn't written (the PL only reads it); the routine built
    // from it is in the overlay, its end address and the arm after it
    try testing.expectEqual(@as(u8, 0xA5), b.space[0]);
    var ovl: [gb_restore.overlay_size]u8 = undefined;
    const r = try gb_restore.build(a.space[0..gb_restore.table_size], &ovl);
    try testing.expectEqualSlices(u8, &ovl, b.space[0x6000..][0..gb_restore.overlay_size]);
    try testing.expectEqualSlices(u8, &.{ @truncate(r.end), @truncate(r.end >> 8), 1 }, b.space[0x1F10..][0..3]);
    // the sections and cartridge RAM equal; the rest of the space (no
    // section) as it was
    for (gb.sections) |sec| try testing.expectEqualSlices(u8, a.space[sec.addr..][0..sec.len], b.space[sec.addr..][0..sec.len]);
    try testing.expectEqual(@as(u8, 0xA5), b.space[0x1080]);
    try testing.expectEqualSlices(u8, a.cram[0..cram], b.cram[0..cram]);
    try testing.expectEqual(@as(u8, 0x5A), b.cram[cram]);
    try testing.expectEqual(@as(u32, 0), b.refused);
}

test "the direct path (no restore routine) with a slot ending inside a block: CHIP-8's layout, every section byte" {
    // CHIP-8's sections (platform/ps/systems/chip8_state.zig), on the model's windows
    const c8: Spec = .{ .mode_addr = 0x140050, .wbase = 0x150000, .rbase = 0x160000, .status = 0x1F00, .sections = &.{
        .{ .addr = 0x2000, .len = 0x1000 },
        .{ .addr = 0x3000, .len = 0x100 },
    }, .cram = struct {
        fn f(_: []const u8) u32 {
            return 0;
        }
    }.f };
    const size = fileSize(c8, 0);
    try testing.expectEqual(@as(u32, 8960), size);
    const img = try testing.allocator.alloc(u8, 512 + size + 512);
    defer testing.allocator.free(img);
    @memset(img, 0);
    var card: Card = .{ .data = img };
    var fs: OneFile = .{ .card = &card };
    const e = entryOf(size);
    var a: Model = .{ .record = 0x40, .version = 1 };
    for (&a.space, 0..) |*x, i| x.* = @truncate(i *% 11 +% 5);
    var la: pl.Link(Model) = .{ .port = &a };
    const lay = try freeze(&la, c8, gbSave);
    const h: Header = .{ .system = 1, .layout = lay, .cram = 0, .rom = .{}, .used = true };
    var buf: [512]u8 = undefined;
    var old: [512]u8 = undefined;
    try save(&fs, &card, e, &la, c8, gbSave, lay, h, &buf, &old);
    thaw(&la, c8, gbSave);
    var b: Model = .{ .record = 0x40, .version = 1 };
    @memset(&b.space, 0xA5);
    var lb: pl.Link(Model) = .{ .port = &b };
    const lay2 = try freeze(&lb, c8, gbSave);
    var work: [1]u8 = undefined;
    try load(&fs, e, &lb, c8, gbSave, lay2, 0, &work, 0);
    thaw(&lb, c8, gbSave);
    try testing.expectEqualSlices(u8, a.space[0..0x40], b.space[0..0x40]);
    for (c8.sections) |sec| try testing.expectEqualSlices(u8, a.space[sec.addr..][0..sec.len], b.space[sec.addr..][0..sec.len]);
}

test "a slot cut off half way reads as empty; a slot of the wrong size is refused" {
    const size = fileSize(gb, 0);
    const img = try testing.allocator.alloc(u8, 512 + size + 512);
    defer testing.allocator.free(img);
    @memset(img, 0);
    var card: Card = .{ .data = img };
    var fs: OneFile = .{ .card = &card };
    var a: Model = .{};
    var la: pl.Link(Model) = .{ .port = &a };
    const lay = try freeze(&la, gb, gbSave);
    var buf: [512]u8 = undefined;
    var old: [512]u8 = undefined;
    const h: Header = .{ .system = 2, .layout = lay, .cram = 0, .rom = .{}, .used = true };
    try testing.expectError(error.WrongSize, save(&fs, &card, entryOf(size - 1), &la, gb, gbSave, lay, h, &buf, &old));
    var work: [gb_restore.table_size + gb_restore.overlay_size]u8 = undefined;
    try testing.expectError(error.WrongSize, load(&fs, entryOf(size + 512), &la, gb, gbSave, lay, 0, &work, 0));
    // the PL goes quiet half way: the slot's header says unused
    a.answers = 10000;
    try testing.expectError(error.NoAnswer, save(&fs, &card, entryOf(size), &la, gb, gbSave, lay, h, &buf, &old));
    const got = (try readHeader(&fs, &card, entryOf(size), &buf)).?;
    try testing.expect(!got.used);
    try testing.expectEqual(@as(?Refusal, .empty), check(got, 2, lay, .{}, 0));
}

test "clearSections: every section zeroed through the window, frozen; nothing else touched" {
    var a: Model = .{};
    var la: pl.Link(Model) = .{ .port = &a };
    @memset(&a.space, 0xAA);
    a.mode = true;
    clearSections(&la, gb);
    for (gb.sections) |sec| try testing.expect(std.mem.allEqual(u8, a.space[sec.addr..][0..sec.len], 0));
    try testing.expectEqual(@as(u8, 0xAA), a.space[0x0FFF]);
    try testing.expectEqual(@as(u8, 0xAA), a.space[0x1080]);
    try testing.expectEqual(@as(u8, 0xAA), a.space[0x6000]);
    try testing.expectEqual(@as(u32, 0), a.refused);
}

test "not frozen: an error after the tries, nothing written" {
    var a: Model = .{};
    var la: pl.Link(Model) = .{ .port = &a };
    // a PL that never freezes: no state mode decoded
    const never: Spec = .{ .mode_addr = 0x140060, .wbase = gb.wbase, .rbase = gb.rbase, .status = gb.status, .sections = gb.sections, .cram = gbCram };
    try testing.expectError(error.NotFrozen, freeze(&la, never, gbSave));
}

test "prepare: a table the routine can't put back (an MBC it doesn't know) is refused before the PL is touched" {
    const size = fileSize(gb, 0);
    const img = try testing.allocator.alloc(u8, 512 + size + 512);
    defer testing.allocator.free(img);
    @memset(img, 0);
    // the slot's table: an MBC the routine doesn't know
    img[512 + header_bytes + gb_restore.t.mbc] = 7;
    var card: Card = .{ .data = img };
    var fs: OneFile = .{ .card = &card };
    var work: [gb_restore.table_size + gb_restore.overlay_size]u8 = undefined;
    const lay: Layout = .{ .record = gb_restore.table_size, .version = 3 };
    try testing.expectError(error.CantRestore, prepare(&fs, entryOf(size), gb, lay, &work));
    img[512 + header_bytes + gb_restore.t.mbc] = 1;
    img[512 + header_bytes + gb_restore.t.ly] = 144;
    img[512 + header_bytes + gb_restore.t.dot] = 60;
    img[512 + header_bytes + gb_restore.t.lcdc] = 0x91;
    _ = try prepare(&fs, entryOf(size), gb, lay, &work);
}

test "a load that fails half way leaves the core held; one that goes in lets it go" {
    const cram: u32 = 0x2000;
    const size = fileSize(gb, cram);
    const img = try testing.allocator.alloc(u8, 512 + size + 512);
    defer testing.allocator.free(img);
    @memset(img, 0);
    // a slot: a DMG table the routine can restore
    const tb = img[512 + header_bytes ..][0..gb_restore.table_size];
    tb[gb_restore.t.mbc] = 1;
    tb[gb_restore.t.ly] = 145;
    tb[gb_restore.t.dot] = 100;
    tb[gb_restore.t.lcdc] = 0x91;
    var card: Card = .{ .data = img };
    var fs: OneFile = .{ .card = &card };
    const e = entryOf(size);
    var b: Model = .{};
    @memset(&b.space, 0xA5);
    var lb: pl.Link(Model) = .{ .port = &b };
    const lay = try freeze(&lb, gb, gbSave);
    var work: [gb_restore.table_size + gb_restore.overlay_size]u8 = undefined;
    const end = try prepare(&fs, e, gb, lay, &work);
    // the card fails in the middle of WRAM: part of the memories written
    fs.fail_after = 60;
    try testing.expectError(error.ReadFailed, loadHeld(&fs, e, &lb, gb, gbSave, lay, cram, &work, end));
    try testing.expect(b.held);
    try testing.expectEqual(@as(u8, 0), b.space[0x1000]); // HRAM: the slot's
    try testing.expectEqual(@as(u8, 0xA5), b.space[0xF000]); // WRAM's end: not reached
    // the whole slot: let go
    fs.fail_after = null;
    try loadHeld(&fs, e, &lb, gb, gbSave, lay, cram, &work, end);
    try testing.expect(!b.held);
}

test "prepare: a Spec's fills copy slot bytes into the table first (the Game Gear's RAM under its mapper)" {
    const gg_state = @import("systems/gg_state.zig");
    const gg_restore = @import("systems/gg_restore.zig");
    const spec = gg_state.spec;
    const size = fileSize(spec, 0);
    const img = try testing.allocator.alloc(u8, 512 + size + 512);
    defer testing.allocator.free(img);
    @memset(img, 0);
    // RAM's DFFCh-DFFFh in the slot: state space 9FFCh, after VRAM's 16 KiB
    const ram = 512 + header_bytes + record_max + 0x4000;
    @memcpy(img[ram + 0x1FFC ..][0..4], &[_]u8{ 0x11, 0x22, 0x33, 0x44 });
    var card: Card = .{ .data = img };
    var fs: OneFile = .{ .card = &card };
    var work: [gg_restore.table_size + gg_restore.overlay_size]u8 = undefined;
    const lay: Layout = .{ .record = gg_restore.table_size, .version = 1 };
    _ = try prepare(&fs, entryOf(size), spec, lay, &work);
    try testing.expectEqualSlices(u8, &[_]u8{ 0x11, 0x22, 0x33, 0x44 }, work[gg_restore.t.ramtail..][0..4]);
}
