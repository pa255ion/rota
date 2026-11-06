//! The ROM menu of the PS app, as pure state: board buttons in, actions and
//! a 40 x 30 text screen out (shown by the PL's text overlay,
//! Platform.Video.TextOverlay). No hardware access here, so it is tested
//! on the desktop; relay.zig does the SD card, the PL and the timing.
//!
//! Buttons (ZedBoard): U/D move the cursor (held: repeat), R or C opens a
//! directory or loads a ROM, L goes up a directory. Above the system's
//! folder is the Systems level (side track F): the bitstreams in
//! `/systems`; R or C starts one (the relay reconfigures the PL), L goes
//! back to the game. In a game, holding the system's menu combination for a second
//! opens the menu (`Buttons.menuHold`, `System.menu_combo`): CHIP-8, C
//! alone (only a modifier for CHIP-8 keys, so a long press of it alone does
//! nothing in a game); Game Boy, Up and Down together (C is its A button,
//! often held; a real D-pad can't press Up and Down at once).

const std = @import("std");
const gbc_palette = @import("gbc_palette.zig");
const options = @import("options.zig");
/// public for the desktop's link (its --state-slots, platform/host/saves.zig)
pub const savestate = @import("savestate.zig");
/// input movies (side track O; for the desktop's link: its --movie-slots)
pub const movie = @import("movie.zig");
const pl = @import("pl.zig");

pub const cols = pl.cols;
pub const rows = pl.rows;

/// What a system needs from the menu, chosen by the system ID the PL
/// reports on PS_IN (Platform.Ps.Link; IDs reserved in
/// docs/ARCHITECTURE.md: 1 CHIP-8, 2 Game Boy, 3 NES, 5 Atari 800XL). A new system adds
/// one entry here, and its top reports the ID; the relay does the rest
/// (relay.zig: list `dir`, check, hold, stream the file to `load_addr`,
/// clear, release).
/// See `System.ddr_info`.
pub const DdrInfo = struct {
    addr: u24,
    of: *const fn (rom: []const u8) [2]u8,
};

/// See `System.companion` (M6: the Master System's BIOS).
pub const Companion = struct {
    /// its name in the system's folder on the card (any case)
    name: []const u8,
    /// where it goes in DDR (the media region; its system's doc gives the layout)
    ddr_addr: u32,
    max_size: u32,
    /// a message when the file can't be used (its size)
    check: *const fn (size: u32) ?[]const u8,
    /// `ddr_info`'s word for a companion of `size` bytes (0: none on the card)
    word: *const fn (w: [2]u8, size: u32) [2]u8,
};

pub const System = struct {
    id: u8,
    name: []const u8,
    /// directory on the card holding this system's ROMs (any case)
    dir: []const u8,
    /// file name endings listed (any case)
    exts: []const []const u8,
    /// where the ROM goes in the system's link address map
    load_addr: u24,
    /// largest ROM (bigger files are refused)
    max_size: u32,
    /// the buttons held (exactly these, for `Buttons.hold_ms`) that open
    /// the menu in a game; something the game itself never asks for
    menu_combo: pl.Buttons = .{ .c = true, .d = false, .l = false, .r = false, .u = false },
    /// how to say it (the relay prints it at start-up)
    menu_combo_name: []const u8 = "C",
    /// clear (zero) from the ROM's end up to `max_size` on load (CHIP-8:
    /// the rest of RAM; the Game Boy doesn't need it)
    clear_rest: bool = false,
    /// a look at the file before loading: its first block (up to 512
    /// bytes) and its size; a reason to refuse it, or null to go ahead
    check: ?*const fn (head: []const u8, size: u32) ?[]const u8 = null,
    /// M7's request (2026-10-16): a check of the whole ROM once it is in
    /// DDR (systems with `ddr_addr`; a header past the first block, the
    /// SNES's at 7FC0h or FFC0h), before the core is let go: a reason
    /// refuses it, the core stays held (as a companion file refused).
    /// `link --load` runs it too, on the file
    check_ddr: ?*const fn (rom: []const u8) ?[]const u8 = null,
    /// side track Q (side K agreed, 2026-10-26: the 800XL's tapes): a
    /// file the PL reads in another form, made in DDR once it is there
    /// (`DdrImage`; systems with `ddr_addr`); `link --load` makes it too.
    /// Null: the file as it is
    ddr_image: ?DdrImage = null,
    /// battery saves (side track I; save.zig): how big a ROM's save file
    /// is, and where its RAM is in the system's link map; null = none
    save: ?Save = null,
    /// the ROM goes into DDR from here instead of over the link (side
    /// track I, phase 2: the Game Boy's cartridge, Gb.Top.romBase), while
    /// the system is held; `load_addr` is then unused
    ddr_addr: ?u32 = null,
    /// with `ddr_addr`: parts of the file (from its first block) that also
    /// go over the link once it is in DDR, each to `load_addr` + its offset,
    /// read back from DDR (side track D: the NES's header and CHR ROM; its
    /// PRG ROM is read from DDR); null = none
    ddr_link: ?*const fn (head: []const u8) [2]Range = null,
    /// the Game Boy Color: DMG cartridges' colours chosen by the PS
    /// (gbc_palette.zig): a "Palette:" line at the top of the ROM lists
    palette: bool = false,
    /// a few bytes worked out from the whole ROM once it is in DDR, written
    /// to `addr` in the system's link map while it is still held (the Game
    /// Gear, M5: its mapper and size, systems/gg_cart.zig)
    ddr_info: ?DdrInfo = null,
    /// a second file from the system's folder, loaded into DDR before the
    /// ROM when the card has it (M6: the Master System's BIOS, systems/sms_cart.zig);
    /// with `ddr_info`, which says so to the PL
    companion: ?Companion = null,
    /// side track L: the core's sound goes through Platform.Audio.Volume,
    /// so the Options page has a "Volume" row
    volume: bool = false,
    /// side track L: the fastest speed the core reaches, in quarters
    /// (options.zig; measured, PLAN.md L.4); 0 = the core has no speed
    /// control, no "Speed" row
    speed_top: u4 = 0,
    /// side track L, L.7: save states (savestate.zig): the Options page's
    /// slot rows, for a game loaded from the card; null = none
    state: ?savestate.Spec = null,
    /// M7's request (2026-10-16): the key word's bits 16 to 31 (keymap.Word;
    /// a second pad), written to this address of the system's link map on
    /// each change, two bytes, low first; null = the system has 16 bits
    /// (PS_GPIO's input word) at most
    keys_hi: ?u24 = null,
    /// side track O, O.2b: input movies (movie.zig): the core's tops have
    /// frame-synchronous input (Platform.Ps.FrameInput, the toggle on PS_IN
    /// bit 7), `make sd` makes its movie slots, the Options page its rows
    movies: bool = false,
    /// side track O, O.4: screenshots (Platform.Video.Capture in its tops):
    /// the picture's size; null = none
    screen: ?Picture = null,
};

/// A system's picture, as its frame buffer holds it.
pub const Picture = struct { w: u16, h: u16 };

/// `System.ddr_image`: the size of the form the PL reads for a file (0:
/// it reads the file as it is), and that form written into `out`.
pub const DdrImage = struct {
    size: *const fn (file: []const u8) usize,
    write: *const fn (file: []const u8, out: []u8) void,
};

/// Where the PS app keeps the file while it makes its `DdrImage`
/// (load.ddrImageIn): above the system's media, inside the media region.
/// The file must fit between it and the region's end (`media_end`; a
/// bigger one goes in as it is), and the space is free only while a load
/// runs: the image's maker reads the copy there, nothing keeps it after.
pub const ddr_image_scratch: u32 = 0x0800_0000;
/// The media region's end (exclusive): 0x02000000-0x0FFFFFFF
/// (ARCHITECTURE.md, "Resource budgets").
pub const media_end: u32 = 0x1000_0000;

/// A part of a ROM file: its offset and length (`System.ddr_link`).
pub const Range = struct { off: u32, len: u32 };

/// Battery-backed RAM on a cartridge (side track I, phase 1; save.zig,
/// docs/systems/gameboy.md "Side track I"). The save file of a ROM at
/// `/<dir>/<path>/<name>.<ext>` is `/saves/<dir>/<path>/<name>.sav`, made
/// at full size by `make sd` (link --save-slots, from `size`), rewritten
/// in place by the PS (the FAT is never written).
pub const Save = struct {
    /// bytes of the save file for a ROM (from its first block), 0 if it
    /// has no battery-backed RAM the system supports
    size: *const fn (head: []const u8) u32,
    /// M7's request: the save's size from the whole ROM (once in DDR), for a
    /// header past the first block; used instead of `size` when given (the
    /// desktop's `--save-slots` reads whole ROMs too)
    size_of_rom: ?*const fn (rom: []const u8) u32 = null,
    /// cartridge RAM, written while held (the save goes in before the
    /// game starts)
    ram_addr: u24,
    /// a byte written at read_addr + n reads byte n of the RAM onto the
    /// byte channel, while the game runs
    read_addr: u24,
    /// a write of 1 here puts those reads on the byte channel, 0 the
    /// system's own bytes again
    ctrl_addr: u24,
    /// any write here clears the dirty flag (PS_IN bit 6)
    clear_addr: u24,
    /// the cartridge has a clock (MBC3): its state is the file's last
    /// `clock_footer` bytes, in the layout other emulators use (VBA-M,
    /// BGB): S M H DL DH and the latched five as 32-bit little-endian
    /// words, then a 64-bit time stamp (0 here: the board has no clock
    /// of its own, so time doesn't advance while it is off)
    clock: ?*const fn (head: []const u8) bool = null,
    /// the clock's ten start bytes, written while held (as the RAM)
    clock_load_addr: u24 = 0,
    /// reads of the clock's ten bytes, as the RAM's read window
    clock_read_addr: u24 = 0,
};

/// The footer a cartridge clock adds to its save file.
pub const clock_footer = 48;

/// The folder of save files on the card.
pub const saves_dir = "saves";

pub const systems = @import("systems.zig").all;

/// "nes.bit" -> "nes" (any case of the ending; other names unchanged).
pub fn bitStem(name: []const u8) []const u8 {
    return if (hasExt(name, &bit_ext)) name[0 .. name.len - 4] else name;
}

/// `/systems/last.txt`, the system started last (side track F): the name
/// of its bitstream without ".bit", then spaces, a newline at the end.
/// The file keeps its size (`make sd` makes it 32 bytes), so the PS
/// rewrites its one block in place without touching the FAT.
pub const last_file = "last.txt";

/// The name in the file's text ("" if none).
pub fn readLast(text: []const u8) []const u8 {
    return std.mem.trim(u8, text, " \t\r\n\x00");
}

/// Writes `stem` into the start of the file's block (`size` bytes of it):
/// false if it doesn't fit.
pub fn writeLast(block: *[512]u8, size: u32, stem: []const u8) bool {
    if (size == 0 or size > block.len or stem.len + 1 > size) return false;
    @memset(block[0..size], ' ');
    @memcpy(block[0..stem.len], stem);
    block[size - 1] = '\n';
    return true;
}

pub fn systemById(id: u8) ?*const System {
    for (&systems) |*s| if (s.id == id) return s;
    return null;
}

pub fn hasExt(name: []const u8, exts: []const []const u8) bool {
    for (exts) |e| {
        if (name.len > e.len and std.ascii.eqlIgnoreCase(name[name.len - e.len ..], e)) return true;
    }
    return false;
}

// --- Screen ---------------------------------------------------------------------
pub const Screen = struct {
    cells: [rows][cols]u8 = [_][cols]u8{[_]u8{' '} ** cols} ** rows,

    pub fn clear(s: *Screen) void {
        for (&s.cells) |*r| @memset(r, ' ');
    }

    /// Text at (row, col), cut at the right edge; inverse sets bit 7.
    pub fn put(s: *Screen, row: usize, col: usize, text: []const u8, inverse: bool) void {
        var c = col;
        for (text) |ch| {
            if (c >= cols) break;
            s.cells[row][c] = (if (ch >= 0x20 and ch < 0x7F) ch else '?') | @as(u8, if (inverse) 0x80 else 0);
            c += 1;
        }
    }

    /// A whole row in inverse video (bar), text from col.
    pub fn bar(s: *Screen, row: usize, col: usize, text: []const u8) void {
        @memset(&s.cells[row], ' ' | 0x80);
        s.put(row, col, text, true);
    }

    /// The text of a row without inverse bits, trailing spaces dropped
    /// (for tests).
    pub fn line(s: *const Screen, row: usize, buf: *[cols]u8) []const u8 {
        var n: usize = 0;
        for (s.cells[row], 0..) |ch, i| {
            buf[i] = ch & 0x7F;
            if (buf[i] != ' ') n = i + 1;
        }
        return buf[0..n];
    }
};

// --- Buttons: edges, repeat, the long C press -------------------------------
pub const Event = enum { none, up, down, left, right, center };

pub const Buttons = struct {
    prev: pl.Buttons = @bitCast(@as(u5, 0)),
    /// time the current U/D press started, or of its last repeat
    rep_ms: u64 = 0,
    repeating: bool = false,
    /// C held alone (since c_since)
    c_down: bool = false,
    c_since: u64 = 0,
    c_fired: bool = false,

    pub const repeat_delay_ms = 400;
    pub const repeat_ms = 90;
    pub const hold_ms = 1000;

    /// One poll: the event for this moment (press edges; U/D repeat while held).
    pub fn poll(b: *Buttons, t: u64, now: pl.Buttons) Event {
        const p = b.prev;
        b.prev = now;
        if (now.u and !p.u) return b.startRepeat(t, .up);
        if (now.d and !p.d) return b.startRepeat(t, .down);
        if (now.l and !p.l) return .left;
        if (now.r and !p.r) return .right;
        if (now.c and !p.c) return .center;
        if ((now.u or now.d) and !(now.u and now.d)) {
            const wait: u64 = if (b.repeating) repeat_ms else repeat_delay_ms;
            if (t - b.rep_ms >= wait) {
                b.rep_ms = t;
                b.repeating = true;
                return if (now.u) .up else .down;
            }
        }
        return .none;
    }

    fn startRepeat(b: *Buttons, t: u64, e: Event) Event {
        b.rep_ms = t;
        b.repeating = false;
        return e;
    }

    /// In a game: true once, when exactly `combo` has been held for
    /// `hold_ms` (any other button cancels).
    pub fn menuHold(b: *Buttons, t: u64, now: pl.Buttons, combo: pl.Buttons) bool {
        b.prev = now;
        const alone = @as(u5, @bitCast(now)) == @as(u5, @bitCast(combo));
        if (!alone) {
            b.c_down = false;
            b.c_fired = false;
            return false;
        }
        if (!b.c_down) {
            b.c_down = true;
            b.c_since = t;
        }
        if (!b.c_fired and t - b.c_since >= hold_ms) {
            b.c_fired = true;
            return true;
        }
        return false;
    }

    pub fn anyHeld(b: *const Buttons) bool {
        return @as(u5, @bitCast(b.prev)) != 0;
    }
};

// --- The menu -------------------------------------------------------------------
pub const max_entries = 256;
pub const max_name = 63;
pub const max_depth = 8;

pub const Entry = struct {
    name_buf: [max_name]u8 = undefined,
    len: u8 = 0,
    is_dir: bool = false,
    size: u32 = 0,
    /// opaque for the menu: where the caller finds the file again
    ref: u32 = 0,

    pub fn name(e: *const Entry) []const u8 {
        return e.name_buf[0..e.len];
    }
};

pub const Action = union(enum) {
    /// nothing changed: no redraw needed
    none,
    /// only the menu's own state changed (the cursor): redraw
    moved,
    /// open entry i (a directory): the caller lists it with `enter`
    open_dir: usize,
    /// load entry i (a ROM)
    load: usize,
    /// go to the parent directory: the caller lists it with `leave`
    up,
    /// close the menu, back to the running game
    resume_game,
    /// try the card again (after an error)
    retry,
    /// from the top of the ROM list up to the Systems level (side track
    /// F): the caller lists `/systems` with `beginSystems`
    systems,
    /// start system i of the Systems level (a bitstream): the caller
    /// configures the PL with it, or goes back to the ROM list if it is
    /// the one running (`isRunning`)
    pick_system: usize,
    /// the running game's state into slot n (1 to 4), or back from it
    /// (side track L, L.7)
    save_state: u8,
    load_state: u8,
    /// input movies (side track O, O.2b): record one from power-on into
    /// the game's movie slot, play the slot's, stop the one running
    record_movie,
    play_movie,
    stop_movie,
    /// a screenshot into /screens (side track O, O.4)
    screenshot,
    /// the Controls page (side K): open it (the relay writes its rows,
    /// then `openControls`), button i's keyboard keys cleared, every key
    /// back to its default, and back to Options (the relay saves the key
    /// map if `ctl.changed`)
    controls,
    clear_key: usize,
    reset_keys,
    controls_done,
};

/// The menu's two levels: the running system's ROMs (its folder and
/// below), and above them the systems on the card (`/systems/*.bit`);
/// and the Options page (side track L), from the "<Options>" line at the
/// top of the ROM lists.
pub const Mode = enum { roms, systems, options, controls };
pub const bit_ext = [_][]const u8{".bit"};

/// Rows of the list (rows 3 to 26).
pub const list_top = 3;
pub const list_rows = 24;

/// The `ref` of the "Palette:" line (systems with `palette`).
pub const palette_ref = std.math.maxInt(u32);
/// The `ref` of the "<Options>" line (systems with `volume` or `speed_top`).
pub const options_ref = std.math.maxInt(u32) - 1;

/// The Options page's rows (side track L).
pub const OptRow = enum { volume, speed, slot, save_state, load_state, record_movie, play_movie, stop_movie, screenshot, controls };
pub const max_opt_rows = 9;

/// The Controls page (side K, 2026-10-18): the running system's buttons and
/// their keyboard keys, one row each, then "Reset to defaults". The relay
/// writes each row's text (`line`; the key map is its, not the menu's:
/// `link` builds this file and keymap.zig as two modules) and sets the
/// keys; the menu moves the cursor and says when it waits for a key.
pub const Controls = struct {
    pub const max = 32;
    /// buttons (0: the running system has no key map)
    n: u8 = 0,
    row: u8 = 0,
    top: u8 = 0,
    /// waiting for the key for button `row` (the relay takes it)
    waiting: bool = false,
    /// a key changed since the page opened (the relay saves on leaving)
    changed: bool = false,
    text: [max][cols]u8 = undefined,
    len: [max]u8 = undefined,

    /// Row i's text, as the relay builds it in place.
    pub fn line(c: *Controls, i: usize) *[cols]u8 {
        return &c.text[i];
    }

    /// The reset row after the buttons.
    fn rows(c: *const Controls) usize {
        return @as(usize, c.n) + 1;
    }
};

/// A movie running (side track O; the relay keeps the menu told).
pub const MovieState = enum { off, recording, playing };

pub const Menu = struct {
    system: *const System,
    /// the palette for DMG cartridges (systems with `palette`)
    pal: gbc_palette.Choice = 0,
    /// the volume and speed (side track L): the relay sends them to the
    /// PL when the menu closes
    opts: options.Settings = .{},
    /// the cursor on the Options page
    opt_row: usize = 0,
    /// save states (L.7): the slot chosen (1 to 4), whether each slot holds
    /// a state of this game (null: not known), and whether the running
    /// game has slots (loaded from the card; the relay sets it)
    slot: u8 = 1,
    slot_used: [savestate.slots]?bool = [_]?bool{null} ** savestate.slots,
    states_ok: bool = false,
    /// input movies (side track O, O.2b): whether the running game has a
    /// movie slot (a PS app built with movies, the slot file on the card;
    /// the relay sets it), the movie running, and the slot's frames (null:
    /// not known, 0: empty)
    movies_ok: bool = false,
    movie: MovieState = .off,
    /// screenshots (side track O, O.4): the running game can have one (a
    /// PS app built with them, a system with `screen`, a game from the card)
    shots_ok: bool = false,
    movie_frames: ?u32 = null,
    /// the Controls page (the relay sets `ctl.n` when a system starts)
    ctl: Controls = .{},
    entries: [max_entries]Entry = undefined,
    n: usize = 0,
    /// more entries than fit were found
    truncated: bool = false,
    cursor: usize = 0,
    top: usize = 0,
    /// path below the system directory, e.g. "/CHIP8/timendus"
    path_buf: [96]u8 = undefined,
    path_len: usize = 0,
    depth: usize = 0,
    /// cursor positions to go back to, per depth
    saved: [max_depth]usize = [_]usize{0} ** max_depth,
    /// an error instead of a list (no card, ...): C retries
    err: ?[]const u8 = null,
    /// status line
    msg_buf: [cols]u8 = undefined,
    msg_len: usize = 0,
    /// the USB host's state (usb.Status.text, side track E), on the row
    /// under the path; "" = none
    usb: []const u8 = "",
    /// The PS app's version mark, drawn at the title bar's right end
    /// (`PS <hash of its sources>`, the relay sets it; tools/ps-version.sh,
    /// `make version`): which build is on the board. Empty: none.
    version: []const u8 = "",
    /// The Systems level's build dates ("10-18", from each .bit header's
    /// date: `setBitDate`; M7's request for the user, 2026-11-03), by entry
    /// index; the first `bit_dates.len` systems only, "" = none.
    bit_dates: [16][5]u8 = undefined,
    bit_date_ok: u16 = 0,
    mode: Mode = .roms,

    /// In place: a Menu is about 20 KB, more than the PS app's stack
    /// should ever hold (built by value on the stack, it overflowed the
    /// first board run's 16 KB stack).
    pub fn init(m: *Menu, system: *const System) void {
        m.system = system;
        m.n = 0;
        m.truncated = false;
        m.cursor = 0;
        m.top = 0;
        m.depth = 0;
        m.saved = [_]usize{0} ** max_depth;
        m.err = null;
        m.msg_len = 0;
        m.usb = "";
        m.version = "";
        m.bit_date_ok = 0;
        m.mode = .roms;
        m.setPath("/");
        // save states (L.7): per game, nothing known yet; the Menu is
        // undefined until here (in DDR on the board)
        m.slot = 1;
        m.slot_used = [_]?bool{null} ** savestate.slots;
        m.states_ok = false;
        m.movies_ok = false;
        m.shots_ok = false;
        m.movie = .off;
        m.movie_frames = null;
        m.opt_row = 0;
        m.ctl.n = 0;
        m.ctl.row = 0;
        m.ctl.top = 0;
        m.ctl.waiting = false;
        m.ctl.changed = false;
    }

    /// Start listing the Systems level (then `add` the files of
    /// `/systems`, then `done`); `beginRoms` goes back.
    pub fn beginSystems(m: *Menu) void {
        m.mode = .systems;
        m.setPath("/systems");
        m.bit_date_ok = 0;
        m.begin();
    }

    /// Entry i of the Systems level was built on `date`, a .bit header's
    /// field c ("2026/10/18", pcap.Header.date): shown as "10-18" at the
    /// line's right end. Anything else is ignored (no date shown).
    pub fn setBitDate(m: *Menu, i: usize, date: []const u8) void {
        if (i >= m.bit_dates.len or date.len != 10 or date[4] != '/' or date[7] != '/') return;
        m.bit_dates[i] = .{ date[5], date[6], '-', date[8], date[9] };
        m.bit_date_ok |= @as(u16, 1) << @intCast(i);
    }

    fn bitDate(m: *const Menu, i: usize) ?[]const u8 {
        if (i >= m.bit_dates.len or m.bit_date_ok & (@as(u16, 1) << @intCast(i)) == 0) return null;
        return &m.bit_dates[i];
    }

    /// Back to the ROM level (the caller lists the system's folder again,
    /// from depth 0).
    pub fn beginRoms(m: *Menu) void {
        m.mode = .roms;
        m.depth = 0;
    }

    /// Entry i of the Systems level is the bitstream of the system running
    /// now: its name without ".bit" is the system's folder name ("nes.bit"
    /// for the NES, whose ROMs are in /nes; `make sd` names them so).
    pub fn isRunning(m: *const Menu, i: usize) bool {
        return std.ascii.eqlIgnoreCase(bitStem(m.entries[i].name()), m.system.dir);
    }

    pub fn setPath(m: *Menu, p: []const u8) void {
        const n = @min(p.len, m.path_buf.len);
        @memcpy(m.path_buf[0..n], p[0..n]);
        m.path_len = n;
    }

    pub fn path(m: *const Menu) []const u8 {
        return m.path_buf[0..m.path_len];
    }

    pub fn message(m: *Menu, text: []const u8) void {
        const n = @min(text.len, cols);
        @memcpy(m.msg_buf[0..n], text[0..n]);
        m.msg_len = n;
    }

    /// Start a new listing (the caller then `add`s, then `done`s).
    pub fn begin(m: *Menu) void {
        m.n = 0;
        m.truncated = false;
        m.err = null;
    }

    /// Add a directory entry; files without one of the system's endings are
    /// skipped. Returns false once the list is full.
    pub fn add(m: *Menu, name: []const u8, is_dir: bool, size: u32, ref: u32) bool {
        switch (m.mode) {
            .roms, .options, .controls => if (!is_dir and !hasExt(name, m.system.exts)) return true,
            .systems => if (is_dir or !hasExt(name, &bit_ext)) return true,
        }
        if (m.n == max_entries) {
            m.truncated = true;
            return false;
        }
        var e: Entry = .{ .is_dir = is_dir, .size = size, .ref = ref };
        const n = @min(name.len, max_name);
        @memcpy(e.name_buf[0..n], name[0..n]);
        e.len = @intCast(n);
        m.entries[m.n] = e;
        m.n += 1;
        return true;
    }

    /// Sort (directories first, then by name, ignoring case) and put the
    /// cursor at `cursor`.
    pub fn done(m: *Menu, cursor: usize) void {
        std.sort.insertion(Entry, m.entries[0..m.n], {}, lessThan);
        // the Game Boy Color: the palette line first (the last entry goes
        // if the list is full)
        if (m.mode == .roms and m.system.palette) {
            if (m.n == max_entries) m.n -= 1;
            std.mem.copyBackwards(Entry, m.entries[1 .. m.n + 1], m.entries[0..m.n]);
            m.n += 1;
            m.entries[0] = .{ .ref = palette_ref };
            m.paletteLabel();
        }
        // the "<Options>" line first of all, at the system's top folder
        if (m.mode == .roms and m.depth == 0 and m.hasOptions()) {
            if (m.n == max_entries) m.n -= 1;
            std.mem.copyBackwards(Entry, m.entries[1 .. m.n + 1], m.entries[0..m.n]);
            m.n += 1;
            m.entries[0] = .{ .ref = options_ref };
            const t = "<Options>";
            @memcpy(m.entries[0].name_buf[0..t.len], t);
            m.entries[0].len = t.len;
        }
        m.cursor = if (m.n == 0) 0 else @min(cursor, m.n - 1);
        m.top = 0;
        m.scroll();
    }

    fn paletteLabel(m: *Menu) void {
        // first, or second after "<Options>" (side track L)
        const i: usize = if (m.n > 1 and m.entries[0].ref == options_ref) 1 else 0;
        const e = &m.entries[i];
        const t = std.fmt.bufPrint(&e.name_buf, "<Palette: {s}>", .{gbc_palette.label(m.pal)}) catch "<Palette>";
        e.len = @intCast(t.len);
    }

    /// The system has something on the Options page (side track N: save
    /// states alone too, as CHIP-8's, once a game from the card runs).
    pub fn hasOptions(m: *const Menu) bool {
        return m.system.volume or m.system.speed_top != 0 or (m.system.state != null and m.states_ok) or m.ctl.n > 0;
    }

    /// Entry i is the "<Options>" line.
    pub fn isOptions(m: *const Menu, i: usize) bool {
        return m.mode == .roms and i < m.n and m.entries[i].ref == options_ref;
    }

    /// The Options page's rows for this system, in order.
    pub fn optRows(m: *const Menu, buf: *[max_opt_rows]OptRow) []const OptRow {
        var n: usize = 0;
        if (m.system.volume) {
            buf[n] = .volume;
            n += 1;
        }
        if (m.system.speed_top != 0) {
            buf[n] = .speed;
            n += 1;
        }
        if (m.system.state != null and m.states_ok) {
            for ([_]OptRow{ .slot, .save_state, .load_state }) |r| {
                buf[n] = r;
                n += 1;
            }
        }
        if (m.movies_ok) {
            const rs: []const OptRow = if (m.movie == .off) &.{ .record_movie, .play_movie } else &.{.stop_movie};
            for (rs) |r| {
                buf[n] = r;
                n += 1;
            }
        }
        if (m.shots_ok) {
            buf[n] = .screenshot;
            n += 1;
        }
        if (m.ctl.n > 0) {
            buf[n] = .controls;
            n += 1;
        }
        return buf[0..n];
    }

    /// A new game starts at 100 % (the speed is not kept; PLAN.md, side
    /// track L).
    pub fn newGame(m: *Menu) void {
        m.opts.speed = options.speed_normal;
    }

    fn handleOptions(m: *Menu, e: Event) Action {
        var buf: [max_opt_rows]OptRow = undefined;
        const rs = m.optRows(&buf);
        switch (e) {
            .none => {},
            .up => if (rs.len > 1) {
                m.opt_row = if (m.opt_row == 0) rs.len - 1 else m.opt_row - 1;
                return .moved;
            },
            .down => if (rs.len > 1) {
                m.opt_row = if (m.opt_row + 1 >= rs.len) 0 else m.opt_row + 1;
                return .moved;
            },
            .left, .right => if (m.opt_row < rs.len) {
                const up = e == .right;
                const before = m.opts;
                const slot = m.slot;
                switch (rs[m.opt_row]) {
                    .volume => m.opts.volume = options.volumeStep(m.opts.volume, up),
                    .speed => m.opts.speed = options.speedStep(m.opts.speed, up, m.system.speed_top),
                    .slot => m.slot = if (up) @min(m.slot + 1, savestate.slots) else @max(m.slot - 1, 1),
                    .save_state, .load_state, .record_movie, .play_movie, .stop_movie, .screenshot, .controls => {},
                }
                if (!std.meta.eql(before, m.opts) or slot != m.slot) return .moved;
            },
            .center => {
                if (m.opt_row < rs.len) switch (rs[m.opt_row]) {
                    .save_state => return .{ .save_state = m.slot },
                    .load_state => return .{ .load_state = m.slot },
                    .record_movie => return .record_movie,
                    .play_movie => return .play_movie,
                    .stop_movie => return .stop_movie,
                    .screenshot => return .screenshot,
                    .controls => return .controls,
                    else => {},
                };
                m.mode = .roms;
                return .moved;
            },
        }
        return .none;
    }

    /// The Controls page, its rows written (`ctl.line`, `ctl.len`).
    pub fn openControls(m: *Menu) void {
        m.mode = .controls;
        m.ctl.row = 0;
        m.ctl.top = 0;
        m.ctl.waiting = false;
        m.ctl.changed = false;
    }

    /// The key for the button waited for came (the relay set it, or not:
    /// cancelled), its row rewritten.
    pub fn keyTaken(m: *Menu, changed: bool) void {
        m.ctl.waiting = false;
        if (changed) m.ctl.changed = true;
    }

    fn handleControls(m: *Menu, e: Event) Action {
        const c = &m.ctl;
        if (e == .none) return .none;
        // waiting for a key: a board button cancels
        if (c.waiting) {
            c.waiting = false;
            return .moved;
        }
        const n = c.rows();
        switch (e) {
            .none => {},
            .up, .down => {
                c.row = @intCast(if (e == .up) (if (c.row == 0) n - 1 else c.row - 1) else (if (c.row + 1 >= n) 0 else c.row + 1));
                if (c.row < c.top) c.top = c.row;
                if (c.row >= @as(usize, c.top) + list_rows) c.top = @intCast(c.row + 1 - list_rows);
                return .moved;
            },
            .left => {
                m.mode = .options;
                return .controls_done;
            },
            .right => if (c.row < c.n) return .{ .clear_key = c.row },
            .center => {
                if (c.row == c.n) return .reset_keys;
                c.waiting = true;
                return .moved;
            },
        }
        return .none;
    }

    /// Entry i is the "Palette:" line.
    pub fn isPalette(m: *const Menu, i: usize) bool {
        return m.mode == .roms and m.system.palette and i < m.n and m.entries[i].ref == palette_ref;
    }

    fn lessThan(_: void, a: Entry, b: Entry) bool {
        if (a.is_dir != b.is_dir) return a.is_dir;
        return std.ascii.lessThanIgnoreCase(a.name(), b.name());
    }

    pub fn fail(m: *Menu, what: []const u8) void {
        m.err = what;
        m.n = 0;
    }

    /// Entering directory i: remember the cursor, extend the path.
    pub fn enter(m: *Menu, i: usize) void {
        if (m.depth < max_depth) m.saved[m.depth] = i;
        m.depth += 1;
        const name = m.entries[i].name();
        if (m.path_len > 0 and m.path_buf[m.path_len - 1] != '/') m.append("/");
        m.append(name);
    }

    /// Leaving the current directory: the cursor to go back to.
    pub fn leave(m: *Menu) usize {
        if (m.depth == 0) return 0;
        m.depth -= 1;
        while (m.path_len > 1 and m.path_buf[m.path_len - 1] != '/') m.path_len -= 1;
        if (m.path_len > 1) m.path_len -= 1;
        return if (m.depth < max_depth) m.saved[m.depth] else 0;
    }

    fn append(m: *Menu, s: []const u8) void {
        const n = @min(s.len, m.path_buf.len - m.path_len);
        @memcpy(m.path_buf[m.path_len..][0..n], s[0..n]);
        m.path_len += n;
    }

    fn scroll(m: *Menu) void {
        if (m.cursor < m.top) m.top = m.cursor;
        if (m.cursor >= m.top + list_rows) m.top = m.cursor + 1 - list_rows;
    }

    pub fn handle(m: *Menu, e: Event) Action {
        if (m.err != null) return switch (e) {
            .center, .right => .retry,
            .left => .resume_game,
            else => .none,
        };
        if (m.mode == .options) return m.handleOptions(e);
        if (m.mode == .controls) return m.handleControls(e);
        switch (e) {
            .none => {},
            .up => if (m.n > 1) {
                m.cursor = if (m.cursor == 0) m.n - 1 else m.cursor - 1;
                m.scroll();
                return .moved;
            },
            .down => if (m.n > 1) {
                m.cursor = if (m.cursor + 1 == m.n) 0 else m.cursor + 1;
                m.scroll();
                return .moved;
            },
            .left => return switch (m.mode) {
                .systems => .resume_game,
                .roms, .options, .controls => if (m.depth == 0) .systems else .up,
            },
            .right, .center => if (m.n > 0) {
                if (m.mode == .systems) return .{ .pick_system = m.cursor };
                if (m.isOptions(m.cursor)) {
                    m.mode = .options;
                    m.opt_row = 0;
                    m.msg_len = 0;
                    return .moved;
                }
                if (m.isPalette(m.cursor)) {
                    m.pal = (m.pal + 1) % gbc_palette.choices;
                    m.paletteLabel();
                    return .moved;
                }
                return if (m.entries[m.cursor].is_dir) .{ .open_dir = m.cursor } else .{ .load = m.cursor };
            },
        }
        return .none;
    }

    /// The version mark for a title `title_len` long: the whole mark
    /// ("PS 3fa9c1 10-18") where it fits after the title with a space
    /// between, else without its date ("PS 3fa9c1"), which may cover the
    /// title's end as before.
    fn titleMark(m: *const Menu, title_len: usize) []const u8 {
        const v = m.version;
        if (1 + title_len + 1 + v.len + 1 <= cols) return v;
        const sp1 = std.mem.indexOfScalar(u8, v, ' ') orelse return v;
        const sp2 = std.mem.indexOfScalarPos(u8, v, sp1 + 1, ' ') orelse return v;
        return v[0..sp2];
    }

    pub fn render(m: *const Menu, s: *Screen) void {
        s.clear();
        var title: [cols]u8 = undefined;
        const ttl = switch (m.mode) {
            .roms => std.fmt.bufPrint(&title, "ROM menu - {s}", .{m.system.name}) catch "ROM menu",
            .systems => std.fmt.bufPrint(&title, "Systems - running {s}", .{m.system.name}) catch "Systems",
            .options => std.fmt.bufPrint(&title, "Options - {s}", .{m.system.name}) catch "Options",
            .controls => std.fmt.bufPrint(&title, "Controls - {s}", .{m.system.name}) catch "Controls",
        };
        s.bar(0, 1, ttl);
        const v = m.titleMark(ttl.len);
        if (v.len > 0 and v.len + 2 <= cols) s.put(0, cols - 1 - v.len, v, true);
        // the path, cut from the left if it's too long
        const p = m.path();
        if (p.len <= cols - 1) s.put(1, 1, p, false) else {
            s.put(1, 1, "~", false);
            s.put(1, 2, p[p.len - (cols - 3) ..], false);
        }
        s.put(2, 1, m.usb, false);
        if (m.mode == .controls) {
            const c = &m.ctl;
            var r: usize = 0;
            while (r < list_rows and c.top + r < c.rows()) : (r += 1) {
                const i = c.top + r;
                const text = if (i == c.n) " Reset to defaults" else c.text[i][0..c.len[i]];
                if (i == c.row) s.bar(list_top + r, 1, text) else s.put(list_top + r, 1, text, false);
            }
            if (c.top > 0) s.put(list_top, cols - 1, "^", false);
            if (c.top + list_rows < c.rows()) s.put(list_top + list_rows - 1, cols - 1, "v", false);
            s.put(rows - 2, 1, if (m.msg_len > 0) m.msg_buf[0..m.msg_len] else "Saved to keymap.conf when you go back.", false);
            s.bar(rows - 1, 1, if (c.waiting)
                "Press a key  (Esc: cancel)"
            else if (c.row == c.n)
                "U/D move  C reset all keys  L back"
            else
                "U/D move  C set  R clear  L back");
            return;
        }
        if (m.mode == .options) {
            var buf: [max_opt_rows]OptRow = undefined;
            const rs = m.optRows(&buf);
            for (rs, 0..) |r, i| {
                var t: [cols]u8 = undefined;
                const text = switch (r) {
                    .volume => options.volumeText(m.opts.volume, t[1..]),
                    .speed => options.speedText(m.opts.speed, t[1..]),
                    .slot => slotText(m.slot, m.slot_used[m.slot - 1], t[1..]),
                    .save_state => lit(t[1..], "Save state"),
                    .load_state => lit(t[1..], "Load state"),
                    .record_movie => lit(t[1..], "Record movie (from power-on)"),
                    .play_movie => movieText(m.movie_frames, t[1..]),
                    .stop_movie => lit(t[1..], if (m.movie == .recording) "Stop recording" else "Stop the movie"),
                    .screenshot => lit(t[1..], "Screenshot (to /screens)"),
                    .controls => lit(t[1..], "Controls (keyboard keys)"),
                };
                t[0] = ' ';
                const line = t[0 .. text.len + 1];
                if (i == m.opt_row) s.bar(list_top + i, 1, line) else s.put(list_top + i, 1, line, false);
            }
            const r: ?OptRow = if (m.opt_row < rs.len) rs[m.opt_row] else null;
            const act = r == .save_state or r == .load_state;
            const movie_act = r == .record_movie or r == .play_movie or r == .stop_movie or r == .screenshot or r == .controls;
            s.put(rows - 2, 1, if (m.msg_len > 0) m.msg_buf[0..m.msg_len] else "Applied when the game goes on.", false);
            s.bar(rows - 1, 1, if (act) "U/D move  C do it (the slot above)" else if (movie_act) "U/D move  C do it" else "U/D move  L/R change  C back");
            return;
        }
        if (m.err) |e| {
            s.put(list_top + 1, 2, e, false);
            s.put(list_top + 3, 2, "Insert a card and press C to retry,", false);
            s.put(list_top + 4, 2, "or L to go back to the game.", false);
        } else if (m.n == 0) {
            s.put(list_top + 1, 2, if (m.mode == .roms) "(no ROMs here)" else "(no .bit files in /systems)", false);
        } else {
            var r: usize = 0;
            while (r < list_rows and m.top + r < m.n) : (r += 1) {
                const i = m.top + r;
                const e = &m.entries[i];
                var buf: [cols]u8 = undefined;
                const text = if (m.mode == .roms) entryText(e, &buf) else systemText(e, m.isRunning(i), m.bitDate(i), &buf);
                if (i == m.cursor) s.bar(list_top + r, 1, text) else s.put(list_top + r, 1, text, false);
            }
            if (m.top > 0) s.put(list_top, cols - 1, "^", false);
            if (m.top + list_rows < m.n) s.put(list_top + list_rows - 1, cols - 1, "v", false);
        }
        // the Systems level: the PS app's whole mark on the status line when
        // it has nothing else to say (the title bar's is cut to the hash)
        s.put(rows - 2, 1, if (m.msg_len == 0 and m.mode == .systems and m.err == null) m.version else m.msg_buf[0..m.msg_len], false);
        s.bar(rows - 1, 1, if (m.err != null)
            "C retry   L back to the game"
        else switch (m.mode) {
            .roms => if (m.depth == 0) "U/D move  R/C open  L systems" else "U/D move  R/C open  L back",
            .systems => "U/D move  R/C start  L back to game",
            .options, .controls => unreachable,
        });
    }

    /// `text` copied into `buf` (the Options rows are built in place).
    fn lit(buf: []u8, text: []const u8) []const u8 {
        @memcpy(buf[0..text.len], text);
        return buf[0..text.len];
    }

    /// "Play movie  (1234 frames)" (empty, or nothing if not known).
    fn movieText(frames: ?u32, buf: []u8) []const u8 {
        const f = frames orelse return lit(buf, "Play movie");
        if (f == 0) return lit(buf, "Play movie  (empty)");
        return std.fmt.bufPrint(buf, "Play movie  ({d} frames)", .{f}) catch lit(buf, "Play movie");
    }

    /// "State slot: 2  (saved)" (empty, or nothing if not known).
    fn slotText(slot: u8, used: ?bool, buf: []u8) []const u8 {
        const tail: []const u8 = if (used) |u| (if (u) "  (saved)" else "  (empty)") else "";
        const head = "State slot: ";
        @memcpy(buf[0..head.len], head);
        buf[head.len] = '0' + slot;
        @memcpy(buf[head.len + 1 ..][0..tail.len], tail);
        return buf[0 .. head.len + 1 + tail.len];
    }

    /// " nes" for nes.bit, " nes  (running)" for the one in the PL; with
    /// its build date, that at the line's right end ("10-18 "), short of
    /// the scroll marks' column.
    fn systemText(e: *const Entry, running: bool, date: ?[]const u8, buf: *[cols]u8) []const u8 {
        const stem = bitStem(e.name());
        const k = @min(stem.len, @as(usize, if (date != null) cols - 23 else cols - 16));
        buf[0] = ' ';
        @memcpy(buf[1..][0..k], stem[0..k]);
        var n = 1 + k;
        if (running) {
            const tag = "  (running)";
            @memcpy(buf[n..][0..tag.len], tag);
            n += tag.len;
        }
        const d = date orelse return buf[0..n];
        const w = cols - 3; // cols 1 to cols - 3; the last two left free
        @memset(buf[n .. w - d.len - 1], ' ');
        @memcpy(buf[w - d.len - 1 ..][0..d.len], d);
        buf[w - 1] = ' ';
        return buf[0..w];
    }

    /// " name/" for directories, " name" for files, cut to fit (with "~").
    fn entryText(e: *const Entry, buf: *[cols]u8) []const u8 {
        const room = cols - 3; // bar starts at col 1, one space each side
        buf[0] = ' ';
        var n: usize = 1;
        const name = e.name();
        const suffix: []const u8 = if (e.is_dir) "/" else "";
        if (name.len + suffix.len <= room) {
            @memcpy(buf[n..][0..name.len], name);
            n += name.len;
            @memcpy(buf[n..][0..suffix.len], suffix);
            n += suffix.len;
        } else {
            const keep = room - 1 - suffix.len;
            @memcpy(buf[n..][0..keep], name[0..keep]);
            n += keep;
            buf[n] = '~';
            n += 1;
            @memcpy(buf[n..][0..suffix.len], suffix);
            n += suffix.len;
        }
        return buf[0..n];
    }
};

// --- Tests -------------------------------------------------------------------------
const testing = std.testing;

fn btn(comptime s: []const u8) pl.Buttons {
    var b: pl.Buttons = @bitCast(@as(u5, 0));
    inline for (s) |ch| switch (ch) {
        'c' => b.c = true,
        'd' => b.d = true,
        'l' => b.l = true,
        'r' => b.r = true,
        'u' => b.u = true,
        else => @compileError("button"),
    };
    return b;
}

fn testMenu() Menu {
    var m: Menu = undefined;
    m.init(&systems[0]);
    m.setPath("/CHIP8");
    m.begin();
    _ = m.add("readme.txt", false, 10, 0);
    _ = m.add("Tetris.ch8", false, 494, 1);
    _ = m.add("timendus", true, 0, 2);
    _ = m.add("brix.CH8", false, 280, 3);
    _ = m.add("A very long name for a ROM file that does not fit.ch8", false, 1, 4);
    m.done(0);
    return m;
}

test "listing: other files skipped, directories first, names sorted ignoring case" {
    const m = testMenu();
    try testing.expectEqual(@as(usize, 4), m.n);
    try testing.expectEqualStrings("timendus", m.entries[0].name());
    try testing.expectEqualStrings("A very long name for a ROM file that does not fit.ch8", m.entries[1].name());
    try testing.expectEqualStrings("brix.CH8", m.entries[2].name());
    try testing.expectEqualStrings("Tetris.ch8", m.entries[3].name());
}

test "cursor wraps, select gives open or load, L at the top goes up to the systems" {
    var m = testMenu();
    try testing.expectEqual(Action.moved, m.handle(.up));
    try testing.expectEqual(@as(usize, 3), m.cursor);
    try testing.expectEqual(Action{ .load = 3 }, m.handle(.right));
    _ = m.handle(.down);
    try testing.expectEqual(@as(usize, 0), m.cursor);
    try testing.expectEqual(Action{ .open_dir = 0 }, m.handle(.center));
    try testing.expectEqual(Action.systems, m.handle(.left));
}

test "enter and leave keep the path and the cursor" {
    var m = testMenu();
    m.cursor = 0;
    m.enter(0);
    try testing.expectEqualStrings("/CHIP8/timendus", m.path());
    m.begin();
    _ = m.add("1-chip8-logo.ch8", false, 1, 0);
    m.done(0);
    try testing.expectEqual(Action.up, m.handle(.left));
    try testing.expectEqual(@as(usize, 0), m.leave());
    try testing.expectEqualStrings("/CHIP8", m.path());
    try testing.expectEqual(Action.systems, m.handle(.left));
}

test "render: the version mark at the title bar's right end, none until set" {
    var m = testMenu();
    var s: Screen = .{};
    var b: [cols]u8 = undefined;
    m.render(&s);
    try testing.expect(std.mem.indexOf(u8, s.line(0, &b), "PS ") == null);
    m.version = "PS 3fa9c1";
    m.render(&s);
    const row = s.line(0, &b);
    try testing.expect(std.mem.endsWith(u8, row, "PS 3fa9c1"));
    try testing.expectEqual(cols - 1, row.len); // one space after it
    try testing.expect(std.mem.startsWith(u8, row, " ROM menu - ")); // the title stays
    try testing.expect(s.cells[0][cols - 2] & 0x80 != 0); // in the bar's inverse video
}

test "render: the USB state on the row under the path, empty until set" {
    var m = testMenu();
    var s: Screen = .{};
    var b: [cols]u8 = undefined;
    m.render(&s);
    try testing.expectEqualStrings("", s.line(2, &b));
    m.usb = "USB: device connected (full speed)";
    m.render(&s);
    try testing.expectEqualStrings(" USB: device connected (full speed)", s.line(2, &b));
    try testing.expectEqualStrings("  timendus/", s.line(list_top, &b));
}

test "render: title, path, entries with the cursor bar, help" {
    var m = testMenu();
    _ = m.handle(.down);
    m.message("Loaded brix.CH8");
    var s: Screen = .{};
    m.render(&s);
    var b: [cols]u8 = undefined;
    try testing.expectEqualStrings(" ROM menu - CHIP-8", s.line(0, &b));
    try testing.expectEqualStrings(" /CHIP8", s.line(1, &b));
    try testing.expectEqualStrings("  timendus/", s.line(list_top, &b));
    try testing.expectEqualStrings("  A very long name for a ROM file that~", s.line(list_top + 1, &b));
    try testing.expectEqualStrings("  brix.CH8", s.line(list_top + 2, &b));
    try testing.expectEqualStrings(" Loaded brix.CH8", s.line(rows - 2, &b));
    // the cursor row is inverse across the whole width, the others aren't
    try testing.expect(s.cells[list_top + 1][0] & 0x80 != 0 and s.cells[list_top + 1][cols - 1] & 0x80 != 0);
    try testing.expect(s.cells[list_top][2] & 0x80 == 0);
    try testing.expect(s.line(list_top + 1, &b).len <= cols);
}

test "scrolling keeps the cursor on screen" {
    var m: Menu = undefined;
    m.init(&systems[0]);
    m.begin();
    var name: [8]u8 = undefined;
    for (0..60) |i| _ = m.add(std.fmt.bufPrint(&name, "r{d:0>2}.ch8", .{i}) catch unreachable, false, 1, 0);
    m.done(0);
    for (0..30) |_| _ = m.handle(.down);
    try testing.expectEqual(@as(usize, 30), m.cursor);
    try testing.expect(m.cursor >= m.top and m.cursor < m.top + list_rows);
    _ = m.handle(.up);
    for (0..40) |_| _ = m.handle(.up);
    try testing.expectEqual(@as(usize, 49), m.cursor); // wrapped past 0
    try testing.expect(m.cursor >= m.top and m.cursor < m.top + list_rows);
    var s: Screen = .{};
    m.render(&s);
    var b: [cols]u8 = undefined;
    try testing.expectEqualStrings("  r49.ch8", s.line(list_top + (m.cursor - m.top), &b));
}

// The relay redraws for every action but `none` (on the board, cursor
// moves once reported `none` and the bar never moved, 2026-10-03): every
// event that changes the picture must say so.
test "an event that changes the screen never answers none" {
    var name: [8]u8 = undefined;
    for ([_]usize{ 0, 1, 2, 5, 30 }) |count| {
        var m: Menu = undefined;
        m.init(&systems[0]);
        m.begin();
        for (0..count) |i| _ = m.add(std.fmt.bufPrint(&name, "r{d:0>2}.ch8", .{i}) catch unreachable, false, 1, 0);
        m.done(0);
        for (0..80) |k| {
            const e: Event = switch (k % 7) {
                0, 1, 2 => .down,
                3, 4 => .up,
                5 => .none,
                else => .down,
            };
            var before: Screen = .{};
            m.render(&before);
            const a = m.handle(e);
            var after: Screen = .{};
            m.render(&after);
            if (a == .none) try testing.expectEqualSlices(u8, std.mem.asBytes(&before), std.mem.asBytes(&after));
        }
    }
}

fn systemsMenu(running: *const System) Menu {
    var m: Menu = undefined;
    m.init(running);
    m.beginSystems();
    _ = m.add("gb.bit", false, 4045762, 0);
    _ = m.add("readme.txt", false, 10, 1);
    _ = m.add("old", true, 0, 2);
    _ = m.add("NES.BIT", false, 4045763, 3);
    _ = m.add("chip8.bit", false, 4045765, 4);
    _ = m.add("last.txt", false, 32, 5);
    m.done(0);
    return m;
}

test "systems level: only .bit files, sorted; the running one marked; L back to the game" {
    var m = systemsMenu(systemById(3).?);
    try testing.expectEqual(@as(usize, 3), m.n);
    try testing.expectEqualStrings("chip8.bit", m.entries[0].name());
    try testing.expectEqualStrings("gb.bit", m.entries[1].name());
    try testing.expectEqualStrings("NES.BIT", m.entries[2].name());
    try testing.expect(!m.isRunning(0) and !m.isRunning(1) and m.isRunning(2));
    try testing.expectEqual(Action{ .pick_system = 0 }, m.handle(.center));
    _ = m.handle(.down);
    try testing.expectEqual(Action{ .pick_system = 1 }, m.handle(.right));
    try testing.expectEqual(Action.resume_game, m.handle(.left));
    var s: Screen = .{};
    m.render(&s);
    var b: [cols]u8 = undefined;
    try testing.expectEqualStrings(" Systems - running NES", s.line(0, &b));
    try testing.expectEqualStrings(" /systems", s.line(1, &b));
    try testing.expectEqualStrings("  chip8", s.line(list_top, &b));
    try testing.expectEqualStrings("  gb", s.line(list_top + 1, &b));
    try testing.expectEqualStrings("  NES  (running)", s.line(list_top + 2, &b));
    try testing.expectEqualStrings(" U/D move  R/C start  L back to game", s.line(rows - 1, &b));
}

test "systems level: each bitstream's build date at the line's right end, the PS mark below" {
    var m = systemsMenu(systemById(3).?);
    m.version = "PS 3fa9c1 10-18";
    m.setBitDate(0, "2026/10/18");
    m.setBitDate(2, "2026/11/03");
    m.setBitDate(1, "18.10.2026"); // not Vivado's: none shown
    var s: Screen = .{};
    var b: [cols]u8 = undefined;
    m.render(&s);
    const l0 = s.line(list_top, &b);
    try testing.expect(std.mem.startsWith(u8, l0, "  chip8 "));
    try testing.expect(std.mem.endsWith(u8, l0, "10-18"));
    try testing.expectEqual(cols - 3, l0.len); // a space, then the scroll marks' column free
    try testing.expectEqualStrings("  gb", s.line(list_top + 1, &b));
    const l2 = s.line(list_top + 2, &b);
    try testing.expect(std.mem.startsWith(u8, l2, "  NES  (running) "));
    try testing.expect(std.mem.endsWith(u8, l2, "11-03"));
    try testing.expectEqualStrings(" PS 3fa9c1 10-18", s.line(rows - 2, &b));
    // a message takes the status line back
    m.message("Loading...");
    m.render(&s);
    try testing.expectEqualStrings(" Loading...", s.line(rows - 2, &b));
    // a new listing forgets the dates
    m.beginSystems();
    _ = m.add("gb.bit", false, 1, 0);
    m.done(0);
    m.msg_len = 0;
    m.render(&s);
    try testing.expectEqualStrings("  gb", s.line(list_top, &b));
}

test "render: the version mark's date only where the title leaves room" {
    var m = testMenu();
    var s: Screen = .{};
    var b: [cols]u8 = undefined;
    m.version = "PS 3fa9c1 10-18+";
    m.render(&s);
    const row = s.line(0, &b);
    try testing.expect(std.mem.endsWith(u8, row, " PS 3fa9c1 10-18+"));
    try testing.expect(std.mem.startsWith(u8, row, " ROM menu - "));
    // a long title: the hash alone
    m.beginSystems();
    m.done(0);
    m.render(&s);
    try testing.expect(std.mem.endsWith(u8, s.line(0, &b), "PS 3fa9c1"));
}

test "systems level: from the ROM root up, and back down to the ROMs" {
    var m = testMenu();
    var s: Screen = .{};
    var b: [cols]u8 = undefined;
    m.render(&s);
    try testing.expectEqualStrings(" U/D move  R/C open  L systems", s.line(rows - 1, &b));
    m.cursor = 0;
    m.enter(0); // a subfolder: L goes up, not to the systems
    m.begin();
    _ = m.add("1-chip8-logo.ch8", false, 1, 0);
    m.done(0);
    m.render(&s);
    try testing.expectEqualStrings(" U/D move  R/C open  L back", s.line(rows - 1, &b));
    try testing.expectEqual(Action.up, m.handle(.left));
    _ = m.leave();
    try testing.expectEqual(Action.systems, m.handle(.left));
    m.beginSystems();
    _ = m.add("chip8.bit", false, 1, 0);
    _ = m.add("tetris.ch8", false, 1, 1); // a ROM in /systems: not a system
    m.done(0);
    try testing.expectEqual(@as(usize, 1), m.n);
    try testing.expect(m.isRunning(0));
    // back to the ROMs: ROM endings again, from the top
    m.beginRoms();
    m.setPath("/chip8");
    m.begin();
    _ = m.add("chip8.bit", false, 1, 0);
    _ = m.add("pong.ch8", false, 1, 1);
    m.done(0);
    try testing.expectEqual(@as(usize, 1), m.n);
    try testing.expectEqual(@as(usize, 0), m.depth);
    try testing.expectEqual(Action.systems, m.handle(.left));
    // empty /systems
    m.beginSystems();
    m.done(0);
    m.render(&s);
    try testing.expectEqualStrings("  (no .bit files in /systems)", s.line(list_top + 1, &b));
    try testing.expectEqual(Action.none, m.handle(.center));
    try testing.expectEqualStrings("nes", bitStem("nes.BIT"));
    try testing.expectEqualStrings("nes.bin", bitStem("nes.bin"));
}

test "last.txt: written in place keeping its size, read back; the rest of the block untouched" {
    var blk = [_]u8{0xEE} ** 512;
    try testing.expect(writeLast(&blk, 32, "nes"));
    try testing.expectEqualStrings("nes" ++ " " ** 28 ++ "\n", blk[0..32]);
    try testing.expectEqual(@as(u8, 0xEE), blk[32]);
    try testing.expectEqualStrings("nes", readLast(blk[0..32]));
    try testing.expect(writeLast(&blk, 32, "chip8"));
    try testing.expectEqualStrings("chip8", readLast(blk[0..32]));
    // as make sd writes it (printf '%-31s\n' chip8)
    try testing.expectEqualStrings("chip8", readLast("chip8" ++ " " ** 26 ++ "\n"));
    try testing.expectEqualStrings("", readLast("\n"));
    // too small, too big
    try testing.expect(!writeLast(&blk, 3, "nes"));
    try testing.expect(!writeLast(&blk, 0, "nes"));
    try testing.expect(!writeLast(&blk, 513, "nes"));
}

test "error screen: C retries, L resumes" {
    var m = testMenu();
    m.fail("No SD card");
    try testing.expectEqual(Action.retry, m.handle(.center));
    try testing.expectEqual(Action.resume_game, m.handle(.left));
    try testing.expectEqual(Action.none, m.handle(.down));
    var s: Screen = .{};
    m.render(&s);
    var b: [cols]u8 = undefined;
    try testing.expectEqualStrings("  No SD card", s.line(list_top + 1, &b));
}

test "buttons: edges, repeat after a delay, no double events" {
    var b: Buttons = .{};
    try testing.expectEqual(Event.none, b.poll(0, btn("")));
    try testing.expectEqual(Event.down, b.poll(10, btn("d")));
    try testing.expectEqual(Event.none, b.poll(100, btn("d")));
    try testing.expectEqual(Event.none, b.poll(409, btn("d")));
    try testing.expectEqual(Event.down, b.poll(410, btn("d")));
    try testing.expectEqual(Event.none, b.poll(450, btn("d")));
    try testing.expectEqual(Event.down, b.poll(500, btn("d")));
    try testing.expectEqual(Event.none, b.poll(510, btn("")));
    try testing.expectEqual(Event.center, b.poll(520, btn("c")));
    try testing.expectEqual(Event.none, b.poll(2000, btn("c"))); // C doesn't repeat
    try testing.expectEqual(Event.left, b.poll(2010, btn("cl")));
}

test "menu hold, Game Boy: Up+Down together for a second; A (C) held doesn't" {
    var b: Buttons = .{};
    const ud = systems[1].menu_combo;
    try testing.expect(!b.menuHold(0, btn("c"), ud));
    try testing.expect(!b.menuHold(5000, btn("c"), ud)); // A held 5 s: a game, not the menu
    try testing.expect(!b.menuHold(5010, btn("u"), ud));
    try testing.expect(!b.menuHold(6100, btn("u"), ud)); // Up alone: no
    try testing.expect(!b.menuHold(6200, btn("du"), ud));
    try testing.expect(!b.menuHold(7199, btn("du"), ud));
    try testing.expect(b.menuHold(7200, btn("du"), ud));
    try testing.expect(!b.menuHold(9000, btn("du"), ud)); // once
    try testing.expect(!b.menuHold(9010, btn("cdu"), ud)); // with A too: cancelled
}

test "menu hold: C alone for a second, once; another button cancels" {
    var b: Buttons = .{};
    try testing.expect(!b.menuHold(100, btn("c"), systems[0].menu_combo));
    try testing.expect(!b.menuHold(1099, btn("c"), systems[0].menu_combo));
    try testing.expect(b.menuHold(1100, btn("c"), systems[0].menu_combo));
    try testing.expect(!b.menuHold(3000, btn("c"), systems[0].menu_combo));
    try testing.expect(!b.menuHold(3010, btn(""), systems[0].menu_combo));
    try testing.expect(!b.menuHold(4000, btn("c"), systems[0].menu_combo));
    try testing.expect(!b.menuHold(4500, btn("cr"), systems[0].menu_combo)); // C+R is key B
    try testing.expect(!b.menuHold(5200, btn("c"), systems[0].menu_combo));
    try testing.expect(b.menuHold(6200, btn("c"), systems[0].menu_combo));
}

test "systems and endings" {
    try testing.expectEqualStrings("CHIP-8", systemById(1).?.name);
    try testing.expectEqualStrings("Game Boy", systemById(2).?.name);
    try testing.expect(systemById(10) == null); // (9: the Super Nintendo, M7)
    try testing.expect(systemById(0) == null); // 0: no system (a PL without PS_IN)
    for (systems, 0..) |s, i| for (systems[i + 1 ..]) |t| try testing.expect(s.id != t.id);
    try testing.expect(hasExt("Tetris.GB", systems[1].exts));
    try testing.expect(!hasExt("tetris.gba", systems[1].exts));
    try testing.expect(hasExt("PONG.C8", systems[0].exts));
    try testing.expect(!hasExt(".ch8x", systems[0].exts));
    try testing.expect(!hasExt(".ch8", systems[0].exts)); // nothing before the ending
}

test "the Game Boy Color's palette line: after <Options>, cycles through the choices, loads nothing" {
    var gbc: ?*const System = null;
    for (&systems) |*sy| {
        if (sy.id == 6) gbc = sy;
    }
    var m: Menu = .{ .system = gbc.? };
    m.begin();
    _ = m.add("zelda.gbc", false, 1, 0);
    _ = m.add("blue.gb", false, 1, 1);
    m.done(1);
    try testing.expectEqual(@as(usize, 4), m.n);
    try testing.expect(m.isOptions(0));
    try testing.expect(m.isPalette(1));
    try testing.expectEqualStrings("<Palette: Auto>", m.entries[1].name());
    try testing.expectEqualStrings("blue.gb", m.entries[2].name());
    try testing.expectEqual(Action.moved, m.handle(.center));
    try testing.expectEqualStrings("<Palette: Default>", m.entries[1].name());
    try testing.expectEqual(Action.moved, m.handle(.right));
    try testing.expectEqualStrings("<Palette: Right>", m.entries[1].name());
    var k: usize = 0;
    while (k < gbc_palette.choices - 2) : (k += 1) _ = m.handle(.center);
    try testing.expectEqualStrings("<Palette: Auto>", m.entries[1].name());
    // the choice stays when the list is made again
    _ = m.handle(.center);
    m.begin();
    _ = m.add("blue.gb", false, 1, 1);
    m.done(1);
    try testing.expectEqualStrings("<Palette: Default>", m.entries[1].name());
    _ = m.handle(.down);
    try testing.expectEqual(Action{ .load = 2 }, m.handle(.center));
}

test "the Game Boy's lists have no palette line" {
    var m: Menu = .{ .system = &systems[1] };
    m.begin();
    _ = m.add("blue.gb", false, 1, 0);
    m.done(0);
    try testing.expectEqual(@as(usize, 2), m.n);
    try testing.expect(m.isOptions(0));
    try testing.expect(!m.isPalette(0) and !m.isPalette(1));
}

test {
    _ = gbc_palette;
    _ = options;
}

/// The Game Boy with both options on (side track L's tests: no system
/// has them until its core is wired).
fn optionsSystem(vol: bool, top: u4) System {
    var sy = systems[1];
    sy.volume = vol;
    sy.speed_top = top;
    return sy;
}

test "options: the <Options> line first at the system's top folder only, none without options" {
    const sy = optionsSystem(true, 6);
    var m: Menu = undefined;
    m.init(&sy);
    m.opts = .{}; // as the relay at start-up
    m.begin();
    _ = m.add("zelda.gb", false, 1, 0);
    _ = m.add("demos", true, 0, 1);
    m.done(0);
    try testing.expectEqual(@as(usize, 3), m.n);
    try testing.expect(m.isOptions(0));
    try testing.expectEqualStrings("<Options>", m.entries[0].name());
    try testing.expectEqualStrings("demos", m.entries[1].name());
    m.enter(1);
    m.begin();
    _ = m.add("a.gb", false, 1, 2);
    m.done(0);
    try testing.expectEqual(@as(usize, 1), m.n);
    try testing.expect(!m.isOptions(0));
    // the volume on every system with sound (L.2), none on CHIP-8 (its
    // beeper has no level); the speed where measured (L.4), never below
    // 100 % or past 200 %
    for (&systems) |*t| {
        try testing.expectEqual(t.id != 1, t.volume);
        try testing.expect(t.speed_top == 0 or (t.speed_top >= 4 and t.speed_top <= 8));
    }
    try testing.expectEqual(@as(u4, 5), systemById(2).?.speed_top);
    try testing.expectEqual(@as(u4, 6), systemById(3).?.speed_top);
    try testing.expectEqual(@as(u4, 4), systemById(6).?.speed_top);
    try testing.expectEqual(@as(u4, 5), systemById(5).?.speed_top);
    try testing.expectEqual(@as(u4, 4), systemById(7).?.speed_top);
    try testing.expectEqual(@as(u4, 4), systemById(8).?.speed_top);
}

test "options: the page, its rows, L/R change, the ends, C back to the same list" {
    const sy = optionsSystem(true, 6);
    var m: Menu = undefined;
    m.init(&sy);
    m.opts = .{}; // as the relay at start-up
    m.begin();
    _ = m.add("zelda.gb", false, 1, 0);
    m.done(0);
    try testing.expectEqual(Action.moved, m.handle(.center));
    try testing.expectEqual(Mode.options, m.mode);
    var s: Screen = .{};
    var b: [cols]u8 = undefined;
    m.render(&s);
    try testing.expectEqualStrings(" Options - Game Boy", s.line(0, &b));
    try testing.expectEqualStrings("  Volume: ########## 10", s.line(list_top, &b));
    try testing.expectEqualStrings("  Speed: 100%", s.line(list_top + 1, &b));
    try testing.expectEqualStrings(" U/D move  L/R change  C back", s.line(rows - 1, &b));
    try testing.expect(s.cells[list_top][0] & 0x80 != 0 and s.cells[list_top + 1][0] & 0x80 == 0);
    // volume: at the top already, then down to 0 and no further
    try testing.expectEqual(Action.none, m.handle(.right));
    for (0..10) |_| try testing.expectEqual(Action.moved, m.handle(.left));
    try testing.expectEqual(Action.none, m.handle(.left));
    try testing.expectEqual(@as(u4, 0), m.opts.volume);
    // speed: up to the system's limit (150 %), down to 50 %
    try testing.expectEqual(Action.moved, m.handle(.down));
    try testing.expectEqual(Action.moved, m.handle(.right));
    try testing.expectEqual(Action.moved, m.handle(.right));
    try testing.expectEqual(Action.none, m.handle(.right));
    try testing.expectEqual(@as(u4, 6), m.opts.speed);
    for (0..4) |_| try testing.expectEqual(Action.moved, m.handle(.left));
    try testing.expectEqual(Action.none, m.handle(.left));
    try testing.expectEqual(@as(u4, 2), m.opts.speed);
    m.render(&s);
    try testing.expectEqualStrings("  Speed: 50%", s.line(list_top + 1, &b));
    try testing.expectEqualStrings("  Volume: .......... 0", s.line(list_top, &b));
    // U/D wrap over the two rows
    try testing.expectEqual(Action.moved, m.handle(.down));
    try testing.expectEqual(@as(usize, 0), m.opt_row);
    // C: back to the ROM list as it was; the values stay
    try testing.expectEqual(Action.moved, m.handle(.center));
    try testing.expectEqual(Mode.roms, m.mode);
    try testing.expectEqual(@as(usize, 2), m.n);
    try testing.expectEqual(@as(usize, 0), m.cursor);
    try testing.expectEqual(@as(u4, 0), m.opts.volume);
    _ = m.handle(.down);
    try testing.expectEqual(Action{ .load = 1 }, m.handle(.center));
}

test "options: only the rows the system has; no fast-forward on a core without it" {
    const vol_only = optionsSystem(true, 0);
    var m: Menu = undefined;
    m.init(&vol_only);
    var buf: [max_opt_rows]OptRow = undefined;
    try testing.expectEqualSlices(OptRow, &.{.volume}, m.optRows(&buf));
    const slow_only = optionsSystem(false, 4);
    m.init(&slow_only);
    m.opts = .{};
    try testing.expectEqualSlices(OptRow, &.{.speed}, m.optRows(&buf));
    m.begin();
    m.done(0);
    _ = m.handle(.center);
    try testing.expectEqual(Action.none, m.handle(.right));
    try testing.expectEqual(Action.moved, m.handle(.left));
    try testing.expectEqual(Action.moved, m.handle(.right));
    try testing.expectEqual(Action.none, m.handle(.right));
    try testing.expectEqual(@as(u4, 4), m.opts.speed);
    // one row: U/D change nothing
    try testing.expectEqual(Action.none, m.handle(.down));
}

test "options: a new game starts at 100 %, the volume stays; init keeps both" {
    const sy = optionsSystem(true, 6);
    var m: Menu = undefined;
    m.init(&sy);
    m.opts = .{}; // as the relay at start-up
    m.opts = .{ .volume = 3, .speed = 6 };
    m.init(&sy);
    try testing.expectEqual(options.Settings{ .volume = 3, .speed = 6 }, m.opts);
    m.newGame();
    try testing.expectEqual(options.Settings{ .volume = 3, .speed = 4 }, m.opts);
}

test "options: the screenshot row, last, when the game can have one" {
    var m: Menu = undefined;
    m.init(systemById(2).?);
    m.opts = .{};
    var buf: [max_opt_rows]OptRow = undefined;
    m.shots_ok = true;
    m.movies_ok = true;
    m.states_ok = true;
    try testing.expectEqualSlices(OptRow, &.{ .volume, .speed, .slot, .save_state, .load_state, .record_movie, .play_movie, .screenshot }, m.optRows(&buf));
    m.begin();
    m.done(0);
    _ = m.handle(.center);
    m.opt_row = 7;
    var s: Screen = .{};
    var b: [cols]u8 = undefined;
    m.render(&s);
    try testing.expectEqualStrings("  Screenshot (to /screens)", s.line(list_top + 7, &b));
    try testing.expectEqual(Action.screenshot, m.handle(.center));
    try testing.expectEqual(@as(?Picture, .{ .w = 160, .h = 144 }), systemById(2).?.screen);
}

test "controls: the Options row, the page (its rows from the relay, scrolling), waiting for a key, clear, reset, back" {
    var m: Menu = undefined;
    m.init(systemById(2).?);
    m.opts = .{};
    var buf: [max_opt_rows]OptRow = undefined;
    try testing.expectEqualSlices(OptRow, &.{ .volume, .speed }, m.optRows(&buf));
    // a system with a key map: the row last; CHIP-8 (no volume or speed)
    // gets an <Options> line for it
    m.ctl.n = 30;
    for (0..30) |i| {
        const t = std.fmt.bufPrint(m.ctl.line(i), " button {d}", .{i}) catch unreachable;
        m.ctl.len[i] = @intCast(t.len);
    }
    try testing.expectEqualSlices(OptRow, &.{ .volume, .speed, .controls }, m.optRows(&buf));
    var c8: Menu = undefined;
    c8.init(systemById(1).?);
    try testing.expect(!c8.hasOptions());
    c8.ctl.n = 16;
    try testing.expect(c8.hasOptions());
    m.begin();
    m.done(0);
    _ = m.handle(.center);
    _ = m.handle(.down);
    _ = m.handle(.down);
    try testing.expectEqual(Action.controls, m.handle(.center));
    m.openControls();
    var s: Screen = .{};
    var b: [cols]u8 = undefined;
    m.render(&s);
    try testing.expectEqualStrings("  button 0", s.line(list_top, &b));
    try testing.expectEqualStrings(" U/D move  C set  R clear  L back", s.line(rows - 1, &b));
    // C waits; any board button cancels; the relay's key: keyTaken
    try testing.expectEqual(Action.moved, m.handle(.center));
    try testing.expect(m.ctl.waiting);
    m.render(&s);
    try testing.expectEqualStrings(" Press a key  (Esc: cancel)", s.line(rows - 1, &b));
    try testing.expectEqual(Action.moved, m.handle(.down));
    try testing.expect(!m.ctl.waiting);
    try testing.expectEqual(@as(u8, 0), m.ctl.row);
    _ = m.handle(.center);
    m.keyTaken(true);
    try testing.expect(!m.ctl.waiting and m.ctl.changed);
    // R clears; up from the first row: the reset row, last (30 buttons:
    // the list scrolls)
    try testing.expectEqual(Action{ .clear_key = 0 }, m.handle(.right));
    _ = m.handle(.up);
    try testing.expectEqual(@as(u8, 30), m.ctl.row);
    m.render(&s);
    try testing.expectEqualStrings("  Reset to defaults", s.line(list_top + list_rows - 1, &b));
    try testing.expect(std.mem.endsWith(u8, s.line(list_top, &b), "^"));
    try testing.expectEqual(Action.reset_keys, m.handle(.center));
    try testing.expectEqual(Action.none, m.handle(.right));
    // L: back to Options, the relay saves
    try testing.expectEqual(Action.controls_done, m.handle(.left));
    try testing.expectEqual(Mode.options, m.mode);
}

test "options: the movie rows, with a movie slot; record and play, or stop the one running" {
    var m: Menu = undefined;
    m.init(systemById(2).?);
    m.opts = .{};
    var buf: [max_opt_rows]OptRow = undefined;
    try testing.expectEqualSlices(OptRow, &.{ .volume, .speed }, m.optRows(&buf));
    m.movies_ok = true;
    try testing.expectEqualSlices(OptRow, &.{ .volume, .speed, .record_movie, .play_movie }, m.optRows(&buf));
    m.states_ok = true;
    try testing.expectEqualSlices(OptRow, &.{ .volume, .speed, .slot, .save_state, .load_state, .record_movie, .play_movie }, m.optRows(&buf));
    m.states_ok = false;
    m.begin();
    m.done(0);
    _ = m.handle(.center);
    try testing.expectEqual(Mode.options, m.mode);
    _ = m.handle(.down);
    _ = m.handle(.down);
    var s: Screen = .{};
    var b: [cols]u8 = undefined;
    m.render(&s);
    try testing.expectEqualStrings("  Record movie (from power-on)", s.line(list_top + 2, &b));
    try testing.expectEqualStrings("  Play movie", s.line(list_top + 3, &b));
    try testing.expectEqualStrings(" U/D move  C do it", s.line(rows - 1, &b));
    try testing.expectEqual(Action.none, m.handle(.left));
    try testing.expectEqual(Action.record_movie, m.handle(.center));
    _ = m.handle(.down);
    try testing.expectEqual(Action.play_movie, m.handle(.center));
    m.movie_frames = 0;
    m.render(&s);
    try testing.expectEqualStrings("  Play movie  (empty)", s.line(list_top + 3, &b));
    m.movie_frames = 1234;
    m.render(&s);
    try testing.expectEqualStrings("  Play movie  (1234 frames)", s.line(list_top + 3, &b));
    // a movie running: only Stop
    m.movie = .recording;
    try testing.expectEqualSlices(OptRow, &.{ .volume, .speed, .stop_movie }, m.optRows(&buf));
    m.opt_row = 2;
    m.render(&s);
    try testing.expectEqualStrings("  Stop recording", s.line(list_top + 2, &b));
    try testing.expectEqual(Action.stop_movie, m.handle(.center));
    m.movie = .playing;
    m.render(&s);
    try testing.expectEqualStrings("  Stop the movie", s.line(list_top + 2, &b));
}

test "options: the state rows, for a game from the card; slot 1 to 4; C saves or loads it" {
    var m: Menu = undefined;
    m.init(systemById(2).?);
    m.opts = .{};
    var buf: [max_opt_rows]OptRow = undefined;
    // no game from the card yet: no slot rows
    m.states_ok = false;
    try testing.expectEqualSlices(OptRow, &.{ .volume, .speed }, m.optRows(&buf));
    m.states_ok = true;
    try testing.expectEqualSlices(OptRow, &.{ .volume, .speed, .slot, .save_state, .load_state }, m.optRows(&buf));
    m.begin();
    m.done(0);
    m.message("Loaded tetris.gb");
    _ = m.handle(.center);
    try testing.expectEqual(Mode.options, m.mode);
    _ = m.handle(.down);
    _ = m.handle(.down);
    var s: Screen = .{};
    var b: [cols]u8 = undefined;
    m.render(&s);
    try testing.expectEqualStrings("  State slot: 1", s.line(list_top + 2, &b));
    try testing.expectEqualStrings(" Applied when the game goes on.", s.line(rows - 2, &b));
    try testing.expectEqual(Action.none, m.handle(.left));
    for (0..3) |_| try testing.expectEqual(Action.moved, m.handle(.right));
    try testing.expectEqual(Action.none, m.handle(.right));
    m.slot_used = .{ true, null, false, true };
    m.render(&s);
    try testing.expectEqualStrings("  State slot: 4  (saved)", s.line(list_top + 2, &b));
    _ = m.handle(.left);
    m.render(&s);
    try testing.expectEqualStrings("  State slot: 3  (empty)", s.line(list_top + 2, &b));
    _ = m.handle(.down);
    m.render(&s);
    try testing.expectEqualStrings("  Save state", s.line(list_top + 3, &b));
    try testing.expectEqualStrings(" U/D move  C do it (the slot above)", s.line(rows - 1, &b));
    try testing.expectEqual(Action{ .save_state = 3 }, m.handle(.center));
    _ = m.handle(.down);
    try testing.expectEqual(Action{ .load_state = 3 }, m.handle(.center));
    try testing.expectEqual(Mode.options, m.mode);
    // C on a value row: back to the list
    _ = m.handle(.down);
    try testing.expectEqual(Action.moved, m.handle(.center));
    try testing.expectEqual(Mode.roms, m.mode);
    // every system has save states now (side track N: CHIP-8's the direct
    // path, no restore routine); the Game Gear and the Master System share
    // one Spec (N.3)
    try testing.expect(systemById(1).?.state != null and systemById(1).?.state.?.restore == null);
    try testing.expect(systemById(3).?.state != null);
    try testing.expect(systemById(7).?.state != null and systemById(8).?.state != null);
    try testing.expectEqual(systemById(7).?.state.?.mode_addr, systemById(8).?.state.?.mode_addr);
}

test "options: a system with save states alone (CHIP-8) gets the <Options> line once a game from the card runs" {
    var m: Menu = undefined;
    m.init(systemById(1).?);
    m.states_ok = false;
    try testing.expect(!m.hasOptions());
    m.states_ok = true;
    try testing.expect(m.hasOptions());
    var buf: [max_opt_rows]OptRow = undefined;
    try testing.expectEqualSlices(OptRow, &.{ .slot, .save_state, .load_state }, m.optRows(&buf));
}

test "options: an event that changes the page never answers none" {
    const sy = optionsSystem(true, 7);
    var m: Menu = undefined;
    m.init(&sy);
    m.opts = .{}; // as the relay at start-up
    m.begin();
    m.done(0);
    _ = m.handle(.center);
    for (0..200) |k| {
        const e: Event = switch (k % 9) {
            0, 1, 2 => .left,
            3, 4, 5 => .right,
            6 => .down,
            7 => .up,
            else => .none,
        };
        var before: Screen = .{};
        m.render(&before);
        const a = m.handle(e);
        var after: Screen = .{};
        m.render(&after);
        if (a == .none) try testing.expectEqualSlices(u8, std.mem.asBytes(&before), std.mem.asBytes(&after));
        try testing.expectEqual(Mode.options, m.mode);
    }
}

test "the Game Boy Color has the Game Boy's battery saves" {
    var gb: ?*const System = null;
    var gbc: ?*const System = null;
    for (&systems) |*sy| {
        if (sy.id == 2) gb = sy;
        if (sy.id == 6) gbc = sy;
    }
    const a = gb.?.save.?;
    const b = gbc.?.save.?;
    try testing.expectEqual(a.ram_addr, b.ram_addr);
    try testing.expectEqual(a.read_addr, b.read_addr);
    try testing.expectEqual(a.ctrl_addr, b.ctrl_addr);
    try testing.expectEqual(a.clock_load_addr, b.clock_load_addr);
    // a Color cartridge with 128 KiB of battery RAM (µCity's header)
    var head = [_]u8{0} ** 0x150;
    head[0x143] = 0xC0;
    head[0x147] = 0x1B;
    head[0x149] = 0x04;
    try testing.expectEqual(@as(u32, 128 << 10), b.size(&head));
}

test {
    _ = @import("systems.zig");
}
