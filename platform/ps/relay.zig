//! PS app (bare metal, Cortex-A9 core 0, in OCM): the board's helper for
//! every system with a PS (milestone 1b). Two jobs, polled in one loop:
//!
//! * The relay: commands from the desktop over the USB-UART (J14 -> PS
//!   UART1, protocol.zig): key states, ROM loading, ping, bit rate. If no
//!   valid key state arrives for 0.5 s, all keys are released, so a desktop
//!   script that dies with a key held can't leave it stuck.
//! * The ROM menu (menu.zig), on the monitor through the PL's text overlay
//!   and driven by the board buttons: ROMs from the SD card (sd.zig,
//!   fat.zig), loaded into the system through the PS link (pl.zig). It
//!   opens at start-up and when C is held alone for a second in a game.
//!
//! * Writing the SD card from the desktop (`make sd`, for users without a
//!   card reader): the S/D/Z/O commands of protocol.zig; every block is
//!   read back and compared before the reply.
//!
//! The PL design says which system it is (the system ID on the PS link,
//! Platform.Ps.Link); without a known one (a design without the PS link)
//! the menu stays off and only the relay runs.
//!
//! Loaded by `make prog SYSTEM=chip8` through xsdb after ps7_init (which sets
//! up clocks, MIO, UART1 at 115200 8N1 and SD 0); this app sets the UART up
//! again anyway, so it doesn't depend on that. No interrupts, MMU or caches.
//! Or (side track F) the boot ROM loads it from BOOT.BIN on the SD card
//! (`make boot-image`) as the first-stage program: then it runs ps7_init
//! itself (ps7.zig), and the PL is empty until it is configured.

const hw = @import("hw.zig");
const protocol = @import("protocol.zig");
const pl = @import("pl.zig");
const options = @import("options.zig");
const savestate = @import("savestate.zig");
const menu = @import("menu.zig");
const sd = @import("sd.zig");
const fat = @import("fat.zig");
const rom_load = @import("load.zig");
const save = @import("save.zig");
const ddrtest = @import("ddrtest.zig");
const usb = @import("usb.zig");
const keymap = @import("keymap.zig");
const gbc_palette = @import("gbc_palette.zig");
const usb_keys = @import("usb_keys.zig");
const a800_keys = @import("a800_keys.zig");
const ps7 = @import("ps7.zig");
const mmu = @import("mmu.zig");
const pcap = @import("pcap.zig");
const splash = @import("splash.zig");
const movie = @import("movie.zig");
const png = @import("png.zig");

/// The board this app is built for (systems/state_board.zig reads it).
pub const ps_board = @import("board").name;

const Card = sd.Sd(sd.MmioBus);
const Fat = fat.Fs(Card);

var port: hw.GpioPort = .{};
var link: pl.Link(hw.GpioPort) = .{ .port = &port };

// The menu and the card (globals: the FAT reader keeps a pointer to the
// card, and the lists are too big for the stack).
var system: ?*const menu.System = null;
/// The system ID the PL reported (0 = none), also for a system without a
/// menu row (the Game Gear, M5: keys only so far).
var pl_id: u8 = 0;
// in DDR (link.ld's .ddr, never zeroed): `init` writes it before use
var m: menu.Menu linksection(".ddr") = undefined;
var buttons: menu.Buttons = .{};
var screen: menu.Screen = .{};
var menu_open = false;
/// The volume as `/settings.txt` holds it (side track L, options.zig);
/// null: no such file on the mounted card, nothing is kept.
var settings_kept: ?u4 = null;
/// After opening the menu with a button, ignore the buttons until all
/// are released (the C that opened it isn't a "select").
var wait_release = false;
/// The menu is closed, the game still paused until the buttons are up.
var resume_pending = false;
/// The boot splash plays (side track J, splash.zig): any input, released,
/// opens the menu.
var splash_wait: ?splash.Wait = null;
/// The system's text output (the byte channel of PS_IN: the Game Boy's
/// serial port), passed on to the desktop as "serial: <line>" lines.
var byte_in: pl.ByteIn = .{};
var serial_lines: pl.Lines = .{};
var serial_last: u64 = 0;
var bus: sd.MmioBus = .{};
// The OCM's budget (192 KiB with the stacks; side K, 2026-10-18, side N's
// state code beside F1 and the screenshots): what is set at run time
// before its first use lives in DDR (NOLOAD, uncached: its speed doesn't
// matter here), as the menu: the card and its file system, the key maps
// and the USB keyboard's lookup (below)
var card: Card linksection(".ddr") = undefined;
var fs: Fat linksection(".ddr") = undefined;
var mounted = false;
/// `/systems` on the mounted card (side track F), if there is one
var systems_dir: ?fat.Dir = null;
/// The directory entries behind the menu's list (menu.Entry.ref indexes
/// them) and the directories down to the one shown. The entries are in
/// DDR (about 20 KiB; side track O moved them out of the OCM for the
/// movies' code), always written before they are read.
var fents: [menu.max_entries]fat.Entry linksection(".ddr") = undefined;
var dirs: [menu.max_depth + 1]fat.Dir = undefined;
/// Writes allowed (after an S command opened the card).
var sd_writable = false;
var sd_buf: [512]u8 = undefined;
var sd_check: [512]u8 = undefined;
/// make sd's M command: up to protocol.chunk_blocks blocks (side track G
/// phase 2); also the zeros of Z, written a chunk at a time
var sd_chunk: [protocol.chunk_blocks * 512]u8 = undefined;
/// The USB host on J13 (side track E): followed in the main loop, its
/// state on the menu's third row and over the UART.
var usb_bus: usb.MmioBus = .{};
var usb_host: usb.Host(usb.MmioBus) = undefined;
/// the controller's DMA memory (frame list, queue heads, transfer
/// descriptors, buffers), in DDR (usb.dma_ddr_addr)
const usb_dma: *volatile usb.Dma = @ptrFromInt(usb.dma_ddr_addr);
/// the status line's text; the menu keeps a slice of it
var usb_text: [40]u8 = undefined;
var usb_failures_seen: u32 = 0;
/// The key maps (keymap.zig, shared with the desktop's `link`): the
/// defaults, or `/keymap.conf` from the SD card (read when the card is
/// mounted, i.e. whenever the menu opens).
var key_config: keymap.Config linksection(".ddr") = undefined;
var usb_kb: usb_keys.Keyboard linksection(".ddr") = undefined;
/// The system's keys held on the USB keyboard, ORed with the desktop's
/// (`desk_keys`, from the K command) on their way to the PL.
var usb_word: keymap.Word = 0;
var desk_keys: keymap.Word = 0;
/// the key word's bits 16 to 31 as last written (menu.System.keys_hi)
var keys_hi_sent: ?u16 = null;
/// The USB keys are held back from the game while the menu is open, and
/// after it closes until they are all up (the Enter that loaded a ROM, the
/// Esc that closed the menu are not the game's).
var usb_hold = false;
/// The USB keyboard in the menu (step 5): the same edges and repeat as
/// the board buttons; the `[global] menu` key's last state.
var usb_buttons: menu.Buttons = .{};
var usb_menu_prev = false;
/// The menu was opened from the keyboard: ignore it until all is up.
var usb_wait_release = false;
// in DDR (link.ld's .ddr, never zeroed): readFile fills what is read
var keymap_buf: [4096]u8 linksection(".ddr") = undefined;
/// Battery saves (side track I; save.zig): the running game's save file
/// on the mounted card, if it has one, and its path (found again after
/// every mount: the card may have been swapped meanwhile).
var save_file: ?fat.Entry = null;
/// save states (side track L, L.7): the game loaded from the card, for its
/// slots (savestate.zig): their folder and name, the ROM's identity, its
/// cartridge RAM; `m.states_ok` says whether there is one
var state_dir: [96]u8 = undefined;
var state_dir_len: usize = 0;
var state_name: [fat.max_name]u8 = undefined;
var state_name_len: usize = 0;
var state_rom: savestate.RomId = .{};
var state_cram: u32 = 0;
var save_path_buf: [save.max_path]u8 = undefined;
var save_path_len: usize = 0;
/// The cartridge has a clock (MBC3): its time is part of the save.
var save_clock = false;
/// The game wrote its RAM since the last write-back; when, last.
var save_pending = false;
var save_last_ms: u64 = 0;
/// Written back once the RAM has been quiet this long (and when the menu
/// opens, before anything else is loaded): a power-off within it loses
/// the newest save.
const save_quiet_ms = 2000;
/// The card was opened for reading by an R command.
var sd_read_ready = false;

/// The key map section for the system the PL says it is (none for M0e).
fn keymapSystem() ?keymap.System {
    return switch (if (system) |s| s.id else pl_id) {
        1 => .chip8,
        2 => .gb,
        3 => .nes,
        5 => .a800xl,
        6 => .gb, // the Game Boy Color: the Game Boy's keys (side track I)
        7 => .gg,
        8 => .sms, // the Master System (M6; pad 2 in bits 8-13)
        9 => .snes, // the Super Nintendo (M7; pad 1 only so far)
        else => null,
    };
}

/// The USB keyboard's keys follow the current map (no keys for a system
/// without one).
fn applyKeymap() void {
    usb_kb.menu = key_config.menu;
    if (keymapSystem()) |ks| {
        usb_kb.setMap(key_config.map(ks));
    } else {
        // a constant, not a Map on the stack (576 bytes with 32 buttons)
        const none = comptime keymap.Map{ .system = .chip8 };
        usb_kb.setMap(&none);
    }
}

/// The 800XL's key bytes as last written (a800_keys.zig): sent again only
/// when they change.
var a800_sent: [a800_keys.len]u8 = [_]u8{0} ** a800_keys.len;

/// The keys to the PL: the desktop's, and the USB keyboard's unless held
/// back. The 800XL takes its keyboard as link writes (A800.Loader), the
/// joystick from the key map in the same bytes; the others on the user
/// bits.
fn pushKeys() void {
    const word = desk_keys | if (usb_hold) 0 else usb_word;
    // a movie (side track O): recording, the word waits for the next
    // frame toggle (moviePoll); playing, the movie's words go in, not ours
    if (movies_built and mv.mode != .off) {
        mv_live = @truncate(word);
        return;
    }
    if (system != null and system.?.id == 5) {
        const none = [_]u8{0} ** 8;
        const b = a800_keys.bytes(if (usb_hold) &none else &usb_kb.report, @truncate(word));
        if (!std.mem.eql(u8, &b, &a800_sent)) {
            a800_sent = b;
            link.write(a800_keys.base, &b);
        }
        return;
    }
    link.setUser(@truncate(word));
    // bits 16 to 31 (a second pad, M7's request): link writes, on change
    if (system) |s| if (s.keys_hi) |addr| {
        const hi: u16 = @truncate(word >> 16);
        if (keys_hi_sent != hi) {
            keys_hi_sent = hi;
            link.write(addr, &.{ @truncate(hi), @truncate(hi >> 8) });
        }
    };
}

/// A key word in hex: 4 digits while it fits (as before: tools/test_link.py
/// reads them), else 8.
fn putKeys(w: keymap.Word) void {
    if (w <= 0xFFFF) hw.puts(&hw.hex4(@intCast(w))) else hw.puts(&hw.hex8(w));
}

fn nowMs() u64 {
    return hw.now() / (hw.gtimer_hz / 1000);
}

extern var __bssd_start: u32;
extern var __bssd_end: u32;

export fn main() noreturn {
    const started = ps7.start();
    // DDR is up: the app's .bss there cleared (link.ld), then the MMU and
    // the L1 caches on (M1, mmu.zig); nothing before this line may touch
    // a variable in .bss
    @memset(@as([*]volatile u32, @ptrCast(&__bssd_start))[0 .. (@intFromPtr(&__bssd_end) - @intFromPtr(&__bssd_start)) / 4], 0);
    mmu.enable();
    var pl_configured = ps7.plConfigured();
    hw.init(pl_configured);
    hw.puts("relay: ready (K<hex> keys, H1/H0 hold, L<addr><len> load)\r\n");
    say("relay: ");
    say(ps7.describe(started));
    say(if (pl_configured) ", PL configured\r\n" else ", PL empty\r\n");
    key_config.setDefaults();
    // the volume and speed (side track L): `m` is undefined until the
    // first menu, and its `init` keeps them across systems
    m.opts = .{};
    if (movies_built) mv.init();
    usb_kb.reset(key_config.map(.chip8));
    // booted from the SD card: the PL is empty until we configure it
    if (!pl_configured) pl_configured = bootSystem();
    startSystem(pl_configured, true);

    var p: protocol.Parser = .{};
    var last = hw.now();
    const timeout = hw.gtimer_hz / 2;
    var baud: u3 = 0;
    var activity: protocol.Activity = .{ .last = hw.now() };
    var last_poll: u64 = 0;
    var stack_checked: u64 = 0;
    while (true) {
        if (hw.now() - stack_checked > hw.gtimer_hz) {
            stack_checked = hw.now();
            checkStack();
        }
        if (system != null) pollSerial();
        if (movies_built and mv.mode != .off and !menu_open) moviePoll();
        if (usb_host.poll()) |s| usbChanged(s);
        if (usb_host.takeReport()) |r| {
            const before = usb_kb.report;
            const w = usb_kb.feed(r);
            // the Controls page waits for a key: the one just pressed
            if (menu_open and m.mode == .controls and m.ctl.waiting) {
                if (usb_keys.newKey(&before, &usb_kb.report)) |in| controlsKey(in);
            }
            if (movies_built or shots_built) hotKeys();
            // (the 800XL's keyboard is in the whole report, not the word)
            if (w != usb_word or (system != null and system.?.id == 5)) {
                usb_word = w;
                pushKeys();
                hw.puts("usb: keys ");
                putKeys(w);
                hw.puts("\r\n");
            }
        }
        if (hw.getc()) |c| {
            const cmd_opt = p.feed(c);
            // silence (for the fall-back below) counts from the start and
            // the end of every command: a long load or SD zero-fill would
            // otherwise drop the rate under the desktop's next command
            // (protocol.Activity; both happened on the board)
            if (cmd_opt != null) activity.command(hw.now());
            defer if (cmd_opt != null) activity.commandDone(hw.now());
            if (cmd_opt) |cmd| switch (cmd) {
                .keys => |k| {
                    last = hw.now();
                    if (k != desk_keys) {
                        desk_keys = k;
                        pushKeys();
                        hw.puts("k");
                        putKeys(k);
                        hw.puts("\r\n");
                    }
                },
                .hold => |on| {
                    // the desktop loads another cartridge: the save first
                    if (on) {
                        flushSave();
                        save_file = null;
                        splash_wait = null;
                        // a new game starts at 100 % (side track L); no
                        // slots for a game from the desktop
                        m.newGame();
                        m.states_ok = false;
                        movieStop();
                        m.movies_ok = false;
                    } else link.settings(m.opts);
                    link.setHold(on);
                    // a ROM loaded from the desktop runs at once
                    if (!on and (menu_open or resume_pending)) closeMenu(true);
                    hw.puts(if (on) "h1\r\n" else "h0\r\n");
                },
                .load => |l| {
                    load(l.addr, l.len, l.wide);
                    activity.loadDone(hw.now());
                },
                .screenshot => {
                    if (!shots_built) {
                        hw.puts("xno screenshots in this build\r\n");
                    } else if (takeScreenshot()) |why| {
                        hw.puts("x");
                        hw.puts(why);
                        hw.puts("\r\n");
                    } else {
                        hw.puts("g");
                        hw.puts(shot_path_buf[0..shot_path_len]);
                        hw.puts("\r\n");
                    }
                },
                .frame => |step| {
                    if (movies_built) {
                        if (step) frameStep() else frameRun();
                    }
                    hw.puts(if (step) "f1\r\n" else "f0\r\n");
                },
                .ping => {
                    // the start-up log goes to the first ping: link always
                    // starts with one, while junk from the bridge at power-on
                    // can make up any other command (a key frame)
                    replayStartLog();
                    hw.puts("p\r\n");
                },
                .sd_open => sdOpen(),
                .sd_write => |w| sdWrite(w.lba, w.sum),
                .sd_zero => |z| sdZero(z.lba, z.count),
                .sd_read => |lba| sdRead(lba),
                .sd_multi => |mw| sdMulti(mw.lba, mw.count, mw.sum),
                .sd_hash => |h| sdHash(h.lba, h.count),
                .ddr_suite => if (m0e_board) ddrSuite(null) else noDdrTest(),
                .ddr_run => |r| if (!m0e_board) noDdrTest() else ddrSuite(.{
                    .base = r.base,
                    .mode = r.mode,
                    .words = r.words,
                    .busy = r.flags & 1 != 0,
                    .keep = r.flags & 2 != 0,
                }),
                .ddr_write => |w| ddrWrite(w.addr, w.len),
                .ddr_link => |y| ddrToLink(y.ddr, y.link, y.len),
                .menu => {
                    if (system != null) {
                        resume_pending = false;
                        openMenu();
                    }
                    hw.puts("o\r\n");
                },
                .baud => |b| {
                    hw.puts("b");
                    hw.putc('0' + @as(u8, b));
                    hw.puts("\r\n");
                    hw.uartBaud(b);
                    baud = b;
                    p = .{};
                },
            };
        } else if (activity.fallBack(hw.now(), baud, hw.gtimer_hz * 6 / 10)) {
            // nobody talks to us at this rate (the bridge can't, or the
            // desktop side restarted at 115200): fall back
            hw.uartBaud(0);
            baud = 0;
            p = .{};
        } else if (desk_keys != 0 and hw.now() - last > timeout) {
            desk_keys = 0;
            pushKeys();
            hw.puts("relay: timeout, keys released\r\n");
        } else if (system != null) {
            // the buttons, every millisecond (the PL debounces them)
            const t = nowMs();
            if (t != last_poll) {
                last_poll = t;
                if (splash_wait != null) {
                    pollSplash(t);
                } else {
                    pollUsbNav(t);
                    pollButtons(t);
                    pollSave(t);
                }
            }
        }
    }
}

// --- The start-up log ------------------------------------------------------------------
// Booted from the SD card, nobody listens on J14 while the app starts (the
// bridge re-enumerates with the board's power), so what start-up says is
// kept and sent again, once, before the reply to the first ping from
// the desktop (make keys, make ping show it as "board: ..." lines).
var start_log: [512]u8 = undefined;
var start_log_len: usize = 0;
var start_log_sent = false;

fn say(s: []const u8) void {
    hw.puts(s);
    const k = @min(s.len, start_log.len - start_log_len);
    @memcpy(start_log[start_log_len..][0..k], s[0..k]);
    start_log_len += k;
}

fn sayDec(v: u32) void {
    var buf: [10]u8 = undefined;
    var i: usize = buf.len;
    var x = v;
    while (true) {
        i -= 1;
        buf[i] = '0' + @as(u8, @intCast(x % 10));
        x /= 10;
        if (x == 0) break;
    }
    say(buf[i..]);
}

fn sayLine(what: []const u8, err: []const u8) void {
    say(what);
    say(err);
    say("\r\n");
}

fn replayStartLog() void {
    if (start_log_sent) return;
    start_log_sent = true;
    var lines = std.mem.splitSequence(u8, start_log[0..start_log_len], "\r\n");
    while (lines.next()) |l| {
        if (l.len == 0) continue;
        hw.puts("start-up: ");
        hw.puts(l);
        hw.puts("\r\n");
    }
}

// --- Systems: the PL from the SD card (side track F) ---------------------------------
/// Start whatever the PL holds: a fresh link, the USB host started again
/// (its PHY's reset pin comes from the PL, so a new configuration may have
/// reset it; PLAN.md, notes from side track E), the system ID read, its
/// menu opened (or first its boot splash, if `play_splash` and the card has
/// one). Without a known system only the relay runs.
fn startSystem(pl_configured: bool, play_splash: bool) void {
    splash_wait = null;
    link = .{ .port = &port };
    hw.gpioWrite(0);
    byte_in = .{};
    // a new PL: no game, so no save (the menu wrote it back when it opened)
    save_file = null;
    save_pending = false;
    serial_lines = .{};
    usb_host = usb.Host(usb.MmioBus).init(&usb_bus, usb_dma);
    usb_word = 0;
    desk_keys = 0;
    keys_hi_sent = null; // a new PL: its pad-2 register starts at 0
    // a new PL: its 800XL key register starts at nothing held
    a800_sent = [_]u8{0} ** a800_keys.len;
    // the PL's registered PS_IN is valid a few clocks after configuration
    hw.delayTicks(hw.ms(1));
    pl_id = if (pl_configured) link.inputs().system else 0;
    system = if (pl_configured) menu.systemById(pl_id) else null;
    applyKeymap();
    if (system) |s| {
        m.init(s);
        m.version = "PS " ++ build_info.ps_version;
        // the Controls page: the system's buttons (none without a key map)
        m.ctl.n = if (keymapSystem()) |ks| @intCast(keymap.buttons(ks).len) else 0;
        say("menu: ");
        say(s.name);
        say(", hold ");
        say(s.menu_combo_name);
        say(" for the menu\r\n");
        if (!(play_splash and playSplash(s))) openMenu();
    } else {
        say(if (keymapSystem() != null) "menu: keys only for this system, menu off\r\n" else "menu: no system ID from the PL, menu off\r\n");
    }
}

/// The system's boot splash (side track J, splash.zig): its file from
/// `/systems/boot` loaded as a ROM, with no save and the overlay hidden.
/// False, with the reason on the UART, if there is none or it can't be
/// loaded: the caller opens the menu as before.
// noinline: inlined, its loading frame sat in startSystem's on every system
// switch (Systems menu -> startSystem -> openMenu -> mountAndList), and the
// stack overflowed into the card's state ("listing failed: BadCluster",
// 2026-10-08)
noinline fn playSplash(s: *const menu.System) bool {
    mounted = false; // fs is remounted in place below: unusable if that fails
    card = Card.init(&bus) catch |e| {
        sayLine("splash: no SD card: ", @errorName(e));
        return false;
    };
    fs.mountInto(&card) catch |e| {
        sayLine("splash: no FAT file system: ", @errorName(e));
        return false;
    };
    mounted = true;
    // the splash at the volume kept on the card (side track L)
    loadSettings();
    const found = splash.find(fat.Entry, &fs, findSystems(), s) catch |e| {
        sayLine("splash: read failed: ", @errorName(e));
        return false;
    };
    const e = switch (found) {
        .file => |f| f,
        .none => |why| {
            sayLine("splash: none, ", why);
            return false;
        },
    };
    var r = fs.reader(e);
    const prepared = rom_load.prepare(s, e.size, &r) catch |err| {
        sayLine("splash: read failed: ", @errorName(err));
        return false;
    };
    const head = switch (prepared) {
        .refused => |why| {
            sayLine("splash: refused: ", why);
            return false;
        },
        .head => |h| h,
    };
    // the NES: what also goes over the link once the file is in DDR (its
    // header and CHR ROM, side track D), from the header before the
    // stream reuses its buffer (as loadRom)
    const parts: ?[2]menu.Range = if (s.ddr_link) |f| f(head) else null;
    if (s.ddr_addr) |base| {
        // a companion file first (M6: the Master System's BIOS), if any;
        // the core held from here, and held it stays if that fails
        // (load.loadCompanionHeld)
        var companion_size: u32 = 0;
        switch (rom_load.loadCompanionHeld(s, &fs, &card, &rom_mem, &link) catch |err| {
            sayLine("splash: read failed: ", @errorName(err));
            return false;
        }) {
            .none => {},
            .loaded => |k| companion_size = k,
            .refused => |why| {
                sayLine("splash: refused: ", why);
                return false;
            },
        }
        var n = rom_load.streamDdr(s, &fs, &card, e, &rom_mem) catch |err| {
            sayLine("splash: read failed: ", @errorName(err));
            return false;
        };
        asm volatile ("dsb" ::: .{ .memory = true });
        n = ddrImage(s, n);
        if (parts) |p| _ = rom_load.sendFromDdr(s, p, @as([*]const u8, @ptrFromInt(base))[0..n], &link);
        // the facts from the whole ROM (the Game Gear's mapper and size), as
        // loadRom writes them while the core is held
        if (s.ddr_info) |di| {
            const w0 = di.of(@as([*]const u8, @ptrFromInt(base))[0..n]);
            const w = if (s.companion) |c| c.word(w0, companion_size) else w0;
            link.write(di.addr, &w);
        }
    } else _ = rom_load.streamHeld(s, head, &r, &link) catch |err| {
        sayLine("splash: read failed: ", @errorName(err));
        return false;
    };
    link.setHold(false);
    link.settings(m.opts);
    link.control(.{});
    menu_open = false;
    resume_pending = false;
    // the keyboard's keys stay out of the splash
    usb_hold = true;
    pushKeys();
    splash_wait = .{};
    say("splash: ");
    say(e.name());
    say(" plays; any key or button opens the menu\r\n");
    return true;
}

/// While the splash plays, every millisecond: any input ends it, and the
/// menu opens once that input is released. The menus' button state is
/// kept current meanwhile, so no press is left over for it.
fn pollSplash(t: u64) void {
    const b = link.inputs().buttons;
    const nav = usb_kb.nav();
    _ = buttons.poll(t, b);
    _ = usb_buttons.poll(t, .{ .u = nav.up, .d = nav.down, .l = nav.left, .r = nav.right, .c = nav.center });
    usb_menu_prev = nav.menu;
    const any = @as(u5, @bitCast(b)) != 0 or usb_word != 0 or @as(u8, @bitCast(nav)) != 0 or desk_keys != 0;
    if (splash_wait.?.poll(any)) {
        say("splash: done, menu\r\n");
        openMenu();
    }
}

/// `/systems` on the mounted card, if it is there.
fn findSystems() ?fat.Dir {
    const d = (fs.find(fs.root(), "systems") catch null) orelse return null;
    return if (d.is_dir) fs.openDir(d) else null;
}

/// At power-on with an empty PL: the system started last
/// (`/systems/last.txt`), else the first bitstream in `/systems`. True if
/// the PL is configured afterwards.
fn bootSystem() bool {
    mounted = false; // fs is remounted in place below: unusable if that fails
    card = Card.init(&bus) catch |e| {
        sayLine("systems: no SD card: ", @errorName(e));
        return false;
    };
    say(if (card.wide) "systems: SD card on the 4-bit bus\r\n" else "systems: SD card on the 1-bit bus\r\n");
    fs.mountInto(&card) catch |e| {
        sayLine("systems: no FAT file system: ", @errorName(e));
        return false;
    };
    mounted = false; // the menu mounts the card again
    const dir = findSystems() orelse {
        say("systems: no /systems folder on the card\r\n");
        return false;
    };
    // the last one, if the card names one that is there
    var tried_buf: [fat.max_name]u8 = undefined;
    var tried: []const u8 = "";
    var text: [64]u8 = undefined;
    if (fs.find(dir, menu.last_file) catch null) |le| if (!le.is_dir) {
        const stem = menu.readLast(text[0 .. fs.readFile(le, &text) catch 0]);
        var name_buf: [fat.max_name]u8 = undefined;
        if (stem.len > 0) if (std.fmt.bufPrint(&name_buf, "{s}.bit", .{stem}) catch null) |name| {
            if (fs.find(dir, name) catch null) |be| {
                if (!be.is_dir) {
                    @memcpy(tried_buf[0..be.name().len], be.name());
                    tried = tried_buf[0..be.name().len];
                    if (loadBitstream(be) == .ok) return true;
                }
            } else {
                say("systems: the last used, ");
                say(name);
                say(", is not on the card\r\n");
            }
        };
    };
    var it = fs.iterate(dir);
    while (it.next() catch |e| {
        sayLine("systems: listing failed: ", @errorName(e));
        return false;
    }) |e| {
        if (e.is_dir or !menu.hasExt(e.name(), &menu.bit_ext)) continue;
        if (std.ascii.eqlIgnoreCase(e.name(), tried)) continue;
        return loadBitstream(e) == .ok;
    }
    say("systems: no .bit file in /systems\r\n");
    return false;
}

const Loaded = enum {
    /// in the PL, running
    ok,
    /// refused before the PL was touched (read error, not a bitstream for
    /// this board): the running system is still there
    refused,
    /// the PL was cleared, but the new configuration failed: it is empty
    failed,
};

/// A .bit file from the card into the PL (pcap.zig): header checked, data
/// swapped into DDR, the PL cleared and configured. Reports over the UART.
fn loadBitstream(e: fat.Entry) Loaded {
    const t0 = hw.now();
    say("systems: loading ");
    say(e.name());
    say("\r\n");
    var r = fs.reader(e);
    const first = (r.next() catch |err| return bitFailed("read failed: ", @errorName(err))) orelse
        return bitFailed("empty file", "");
    const h = pcap.parseHeader(first, e.size, hw.pl_part) catch |err|
        return bitFailed("not a bitstream for this board: ", @errorName(err));
    // the header's text lives in the reader's sector buffer: keep a copy
    var about_buf: [64]u8 = undefined;
    const about = std.fmt.bufPrint(&about_buf, "{s} ({s} {s})", .{ h.name(), h.date, h.time }) catch h.name();
    var mem: pcap.DdrMem = .{};
    var sink: BitSink = .{ .sw = .{ .mem = &mem }, .skip = h.data_offset, .left = e.size };
    // the whole file again, in runs of consecutive blocks (multi-block
    // reads: the card's access time once per run, not once per block)
    var runs = fs.runs(e, 0xFFFF);
    while (runs.next() catch |err| return bitFailed("read failed: ", @errorName(err))) |run| {
        sink.left = run.bytes;
        card.readBlocks(run.lba, @intCast(run.blocks), &sink) catch |err|
            return bitFailed("read failed: ", @errorName(err));
    }
    const sw = sink.sw;
    if (sw.words * 4 != h.data_len) return bitFailed("file shorter than its header says", "");
    if (sw.sync_at == null) return bitFailed("no sync word", "");
    const t1 = hw.now();
    var pbus: pcap.MmioBus = .{};
    var p: pcap.Pcap(pcap.MmioBus) = .{ .bus = &pbus };
    p.program(pcap.ddr_addr, sw.words) catch |err| {
        say("systems: PCAP failed, INT_STS ");
        say(&hw.hex8(p.err_bits));
        say("\r\n");
        _ = bitFailed("PCAP: ", @errorName(err));
        return .failed;
    };
    const t2 = hw.now();
    say("systems: ");
    say(about);
    say(" in the PL: read ");
    sayDec(@intCast((t1 - t0) / hw.ms(1)));
    say(" ms, configured ");
    sayDec(@intCast((t2 - t1) / hw.ms(1)));
    say(" ms\r\n");
    return .ok;
}

const BitSink = pcap.BlockSink(pcap.DdrMem);

fn bitFailed(what: []const u8, err: []const u8) Loaded {
    say("systems: ");
    sayLine(what, err);
    return .refused;
}

/// The Systems level: the bitstreams in `/systems`, the cursor on the
/// running one.
fn listSystems() void {
    m.beginSystems();
    const dir = systems_dir orelse {
        m.done(0);
        return;
    };
    var it = fs.iterate(dir);
    var k: u32 = 0;
    while (true) {
        const e = (it.next() catch |err| {
            m.fail("SD card: read error.");
            report("menu: listing failed: ", @errorName(err));
            return;
        }) orelse break;
        if (k == fents.len) break;
        fents[k] = e;
        if (!m.add(e.name(), e.is_dir, e.size, k)) break;
        if (m.n > 0 and m.entries[m.n - 1].ref == k) k += 1;
    }
    m.done(0);
    for (0..m.n) |i| if (m.isRunning(i)) {
        m.cursor = i;
    };
    // each bitstream's build date (its header's field c; M7's request for
    // the user, 2026-11-03): one block read each, none shown if it fails
    for (0..m.n) |i| {
        const e = fents[m.entries[i].ref];
        const lba = fs.firstBlock(e) catch continue;
        card.readBlock(lba, &sd_buf) catch continue;
        const h = pcap.parseHeader(&sd_buf, e.size, hw.pl_part) catch continue;
        m.setBitDate(i, h.date);
    }
}

/// A system chosen on the Systems level: the running one goes back to its
/// ROMs; another is loaded into the PL and its menu opens. If that fails
/// after the PL was cleared, the system that ran is loaded again.
fn pickSystem(i: usize) void {
    if (m.isRunning(i)) {
        mountAndList();
        return;
    }
    // a movie ends with its system (side track O)
    movieStop();
    const e = fents[m.entries[i].ref];
    var stem_buf: [fat.max_name]u8 = undefined;
    const stem = stem_buf[0..menu.bitStem(e.name()).len];
    @memcpy(stem, menu.bitStem(e.name()));
    // the running system's own bitstream, to fall back to
    var prev: ?fat.Entry = null;
    var prev_name: [fat.max_name]u8 = undefined;
    if (std.fmt.bufPrint(&prev_name, "{s}.bit", .{system.?.dir}) catch null) |pn| {
        if (systems_dir) |d| prev = fs.find(d, pn) catch null;
    }
    var msg: [menu.cols]u8 = undefined;
    m.message(std.fmt.bufPrint(&msg, "Starting {s}...", .{stem}) catch "Starting...");
    show(false);
    switch (loadBitstream(e)) {
        .ok => {
            rememberLast(stem);
            startSystem(true, key_config.splash_on_switch);
        },
        .refused => {
            m.message(std.fmt.bufPrint(&msg, "{s}: not a bitstream for this board.", .{stem}) catch "Not a bitstream.");
            show(false);
        },
        .failed => {
            const back = if (prev) |p| loadBitstream(p) == .ok else false;
            startSystem(back, false);
            if (system != null) {
                m.message(std.fmt.bufPrint(&msg, "Could not start {s}.", .{stem}) catch "Could not start it.");
                show(false);
            }
        },
    }
}

/// `/systems/last.txt` names the system just started (menu.writeLast: its
/// one block rewritten in place). Without the file, nothing is remembered.
fn rememberLast(stem: []const u8) void {
    const dir = systems_dir orelse return;
    const e = (fs.find(dir, menu.last_file) catch null) orelse {
        hw.puts("systems: no /systems/last.txt, the choice is not remembered\r\n");
        return;
    };
    const lba = fs.firstBlock(e) catch return;
    card.readBlock(lba, &sd_buf) catch |err| return report("systems: last.txt: ", @errorName(err));
    if (!menu.writeLast(&sd_buf, e.size, stem)) return report("systems: last.txt: ", "too small");
    card.writeBlock(lba, &sd_buf) catch |err| return report("systems: last.txt: ", @errorName(err));
    fs.buf_lba = null; // the reader's cached block may be this one
    hw.puts("systems: remembered ");
    hw.puts(stem);
    hw.puts("\r\n");
}

/// The USB host's state changed: tell the desktop, update the menu.
fn usbChanged(s: usb.Status) void {
    if (s == .no_device and usb_host.connects == 0) {
        hw.puts("usb: PHY ");
        hw.puts(&hw.hex4(usb_host.phyVendor()));
        hw.puts(":");
        hw.puts(&hw.hex4(usb_host.phyProduct()));
        hw.puts("\r\n");
    }
    const text = s.text(&usb_text);
    hw.puts("usb: ");
    hw.puts(text[5..]);
    hw.puts("\r\n");
    if (usb_host.enum_failures != usb_failures_seen) {
        // an enumeration step failed (retried with a port reset, up to 3
        // times): which one, and why
        usb_failures_seen = usb_host.enum_failures;
        hw.puts("usb: enumeration failed in ");
        hw.puts(usb_host.failState());
        hw.puts(": ");
        hw.puts(usb_host.fail_why);
        hw.puts(", portsc ");
        hw.puts(&hw.hex8(usb_host.fail_regs.portsc));
        hw.puts(" sts ");
        hw.puts(&hw.hex8(usb_host.fail_regs.sts));
        hw.puts("\r\n");
    }
    if (s == .no_phy or s == .reset_failed) {
        // where it stopped, and the registers then (usb.zig, usb_hw.zig)
        const r = usb_host.fail_regs;
        hw.puts("usb: failed in ");
        hw.puts(usb_host.failState());
        hw.puts(" after ");
        hw.putDec(@intCast(usb_host.ulpi_step));
        hw.puts(" ULPI ops: cmd ");
        hw.puts(&hw.hex8(r.cmd));
        hw.puts(" sts ");
        hw.puts(&hw.hex8(r.sts));
        hw.puts(" portsc ");
        hw.puts(&hw.hex8(r.portsc));
        hw.puts(" ulpi ");
        hw.puts(&hw.hex8(r.ulpi));
        hw.puts(" mode ");
        hw.puts(&hw.hex8(r.mode));
        hw.puts(" phy ");
        hw.puts(&hw.hex4(usb_host.phyVendor()));
        hw.puts(":");
        hw.puts(&hw.hex4(usb_host.phyProduct()));
        hw.puts("\r\n");
    }
    if (system != null) {
        m.usb = text;
        if (menu_open) show(false);
    }
}

/// L command: load `len` raw bytes from the UART at `addr` of the
/// system's link address map. Replies "l<addr><len><sum>" (hex; the address
/// as wide as it came: 3 or 6 digits), or "e<addr><len>" if the bytes
/// stopped coming for 1 s. Only meaningful while held.
fn load(addr: u24, len: u16, wide: bool) void {
    link.setAddr(addr);
    var sum: u16 = 0;
    var i: u32 = 0;
    while (i < len) : (i += 1) {
        const b = hw.getcTimeout(hw.gtimer_hz) orelse {
            hw.puts("e");
            putAddr(addr, wide);
            hw.puts(&hw.hex4(len));
            hw.puts("\r\n");
            return;
        };
        link.data(&.{b});
        sum = protocol.checksum(sum, b);
    }
    hw.puts("l");
    putAddr(addr, wide);
    hw.puts(&hw.hex4(len));
    hw.puts(&hw.hex4(sum));
    hw.puts("\r\n");
}

// --- DDR (side track C) ------------------------------------------------------------
/// The DDR as ddrtest.zig uses it. With the MMU off every data access is
/// strongly ordered (ARMv7-A ARM, B3.2.1) and uncached, so a write is in
/// DDR before the next access, where the PL's HP port reads it.
const Ddr = struct {
    pub fn write64(_: *Ddr, a: u32, v: u64) void {
        @as(*volatile u64, @ptrFromInt(a)).* = v;
    }
    pub fn read64(_: *Ddr, a: u32) u64 {
        return @as(*volatile u64, @ptrFromInt(a)).*;
    }
    pub fn nowUs(_: *Ddr) u64 {
        return hw.now() / (hw.gtimer_hz / 1_000_000);
    }
    /// A share of the CPU's own DDR traffic during a busy run: 1024 words
    /// read and written back in its area (ddrtest.busy_base).
    pub fn hammer(_: *Ddr, n: *u32) void {
        var i: u32 = 0;
        while (i < 1024) : (i += 1) {
            const a = ddrtest.busy_base + 8 * ((n.* + i) % ddrtest.busy_words);
            const p: *volatile u64 = @ptrFromInt(a);
            p.* = p.* +% 1;
        }
        n.* +%= 1024;
    }
};
var ddr: Ddr = .{};

const Put = struct {
    pub fn line(_: *Put, s: []const u8) void {
        hw.puts(s);
        hw.puts("\r\n");
    }
};

/// T: the suite, or one run; only on M0e's bitstream (system ID 4).
/// M0e (the DDR test) has a ZedBoard top only: its test suite is left out
/// of the Arty's app (about 4 KiB of the OCM's budget).
const m0e_board = std.mem.eql(u8, @import("board").name, "zedboard");

fn noDdrTest() void {
    hw.puts("x the DDR test (M0e) is the ZedBoard's\r\n");
}

fn ddrSuite(one: ?ddrtest.Run) void {
    const id = link.inputs().system;
    if (id != ddrtest.system_m0e) {
        hw.puts("x no DDR test in this bitstream (system ID ");
        hw.putDec(id);
        hw.puts("; make prog SYSTEM=m0e)\r\n");
        return;
    }
    var t: ddrtest.Tester(hw.GpioPort, Ddr) = .{ .link = &link, .mem = &ddr };
    var put: Put = .{};
    var failed: u32 = 0;
    const runs: []const ddrtest.Run = if (one) |*r| r[0..1] else &ddrtest.suite;
    for (runs) |run| {
        if (ddrtest.check(run)) |why| {
            hw.puts("x ");
            hw.puts(why);
            hw.puts("\r\n");
            return;
        }
        const rep = t.go(run) catch |e| {
            hw.puts("x ");
            hw.puts(@errorName(e));
            hw.puts(" (no answer from the PL)\r\n");
            return;
        };
        rep.print(&put);
        if (!rep.ok()) failed += 1;
    }
    hw.puts(if (failed == 0) "t end ok\r\n" else "t end FAIL\r\n");
}

/// W: `len` raw bytes from the UART into DDR at `addr`.
fn ddrWrite(addr: u32, len: u24) void {
    const end = @as(u64, addr) + len;
    if (addr < ddrtest.ddr_low or end > ddrtest.ddr_end) {
        hw.puts("x DDR writes go to 0x00100000 .. 0x1FFFFFFF\r\n");
        // the bytes still come: swallow them
        var k: u32 = 0;
        while (k < len) : (k += 1) _ = hw.getcTimeout(hw.gtimer_hz) orelse return;
        return;
    }
    var sum: u16 = 0;
    var i: u32 = 0;
    while (i < len) : (i += 1) {
        const b = hw.getcTimeout(hw.gtimer_hz) orelse {
            hw.puts("x data stopped\r\n");
            return;
        };
        @as(*volatile u8, @ptrFromInt(addr + i)).* = b;
        sum = protocol.checksum(sum, b);
    }
    asm volatile ("dsb" ::: .{ .memory = true });
    hw.puts("w");
    hw.puts(&hw.hex8(addr));
    hw.puts(hw.hex8(len)[2..]);
    hw.puts(&hw.hex4(sum));
    hw.puts("\r\n");
}

/// Y command (side track D, `make load SYSTEM=nes`): bytes already in DDR
/// onto the PS link, as the SD menu's `load.sendFromDdr`.
fn ddrToLink(addr: u32, at: u24, len: u24) void {
    const end = @as(u64, addr) + len;
    if (addr < ddrtest.ddr_low or end > ddrtest.ddr_end) {
        hw.puts("x DDR reads go from 0x00100000 .. 0x1FFFFFFF\r\n");
        return;
    }
    const bytes: []const u8 = @as([*]const u8, @ptrFromInt(addr))[0..len];
    var sum: u16 = 0;
    for (bytes) |b| sum = protocol.checksum(sum, b);
    link.write(at, bytes);
    hw.puts("y");
    hw.puts(hw.hex8(at)[2..]);
    hw.puts(hw.hex8(len)[2..]);
    hw.puts(&hw.hex4(sum));
    hw.puts("\r\n");
}

fn putAddr(addr: u24, wide: bool) void {
    if (wide) hw.puts(hw.hex8(addr)[2..]) else hw.puts(hw.hex4(@truncate(addr))[1..]);
}

// --- Writing the card (make sd) -------------------------------------------------------
fn sdOpen() void {
    // the card is about to be rewritten: the save goes first, and the
    // game keeps no save file (its blocks will hold other data)
    flushSave();
    save_file = null;
    mounted = false; // the menu reads the card again when it next opens
    sd_writable = false;
    card = Card.init(&bus) catch |e| {
        sdFail(null, @errorName(e));
        return;
    };
    sd_writable = true;
    hw.puts("s");
    hw.putc('0' + @as(u8, @intFromEnum(card.card)));
    hw.puts("\r\n");
}

/// D: 512 bytes from the UART, checked against `sum`, written, read back.
fn sdWrite(lba: u32, sum: u16) void {
    var s: u16 = 0;
    for (&sd_buf) |*b| {
        b.* = hw.getcTimeout(hw.gtimer_hz) orelse {
            sdFail(lba, "data stopped");
            return;
        };
        s = protocol.checksum(s, b.*);
    }
    if (s != sum) return sdFail(lba, "checksum");
    if (!sd_writable) return sdFail(lba, "card not open (S)");
    sdPut(lba) catch |e| return sdFail(lba, e);
    hw.puts("d");
    hw.puts(&hw.hex8(lba));
    hw.puts("\r\n");
}

/// Z: `count` zero blocks from `lba`, read back; a chunk
/// (protocol.chunk_blocks) per multi-block write (side track G phase 2:
/// a block at a time, a fresh card's empty space took minutes).
fn sdZero(lba: u32, count: u32) void {
    if (!sd_writable) return sdFail(lba, "card not open (S)");
    @memset(&sd_chunk, 0);
    var i: u32 = 0;
    while (i < count) {
        const n: u32 = @min(count - i, protocol.chunk_blocks);
        sdPutChunk(lba + i, sd_chunk[0 .. n * 512]) catch |e| return sdFail(lba + i, e);
        i += n;
    }
    hw.puts("z");
    hw.puts(&hw.hex8(lba));
    hw.puts(&hw.hex8(count));
    hw.puts("\r\n");
}

/// M: `count` blocks from the UART, checked against `sum`, written with
/// one multi-block write, read back (side track G phase 2: make sd).
fn sdMulti(lba: u32, count: u8, sum: u16) void {
    const data = sd_chunk[0 .. @as(usize, count) * 512];
    var s: u16 = 0;
    for (data) |*b| {
        b.* = hw.getcTimeout(hw.gtimer_hz) orelse {
            sdFail(lba, "data stopped");
            return;
        };
        s = protocol.checksum(s, b.*);
    }
    if (s != sum) return sdFail(lba, "checksum");
    if (!sd_writable) return sdFail(lba, "card not open (S)");
    sdPutChunk(lba, data) catch |e| return sdFail(lba, e);
    hw.puts("m");
    hw.puts(&hw.hex8(lba));
    hw.puts("\r\n");
}

/// C: the hash of `count` blocks from `lba` as they are on the card.
fn sdHash(lba: u32, count: u8) void {
    if (!sd_writable) return sdFail(lba, "card not open (S)");
    var h: Hasher = .{};
    card.readBlocks(lba, count, &h) catch |e| return sdFail(lba, @errorName(e));
    const v = h.h.final();
    hw.puts("c");
    hw.puts(&hw.hex8(lba));
    hw.puts(&hw.hex8(@truncate(v >> 32)));
    hw.puts(&hw.hex8(@truncate(v)));
    hw.puts("\r\n");
}

/// Hashes the blocks a multi-block read hands over (sd.zig's sink).
const Hasher = struct {
    h: protocol.ChunkHash = protocol.ChunkHash.init(),

    pub fn block(self: *Hasher, b: *const [512]u8) void {
        self.h.update(b);
    }
};

/// `data` (whole blocks) to the blocks from `lba` in one multi-block
/// write, then read back and compared by hash (no second buffer).
fn sdPutChunk(lba: u32, data: []const u8) error{Failed}!void {
    card.writeBlocks(lba, data) catch |e| {
        sd_err = @errorName(e);
        return error.Failed;
    };
    var h: Hasher = .{};
    card.readBlocks(lba, @intCast(data.len / 512), &h) catch |e| {
        sd_err = @errorName(e);
        return error.Failed;
    };
    if (h.h.final() != protocol.chunkHash(data)) {
        sd_err = "read back differs";
        return error.Failed;
    }
}

/// sd_buf to block `lba`, then read back and compare.
fn sdPut(lba: u32) error{Failed}!void {
    card.writeBlock(lba, &sd_buf) catch |e| {
        sd_err = @errorName(e);
        return error.Failed;
    };
    card.readBlock(lba, &sd_check) catch |e| {
        sd_err = @errorName(e);
        return error.Failed;
    };
    if (!std.mem.eql(u8, &sd_buf, &sd_check)) {
        sd_err = "read back differs";
        return error.Failed;
    }
}
var sd_err: []const u8 = "";

fn sdFail(lba: ?u32, what: anytype) void {
    const text: []const u8 = if (@TypeOf(what) == error{Failed}) sd_err else what;
    hw.puts("x");
    if (lba) |l| hw.puts(&hw.hex8(l));
    hw.puts(" ");
    hw.puts(text);
    hw.puts("\r\n");
}

// --- The menu ---------------------------------------------------------------------
/// Every loop: a byte from the system, if one came (at most about one per
/// millisecond); a part line goes out after 0.2 s without bytes.
fn pollSerial() void {
    if (byte_in.poll(link.inputs())) |c| {
        serial_last = hw.now();
        if (serial_lines.add(c)) |line| putSerial(line);
    } else if (serial_lines.len != 0 and hw.now() - serial_last > hw.ms(200)) {
        if (serial_lines.flush()) |line| putSerial(line);
    }
}

fn putSerial(line: []const u8) void {
    hw.puts("serial: ");
    hw.puts(line);
    hw.puts("\r\n");
}

fn pollButtons(t: u64) void {
    const b = link.inputs().buttons;
    if (resume_pending) {
        // the menu is gone; the game goes on once the button that closed
        // it is up (else the game would see it as a key)
        if (@as(u5, @bitCast(b)) == 0) {
            resume_pending = false;
            link.control(.{});
        }
        _ = buttons.menuHold(t, b, system.?.menu_combo);
        return;
    }
    if (!menu_open) {
        if (buttons.menuHold(t, b, system.?.menu_combo)) {
            wait_release = true;
            openMenu();
        }
        return;
    }
    if (wait_release) {
        _ = buttons.poll(t, b);
        if (!buttons.anyHeld()) wait_release = false;
        return;
    }
    menuEvent(buttons.poll(t, b));
}

/// The USB keyboard and the menu (side track E, step 5): the `[global]
/// menu` key (Esc by default) opens it from a game and closes it again;
/// in the menu the arrows, Enter, Backspace and the system's own D-pad and
/// A/B keys act as the board buttons do (usb_keys.Keyboard.nav).
fn pollUsbNav(t: u64) void {
    const nav = usb_kb.nav();
    const menu_edge = nav.menu and !usb_menu_prev;
    usb_menu_prev = nav.menu;
    const b: pl.Buttons = .{ .u = nav.up, .d = nav.down, .l = nav.left, .r = nav.right, .c = nav.center };
    if (!menu_open) {
        _ = usb_buttons.poll(t, b); // keeps its edges current
        if (usb_hold and !resume_pending and usb_word == 0 and @as(u8, @bitCast(nav)) == 0) {
            usb_hold = false;
            pushKeys();
        }
        if (menu_edge and !resume_pending) {
            usb_wait_release = true;
            openMenu();
        }
        return;
    }
    // the Controls page waits for a key (the report loop takes it, Esc
    // too): the keyboard doesn't move the menu meanwhile
    if (m.mode == .controls and m.ctl.waiting) {
        _ = usb_buttons.poll(t, b);
        return;
    }
    if (menu_edge) {
        closeMenu(false);
        return;
    }
    if (usb_wait_release) {
        _ = usb_buttons.poll(t, b);
        if (!usb_buttons.anyHeld() and !nav.menu) usb_wait_release = false;
        return;
    }
    menuEvent(usb_buttons.poll(t, b));
}

/// One menu event, from the board buttons or the keyboard.
fn menuEvent(e: menu.Event) void {
    switch (m.handle(e)) {
        .none => return,
        .moved => {},
        .open_dir => |i| {
            if (m.depth < menu.max_depth) {
                dirs[m.depth + 1] = fs.openDir(fents[m.entries[i].ref]);
                m.enter(i);
                list(0);
            }
        },
        .up => {
            const cur = m.leave();
            list(cur);
        },
        .load => |i| {
            loadRom(i);
            if (!menu_open) return;
        },
        .resume_game => {
            closeMenu(false);
            return;
        },
        .retry => mountAndList(),
        .systems => listSystems(),
        .save_state => |n| saveState(n),
        .load_state => |n| loadState(n),
        .record_movie => if (movies_built) startMovie(.record),
        .screenshot => if (shots_built) {
            if (takeScreenshot()) |why| m.message(why) else {
                var msg: [menu.cols]u8 = undefined;
                const p = shot_path_buf[0..shot_path_len];
                m.message(std.fmt.bufPrint(&msg, "Saved {s}", .{p[@min(p.len, p.len -| (menu.cols - 7))..]}) catch "Saved.");
            }
        },
        .play_movie => if (movies_built) startMovie(.play),
        .stop_movie => if (movies_built) {
            const was = mv.mode;
            const frames = mv.frame;
            movieStop();
            if (mounted) refreshMovie();
            var msg: [menu.cols]u8 = undefined;
            m.message(if (was == .record)
                std.fmt.bufPrint(&msg, "Recording saved: {d} frames.", .{frames}) catch "Recording saved."
            else
                "The movie stopped.");
        },
        .pick_system => |i| {
            pickSystem(i);
            // a new system opened its own menu (or none is left)
            if (m.mode == .roms or system == null) return;
        },
        .controls => {
            for (0..m.ctl.n) |i| controlsRow(i);
            m.openControls();
        },
        .clear_key => |i| if (keymapSystem()) |ks| {
            key_config.map(ks).bind[i].clearKeys();
            keysChanged();
        },
        .reset_keys => if (keymapSystem()) |ks| {
            key_config.map(ks).* = keymap.defaults(ks);
            for (0..m.ctl.n) |i| controlsRow(i);
            m.ctl.changed = true;
            applyKeymap();
        },
        .controls_done => if (m.ctl.changed) saveKeymap(),
    }
    show(false);
}

// --- the Controls page (side K, 2026-10-18; menu.Controls) -------------

/// Row i: the button's name, its keyboard keys ("KEY_" left out), "+ pad"
/// if it has pad inputs too (the desktop's; they stay as they are).
noinline fn controlsRow(i: usize) void {
    const ks = keymapSystem() orelse return;
    const line = m.ctl.line(i);
    const name = keymap.buttons(ks)[i].name;
    var n: usize = 0;
    const put = struct {
        fn f(l: *[menu.cols]u8, at: *usize, s: []const u8) void {
            const k = @min(s.len, l.len - at.*);
            @memcpy(l[at.*..][0..k], s[0..k]);
            at.* += k;
        }
    }.f;
    put(line, &n, " ");
    put(line, &n, name);
    while (n < 12 and n < line.len) : (n += 1) line[n] = ' ';
    var pads = false;
    var keys: usize = 0;
    for (key_config.map(ks).bind[i].slice()) |in| {
        if (keymap.isPad(in)) {
            pads = true;
            continue;
        }
        var nb: [32]u8 = undefined;
        const t = keymap.formatInput(in, &nb);
        if (keys > 0) put(line, &n, " ");
        put(line, &n, if (std.mem.startsWith(u8, t, "KEY_")) t[4..] else t);
        keys += 1;
    }
    if (keys == 0) put(line, &n, "(none)");
    if (pads) put(line, &n, " + pad");
    m.ctl.len[i] = @intCast(n);
}

/// Button i's keys changed: its row (and every row: a key moved off
/// another button), the USB keyboard's map.
noinline fn keysChanged() void {
    for (0..m.ctl.n) |j| controlsRow(j);
    m.ctl.changed = true;
    applyKeymap();
}

/// The key pressed while the Controls page waits for one: the menu key
/// cancels, any other becomes the button's keyboard key.
noinline fn controlsKey(in: keymap.Input) void {
    const ks = keymapSystem() orelse return;
    const i = m.ctl.row;
    if (key_config.menu.has(in)) {
        m.keyTaken(false);
    } else if (keymap.setKey(key_config.map(ks), i, in)) {
        keysChanged();
        m.keyTaken(true);
    } else {
        m.keyTaken(false);
        m.message("No room: the button has 8 pad inputs.");
    }
    // the key is still down: the menu takes nothing until it's let go
    usb_wait_release = true;
    show(false);
}

/// The whole key map to `/keymap.conf`, marked as the board's (make sd and
/// sd-sync copy it to the desktop's file); made if the card has none.
noinline fn saveKeymap() void {
    if (!mounted) {
        m.message("Not saved: no card.");
        return;
    }
    var w: std.Io.Writer = .fixed(&keymap_buf);
    keymap.writeBoard(&w, &key_config) catch {
        m.message("Not saved: the key map is over 4 KiB.");
        return;
    };
    const e = (fs.find(fs.root(), "keymap.conf") catch null) orelse
        (fs.create(fs.root(), "keymap.conf", 0) catch |err| return keymapNotSaved(err));
    _ = fs.rewrite(e, w.buffered()) catch |err| return keymapNotSaved(err);
    m.message("Saved to keymap.conf.");
    hw.puts("keymap: /keymap.conf written from the Controls page\r\n");
}

noinline fn keymapNotSaved(err: anyerror) void {
    var msg: [menu.cols]u8 = undefined;
    m.message(std.fmt.bufPrint(&msg, "Not saved: {s}", .{@errorName(err)}) catch "Not saved.");
}

/// Pause the game, (re)read the card, show the menu.
fn openMenu() void {
    splash_wait = null;
    frame_paused = false;
    menu_open = true;
    usb_hold = true;
    pushKeys();
    m.message("");
    // the game's save first, paused, to the card as it is mounted now
    if (save_file != null) {
        link.control(.{ .pause = true });
        flushSave();
    }
    mountAndList();
    if (movies_built) movieStatus();
    show(true);
    link.control(.{ .overlay = true, .pause = true });
}

/// Hide the menu; the game stays paused until the buttons are released
/// (pollButtons), except when `now` (a ROM loaded from the desktop).
fn closeMenu(now: bool) void {
    menu_open = false;
    resume_pending = !now;
    // the Options page's values (side track L), before the game goes on;
    // a new volume to the card
    link.settings(m.opts);
    if (mounted and settings_kept != null and settings_kept.? != m.opts.volume) saveSettings();
    link.control(.{ .pause = !now });
}

/// The card is read again every time the menu opens, so cards can be
/// swapped while a game runs.
fn mountAndList() void {
    const s = system.?;
    mounted = false;
    systems_dir = null;
    m.beginRoms();
    m.setPath("/");
    hw.puts("menu: SD card init\r\n");
    card = Card.init(&bus) catch |e| {
        m.fail(switch (e) {
            error.NoCard => "No SD card.",
            error.Timeout => "SD card: no answer (timeout).",
            error.Unsupported => "SD card: unsupported card.",
            else => "SD card: error.",
        });
        report("menu: SD init failed: ", @errorName(e));
        return;
    };
    hw.puts("menu: SD card ready, type ");
    hw.putc('0' + @as(u8, @intFromEnum(card.card)));
    hw.puts(", mounting\r\n");
    fs.mountInto(&card) catch |e| {
        m.fail("No FAT file system on the card.");
        report("menu: mount failed: ", @errorName(e));
        return;
    };
    mounted = true;
    resolveSave();
    systems_dir = findSystems();
    loadKeymap();
    loadSettings();
    if (m.states_ok) refreshSlots();
    // the system's directory, if there is one, else the root
    dirs[0] = fs.root();
    const sub = fs.find(dirs[0], s.dir) catch null;
    if (sub) |e| if (e.is_dir) {
        dirs[0] = fs.openDir(e);
        var path: [64]u8 = undefined;
        path[0] = '/';
        @memcpy(path[1..][0..e.name().len], e.name());
        m.setPath(path[0 .. 1 + e.name().len]);
    };
    if (m.path_len == 1) m.message("(no system folder: listing /)");
    // (timed: M1's gate compares the listing before and after the caches)
    const t0 = hw.now();
    list(0);
    hw.puts("menu: card mounted, ");
    hw.putDec(@intCast(m.n));
    hw.puts(" entries listed in ");
    hw.putDec(@intCast((hw.now() - t0) / hw.ms(1)));
    hw.puts(" ms\r\n");
}

fn list(cursor: usize) void {
    m.begin();
    var it = fs.iterate(dirs[m.depth]);
    var k: u32 = 0;
    while (true) {
        const e = (it.next() catch |err| {
            m.fail("SD card: read error.");
            report("menu: listing failed: ", @errorName(err));
            return;
        }) orelse break;
        if (k == fents.len) {
            m.truncated = true;
            break;
        }
        fents[k] = e;
        if (!m.add(e.name(), e.is_dir, e.size, k)) break;
        // only entries the menu kept use up a slot
        if (m.n > 0 and m.entries[m.n - 1].ref == k) k += 1;
    }
    m.done(cursor);
    if (m.truncated) m.message("(list cut at 256 entries)");
}

/// Load entry i (load.zig): the system's checks, then the file streamed a
/// block at a time over the link (about 4 us per byte: 1 s for 256 KiB),
/// or for a system with its ROM in DDR (the Game Boy, side track I)
/// straight into DDR in multi-block reads. A read error part way leaves
/// the core held (no half-loaded ROM runs) and the menu open.
// noinline: the stack check follows it by name (tools/check-ps-stack.py); the
// movies' calls (side track O) once tipped it into menuEvent
noinline fn loadRom(i: usize) void {
    const s = system.?;
    const e = fents[m.entries[i].ref];
    var r = fs.reader(e);
    const head = switch (rom_load.prepare(s, e.size, &r) catch |err| return readFailed(err)) {
        .refused => |why| {
            m.message(why);
            return;
        },
        .head => |h| h,
    };
    // the save's size from the header (the reader reuses head's buffer)
    var save_bytes: u32 = if (s.save) |sv| sv.size(head) else 0;
    const clock = if (s.save) |sv| (if (sv.clock) |c| c(head) else false) else false;
    // the Game Boy Color: a DMG cartridge's colours (gbc_palette.zig), from
    // the header and the menu's choice
    const pal: ?[gbc_palette.bytes]u8 = if (s.palette) gbc_palette.paletteBytes(head, m.pal) else null;
    // the NES: what also goes over the link once the file is in DDR (its
    // header and CHR ROM, side track D), from the header
    const parts: ?[2]menu.Range = if (s.ddr_link) |f| f(head) else null;
    movieStop();
    // save states: which game this is, from the header (side track L)
    const st_rom = savestate.RomId.ofGb(head, e.size);
    const st_cram: u32 = if (s.state) |sp| sp.cram(head) else 0;
    m.message("Loading...");
    // a new game starts at 100 % (side track L; sent when the menu closes)
    m.newGame();
    show(false);
    // the old game's save was written back when the menu opened; from
    // here its RAM belongs to the new cartridge, even if the load fails
    save_file = null;
    save_pending = false;
    // a companion file (M6: the Master System's BIOS): into DDR first, if
    // the card has one; a bad one stops here, before the cartridge, with
    // the core held (load.loadCompanionHeld: the old game's save is no
    // longer written back; starting a game lets go)
    var companion_size: u32 = 0;
    m.states_ok = false;
    const n = if (s.ddr_addr) |_| blk: {
        switch (rom_load.loadCompanionHeld(s, &fs, &card, &rom_mem, &link) catch |err| return readFailed(err)) {
            .none => {},
            .loaded => |k| companion_size = k,
            .refused => |why| {
                m.message(why);
                show(false);
                return;
            },
        }
        const t0 = hw.now();
        var k = rom_load.streamDdr(s, &fs, &card, e, &rom_mem) catch |err| return readFailed(err);
        asm volatile ("dsb" ::: .{ .memory = true });
        k = ddrImage(s, k);
        hw.puts("menu: into DDR in ");
        hw.putDec(@intCast((hw.now() - t0) / hw.ms(1)));
        hw.puts(" ms\r\n");
        if (parts) |p| {
            const file: []const u8 = @as([*]const u8, @ptrFromInt(s.ddr_addr.?))[0..k];
            const sent = rom_load.sendFromDdr(s, p, file, &link);
            hw.puts("menu: ");
            hw.putDec(sent);
            hw.puts(" bytes over the link\r\n");
        }
        break :blk k;
    } else rom_load.streamHeld(s, head, &r, &link) catch |err| return readFailed(err);
    // the whole ROM in DDR (M7's request): its own check, still held; and
    // the save's size from it
    if (s.ddr_addr) |base| {
        const file: []const u8 = @as([*]const u8, @ptrFromInt(base))[0..n];
        if (s.check_ddr) |check| if (check(file)) |why| {
            m.message(why);
            show(false);
            return;
        };
        if (s.save) |sv| if (sv.size_of_rom) |size| {
            save_bytes = size(file);
        };
    }
    const note = startSave(s, e.name(), save_bytes, clock);
    // still held: the core takes the palette when it starts
    if (pal) |p| link.write(gbc_palette.load_addr, &p);
    // and the facts from the whole ROM (the Game Gear's mapper and size)
    if (s.ddr_info) |di| if (s.ddr_addr) |base| {
        const w0 = di.of(@as([*]const u8, @ptrFromInt(base))[0..n]);
        const w = if (s.companion) |c| c.word(w0, companion_size) else w0;
        link.write(di.addr, &w);
    };
    link.setHold(false);
    game_save_bytes = save_bytes;
    if (s.state != null or s.movies) rememberGame(e.name(), st_rom, st_cram);
    hw.puts("menu: loaded ");
    hw.puts(e.name());
    hw.puts(" (");
    hw.putDec(n);
    hw.puts(" bytes)\r\n");
    if (note) |why| {
        // something about the save to read first: the game waits paused
        // in the open menu (L plays it)
        m.message(why);
        return;
    }
    m.message(e.name());
    closeMenu(false);
}

/// A system's `ddr_image` (side track Q, side K agreed 2026-10-26: the
/// 800XL's tapes): the file just put at `ddr_addr` copied above it
/// (`menu.ddr_image_scratch`), its image written at `ddr_addr`; the size
/// the system then has there (the file's, without one).
fn ddrImage(s: *const menu.System, size: u32) u32 {
    if (s.ddr_image == null) return size;
    const base = s.ddr_addr.?;
    const media: []u8 = @as([*]u8, @ptrFromInt(base))[0 .. menu.media_end - base];
    const n = rom_load.ddrImageIn(s.ddr_image, media, size, menu.ddr_image_scratch - base);
    asm volatile ("dsb" ::: .{ .memory = true });
    if (n != size) {
        hw.puts("menu: its DDR image, ");
        hw.putDec(n);
        hw.puts(" bytes\r\n");
    }
    return n;
}

/// DDR as load.streamDdr writes it: with the MMU off every access is
/// strongly ordered and uncached (as `Ddr` above), so the PL's HP port
/// reads what was written.
const RomMem = struct {
    pub fn write(_: *RomMem, addr: u32, bytes: []const u8) void {
        for (bytes, 0..) |b, i| @as(*volatile u8, @ptrFromInt(addr + i)).* = b;
    }
};
var rom_mem: RomMem = .{};

// --- Input movies (side track O, O.2b; movie.zig) ----------------------------------------
// One slot per game, `/movies/<path>/<name>.mv1`, made when it first
// records (side track K, F1; `make sd` puts back what the card had). A
// movie starts at power-on: the core held, its cartridge RAM cleared,
// let go with the movie register's sync bit set, so the core sees one
// input word a frame (Platform.Ps.FrameInput). The game's battery save
// is written to the card first, and not again until the game is loaded
// again: a movie's game never overwrites the player's save. Left out of a
// PS app built with `-Dmovies=false` (MOVIES=0).
const movies_built = build_info.movies;
/// in DDR (NOLOAD, set in main): its two blocks didn't fit the OCM
var mv: movie.Engine linksection(".ddr") = undefined;
/// the keyboard's and the desktop's word while a movie records
var mv_live: u16 = 0;
var mv_started_ms: u64 = 0;
/// the game's battery save size (save.clear at a power-on start)
var game_save_bytes: u32 = 0;
/// the slot number (one slot a game for now)
const movie_slot = 1;

/// The game's movie slot, null with the reason in the menu's message;
/// `make`: made if it isn't there (recording into it; side track K, F1:
/// `movie_frames_made` frames).
noinline fn movieFile(make: bool) ?fat.Entry {
    var buf: [save.max_path]u8 = undefined;
    const path = movie.moviePath(&buf, state_dir[0..state_dir_len], state_name[0..state_name_len], movie_slot) orelse {
        m.message("The movie slot's path is too long.");
        return null;
    };
    const f = save.find(&fs, path) catch |err| {
        report("movies: lookup failed: ", @errorName(err));
        m.message("The card could not be read.");
        return null;
    };
    if (f) |e| return e;
    if (!make) {
        m.message("The movie slot is empty.");
        return null;
    }
    const made = save.makeFile(&fs, path, movie.fileSize(savestate.movieArea(system.?.state, state_cram), movie_frames_made)) catch |err| {
        fs.buf_lba = null;
        report("movies: making the slot failed: ", @errorName(err));
        m.message(if (err == error.CardFull) "The card is full." else "Couldn't make the movie's file.");
        return null;
    };
    fs.buf_lba = null;
    return made;
}

/// A movie slot the board makes holds 30 minutes (make sd's MOVIE_MINUTES
/// default; one made by make sd keeps its own size).
const movie_frames_made: u32 = 30 * 60 * 60;

fn movieWordsAt() u32 {
    return movie.wordsOffset(savestate.movieArea(system.?.state, state_cram));
}

/// Whether the game has a movie slot, and what it holds.
noinline fn refreshMovie() void {
    // a game with movies has a slot, made when it first records (F1)
    m.movies_ok = true;
    m.movie_frames = 0;
    const e = movieFile(false) orelse {
        m.message("");
        return;
    };
    m.message("");
    if (e.size <= movieWordsAt()) return;
    defer fs.buf_lba = null;
    card.readBlock(fs.firstBlock(e) catch return, &sd_buf) catch return;
    const h = movie.Header.decode(sd_buf[0..movie.header_bytes]);
    m.movie_frames = if (movie.check(h, system.?.id, &state_rom.title, state_rom.sum, state_rom.size) == null) h.?.frames else 0;
}

/// Record into, or play from, the game's slot (the menu open, the game
/// paused): the game starts again from power-on, and goes on when the
/// menu closes.
noinline fn startMovie(mode: movie.Mode) void {
    const s = system.?;
    const e = movieFile(mode == .record) orelse return;
    var blocks: movie.Blocks = .{};
    {
        defer fs.buf_lba = null;
        var runs = fs.runs(e, 0xFFFF);
        while (runs.next() catch |err| return movieFailed(err)) |r|
            blocks.add(r.lba, r.blocks) catch return m.message("The movie slot is in too many pieces (make sd).");
    }
    const words_at = movieWordsAt();
    var h: movie.Header = .{ .system = s.id, .frames = 0, .words_at = words_at, .title = state_rom.title, .sum = state_rom.sum, .rom_size = state_rom.size };
    if (mode == .play) {
        card.readBlock(blocks.at(0) catch unreachable, &sd_buf) catch |err| return movieFailed(err);
        const got = movie.Header.decode(sd_buf[0..movie.header_bytes]);
        if (movie.check(got, s.id, &state_rom.title, state_rom.sum, state_rom.size)) |why| return m.message(movie.refusalText(why));
        h = got.?;
    }
    // a true power-on needs the core's memories cleared (they keep their
    // bytes over a reset): through the save states' window, frozen. A
    // bitstream without it can't give a movie the same start twice
    const spec = s.state orelse return m.message("Movies need save states here (Arty).");
    const sv = s.save orelse return m.message("Movies need save states here (Arty).");
    _ = savestate.freeze(&link, spec, sv) catch {
        savestate.thaw(&link, spec, sv);
        return m.message("This bitstream has no save states: no movies.");
    };
    // the save as it is now, then none while this game runs
    flushSave();
    save_file = null;
    save_pending = false;
    link.movieSync(true);
    link.setHold(true);
    savestate.clearSections(&link, spec);
    if (game_save_bytes > 0) save.clear(&link, sv, game_save_bytes, save_clock);
    savestate.thaw(&link, spec, sv);
    const toggle = link.inputs().frame();
    const ok = if (mode == .record)
        mv.startRecord(&card, blocks, e.size, h, 0, toggle)
    else
        mv.startPlay(&card, blocks, h, toggle);
    fs.buf_lba = null;
    ok catch |err| {
        mv.mode = .off;
        link.movieSync(false);
        link.setHold(false);
        return movieFailed(err);
    };
    mv_live = 0;
    // held, the latch follows the word: frame 0 sees the movie's first
    link.setUser(mv.sync.applied);
    link.setHold(false);
    mv_started_ms = nowMs();
    m.movie = if (mode == .record) .recording else .playing;
    hw.puts(if (mode == .record) "movies: recording\r\n" else "movies: playing\r\n");
    closeMenu(false);
}

/// Each time round the main loop while a movie runs (the menu closed).
fn moviePoll() void {
    const st = mv.poll(&card, link.inputs().frame(), mv_live) catch |err| return movieFailed(err);
    switch (st) {
        .none => if (mv.frame == 0 and nowMs() - mv_started_ms > 1000) {
            // no frame toggle: a bitstream without the latch (MOVIES=0)
            movieStop();
            hw.puts("movies: no frame toggle from the PL\r\n");
            movieNote("This bitstream has no movies (MOVIES=0?).");
        },
        .word => |w| link.setUser(w),
        .ended => |why| {
            const frames = mv.frame;
            movieStop();
            hw.puts("movies: ended after ");
            hw.putDec(frames);
            hw.puts(" frames\r\n");
            var msg: [menu.cols]u8 = undefined;
            movieNote(switch (why) {
                .done => std.fmt.bufPrint(&msg, "The movie ended ({d} frames).", .{frames}) catch "The movie ended.",
                .full => std.fmt.bufPrint(&msg, "Slot full: recording saved, {d} frames.", .{frames}) catch "Slot full: recording saved.",
            });
        },
    }
}

/// Stop the movie running, if any: a recording's end and header to the
/// card; the input live again.
fn movieStop() void {
    // (a movie that ended by itself has its engine off already)
    if (!movies_built or (mv.mode == .off and m.movie == .off)) return;
    mv.stop(&card) catch |err| report("movies: stop failed: ", @errorName(err));
    fs.buf_lba = null;
    link.movieSync(false);
    m.movie = .off;
    m.movie_frames = null;
    pushKeys();
}

/// A note for the next time the menu opens (a movie that ended or failed
/// while the game ran: the message line is cleared when the menu opens).
var movie_note_buf: [menu.cols]u8 = undefined;
var movie_note_len: usize = 0;

fn movieNote(text: []const u8) void {
    movie_note_len = @min(text.len, movie_note_buf.len);
    @memcpy(movie_note_buf[0..movie_note_len], text[0..movie_note_len]);
    m.message(text);
}

/// The menu just opened: the movie running, or the note left by one.
fn movieStatus() void {
    var msg: [menu.cols]u8 = undefined;
    switch (mv.mode) {
        .record => m.message(std.fmt.bufPrint(&msg, "Recording: {d} frames. Options: Stop.", .{mv.frame}) catch "Recording."),
        .play => m.message(std.fmt.bufPrint(&msg, "Playing: frame {d} of {d}.", .{ mv.frame, mv.limit }) catch "Playing."),
        .off => if (movie_note_len > 0) {
            m.message(movie_note_buf[0..movie_note_len]);
            movie_note_len = 0;
        },
    }
}

fn movieFailed(err: anyerror) void {
    report("movies: ", @errorName(err));
    if (mv.mode != .off) {
        mv.mode = .off;
        link.movieSync(false);
        m.movie = .off;
        pushKeys();
    }
    movieNote("The movie slot could not be used.");
}

// --- Frame advance (side track O, O.3; Platform.Ps.FrameInput.frameAdvance) -------
// The first press of `[global] frame_advance` (F11) pauses the game, with
// no overlay; each press after runs it to its next frame start (the
// control register's step bit; the keys held reach the core in that frame,
// and a movie that records takes them); `frame_run` (F10) lets it run on.
// The desktop's F1 / F0 do the same. Opening the menu ends it.
var frame_paused = false;
var frame_keys_prev: u3 = 0;

fn frameStep() void {
    if (system == null or menu_open or resume_pending or splash_wait != null) return;
    if (!frame_paused) {
        frame_paused = true;
        link.control(.{ .pause = true });
        hw.puts("frames: paused\r\n");
    } else link.control(.{ .pause = true, .step = true });
}

fn frameRun() void {
    if (!frame_paused) return;
    frame_paused = false;
    link.control(.{});
    hw.puts("frames: running\r\n");
}

/// The USB keyboard's frame advance and screenshot keys, on their press.
fn hotKeys() void {
    const now: u3 = @as(u3, @intFromBool(usb_kb.holds(&key_config.frame_advance))) |
        @as(u3, @intFromBool(usb_kb.holds(&key_config.frame_run))) << 1 |
        @as(u3, @intFromBool(usb_kb.holds(&key_config.screenshot))) << 2;
    const pressed = now & ~frame_keys_prev;
    frame_keys_prev = now;
    if (movies_built and pressed & 1 != 0) frameStep();
    if (movies_built and pressed & 2 != 0) frameRun();
    if (shots_built and pressed & 4 != 0 and !menu_open) {
        if (takeScreenshot()) |why| {
            hw.puts("screens: ");
            hw.puts(why);
        } else {
            hw.puts("screens: saved ");
            hw.puts(shot_path_buf[0..shot_path_len]);
        }
        hw.puts("\r\n");
    }
}

// --- Screenshots (side track O, O.4; Platform.Video.Capture, png.zig) -------
// The PL writes the running game's next frame into DDR from capture_base
// (R G B 0 a pixel), then a trailer word with the sequence number asked
// for; the PS makes a PNG of it in /screens/<system>/<game> NNN.png (a
// file made at its size, fat.zig's create, then its blocks filled). The
// core runs while it is taken: a paused game (the menu open) for a frame
// or two, one paused by frame advance one step. Left out with
// `-Dscreenshots=false` (SCREENSHOTS=0).
const shots_built = build_info.screenshots;
var shot_seq: u8 = 0;
var shot_path_buf: [save.max_path]u8 = undefined;
var shot_path_len: usize = 0;
const capture_base: usize = 0x1000_0000;

/// The picture in DDR, as png.zig reads it.
const DdrPicture = struct {
    pub fn rgb(_: DdrPicture, i: u32) [3]u8 {
        const v = @as(*const volatile u32, @ptrFromInt(capture_base + 4 * @as(usize, i))).*;
        return .{ @truncate(v), @truncate(v >> 8), @truncate(v >> 16) };
    }
};

/// A screenshot of the running game into the card: null, its path in
/// shot_path_buf; or why not.
noinline fn takeScreenshot() ?[]const u8 {
    const s = system orelse return "No system is running.";
    const sc = s.screen orelse return "This system has no screenshots.";
    if (!m.shots_ok) return "No game from the card is running.";
    if (!mounted) return "No SD card.";
    shot_seq = if (shot_seq == 255) 1 else shot_seq + 1;
    const tw: usize = (4 * @as(usize, sc.w) * sc.h + 7) & ~@as(usize, 7);
    const trailer: *const volatile u64 = @ptrFromInt(capture_base + tw);
    link.write(pl.capture_addr, &.{shot_seq});
    // the core runs while its frame is taken
    const paused = menu_open or resume_pending;
    if (frame_paused) {
        link.control(.{ .pause = true, .step = true });
    } else if (paused) link.control(.{ .overlay = menu_open, .pause = false });
    const t0 = nowMs();
    var t: u64 = trailer.*;
    while (@as(u8, @truncate(t)) != shot_seq and nowMs() - t0 < 500) t = trailer.*;
    if (paused and !frame_paused) link.control(.{ .overlay = menu_open, .pause = true });
    if (@as(u8, @truncate(t)) != shot_seq) return "No picture came (is the game drawing?).";
    if ((t >> 8) & 1 != 0 or @as(u32, @truncate(t >> 40)) != @as(u32, sc.w) * sc.h) return "The picture came cut short.";
    // the first free name
    const name = state_name[0..state_name_len];
    const stem = name[0 .. std.mem.lastIndexOfScalar(u8, name, '.') orelse name.len];
    var n: u16 = 1;
    const path = while (n < 1000) : (n += 1) {
        const p = std.fmt.bufPrint(&shot_path_buf, "/screens/{s}/{s} {d:0>3}.png", .{ s.dir, stem, n }) catch return "The game's name is too long.";
        const found = save.find(&fs, p) catch {
            fs.buf_lba = null;
            return "The card could not be read.";
        };
        if (found == null) break p;
    } else return "999 screenshots of this game: no more.";
    defer fs.buf_lba = null;
    const e = save.makeFile(&fs, path, png.size(sc.w, sc.h)) catch |err| {
        report("screens: making the file failed: ", @errorName(err));
        return if (err == error.CardFull) "The card is full." else "Couldn't make the file.";
    };
    // its blocks, in order, as png.zig makes them
    var wr = png.Writer(DdrPicture).init(.{}, sc.w, sc.h);
    var runs = fs.runs(e, 0xFFFF);
    while (runs.next() catch return "The card could not be read.") |r| {
        var k: u32 = 0;
        while (k < r.blocks) : (k += 1) {
            const got = wr.next(&sd_buf);
            @memset(sd_buf[got..], 0);
            card.writeBlock(r.lba + k, &sd_buf) catch return "Writing the card failed.";
        }
    }
    shot_path_len = path.len;
    return null;
}

// --- Battery saves (side track I, save.zig) --------------------------------------
/// After a cartridge is in (the core still held): its save file into
/// cartridge RAM, or zeros if it has none. A note for the menu if the
/// game won't be saved although it has a battery.
// noinline: inlined into menuEvent's frame (side track J's splash moved
// it), save.load's error code came back at an odd stack offset and LLVM
// read it with ldrh: an alignment fault with the MMU off
// (tools/check-ps-align.sh caught it)
noinline fn startSave(s: *const menu.System, name: []const u8, size: u32, clock: bool) ?[]const u8 {
    save_file = null;
    save_pending = false;
    save_clock = clock;
    const sv = s.save orelse return null;
    if (size == 0) return null;
    const path = save.savePath(&save_path_buf, m.path(), name) orelse {
        save.clear(&link, sv, size, clock);
        return "Folder names too long: no save.";
    };
    save_path_len = path.len;
    const found = save.lookup(&fs, path, size) catch |err| {
        report("save: lookup failed: ", @errorName(err));
        save.clear(&link, sv, size, clock);
        return "SD card: read error; no save.";
    };
    switch (found) {
        .file => |f| {
            _ = save.load(&fs, f, &link, sv, clock) catch |err| {
                report("save: read failed: ", @errorName(err));
                save.clear(&link, sv, size, clock);
                return "SD card: read error; no save.";
            };
            save_file = f;
            hw.puts("save: ");
            hw.puts(path);
            hw.puts(" loaded\r\n");
            return null;
        },
        .missing => {
            // made now (side track K, F1): zeros, as the RAM is cleared
            save.clear(&link, sv, size, clock);
            const f = save.makeFile(&fs, path, size) catch |err| {
                fs.buf_lba = null;
                report("save: making it failed: ", @errorName(err));
                return if (err == error.CardFull) "The card is full: no save. L to play." else "Couldn't make the save file. L to play.";
            };
            fs.buf_lba = null;
            save_file = f;
            hw.puts("save: ");
            hw.puts(path);
            hw.puts(" made\r\n");
            return null;
        },
        .wrong_size => {
            save.clear(&link, sv, size, clock);
            hw.puts("save: ");
            hw.puts(path);
            hw.puts(" has the wrong size, not used\r\n");
            return "Save file has the wrong size. L to play.";
        },
    }
}

/// Every millisecond while a game with a save runs: the dirty flag
/// (PS_IN bit 6) cleared as soon as it is seen, and the RAM written back
/// once it has been quiet for save_quiet_ms.
fn pollSave(t: u64) void {
    if (save_file == null or menu_open) return;
    if (link.inputs().flags & 1 != 0) {
        link.write(system.?.save.?.clear_addr, &.{0});
        save_pending = true;
        save_last_ms = t;
        return;
    }
    if (save_pending and t - save_last_ms >= save_quiet_ms) flushSave();
}

/// Cartridge RAM into the save file now, if the game wrote it since the
/// last time (pending, or the dirty flag still set), or always for a
/// cartridge with a clock (its time moved on: only the footer's block is
/// written then).
fn flushSave() void {
    const f = save_file orelse return;
    const sv = (system orelse return).save orelse return;
    if (!save_pending and link.inputs().flags & 1 == 0 and !save_clock) return;
    save_pending = false;
    if (!mounted) return report("save: ", "no card mounted, not saved");
    const t0 = hw.now();
    const w = save.writeBack(&fs, &card, f, &link, sv, save_clock, &sd_buf, &sd_check) catch |err| {
        // the serial port's receiver: the read-back flipped the toggle
        byte_in.toggle = link.inputs().toggle;
        m.message("Saving failed (SD card).");
        return report("save: write-back failed: ", @errorName(err));
    };
    byte_in.toggle = link.inputs().toggle;
    hw.puts("save: ");
    hw.puts(save_path_buf[0..save_path_len]);
    hw.puts(": ");
    hw.putDec(w.written);
    hw.puts(" of ");
    hw.putDec(w.blocks);
    hw.puts(" blocks written, ");
    hw.putDec(@intCast((hw.now() - t0) / hw.ms(1)));
    hw.puts(" ms\r\n");
}

/// After a new mount: the save file found again by its path, the same
/// file as before (else the card was swapped: no save any more).
fn resolveSave() void {
    const old = save_file orelse return;
    const found = save.lookup(&fs, save_path_buf[0..save_path_len], old.size) catch .missing;
    switch (found) {
        .file => |f| if (f.first_cluster == old.first_cluster) {
            save_file = f;
            return;
        },
        else => {},
    }
    save_file = null;
    m.message("Another card: this game won't be saved.");
    hw.puts("save: the save file is not on this card any more\r\n");
}

/// R: block LBA to the desktop (make saves-pull): "r<lba><sum>", the 512
/// bytes. A save not written back yet goes first, so the pull gets it.
fn sdRead(lba: u32) void {
    flushSave();
    if (!mounted and !sd_writable and !sd_read_ready) {
        card = Card.init(&bus) catch |e| return sdFail(lba, @errorName(e));
        sd_read_ready = true;
    }
    card.readBlock(lba, &sd_buf) catch |e| return sdFail(lba, @errorName(e));
    var sum: u16 = 0;
    for (sd_buf) |b| sum = protocol.checksum(sum, b);
    hw.puts("r");
    hw.puts(&hw.hex8(lba));
    hw.puts(&hw.hex4(sum));
    hw.puts("\r\n");
    for (sd_buf) |b| hw.putc(b);
}

/// `/keymap.conf` from the card (the desktop's format: `make keymap`
/// writes it), over the defaults; without one, the defaults. A bad file is
/// shown on the menu's message row and the defaults stay.
/// `/settings.txt` (side track L, options.zig): the volume kept on the
/// card, read each time the card is mounted. Without the file (or with a
/// bad one) the volume stays as it is and nothing is written.
fn loadSettings() void {
    settings_kept = null;
    const e = (fs.find(fs.root(), options.file) catch null) orelse return;
    if (e.is_dir) return;
    var text: [options.file_size]u8 = undefined;
    const n = fs.readFile(e, &text) catch |err| return report("settings: read failed: ", @errorName(err));
    const v = options.parseVolume(text[0..n]) orelse {
        hw.puts("settings: /settings.txt has no volume, not kept\r\n");
        return;
    };
    m.opts.volume = v;
    settings_kept = v;
}

/// The volume into `/settings.txt`, its one block rewritten in place (as
/// last.txt: rememberLast).
fn saveSettings() void {
    const e = (fs.find(fs.root(), options.file) catch null) orelse return;
    const lba = fs.firstBlock(e) catch return;
    card.readBlock(lba, &sd_buf) catch |err| return report("settings: ", @errorName(err));
    if (!options.writeFile(&sd_buf, e.size, m.opts)) return report("settings: ", "file too small");
    card.writeBlock(lba, &sd_buf) catch |err| return report("settings: ", @errorName(err));
    fs.buf_lba = null; // the reader's cached block may be this one
    settings_kept = m.opts.volume;
    hw.puts("settings: volume ");
    hw.putDec(m.opts.volume);
    hw.puts(" kept\r\n");
}

/// Save states (side track L, L.7; savestate.zig). All `noinline`: inlined,
/// their buffers sat in mountAndList's and menuEvent's frames all through
/// the card's listing (tools/check-ps-stack.py, 2026-10-11).
/// The game just loaded
/// from the card: its slots' folder and name, and what it is.
noinline fn rememberGame(name: []const u8, rom: savestate.RomId, cram: u32) void {
    const p = m.path();
    state_dir_len = @min(p.len, state_dir.len);
    @memcpy(state_dir[0..state_dir_len], p[0..state_dir_len]);
    state_name_len = @min(name.len, state_name.len);
    @memcpy(state_name[0..state_name_len], name[0..state_name_len]);
    state_rom = rom;
    state_cram = cram;
    m.states_ok = system.?.state != null;
    m.shots_ok = shots_built and system.?.screen != null;
    m.slot_used = [_]?bool{null} ** savestate.slots;
    if (mounted and m.states_ok) refreshSlots();
    if (movies_built and system.?.movies and mounted) refreshMovie();
}

/// Slot n's file (1 to 4), null with the reason in the menu's message;
/// `make`: made if it isn't there (saving into it; side track K, F1), else
/// a missing one is an empty slot.
noinline fn slotFile(n: u8, make: bool) ?fat.Entry {
    const spec = system.?.state.?;
    var buf: [save.max_path]u8 = undefined;
    const path = savestate.statePath(&buf, state_dir[0..state_dir_len], state_name[0..state_name_len], n) orelse {
        m.message("The slot's path is too long.");
        return null;
    };
    const found = save.lookup(&fs, path, savestate.fileSize(spec, state_cram)) catch |err| {
        report("states: lookup failed: ", @errorName(err));
        m.message("The card could not be read.");
        return null;
    };
    return switch (found) {
        .file => |f| f,
        .missing => blk: {
            if (!make) {
                m.message(savestate.refusalText(.empty));
                break :blk null;
            }
            const f = save.makeFile(&fs, path, savestate.fileSize(spec, state_cram)) catch |err| {
                fs.buf_lba = null;
                report("states: making the slot failed: ", @errorName(err));
                m.message(if (err == error.CardFull) "The card is full." else "Couldn't make the slot's file.");
                break :blk null;
            };
            fs.buf_lba = null;
            break :blk f;
        },
        .wrong_size => blk: {
            m.message(savestate.refusalText(.wrong_size));
            break :blk null;
        },
    };
}

/// Which slots hold a state of this game (their headers; the bitstream is
/// checked when one is loaded).
noinline fn refreshSlots() void {
    const s = system orelse return;
    for (1..savestate.slots + 1) |k| {
        const n: u8 = @intCast(k);
        const e = slotFile(n, false) orelse {
            // no file: an empty slot (made when a state is saved)
            m.slot_used[k - 1] = false;
            continue;
        };
        const h = savestate.readHeader(&fs, &card, e, &sd_buf) catch null;
        m.slot_used[k - 1] = if (h) |x| x.used and x.system == s.id and std.mem.eql(u8, &x.rom.title, &state_rom.title) and x.rom.sum == state_rom.sum else false;
    }
    m.message("");
}

/// The running game (paused, the menu open) into slot n.
noinline fn saveState(n: u8) void {
    const s = system.?;
    const spec = s.state orelse return;
    const sv = s.save orelse return;
    const e = slotFile(n, true) orelse return;
    const lay = savestate.freeze(&link, spec, sv) catch {
        savestate.thaw(&link, spec, sv);
        m.message("This bitstream has no save states.");
        return;
    };
    defer savestate.thaw(&link, spec, sv);
    const h: savestate.Header = .{ .system = s.id, .layout = lay, .cram = state_cram, .rom = state_rom, .used = true };
    const t0 = hw.now();
    savestate.save(&fs, &card, e, &link, spec, sv, lay, h, &sd_buf, &sd_check) catch |err| {
        report("states: save failed: ", @errorName(err));
        m.message("Saving the state failed.");
        return;
    };
    hw.puts("states: saved in ");
    hw.putDec(@intCast((hw.now() - t0) / hw.ms(1)));
    hw.puts(" ms\r\n");
    m.slot_used[n - 1] = true;
    var msg: [menu.cols]u8 = undefined;
    m.message(std.fmt.bufPrint(&msg, "Saved to slot {d}.", .{n}) catch "Saved.");
}

/// Slot n into the running game (paused, the menu open), if its header
/// says it is this game's, from this bitstream.
/// The table and the overlay of a load through the CPU (savestate.load).
var restore_buf: [savestate.gb.restore.?.work()]u8 linksection(".ddr") = undefined;

noinline fn loadState(n: u8) void {
    const s = system.?;
    const spec = s.state orelse return;
    const sv = s.save orelse return;
    const e = slotFile(n, false) orelse return;
    const h = savestate.readHeader(&fs, &card, e, &sd_buf) catch {
        m.message("The card could not be read.");
        return;
    };
    const lay = savestate.freeze(&link, spec, sv) catch {
        savestate.thaw(&link, spec, sv);
        m.message("This bitstream has no save states.");
        return;
    };
    defer savestate.thaw(&link, spec, sv);
    if (savestate.check(h, s.id, lay, state_rom, state_cram)) |why| {
        m.message(savestate.refusalText(why));
        return;
    }
    // loading through the CPU (L.6c): the routine built first (a slot it
    // can't restore leaves the game alone), then the core held (its
    // reset) while the memories and the routine go in, let go before the
    // thaw
    var end: u16 = 0;
    if (spec.restore != null) {
        end = savestate.prepare(&fs, e, spec, lay, &restore_buf) catch |err| {
            report("states: can't restore: ", @errorName(err));
            m.message("This state can't be loaded here.");
            return;
        };
    }
    const t0 = hw.now();
    const loaded = if (spec.restore != null)
        savestate.loadHeld(&fs, e, &link, spec, sv, lay, state_cram, &restore_buf, end)
    else
        savestate.load(&fs, e, &link, spec, sv, lay, state_cram, &restore_buf, end);
    loaded catch |err| {
        report("states: load failed: ", @errorName(err));
        if (spec.restore != null) {
            // the core stays held (savestate.loadHeld): half a state is no
            // game to go on with; starting one lets go
            m.states_ok = false;
            m.message("Loading failed: start the game again.");
        } else m.message("Loading the state failed.");
        return;
    };
    hw.puts("states: loaded in ");
    hw.putDec(@intCast((hw.now() - t0) / hw.ms(1)));
    hw.puts(" ms\r\n");
    var msg: [menu.cols]u8 = undefined;
    m.message(std.fmt.bufPrint(&msg, "Loaded slot {d}.", .{n}) catch "Loaded.");
}

fn loadKeymap() void {
    const e = (fs.find(fs.root(), "keymap.conf") catch null) orelse {
        key_config.setDefaults();
        applyKeymap();
        return;
    };
    if (e.is_dir or e.size > keymap_buf.len) {
        m.message("keymap.conf: not a file of at most 4 KiB");
        return;
    }
    const n = fs.readFile(e, &keymap_buf) catch |err| {
        report("keymap: read failed: ", @errorName(err));
        return;
    };
    var perr: keymap.ParseError = undefined;
    // straight into key_config, not through the stack (a Config is a map
    // per system: M6, side track K's choice); a bad file leaves the
    // defaults there (parsing an empty text)
    if (keymap.parseInto(keymap_buf[0..n], &perr, &key_config)) |_| {
        applyKeymap();
        hw.puts("keymap: /keymap.conf in use\r\n");
    } else |_| {
        var msg: [menu.cols]u8 = undefined;
        m.message(std.fmt.bufPrint(&msg, "keymap.conf line {d}: {s}", .{ perr.line, perr.msg }) catch "keymap.conf: error");
        hw.puts("keymap: /keymap.conf line ");
        hw.putDec(@intCast(perr.line));
        hw.puts(": ");
        hw.puts(perr.msg);
        hw.puts(" (defaults in use)\r\n");
        keymap.parseInto("", &perr, &key_config) catch {};
        applyKeymap();
    }
}

/// A card read failed while a game was loading. The core stays held, as
/// after a refused BIOS (load.loadCompanionHeld) or a failed state load
/// (savestate.loadHeld): what is in its memory may be half a game, so it
/// doesn't run on; starting a game lets go (side track K, 2026-10-14: the
/// review found this path; the menu loop shows the message).
fn readFailed(err: anyerror) void {
    m.message("SD card: read error.");
    report("menu: read failed: ", @errorName(err));
}

/// Write the screen to the overlay: every row (all) or the ones that changed.
var shown: menu.Screen = .{};
fn show(all: bool) void {
    m.render(&screen);
    for (screen.cells, 0..) |row, r| {
        if (!all and std.mem.eql(u8, &row, &shown.cells[r])) continue;
        link.write(pl.overlay_base + @as(u24, @intCast(r * menu.cols)), &row);
        shown.cells[r] = row;
    }
}

fn report(what: []const u8, err: []const u8) void {
    hw.puts(what);
    hw.puts(err);
    hw.puts("\r\n");
}

// the version mark (build.zig's -Dps-version, tools/ps-version.sh)
const build_info = @import("build_info");
const std = @import("std");

// --- Stack high-water mark ----------------------------------------------------
// The stack is 16 KiB (link.ld) in the 192 KiB of OCM, and an overflow
// would silently overwrite .bss (no MMU): _start paints it with
// stack_paint, and once a second the relay looks how far down the paint
// is gone. Each new KiB is reported over the UART ("board: relay: stack
// ..." in make keys), so board runs show the real depth.
const stack_paint: u32 = 0x5354414B;
extern var __stack_bottom: u32;
extern var __stack_top: u32;
var stack_reported: usize = 0;

fn stackUsed() usize {
    const bottom = @intFromPtr(&__stack_bottom);
    const words = (@intFromPtr(&__stack_top) - bottom) / 4;
    const p: [*]const volatile u32 = @ptrFromInt(bottom);
    var i: usize = 0;
    while (i < words and p[i] == stack_paint) : (i += 1) {}
    return (words - i) * 4;
}

fn checkStack() void {
    const used = stackUsed();
    if (used < stack_reported + 1024) return;
    stack_reported = used;
    hw.puts("relay: stack used ");
    hw.putDec(@intCast(used));
    hw.puts(" of ");
    hw.putDec(@intCast(@intFromPtr(&__stack_top) - @intFromPtr(&__stack_bottom)));
    hw.puts(" bytes\r\n");
}

// --- CPU exceptions -----------------------------------------------------------------
// Vectors at 0x0 (link.ld): anything but reset reports itself over the
// UART and halts, instead of the app just going silent (as the first board
// run did on an alignment fault). DFSR/DFAR (data abort) and IFSR/IFAR
// (prefetch abort) from CP15 (ARMv7-A ARM B4.1.51, B4.1.52).
export fn _vectors() linksection(".vectors") callconv(.naked) noreturn {
    asm volatile (
        \\ b _start
        \\ b 1f
        \\ b 2f
        \\ b 3f
        \\ b 4f
        \\ b .
        \\ b 5f
        \\ b 5f
        \\1: mov r0, #1
        \\ b 6f
        \\2: mov r0, #2
        \\ b 6f
        \\3: mov r0, #3
        \\ mrc p15, 0, r2, c5, c0, 1
        \\ mrc p15, 0, r3, c6, c0, 2
        \\ b 6f
        \\4: mov r0, #4
        \\ mrc p15, 0, r2, c5, c0, 0
        \\ mrc p15, 0, r3, c6, c0, 0
        \\ b 6f
        \\5: mov r0, #5
        \\6: ldr sp, =__fault_stack_top
        \\ mov r1, lr
        \\ b fault
    );
}

/// Repeated every second, so whoever opens the port later (make keys,
/// make sd, which show unknown lines as "board: ...") still sees it.
export fn fault(kind: u32, lr: u32, fsr: u32, far: u32) callconv(.c) noreturn {
    const names = [_][]const u8{ "?", "undefined instruction", "SVC", "prefetch abort", "data abort", "interrupt" };
    while (true) {
        hw.puts("\r\nrelay: CPU exception: ");
        hw.puts(names[if (kind < names.len) kind else 0]);
        hw.puts(", LR 0x");
        hw.puts(&hw.hex8(lr));
        if (kind == 3 or kind == 4) {
            hw.puts(", FSR 0x");
            hw.puts(&hw.hex8(fsr));
            hw.puts(", address 0x");
            hw.puts(&hw.hex8(far));
        }
        hw.puts("; halted (make prog to restart)\r\n");
        hw.delayTicks(hw.gtimer_hz);
    }
}

export fn _start() linksection(".text._start") callconv(.naked) noreturn {
    // Stack (painted with stack_paint, for stackUsed), FPU on (the
    // hard-float ABI lets the compiler use VFP/NEON, and they fault until
    // enabled: CPACR cp10/cp11 full access, FPEXC.EN), clear .bss, then
    // main.
    asm volatile (
        \\ ldr r0, =__stack_bottom
        \\ ldr r1, =__stack_top
        \\ ldr r2, =0x5354414B
        \\2:
        \\ cmp r0, r1
        \\ strlo r2, [r0], #4
        \\ blo 2b
        \\ ldr sp, =__stack_top
        \\ mov r0, #0
        \\ mcr p15, 0, r0, c12, c0, 0
        \\ mrc p15, 0, r0, c1, c0, 2
        \\ orr r0, r0, #(0xF << 20)
        \\ mcr p15, 0, r0, c1, c0, 2
        \\ isb
        \\ mov r0, #0x40000000
        \\ vmsr fpexc, r0
        \\ ldr r0, =__bss_start
        \\ ldr r1, =__bss_end
        \\ mov r2, #0
        \\1:
        \\ cmp r0, r1
        \\ strlo r2, [r0], #4
        \\ blo 1b
        \\ b main
    );
}
