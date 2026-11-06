#!/usr/bin/env python3
"""Integration test for the Zig board link (platform/host/link.zig), no
board needed:

  * a fake PS relay on a pseudo-terminal answers P, B, H, L and K exactly
    like platform/ps/relay.zig (and records when each K line arrives);
  * `link --load` must rebuild the right RAM image through it, at the
    negotiated 921600 baud;
  * a virtual gamepad (/dev/uinput), plugged in after link has started
    (so it must be found through hotplug), presses buttons and the D-pad; each
    change must arrive as the right K line, and the time from injecting the
    event to the K line arriving is the PC-side latency.

    tools/test_link.py [build/host/link]

  * key maps (side track B): the default file location, --keymap,
    --show-keymap, a bad file refused with its line number; `link --remap`
    driven through the same virtual pad (the controls through its stdin),
    and the file it writes then used by link.

Skips the gamepad parts if /dev/uinput isn't writable. Uses a gamepad, not a
keyboard, so nothing gets typed into the desktop's focused window (and
link grabs gamepads, so other programs don't see it either). Every link
run here takes only the virtual pad (--device), and a link without
--device ignores it, so a `make keys` running meanwhile doesn't take it.
Key maps come from an empty temporary XDG_CONFIG_HOME, never the user's.
"""

import fcntl
import os
import select
import struct
import subprocess
import sys
import tempfile
import termios
import threading
import shutil
import atexit
import time

LINK = sys.argv[1] if len(sys.argv) > 1 else "build/host/link"

# every link run finds its key map in an empty config directory (so the
# defaults), never in the user's ~/.config
CONFIG_DIR = tempfile.mkdtemp(prefix="test_link-")
os.environ["XDG_CONFIG_HOME"] = CONFIG_DIR
atexit.register(shutil.rmtree, CONFIG_DIR, True)

EV_SYN, EV_KEY, EV_ABS = 0, 1, 3
BTN_SOUTH, BTN_EAST = 0x130, 0x131
ABS_X, ABS_Y, ABS_HAT0X, ABS_HAT0Y = 0, 1, 0x10, 0x11


RATES = {0: termios.B115200, 1: termios.B921600, 2: termios.B460800, 3: termios.B230400,
         4: termios.B2000000}
SPEED = {0: 115200, 1: 921600, 2: 460800, 3: 230400, 4: 2000000}


def fnv1a64(data):
    """protocol.chunkHash: 64-bit FNV-1a."""
    h = 0xCBF29CE484222325
    for x in data:
        h = ((h ^ x) * 0x100000001B3) & 0xFFFFFFFFFFFFFFFF
    return h


class FakeRelay:
    """Behaves like platform/ps/relay.zig, including its baud rate: input
    only counts if the link's side of the pty is at the rate the relay is
    at (Linux shares one termios between the two ends; at another rate it
    arrives as garbage bytes >= 0x80), and above 115200
    the relay falls back to it after 0.6 s without valid input. Fault modes:
    lose_b (the "b<n>" reply is lost, as with the first relay build),
    no_fast (the bridge can't do more than 115200), max230400 and junk
    (a stray byte before the first command, side track F). The SD
    commands (make sd) write to `card`, 64 MiB of old data (0xA5); mode
    sd_garble corrupts the first block's bytes on the way (the relay
    answers "x... checksum", link must send it again). Side track G phase
    2: B4 (2 Mbaud) only gets through in mode fast2m (the Arty's FTDI
    bridge; every other mode is a bridge that stops at 921600 or lower);
    C (a chunk's hash) and M (a multi-block write) as relay.zig, except in
    mode old_relay, which ignores them as a relay from before them does.
    m_writes / z_writes / d_writes count the write commands."""

    def __init__(self, mode="ok"):
        self.master, self.slave = os.openpty()
        self.mode = mode
        self.ram = bytearray(0x1000)
        self.card = bytearray(b"\xa5") * (64 << 20)
        self.sd_open = False
        self.garbled = False
        self.m_writes = self.z_writes = self.d_writes = 0
        self.cart = bytearray(0x70000)   # cartridges (wide L lines): NES
        self.ddr = bytearray(8 << 20)    # DDR from 0x02000000 (W): Game Boy cartridges
        self.ddr_writes_held = True      # every W came while held (H1)
        self.ddr_links_held = True       # every Y came while held (side track D)
        self.held = False
        self.keys = []          # (monotonic ns, value)
        self.rate = 0
        self.last_valid = time.monotonic()
        self.lock = threading.Lock()
        threading.Thread(target=self.run, daemon=True).start()

    @property
    def path(self):
        return os.ttyname(self.slave)

    def bridge_ok(self):
        limit = {"no_fast": 115200, "max230400": 230400, "fast2m": 2000000}.get(self.mode, 921600)
        return SPEED[self.rate] <= limit

    def in_sync(self):
        return self.bridge_ok() and termios.tcgetattr(self.master)[4] == RATES[self.rate]

    def readn(self, n):
        data = b""
        while len(data) < n:
            data += os.read(self.master, n - len(data))
        return data

    def send(self, b):
        if self.in_sync():
            os.write(self.master, b)

    def valid(self):
        self.last_valid = time.monotonic()

    def run(self):
        # mode junk (side track F): one stray byte >= 0x80 before anything,
        # as the J14 bridge sends when it powers up with the board (SD boot);
        # it opens a key frame that would swallow the first "P\n"
        buf = b"\xfe" if self.mode == "junk" else b""
        try:
            while True:
                r, _, _ = select.select([self.master], [], [], 0.05)
                if self.rate and time.monotonic() - self.last_valid > 0.6:
                    self.rate = 0
                if not r:
                    continue
                data = os.read(self.master, 4096)
                if not self.bridge_ok():
                    continue            # the bridge can't send at this rate
                if not self.in_sync():
                    # at the wrong rate: garbage, in the worst case bytes
                    # >= 0x80 that open key frames (as on the board)
                    buf += b"\xfe" * len(data)
                    continue
                buf += data
                while buf:
                    if buf[0] >= 0x80:  # compact key frame
                        # any byte >= 0x80 starts a new frame (protocol.zig)
                        if len(buf) >= 2 and buf[1] >= 0x80:
                            buf = buf[1:]
                            continue
                        if len(buf) >= 3 and buf[2] >= 0x80:
                            buf = buf[2:]
                            continue
                        if len(buf) < 3:
                            break
                        k = (buf[0] & 3) << 14 | buf[1] << 7 | buf[2]
                        buf = buf[3:]
                        self.valid()
                        with self.lock:
                            self.keys.append((time.monotonic_ns(), k))
                        continue
                    if b"\n" not in buf:
                        break
                    line, buf = buf.split(b"\n", 1)
                    line = line.strip()
                    if line == b"P":
                        self.valid()
                        self.send(b"p\r\n")
                    elif len(line) == 2 and line[:1] == b"B" and line[1:] in (b"0", b"1", b"2", b"3", b"4"):
                        self.valid()
                        if self.mode != "lose_b":
                            self.send(b"b" + line[1:] + b"\r\n")
                        self.rate = int(line[1:])
                    elif line in (b"H0", b"H1"):
                        self.valid()
                        self.held = line == b"H1"
                        self.send(b"h" + line[1:] + b"\r\n")
                    elif line.startswith(b"W") and len(line) == 15:
                        # DDR write (side track I: Game Boy cartridges at 0x02000000)
                        self.valid()
                        addr, n = int(line[1:9], 16), int(line[9:15], 16)
                        data = buf[:n]
                        buf = buf[n:]
                        if len(data) < n:
                            data += self.readn(n - len(data))
                        off = addr - 0x02000000
                        self.ddr[off:off + n] = data
                        self.ddr_writes_held &= self.held
                        self.valid()
                        self.send(b"w%08X%06X%04X\r\n" % (addr, n, sum(data) & 0xFFFF))
                    elif line.startswith(b"Y") and len(line) == 21:
                        # (side track D) DDR onto the PS link: an NES file's header and CHR
                        self.valid()
                        addr, at, n = int(line[1:9], 16), int(line[9:15], 16), int(line[15:21], 16)
                        off = addr - 0x02000000
                        data = self.ddr[off:off + n]
                        self.cart[at:at + n] = data
                        self.ddr_links_held &= self.held
                        self.send(b"y%06X%06X%04X\r\n" % (at, n, sum(data) & 0xFFFF))
                    elif line == b"S":
                        self.valid()
                        self.sd_open = True
                        self.send(b"menu: some message first\r\ns2\r\n")
                    elif line == b"O":
                        self.valid()
                        self.send(b"menu: card mounted, 3 entries\r\no\r\n")
                    elif line.startswith(b"C") and len(line) == 11 and self.sd_open and self.mode != "old_relay":
                        # a chunk's hash, as on the card (side track G phase 2)
                        self.valid()
                        lba, n = int(line[1:9], 16), int(line[9:11], 16)
                        h = fnv1a64(bytes(self.card[lba * 512:(lba + n) * 512]))
                        self.send(b"c%08X%016X\r\n" % (lba, h))
                        self.valid()
                    elif line.startswith(b"M") and len(line) == 15 and self.sd_open and self.mode != "old_relay":
                        # a multi-block write (side track G phase 2)
                        self.valid()
                        lba, n, s = int(line[1:9], 16), int(line[9:11], 16), int(line[11:15], 16)
                        data = buf[:n * 512]
                        buf = buf[n * 512:]
                        if len(data) < n * 512:
                            data += self.readn(n * 512 - len(data))
                        if self.mode == "sd_garble" and not self.garbled:
                            self.garbled = True
                            data = bytes([data[0] ^ 1]) + data[1:]
                        if sum(data) & 0xFFFF != s:
                            self.send(b"x%08X checksum\r\n" % lba)
                        else:
                            self.card[lba * 512:(lba + n) * 512] = data
                            self.m_writes += 1
                            self.send(b"m%08X\r\n" % lba)
                        self.valid()
                    elif line.startswith(b"Z") and len(line) == 17 and self.sd_open:
                        self.valid()
                        self.z_writes += 1
                        lba, n = int(line[1:9], 16), int(line[9:17], 16)
                        self.card[lba * 512:(lba + n) * 512] = bytes(n * 512)
                        if n > 1000:
                            time.sleep(1.0)  # a real card needs seconds for this
                        self.send(b"z%08X%08X\r\n" % (lba, n))
                        self.valid()  # as relay.zig: silence counts from the end of a command
                    elif line.startswith(b"R") and len(line) == 9:
                        # block read (make saves-pull, side track I): needs no S
                        self.valid()
                        lba = int(line[1:9], 16)
                        data = bytes(self.card[lba * 512:(lba + 1) * 512])
                        s = sum(data) & 0xFFFF
                        if self.mode == "sd_garble" and not self.garbled:
                            self.garbled = True
                            data = bytes([data[0] ^ 1]) + data[1:]
                        self.send(b"serial: a game's line between\r\nr%08X%04X\r\n" % (lba, s) + data)
                        self.valid()
                    elif line.startswith(b"D") and len(line) == 13 and self.sd_open:
                        self.valid()
                        lba, s = int(line[1:9], 16), int(line[9:13], 16)
                        data = buf[:512]
                        buf = buf[512:]
                        if len(data) < 512:
                            data += self.readn(512 - len(data))
                        if self.mode == "sd_garble" and not self.garbled:
                            self.garbled = True
                            data = bytes([data[0] ^ 1]) + data[1:]
                        if sum(data) & 0xFFFF != s:
                            self.send(b"x%08X checksum\r\n" % lba)
                        else:
                            self.card[lba * 512:(lba + 1) * 512] = data
                            self.d_writes += 1
                            self.send(b"d%08X\r\n" % lba)
                    elif line.startswith(b"L") and len(line) == 11:
                        # the wide form: a 24-bit address (Game Boy cartridges)
                        self.valid()
                        addr, n = int(line[1:7], 16), int(line[7:11], 16)
                        data = buf[:n]
                        buf = buf[n:]
                        if len(data) < n:
                            data += self.readn(n - len(data))
                        self.cart[addr:addr + n] = data
                        self.valid()  # a completed load is activity, as in protocol.Activity
                        self.send(b"l%06X%04X%04X\r\n" % (addr, n, sum(data) & 0xFFFF))
                    elif line.startswith(b"L") and len(line) == 8:
                        self.valid()
                        addr, n = int(line[1:4], 16), int(line[4:8], 16)
                        data = buf[:n]
                        buf = buf[n:]
                        if len(data) < n:
                            data += self.readn(n - len(data))
                        self.ram[addr:addr + n] = data
                        self.valid()
                        self.send(b"l%03X%04X%04X\r\n" % (addr, n, sum(data) & 0xFFFF))
        except OSError:
            pass


def ioc(direction, typ, nr, size):
    return (direction << 30) | (size << 16) | (ord(typ) << 8) | nr


UI_SET_EVBIT = ioc(1, "U", 100, 4)
UI_SET_KEYBIT = ioc(1, "U", 101, 4)
UI_SET_ABSBIT = ioc(1, "U", 103, 4)
UI_DEV_CREATE = ioc(0, "U", 1, 0)
UI_DEV_DESTROY = ioc(0, "U", 2, 0)


class VirtualPad:
    def __init__(self):
        self.fd = os.open("/dev/uinput", os.O_WRONLY | os.O_NONBLOCK)
        fcntl.ioctl(self.fd, UI_SET_EVBIT, EV_KEY)
        for b in (BTN_SOUTH, BTN_EAST):
            fcntl.ioctl(self.fd, UI_SET_KEYBIT, b)
        fcntl.ioctl(self.fd, UI_SET_EVBIT, EV_ABS)
        absmin = [0] * 64
        absmax = [0] * 64
        for a, lo, hi in ((ABS_X, 0, 255), (ABS_Y, 0, 255), (ABS_HAT0X, -1, 1), (ABS_HAT0Y, -1, 1)):
            fcntl.ioctl(self.fd, UI_SET_ABSBIT, a)
            absmin[a], absmax[a] = lo, hi
        name = b"link test pad".ljust(80, b"\0")
        dev = name + struct.pack("<HHHHI", 0x03, 0x1234, 0x5678, 1, 0)
        dev += struct.pack("<64i", *absmax) + struct.pack("<64i", *absmin)
        dev += struct.pack("<64i", *([0] * 64)) + struct.pack("<64i", *([0] * 64))
        os.write(self.fd, dev)
        fcntl.ioctl(self.fd, UI_DEV_CREATE)
        self.set_abs(ABS_X, 128)
        self.set_abs(ABS_Y, 128)

    def emit(self, typ, code, value):
        os.write(self.fd, struct.pack("<qqHHi", 0, 0, typ, code, value))
        os.write(self.fd, struct.pack("<qqHHi", 0, 0, EV_SYN, 0, 0))

    def set_abs(self, code, value):
        self.emit(EV_ABS, code, value)

    def close(self):
        fcntl.ioctl(self.fd, UI_DEV_DESTROY)
        os.close(self.fd)


def wait_for(cond, timeout):
    end = time.monotonic() + timeout
    while time.monotonic() < end:
        if cond():
            return True
        time.sleep(0.001)
    return False


def main():
    if not os.path.exists(LINK):
        sys.exit(f"test_link: {LINK} missing (make link)")

    # 1. loading, normally and with each fault in the baud switch
    rom = bytes((i * 7 + 3) & 0xFF for i in range(761))
    tmp = tempfile.NamedTemporaryFile(suffix=".ch8", delete=False)
    tmp.write(rom)
    tmp.close()
    romfile = tmp.name
    cache = romfile + ".baud"
    # junk: as on the board after an SD boot, with 230400 cached by an
    # earlier run (the first `make ping` failed there: "board: kA80A")
    for mode, want, cached in (("ok", 921600, None), ("fast2m", 2000000, None), ("lose_b", 921600, None), ("max230400", 230400, None),
                               ("no_fast", 115200, None), ("junk", 230400, "3")):
        relay = FakeRelay(mode)
        if os.path.exists(cache):
            os.unlink(cache)
        if cached is not None:
            with open(cache, "w") as f:
                f.write(cached)
        t0 = time.monotonic()
        r = subprocess.run([LINK, "--port", relay.path, "--load", romfile, "--preset", "0x1FF=1", "--no-input",
                            "--baud-cache", cache], capture_output=True, text=True, timeout=30)
        took = time.monotonic() - t0
        for l in r.stderr.strip().splitlines():
            print(f"  [{mode}] {l}")
        assert r.returncode == 0, f"link --load failed ({mode})"
        assert SPEED[relay.rate] == want, f"{mode}: relay ended at {SPEED[relay.rate]}"
        assert relay.ram[0x200:0x200 + len(rom)] == rom, mode
        assert relay.ram[0x200 + len(rom):] == bytes(0x1000 - 0x200 - len(rom)), mode
        assert relay.ram[0x1FF] == 1, mode
        print(f"test_link: load OK with relay mode {mode} ({want} baud, {took:.1f} s)")
        if mode == "ok":
            # a Game Boy cartridge (--gb): into DDR at 0x02000000 (W, 64 KiB
            # each; side track I), while held; above the old 256 KiB limit
            cart = bytes((i * 13 + 5) & 0xFF for i in range(0x4A000))
            gbfile = romfile + ".gb"
            with open(gbfile, "wb") as f:
                f.write(cart)
            relay2 = FakeRelay("ok")
            r = subprocess.run([LINK, "--port", relay2.path, "--gb", "--load", gbfile, "--no-input",
                                "--baud-cache", cache], capture_output=True, text=True, timeout=60)
            assert r.returncode == 0, f"link --gb --load failed: {r.stderr}"
            assert relay2.ddr[:len(cart)] == cart
            assert relay2.ddr_writes_held, "a DDR write while the core ran"
            assert not relay2.held, "not released after the load"
            assert relay2.cart == bytearray(0x70000) and relay2.ram == bytearray(0x1000), "L lines used for a Game Boy cartridge"
            os.unlink(gbfile)
            print(f"test_link: Game Boy cartridge load OK ({len(cart)} bytes into DDR)")
            # a .nes file (--system nes) above the Game Boy's 256 KiB, as it is from 0;
            # a file without the iNES header is refused
            nes = b"NES\x1a\x12\x08" + bytes(10) + bytes((i * 29 + 1) & 0xFF for i in range(0x12 * 0x4000 + 0x8 * 0x2000))
            nesfile = romfile + ".nes"
            with open(nesfile, "wb") as f:
                f.write(nes)
            relay3 = FakeRelay("ok")
            r = subprocess.run([LINK, "--port", relay3.path, "--system", "nes", "--load", nesfile, "--no-input",
                                "--baud-cache", cache], capture_output=True, text=True, timeout=120)
            assert r.returncode == 0, f"link --system nes --load failed: {r.stderr}"
            # (side track D) the file into DDR, then its header and CHR ROM
            # over the link at their offsets; PRG ROM only in DDR
            prg = 0x12 * 0x4000
            assert relay3.ddr[:len(nes)] == nes
            assert relay3.cart[:16] == nes[:16]
            assert relay3.cart[16 + prg:len(nes)] == nes[16 + prg:]
            assert relay3.cart[16:16 + prg] == bytearray(prg), "PRG ROM sent over the link"
            assert relay3.ddr_writes_held, "a DDR write while the core ran"
            assert relay3.ddr_links_held, "a DDR-to-link copy while the core ran"
            with open(nesfile, "wb") as f:
                f.write(cart)
            r = subprocess.run([LINK, "--port", FakeRelay("ok").path, "--system", "nes", "--load", nesfile, "--no-input",
                                "--baud-cache", cache], capture_output=True, text=True, timeout=30)
            assert r.returncode != 0 and "iNES" in r.stderr, r.stderr
            # (side track D) refused as the menu refuses it: MMC2, and MMC1 above 256 KiB PRG
            for hdr, why in ((b"NES\x1a\x02\x01\x90", "Mapper not supported"), (b"NES\x1a\x20\x01\x10", "MMC1 above 256 KiB")):
                bad = hdr + bytes(16 - len(hdr)) + bytes(hdr[4] * 0x4000 + 0x2000)
                with open(nesfile, "wb") as f:
                    f.write(bad)
                relay4 = FakeRelay("ok")
                r = subprocess.run([LINK, "--port", relay4.path, "--system", "nes", "--load", nesfile, "--no-input",
                                    "--baud-cache", cache], capture_output=True, text=True, timeout=30)
                assert r.returncode != 0 and why in r.stderr, r.stderr
                assert relay4.ddr[:16] == bytes(16) and relay4.cart == bytearray(0x70000), "a refused cartridge was loaded"
            os.unlink(nesfile)
            print(f"test_link: NES load OK ({len(nes)} bytes), a non-iNES file refused")
        if mode == "max230400":
            # the cached rate is used straight away next time
            relay = FakeRelay(mode)
            t0 = time.monotonic()
            r = subprocess.run([LINK, "--port", relay.path, "--ping", "20", "--baud-cache", cache],
                               capture_output=True, text=True, timeout=30)
            took = time.monotonic() - t0
            assert r.returncode == 0 and SPEED[relay.rate] == 230400, r.stderr
            assert "doesn't work" not in r.stderr, "cached rate wasn't used: " + r.stderr
            print(f"test_link: cached rate reused ({took:.1f} s): {r.stderr.strip().splitlines()[-2]}")
    os.unlink(romfile)
    if os.path.exists(cache):
        os.unlink(cache)

    # 2. make sd: link --sd-image against the fake card (the image comes
    # from tools/mksd.sh in make test-zig); a garbled block is sent again
    image_path = "build/ps-test/sd.img"
    if os.path.exists(image_path):
        image = open(image_path, "rb").read()
        for mode in ("ok", "sd_garble", "old_relay", "fast2m"):
            relay = FakeRelay(mode)
            t0 = time.monotonic()
            r = subprocess.run([LINK, "--port", relay.path, "--sd-image", image_path, "--baud-cache", cache],
                               capture_output=True, text=True, timeout=120)
            took = time.monotonic() - t0
            assert r.returncode == 0, f"link --sd-image failed ({mode}): {r.stderr}"
            assert "board: menu: some message first" in r.stderr, r.stderr
            written = 0
            for b in range(len(image) // 512):
                got, want = relay.card[b * 512:(b + 1) * 512], image[b * 512:(b + 1) * 512]
                if got == want:
                    written += got != b"\xa5" * 512
                    continue
                # untouched old data is fine only where the image is zero
                assert got == b"\xa5" * 512 and want == bytes(512), f"{mode}: block {b} differs"
            assert written > 0
            if mode == "sd_garble":
                assert "again" in r.stderr, r.stderr
            if mode == "old_relay":
                assert relay.d_writes > 0 and relay.m_writes == 0, (relay.d_writes, relay.m_writes)
                assert "block by block" in r.stderr, r.stderr
            else:
                assert relay.m_writes > 0 and relay.d_writes == 0, (relay.d_writes, relay.m_writes)
            print(f"test_link: sd image OK with relay mode {mode} ({written} non-old blocks, "
                  f"{relay.m_writes} M, {relay.z_writes} Z, {relay.d_writes} D, {took:.1f} s)")
            if mode == "ok":
                # the same image again onto that card: every chunk skipped
                m0, z0 = relay.m_writes, relay.z_writes
                r = subprocess.run([LINK, "--port", relay.path, "--sd-image", image_path, "--baud-cache", cache],
                                   capture_output=True, text=True, timeout=120)
                assert r.returncode == 0, f"link --sd-image (again) failed: {r.stderr}"
                assert (relay.m_writes, relay.z_writes) == (m0, z0), (relay.m_writes, relay.z_writes, m0, z0)
                assert "unchanged and skipped" in r.stderr, r.stderr
                # one block changed: one chunk written
                changed = bytearray(image)
                b = next(i for i in range(len(image) // 512) if any(image[i * 512:(i + 1) * 512]))
                changed[b * 512] ^= 0xFF
                tmp = image_path + ".changed"
                open(tmp, "wb").write(changed)
                r = subprocess.run([LINK, "--port", relay.path, "--sd-image", tmp, "--baud-cache", cache],
                                   capture_output=True, text=True, timeout=120)
                os.unlink(tmp)
                assert r.returncode == 0, f"link --sd-image (one change) failed: {r.stderr}"
                assert relay.m_writes == m0 + 1, (relay.m_writes, m0)
                assert relay.card[b * 512] == changed[b * 512]
                print("test_link: sd image again: every chunk skipped; one changed block: one chunk written")
        if os.path.exists(cache):
            os.unlink(cache)
    else:
        print(f"test_link: no {image_path}, sd image test skipped (make test-zig makes it)")

    # 2b. make saves-pull: link --saves-pull reads /saves off the fake
    # card (the same image) block by block; a garbled block is read again;
    # a card without a file system fails (make sd then refuses to erase it)
    if os.path.exists(image_path):
        dst = "build/ps-test/pulled-link"
        for mode in ("ok", "sd_garble"):
            shutil.rmtree(dst, ignore_errors=True)
            relay = FakeRelay(mode)
            relay.card[:len(image)] = image
            states = dst + "-states"
            shutil.rmtree(states, ignore_errors=True)
            r = subprocess.run([LINK, "--port", relay.path, "--saves-pull", dst, "--states-dir", states, "--baud-cache", cache],
                               capture_output=True, text=True, timeout=300)
            assert r.returncode == 0, f"link --saves-pull failed ({mode}): {r.stderr}"
            # side track L: the save states too (the stored one make test-zig
            # put on the card; side track K, F1: no empty slots any more)
            st = open(f"{states}/gb/games/count.st2", "rb").read()
            assert st == open("build/ps-test/states/gb/games/count.st2", "rb").read(), "count.st2 differs"
            assert not os.path.exists(f"{states}/gb/games/count.st1"), "an empty slot count.st1 on the card"
            count = open(f"{dst}/gb/games/count.sav", "rb").read()
            assert count == bytes((i * 5 + 1) & 255 for i in range(0x2000)), "count.sav differs"
            assert open(f"{dst}/gb/games/blank.sav", "rb").read() == bytes(0x8000), "blank.sav differs"
            if mode == "sd_garble":
                assert "again" in r.stderr, r.stderr
            print(f"test_link: saves pull OK with relay mode {mode}")
        relay = FakeRelay("ok")  # old data only: no FAT
        r = subprocess.run([LINK, "--port", relay.path, "--saves-pull", dst, "--baud-cache", cache],
                           capture_output=True, text=True, timeout=120)
        assert r.returncode != 0, "link --saves-pull succeeded on a card without a file system"
        print("test_link: saves pull refuses a card without a file system")
        if os.path.exists(cache):
            os.unlink(cache)

    keymap_files()

    # 3. input latency, with a virtual gamepad
    if not os.access("/dev/uinput", os.W_OK):
        print("test_link: /dev/uinput not writable, gamepad and remap tests skipped")
        return
    relay2 = FakeRelay()
    proc = subprocess.Popen([LINK, "--port", relay2.path, "--baud-cache", "/dev/null", "--device", "link test pad"],
                            stderr=subprocess.PIPE, text=True)
    pad = None
    try:
        def seen():
            with relay2.lock:
                return len(relay2.keys) > 0
        assert wait_for(seen, 10), "no keepalive from link"
        # link has settled on its rate (side track G phase 2: it tries
        # 2000000 first, which this bridge can't do; a press during that
        # would come before link watches for new devices)
        assert wait_for(lambda: relay2.rate == 1 and relay2.in_sync(), 10), "link never settled at 921600"
        time.sleep(0.3)
        # plugged in after link started: found through inotify
        pad = VirtualPad()
        time.sleep(0.5)

        def press_and_time(action, want):
            with relay2.lock:
                start_len = len(relay2.keys)
            t0 = time.monotonic_ns()
            action()

            def arrived():
                with relay2.lock:
                    return any(v == want for _, v in relay2.keys[start_len:])
            assert wait_for(arrived, 1.0), f"K{want:04X} never arrived"
            with relay2.lock:
                t1 = next(t for t, v in relay2.keys[start_len:] if v == want)
            return (t1 - t0) / 1000.0

        lat = []
        for _ in range(50):
            lat.append(press_and_time(lambda: pad.emit(EV_KEY, BTN_SOUTH, 1), 1 << 5))   # Cross -> 5
            lat.append(press_and_time(lambda: pad.emit(EV_KEY, BTN_SOUTH, 0), 0))
        lat.append(press_and_time(lambda: pad.set_abs(ABS_HAT0X, 1), 1 << 6))          # D-pad right -> 6
        lat.append(press_and_time(lambda: pad.set_abs(ABS_HAT0X, 0), 0))
        lat.append(press_and_time(lambda: pad.set_abs(ABS_X, 0), 1 << 4))              # stick left -> 4
        lat.append(press_and_time(lambda: pad.set_abs(ABS_X, 128), 0))
        lat.append(press_and_time(lambda: pad.emit(EV_KEY, BTN_EAST, 1), 1 << 0xB))    # Circle -> B
        lat.append(press_and_time(lambda: pad.emit(EV_KEY, BTN_EAST, 0), 0))
        lat.sort()
        print(f"test_link: {len(lat)} gamepad changes, event -> serial line on the PC: "
              f"min {lat[0]:.0f} us, median {lat[len(lat) // 2]:.0f} us, max {lat[-1]:.0f} us")
        print("test_link: gamepad OK (Cross, Circle, D-pad, stick)")
    finally:
        proc.send_signal(2)
        try:
            out = proc.communicate(timeout=5)[1]
        except subprocess.TimeoutExpired:
            proc.kill()
            out = proc.communicate()[1]
        if pad is not None:
            pad.close()
    assert "link test pad" in out, "link didn't report the virtual pad"
    with relay2.lock:
        assert relay2.keys[-1][1] == 0, "link didn't release the keys on exit"
    remap_with_pad()
    print("test_link: OK")


def keymap_files():
    """Key-map files without input devices: the default location, --keymap,
    --show-keymap, a bad file (line number; link stops before the board)."""
    r = subprocess.run([LINK, "--show-keymap", "--system", "gb"], capture_output=True, text=True, timeout=10)
    assert r.returncode == 0 and "(defaults)" in r.stderr and "a = KEY_K BTN_EAST" in r.stderr, r.stderr
    path = os.path.join(CONFIG_DIR, "emulation_clash", "keymap.conf")
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write("# test\n[gb]\na = KEY_SPACE KEY_J\n")
    r = subprocess.run([LINK, "--show-keymap", "--system", "gb"], capture_output=True, text=True, timeout=10)
    assert r.returncode == 0 and path in r.stderr and "a = KEY_SPACE KEY_J" in r.stderr, r.stderr
    assert "KEY_J is bound to both b and a" in r.stderr, r.stderr
    r = subprocess.run([LINK, "--show-keymap", "--system", "nes"], capture_output=True, text=True, timeout=10)
    assert "a = KEY_K BTN_EAST" in r.stderr, "the [gb] section changed the NES: " + r.stderr
    with open(path, "w") as f:
        f.write("[gb]\n\na = KEY_K\nturbo = KEY_T\n")
    relay = FakeRelay("ok")
    r = subprocess.run([LINK, "--port", relay.path, "--system", "gb", "--baud-cache", "/dev/null"],
                       capture_output=True, text=True, timeout=10)
    assert r.returncode == 1 and f"{path}:4: Game Boy has no button 'turbo'" in r.stderr, r.stderr
    assert relay.rate == 0 and "relay on" not in r.stderr, "link talked to the board with a bad key map"
    os.unlink(path)
    r = subprocess.run([LINK, "--show-keymap", "--keymap", path], capture_output=True, text=True, timeout=10)
    assert r.returncode == 1 and "no key map file" in r.stderr, r.stderr
    print("test_link: key-map files OK (default location, --keymap, bad line reported, link refuses)")


class Lines:
    """A process's stderr, line by line, waited on."""

    def __init__(self, stream):
        self.lines = []
        self.cond = threading.Condition()
        threading.Thread(target=self.run, args=(stream,), daemon=True).start()

    def run(self, stream):
        for l in stream:
            with self.cond:
                self.lines.append(l.rstrip("\n"))
                self.cond.notify_all()

    def wait(self, text, timeout=5.0):
        end = time.monotonic() + timeout
        with self.cond:
            while True:
                for i, l in enumerate(self.lines):
                    if text in l:
                        del self.lines[:i + 1]
                        return l
                left = end - time.monotonic()
                assert left > 0, f"never saw '{text}'; last lines: {self.lines[-8:]}"
                self.cond.wait(left)


def remap_with_pad():
    """make keymap through the virtual pad (never a keyboard: that would type
    into the desktop): swap the Game Boy's A and B on the pad, keep the
    rest, write the file; then link with that file sends the swapped bits."""
    path = os.path.join(CONFIG_DIR, "remap", "keymap.conf")
    pad = VirtualPad()
    time.sleep(0.5)  # udev sets the node's permissions
    proc = subprocess.Popen([LINK, "--remap", "--system", "gb", "--keymap", path, "--device", "link test pad"],
                            stdin=subprocess.PIPE, stderr=subprocess.PIPE, text=True, bufsize=1)
    out = Lines(proc.stderr)
    try:
        out.wait("+ link test pad")
        for name in ("up", "down", "left", "right"):
            out.wait(f"] {name} = ")
            proc.stdin.write("\n")
            proc.stdin.flush()
            out.wait(f"{name} kept:")
        out.wait("] b = KEY_J BTN_SOUTH")
        pad.emit(EV_KEY, BTN_EAST, 1)
        pad.emit(EV_KEY, BTN_EAST, 0)
        out.wait("+ BTN_EAST (also bound to a)")
        proc.stdin.write("\n")
        proc.stdin.flush()
        out.wait("b -> KEY_J BTN_EAST")
        out.wait("] a = KEY_K BTN_EAST")
        pad.set_abs(ABS_HAT0X, 1)     # pressed, then Backspace: forgotten
        pad.set_abs(ABS_HAT0X, 0)
        out.wait("+ ABS_HAT0X+")
        proc.stdin.write("\x7f")
        proc.stdin.flush()
        out.wait("forgotten")
        pad.emit(EV_KEY, BTN_SOUTH, 1)
        pad.emit(EV_KEY, BTN_SOUTH, 0)
        out.wait("+ BTN_SOUTH")
        proc.stdin.write("\n")
        proc.stdin.flush()
        out.wait("a -> KEY_K BTN_SOUTH")
        for name in ("start", "select"):
            out.wait(f"] {name} = ")
            proc.stdin.write("\n")
            proc.stdin.flush()
        out.wait("Enter writes")
        assert not os.path.exists(path), "written before the last Enter"
        proc.stdin.write("\n")
        proc.stdin.flush()
        out.wait("written to")
        assert proc.wait(timeout=5) == 0
    finally:
        if proc.poll() is None:
            proc.kill()
    text = open(path).read()
    assert "\n[gb]\nup = KEY_W BTN_DPAD_UP ABS_HAT0Y- ABS_Y-\n" in text, text
    assert "\nb = KEY_J BTN_EAST\na = KEY_K BTN_SOUTH\n" in text, text
    body = "".join(l for l in text.splitlines(True) if not l.startswith("#"))
    # BTN_SOUTH: CHIP-8's 5, the Game Boy's A (the remap), the NES's B,
    # the 800XL's trigger (M4, keymap.zig), the Game Gear's and the Master
    # System's button 1 (M5, M6), the Super Nintendo's B (M7)
    assert "\n[nes]\nup = KEY_W" in body and "\n[a800xl]\nup = KEY_KP8" in body and "\n[gg]\nup = KEY_W" in body and "\n[sms]\nup = KEY_W" in body and "\n[snes]\nup = KEY_W" in body and body.count("BTN_SOUTH") == 7, "other systems changed:\n" + text
    print("test_link: remap through the virtual pad OK (A and B swapped, the rest kept, file written)")

    # Ctrl-C quits without writing
    os.unlink(path)
    proc = subprocess.Popen([LINK, "--remap", "--system", "gb", "--keymap", path, "--device", "link test pad"],
                            stdin=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    out = Lines(proc.stderr)
    out.wait("] up = ")
    proc.send_signal(2)
    assert proc.wait(timeout=5) == 1 and not os.path.exists(path)
    out.wait("nothing written")

    # the remapped file in use: Cross (BTN_SOUTH) is now the Game Boy's A (bit 4)
    with open(path, "w") as f:
        f.write(text)
    relay = FakeRelay()
    proc = subprocess.Popen([LINK, "--port", relay.path, "--system", "gb", "--keymap", path, "--device", "link test pad",
                             "--baud-cache", "/dev/null"], stderr=subprocess.PIPE, text=True)
    out = Lines(proc.stderr)
    try:
        out.wait("+ link test pad")
        for button, bit in ((BTN_SOUTH, 4), (BTN_EAST, 5)):
            with relay.lock:
                n = len(relay.keys)
            pad.emit(EV_KEY, button, 1)

            def arrived():
                with relay.lock:
                    return any(v == 1 << bit for _, v in relay.keys[n:])
            assert wait_for(arrived, 2.0), f"button 0x{button:x} didn't give bit {bit}"
            pad.emit(EV_KEY, button, 0)
        print("test_link: remapped file in use (Cross = A, Circle = B)")
    finally:
        proc.send_signal(2)
        proc.wait(timeout=5)
        pad.close()


if __name__ == "__main__":
    main()
