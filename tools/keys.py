#!/usr/bin/env python3
"""Desktop keyboard -> CHIP-8 keypad on the ZedBoard, through the USB-UART
(J14) and the PS relay app (platform/ps/relay.zig, protocol in
platform/ps/protocol.zig).

    tools/keys.py [--port /dev/ttyACM0]     (normally: make keys)
    tools/keys.py --load ROM [--preset 0x1FF=1] [--no-keys]
                                             (normally: make load ROM=...)

--load first loads a CHIP-8 program over the same link: the PL design is
held (soft reset), the ROM goes to 0x200 with zeros up to the end of RAM
(nothing of the previous program survives), each --preset byte is written,
every block's checksum is checked, and the design restarts with the new
program. Then the keyboard window opens, unless --no-keys.

Opens a small window; while it has focus, the usual CHIP-8 layout

    1 2 3 4        1 2 3 C
    Q W E R   ->   4 5 6 D
    A S D F        7 8 9 E
    Z X C V        A 0 B F

is held/released on the board. The full key state is sent as "K<hex>\\n"
about 30 times a second (so a lost line costs one update), and the relay
releases everything if the updates stop. Lines from the relay are printed.

Standard library only (tkinter, termios). X11 key autorepeat sends
release+press pairs while a key is held; a release only counts if no press
of the same key follows within 40 ms.
"""

import argparse
import glob
import os
import select
import sys
import time
import termios
import tkinter as tk

LAYOUT = {
    "1": 0x1, "2": 0x2, "3": 0x3, "4": 0xC,
    "q": 0x4, "w": 0x5, "e": 0x6, "r": 0xD,
    "a": 0x7, "s": 0x8, "d": 0x9, "f": 0xE,
    "z": 0xA, "x": 0x0, "c": 0xB, "v": 0xF,
}
SEND_MS = 33
RELEASE_GRACE_MS = 40


def find_port():
    """The ZedBoard's USB-UART (Cypress CY7C64225), or a plain guess."""
    for pattern in ("/dev/serial/by-id/*Cypress*", "/dev/serial/by-id/*04b4*"):
        hits = sorted(glob.glob(pattern))
        if hits:
            return hits[0]
    for p in ("/dev/ttyACM0", "/dev/ttyUSB1", "/dev/ttyUSB0"):
        if os.path.exists(p):
            return p
    return None


def open_serial(path):
    """115200 8N1, raw, non-blocking."""
    fd = os.open(path, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK)
    iflag, oflag, cflag, lflag, _, _, cc = termios.tcgetattr(fd)
    iflag = 0
    oflag = 0
    lflag = 0
    cflag = termios.CS8 | termios.CREAD | termios.CLOCAL
    cc[termios.VMIN] = 0
    cc[termios.VTIME] = 0
    termios.tcsetattr(fd, termios.TCSANOW,
                      [iflag, oflag, cflag, lflag, termios.B115200, termios.B115200, cc])
    termios.tcflush(fd, termios.TCIOFLUSH)
    return fd


RAM_END = 0x1000
PROG_START = 0x200


def read_line(fd, buf, timeout):
    """Next line from the relay (without CR/LF), keeping leftovers in buf[0]."""
    end = time.monotonic() + timeout
    while b"\n" not in buf[0]:
        left = end - time.monotonic()
        if left <= 0:
            return None
        r, _, _ = select.select([fd], [], [], left)
        if r:
            try:
                buf[0] += os.read(fd, 256)
            except BlockingIOError:
                pass
    line, buf[0] = buf[0].split(b"\n", 1)
    return line.decode(errors="replace").strip()


def expect(fd, buf, prefix, timeout=3.0):
    """Wait for a relay line starting with prefix; print the others."""
    while True:
        line = read_line(fd, buf, timeout)
        if line is None:
            sys.exit(f"keys: no '{prefix}' reply from the relay (is make prog done?)")
        if line.startswith(prefix):
            return line
        if line:
            print(f"board: {line}")


def write_all(fd, data):
    while data:
        try:
            n = os.write(fd, data)
            data = data[n:]
        except BlockingIOError:
            select.select([], [fd], [], 1.0)


def load_block(fd, buf, addr, data):
    """One L command; checks the relay's checksum reply."""
    write_all(fd, b"L%03X%04X\n" % (addr, len(data)))
    write_all(fd, data)
    want = "l%03X%04X%04X" % (addr, len(data), sum(data) & 0xFFFF)
    got = expect(fd, buf, "l", timeout=3.0 + len(data) / 5000)
    if got != want:
        sys.exit(f"keys: load of {len(data)} bytes at {addr:#05x} failed: relay said {got!r}, expected {want!r}")


def load_rom(fd, rom, presets):
    """Hold, load the ROM (zero-padded to the end of RAM) and presets, release."""
    if len(rom) > RAM_END - PROG_START:
        sys.exit(f"keys: ROM is {len(rom)} bytes, more than {RAM_END - PROG_START}")
    buf = [b""]
    termios.tcflush(fd, termios.TCIFLUSH)
    write_all(fd, b"H1\n")
    expect(fd, buf, "h1")
    t0 = time.monotonic()
    load_block(fd, buf, PROG_START, rom + bytes(RAM_END - PROG_START - len(rom)))
    for addr, value in presets:
        load_block(fd, buf, addr, bytes([value]))
    write_all(fd, b"H0\n")
    expect(fd, buf, "h0")
    print(f"keys: loaded {len(rom)} bytes (+{len(presets)} preset byte(s)) in "
          f"{time.monotonic() - t0:.1f} s; running")


def parse_preset(text):
    try:
        a, v = text.split("=")
        addr, value = int(a, 0), int(v, 0)
    except ValueError:
        raise argparse.ArgumentTypeError(f"want ADDR=VALUE, e.g. 0x1FF=1, not {text!r}")
    if not (0 <= addr < RAM_END and 0 <= value < 256):
        raise argparse.ArgumentTypeError(f"{text}: address or value out of range")
    return addr, value


def encode(state):
    """The relay's line for a key state (bit k = key k)."""
    return b"K%04X\n" % (state & 0xFFFF)


class App:
    def __init__(self, root, fd):
        self.root = root
        self.fd = fd
        self.state = 0
        self.pending_release = {}
        self.rx = b""
        self.label = tk.Label(root, font=("monospace", 14), justify="left", padx=12, pady=12)
        self.label.pack()
        root.title("CHIP-8 keys -> ZedBoard")
        root.bind("<KeyPress>", self.press)
        root.bind("<KeyRelease>", self.release)
        root.protocol("WM_DELETE_WINDOW", self.quit)
        self.show()
        self.tick()

    def key_of(self, event):
        return LAYOUT.get(event.keysym.lower())

    def press(self, event):
        if event.keysym == "Escape":
            self.quit()
            return
        k = self.key_of(event)
        if k is None:
            return
        job = self.pending_release.pop(k, None)
        if job is not None:
            self.root.after_cancel(job)   # autorepeat: still held
        self.set_key(k, True)

    def release(self, event):
        k = self.key_of(event)
        if k is None:
            return
        old = self.pending_release.pop(k, None)
        if old is not None:
            self.root.after_cancel(old)
        self.pending_release[k] = self.root.after(RELEASE_GRACE_MS, self.really_release, k)

    def really_release(self, k):
        self.pending_release.pop(k, None)
        self.set_key(k, False)

    def set_key(self, k, down):
        new = self.state | (1 << k) if down else self.state & ~(1 << k)
        if new != self.state:
            self.state = new
            self.send()
            self.show()

    def send(self):
        try:
            os.write(self.fd, encode(self.state))
        except OSError as e:
            print(f"keys: write failed: {e}", file=sys.stderr)

    def tick(self):
        self.send()
        try:
            self.rx += os.read(self.fd, 256)
        except BlockingIOError:
            pass
        except OSError as e:
            print(f"keys: read failed: {e}", file=sys.stderr)
        while b"\n" in self.rx:
            line, self.rx = self.rx.split(b"\n", 1)
            text = line.decode(errors="replace").strip()
            if text:
                print(f"board: {text}")
        self.root.after(SEND_MS, self.tick)

    def show(self):
        held = [f"{k:X}" for k in range(16) if self.state >> k & 1]
        self.label.config(text=(
            "Keys (this window must have focus):\n"
            "  1 2 3 4 -> 1 2 3 C    Q W E R -> 4 5 6 D\n"
            "  A S D F -> 7 8 9 E    Z X C V -> A 0 B F\n"
            "Esc quits.\n\n"
            f"held: {' '.join(held) or '-'}"))

    def quit(self):
        self.state = 0
        self.send()
        self.root.destroy()


def self_test_load():
    """load_rom against a fake relay on a pseudo-terminal (same replies as
    platform/ps/relay.zig); the RAM image it rebuilds must be right."""
    import threading
    master, slave = os.openpty()
    fd = open_serial(os.ttyname(slave))
    ram = bytearray(RAM_END)

    def fake_relay():
        try:
            serve(os.fdopen(master, "r+b", buffering=0))
        except OSError:
            pass   # the test closed its end

    def serve(f):
        while True:
            line = b""
            while not line.endswith(b"\n"):
                c = f.read(1)
                if not c:
                    return
                line += c
            line = line.strip()
            if line in (b"H1", b"H0"):
                f.write(b"h" + line[1:] + b"\r\n")
            elif line.startswith(b"L"):
                addr, n = int(line[1:4], 16), int(line[4:8], 16)
                data = b""
                while len(data) < n:
                    data += f.read(n - len(data))
                ram[addr:addr + n] = data
                f.write(b"l%03X%04X%04X\r\n" % (addr, n, sum(data) & 0xFFFF))

    t = threading.Thread(target=fake_relay, daemon=True)
    t.start()
    rom = bytes(range(1, 200))
    load_rom(fd, rom, [(0x1FF, 1)])
    assert ram[PROG_START:PROG_START + len(rom)] == rom
    assert ram[PROG_START + len(rom):] == bytes(RAM_END - PROG_START - len(rom))
    assert ram[0x1FF] == 1
    os.close(fd)
    os.close(slave)   # the fake relay's read now fails, and it returns
    t.join(5)
    assert not t.is_alive()


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--port", help="serial device (default: the ZedBoard's USB-UART if found)")
    ap.add_argument("--load", metavar="ROM", help="load this CHIP-8 program first")
    ap.add_argument("--preset", metavar="ADDR=VALUE", type=parse_preset, action="append", default=[],
                    help="extra RAM byte after loading, e.g. 0x1FF=1 (repeatable)")
    ap.add_argument("--no-keys", action="store_true", help="exit after --load")
    ap.add_argument("--self-test", action="store_true", help="check the line encoding and exit")
    args = ap.parse_args()
    if args.self_test:
        assert encode(0) == b"K0000\n"
        assert encode(1 << 0xB | 1 << 2) == b"K0804\n"
        assert len(set(LAYOUT.values())) == 16
        assert parse_preset("0x1FF=1") == (0x1FF, 1)
        # serial setup and a line through a pseudo-terminal
        master, slave = os.openpty()
        fd = open_serial(os.ttyname(slave))
        os.write(fd, encode(0x8001))
        assert os.read(master, 16) == b"K8001\n"
        os.close(fd); os.close(slave); os.close(master)
        self_test_load()
        print("keys: self-test OK")
        return
    port = args.port or find_port()
    if port is None:
        sys.exit("keys: no serial port found; connect J14 (UART) to this PC or pass --port")
    try:
        fd = open_serial(port)
    except OSError as e:
        sys.exit(f"keys: can't open {port}: {e} (are you in the dialout group?)")
    if args.load:
        with open(args.load, "rb") as f:
            rom = f.read()
        print(f"keys: loading {args.load} through {port}")
        load_rom(fd, rom, args.preset)
        if args.no_keys:
            os.close(fd)
            return
    print(f"keys: sending to {port}")
    root = tk.Tk()
    App(root, fd)
    root.mainloop()
    os.close(fd)


if __name__ == "__main__":
    main()
