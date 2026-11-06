#!/usr/bin/env python3
"""make test-desktop: rota-dbg, the terminal debugger (side track R, R.2), in a pty.

    test_dbg_tui.py DESKTOP ROTA_DBG SOCKET ROM

Starts the desktop headless with --debug-socket on ROM (a Game Gear
game), runs rota-dbg in a 100 x 30 pseudo-terminal, presses Space
(pause), s (step) and q, and checks what it drew: the PAUSED header, the
registers, the breakpoints' and memory's panes, and disassembly lines
(address, bytes, a Z80 mnemonic).
"""
import fcntl
import os
import pty
import re
import select
import struct
import subprocess
import sys
import termios
import time


def main() -> int:
    desktop, dbg, sock, rom = sys.argv[1:5]
    env = dict(os.environ, SDL_VIDEODRIVER="dummy", SDL_AUDIODRIVER="dummy")
    env.pop("LD_LIBRARY_PATH", None)
    if os.path.exists(sock):
        os.unlink(sock)
    emu = subprocess.Popen([desktop, "--scale", "1", "--keymap", "/dev/null", "--no-save",
                            "--debug-socket", sock, rom],
                           env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        for _ in range(200):
            if os.path.exists(sock):
                break
            time.sleep(0.05)
        else:
            return fail("no debug socket")
        pid, fd = pty.fork()
        if pid == 0:
            os.execv(dbg, [dbg, sock])
        fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 100, 0, 0))
        out = bytearray()

        def pump(t: float) -> None:
            end = time.time() + t
            while time.time() < end:
                r, _, _ = select.select([fd], [], [], 0.05)
                if r:
                    try:
                        out.extend(os.read(fd, 65536))
                    except OSError:
                        return

        pump(1.0)
        os.write(fd, b" ")
        pump(0.8)
        os.write(fd, b"s")
        pump(0.8)
        shown = bytes(out).decode(errors="replace")
        os.write(fd, b"q")
        pump(0.5)
        _, status = os.waitpid(pid, 0)
        if os.WEXITSTATUS(status) != 0:
            return fail(f"rota-dbg exited {os.WEXITSTATUS(status)}")
    finally:
        emu.kill()
    plain = re.sub(r"\x1b\[[0-9;?]*[A-Za-z]", "\n", shown)
    for part in ["PAUSED", " PC ", "IFF1", "breakpoints", "memory at"]:
        if part not in plain:
            return fail(f"no {part!r} on the screen")
    dis = [l for l in plain.split("\n") if re.match(r"^ [ *][ >][0-9A-F]{4}  ([0-9A-F]{2} )+ +[A-Z]{2,4}", l)]
    if len(dis) < 5:
        return fail(f"{len(dis)} disassembly lines")
    if not any(l[2] == ">" for l in dis):
        return fail("no line marked as PC")
    print(f"test_dbg_tui: rota-dbg drew the paused view, {len(dis)} disassembly lines: OK")
    return 0


def fail(msg: str) -> int:
    print(f"test_dbg_tui: FAIL {msg}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
