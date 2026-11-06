#!/usr/bin/env python3
"""make test-desktop: the desktop's debug socket (side track R, R.1) end to end.

    test_debug.py DESKTOP SOCKET ROM

Starts the desktop headless with --debug-socket SOCKET on ROM (a Game Gear
game), then drives it as rota-dbg would: pause, regs, step, peek and poke,
a "frame" stop, a breakpoint at that PC hit by "continue", delete, and
the run to its end once the client leaves.
"""
import os
import socket
import subprocess
import sys
import time


def main() -> int:
    desktop, sock_path, rom = sys.argv[1:4]
    env = dict(os.environ, SDL_VIDEODRIVER="dummy", SDL_AUDIODRIVER="dummy")
    env.pop("LD_LIBRARY_PATH", None)
    if os.path.exists(sock_path):
        os.unlink(sock_path)
    proc = subprocess.Popen(
        [desktop, "--scale", "1", "--keymap", "/dev/null", "--unpaced", "--no-save",
         "--frames", "2000", "--debug-socket", sock_path, rom],
        env=env, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    try:
        s = connect(sock_path, proc)
        f = s.makefile("rw", buffering=1)
        expect(f.readline(), "ok rota debugger")

        def ask(cmd: str) -> str:
            f.write(cmd + "\n")
            f.flush()
            line = f.readline().strip()
            if not line.startswith("ok"):
                fail(f"{cmd}: {line}")
            return line

        ask("pause")
        regs = ask("regs")
        expect(regs, " PC=")
        ask("step 10")
        old = int(ask("peek C100").split()[-1], 16)
        ask(f"poke C100 {old ^ 0xA5:02X}")
        got = int(ask("peek E100").split()[-1], 16)  # the RAM's mirror
        if got != old ^ 0xA5:
            fail(f"poke: read back {got:02X}, wrote {old ^ 0xA5:02X}")
        ask(f"poke C100 {old:02X}")
        ask("vdp")
        ask("vram 3800 16")
        # a frame's end, then a breakpoint where it stopped: the game
        # comes back there within a few frames
        ask("frame")
        stop = f.readline().strip()
        expect(stop, "stopped frame ")
        pc = stop.split()[-1]
        ask(f"break {pc}")
        expect(ask("breaks"), pc)
        ask("continue")
        s.settimeout(30)
        hit = f.readline().strip()
        if hit != f"stopped break {pc}":
            fail(f"continue with a breakpoint at {pc}: {hit!r}")
        expect(ask("status"), f"ok paused {pc}")
        ask("delete all")
        if ask("breaks") != "ok":
            fail("delete all left breakpoints")
        ask("continue")
        s.close()
        # the client gone, the desktop runs to its 2000 frames and quits
        rc = proc.wait(timeout=120)
        if rc != 0:
            fail(f"the desktop exited {rc}: {proc.stderr.read().decode(errors='replace')[-500:]}")
    finally:
        if proc.poll() is None:
            proc.kill()
    print("test_debug: the debug socket: pause, regs, step, peek/poke, frame, break, continue, delete: OK")
    return 0


def connect(path: str, proc: subprocess.Popen) -> socket.socket:
    for _ in range(200):
        if proc.poll() is not None:
            fail(f"the desktop exited {proc.returncode} before its socket: {proc.stderr.read().decode(errors='replace')[-500:]}")
        if os.path.exists(path):
            s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            try:
                s.connect(path)
                s.settimeout(30)
                return s
            except OSError:
                s.close()
        time.sleep(0.05)
    fail("no debug socket after 10 s")
    raise SystemExit(1)


def expect(line: str, part: str) -> None:
    if part not in line:
        fail(f"expected {part!r} in {line.strip()!r}")


def fail(msg: str) -> None:
    print(f"test_debug: FAIL {msg}", file=sys.stderr)
    raise SystemExit(1)


if __name__ == "__main__":
    sys.exit(main())
