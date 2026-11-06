#!/usr/bin/env python3
"""The PS app's deepest known call paths against its stack (16 KiB,
platform/ps/link.ld), from the frames in the app's assembly (push, vpush,
sub sp in each function's prologue; build.zig installs ps/relay.s beside
the ELF).

A system switch from the Systems menu overflowed it on the board
(2026-10-08: the menu's card listing then failed with BadCluster): side
track J's splash, inlined, put its loading frame into startSystem's on
every switch. The paths below are that switch and the splash; a function
LLVM inlined away counts as 0 (its frame is then in its caller's). A path
needs a margin of MARGIN bytes for interrupts and the calls beyond it:
the board's high-water mark was the full 16 KiB where these frames
counted 14352, so at least 2032 bytes are beyond them.

  tools/check-ps-stack.py ps/relay.s [...]
"""
import re
import sys

STACK = 16384
MARGIN = 2560

# the outermost caller first; each a function name in relay.s (".L" and
# the module prefix as LLVM writes them)
PATHS = {
    "a switch on the Systems menu, then the card's listing": [
        "relay.main", "relay.menuEvent", "relay.startSystem", "relay.mountAndList",
        "relay.list", "fat.Fs(sd.Sd(sd.MmioBus)).Iterator.next", "sd.Sd(sd.MmioBus).readBlocks*"],
    "a switch on the Systems menu, then the card's start": [
        "relay.main", "relay.menuEvent", "relay.startSystem", "relay.mountAndList",
        "sd.Sd(sd.MmioBus).init"],
    "a switch with the splash (splash_on_switch = on)": [
        "relay.main", "relay.menuEvent", "relay.startSystem", "relay.playSplash",
        "load.streamDdr*", "sd.Sd(sd.MmioBus).readBlocks*"],
    # (side track I's review: the path its GBC work grew)
    "a ROM from the menu": [
        "relay.main", "relay.menuEvent", "relay.loadRom", "load.streamDdr*",
        "sd.Sd(sd.MmioBus).readBlocks*"],
    "a ROM from the menu, then its save": [
        "relay.main", "relay.menuEvent", "relay.loadRom", "relay.startSave",
        "save.load*", "sd.Sd(sd.MmioBus).readBlocks*"],
    # (side track L: save states; savestate.save and .load are inlined
    # into the relay's saveState and loadState)
    "a save state, its slot looked up": [
        "relay.main", "relay.menuEvent", "relay.saveState", "relay.slotFile",
        "save.lookup*", "sd.Sd(sd.MmioBus).readBlocks*"],
    "a save state, the state read": [
        "relay.main", "relay.menuEvent", "relay.saveState", "savestate.readBytes*"],
    "a load state": [
        "relay.main", "relay.menuEvent", "relay.loadState", "sd.Sd(sd.MmioBus).readBlocks*"],
    # (side track K, F1: the board makes its files: a folder looked up or
    # made, the clusters, the directory entry with its alias's search)
    "a ROM's save made on the card": [
        "relay.main", "relay.menuEvent", "relay.loadRom", "relay.startSave",
        "save.makeFile*", "fat.Fs(sd.Sd(sd.MmioBus)).addEntry",
        "fat.Fs(sd.Sd(sd.MmioBus)).Iterator.next", "sd.Sd(sd.MmioBus).readBlocks*"],
    "a ROM's save made, its clusters": [
        "relay.main", "relay.menuEvent", "relay.loadRom", "relay.startSave",
        "save.makeFile*", "fat.Fs(sd.Sd(sd.MmioBus)).allocChain", "sd.Sd(sd.MmioBus).writeBlock*"],
    "a save state's slot made": [
        "relay.main", "relay.menuEvent", "relay.saveState", "relay.slotFile",
        "save.makeFile*", "fat.Fs(sd.Sd(sd.MmioBus)).addEntry",
        "fat.Fs(sd.Sd(sd.MmioBus)).Iterator.next", "sd.Sd(sd.MmioBus).readBlocks*"],
    "a screenshot made": [
        "relay.main", "relay.menuEvent", "relay.takeScreenshot",
        "save.makeFile*", "fat.Fs(sd.Sd(sd.MmioBus)).addEntry",
        "fat.Fs(sd.Sd(sd.MmioBus)).Iterator.next", "sd.Sd(sd.MmioBus).readBlocks*"],
    "a movie's slot made": [
        "relay.main", "relay.menuEvent", "relay.startMovie", "relay.movieFile",
        "save.makeFile*", "fat.Fs(sd.Sd(sd.MmioBus)).addEntry",
        "fat.Fs(sd.Sd(sd.MmioBus)).Iterator.next", "sd.Sd(sd.MmioBus).readBlocks*"],
    # (side track K, the Controls page: keymap.conf written, made if
    # missing; its clusters grown)
    "the Controls page saved, keymap.conf made": [
        "relay.main", "relay.menuEvent", "relay.saveKeymap", "fat.Fs(sd.Sd(sd.MmioBus)).addEntry",
        "fat.Fs(sd.Sd(sd.MmioBus)).Iterator.next", "sd.Sd(sd.MmioBus).readBlocks*"],
    "the Controls page saved, its clusters": [
        "relay.main", "relay.menuEvent", "relay.saveKeymap", "fat.Fs(sd.Sd(sd.MmioBus)).allocChain",
        "sd.Sd(sd.MmioBus).writeBlock*"],
    "the menu opened over a game with slots": [
        "relay.main", "relay.mountAndList", "relay.refreshSlots", "relay.slotFile",
        "save.lookup*", "sd.Sd(sd.MmioBus).readBlocks*"],
}


def frames(path):
    out = {}
    cur = None
    n = 0
    for line in open(path):
        # a function's label ("name" or ".Lname"); not LLVM's own labels
        # (.LBB0_1, .Ltmp3, .Lfunc_end2, ...)
        m = re.match(r'^"?(\.L)?([^"\s:][^"]*?)"?:', line)
        internal = m is not None and m.group(1) is not None and re.match(r"(BB|tmp|func|CPI|JTI|cst|_)", m.group(2))
        if m and not internal:
            cur = m.group(2)
            out[cur] = 0
            n = 0
            continue
        if cur is None:
            continue
        s = line.strip()
        if not s or s.startswith(".") or s.startswith("@"):
            continue
        n += 1
        if n > 30:
            continue
        m = re.match(r"push\s*\{([^}]*)\}", s)
        if m:
            out[cur] += 4 * len([r for r in m.group(1).split(",") if r.strip()])
            continue
        m = re.match(r"vpush\s*\{\s*d(\d+)\s*(?:-\s*d(\d+))?\s*\}", s)
        if m:
            out[cur] += 8 * (int(m.group(2) or m.group(1)) - int(m.group(1)) + 1)
            continue
        m = re.match(r"sub\s+sp,\s*sp,\s*#(\d+)", s)
        if m:
            out[cur] += int(m.group(1))
    return out


def frame_of(fs, name):
    """A function's frame, or None if relay.s has no such function."""
    if name.endswith("*"):
        hits = [v for k, v in fs.items() if k.startswith(name[:-1])]
        return max(hits) if hits else None
    return fs.get(name)


def main():
    bad = False
    for asm in sys.argv[1:]:
        fs = frames(asm)
        if "relay.main" not in fs:
            print(f"check-ps-stack.py: {asm}: no relay.main (not the relay's assembly?)", file=sys.stderr)
            sys.exit(1)
        for what, path in PATHS.items():
            parts = [(n, frame_of(fs, n)) for n in path]
            missing = [n for n, v in parts if v is None]
            total = sum(v or 0 for _, v in parts)
            ok = total + MARGIN <= STACK
            # a name not found counts as inlined (0), but the path's ends
            # always exist: a rename or typo there must not pass quietly
            ends_ok = parts[0][1] is not None and parts[-1][1] is not None
            bad |= not ok or not ends_ok
            print(f"check-ps-stack.py: {asm}: {what}: {total} of {STACK} bytes"
                  + ("" if ok else f" (over {STACK - MARGIN}: " + ", ".join(f"{n} {v or 0}" for n, v in parts) + ")")
                  + (f"; not found (inlined?): {', '.join(missing)}" if missing else "")
                  + ("" if ends_ok else "; a path's first or last function is missing: update PATHS"))
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()
