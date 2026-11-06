#!/usr/bin/env python3
"""make ci's result cache (side track K, D3; PLAN.md, "D3"; docs/TESTING.md,
"The result cache").

Each unit of work gets a key, a hash of everything it reads. A unit whose
key has a cached pass is skipped. Only passes are cached, and a unit whose
key can't be computed runs ("unknown means run").

Units:
  suite   a Haskell suite (tools/run-tests.sh): its package's local
          dependency closure (every file in each package's directory), the
          build outputs its sources name ("../../build/..."), the ROM
          manifests, the tier, flake.lock, cabal.project
  hdl     a Clash run (make hdl): its source directories, its .mk, the
          Clash command, flake.lock. The Verilog is cached under this key,
          so make hdl and make bit reuse it
  bit     a Vivado run (make bit): the Clash output directory (Verilog and
          memory images), the board's Tcl, wrappers and XDC, the top, the
          Vivado installation. The .bit and its reports are cached
  chain   a layer 4 chain (make ci's ci-chain-<n>): its commands as
          `make -n -B` prints them (comment lines dropped, the worktree's
          path replaced), the Verilog hash of each top it builds (from the
          hdl entries: an upstream change that leaves the Verilog alone
          costs no run), the files its commands name (PS link scripts as
          ps-encode's hash of the bytes the harness plays, the image
          generators as their binaries), the harness sources

Modes (CI_CACHE): on (read and write; the default, with ALL=1 too since
2026-10-15), write (run everything, record the passes: the full `make ci
ALL=1 CI_CACHE=write`, before milestone tags and weekly), off.

  ci-cache.py hdl-key --dirs "D..." --files "F..." --text T
  ci-cache.py bit-key --dirs "D..." --files "F..." --text T     (make bit)
  ci-cache.py bit-restore KEY VIVADO_DIR      the .bit and its reports (exit 1: not cached)
  ci-cache.py bit-store KEY VIVADO_DIR        after a build that passed its gates
  ci-cache.py hdl-restore KEY DIR [NAME...]   restore DIR's Verilog (exit 1: not cached)
  ci-cache.py hdl-store KEY DIR [NAME...]     after a Clash run (NAMEs: memory images, not kept)
  ci-cache.py suite-key DIR SUITE
  ci-cache.py check UNIT KEY                  exit 0 (and say so) if a pass is cached
  ci-cache.py pass UNIT KEY SECS              record a pass
  ci-cache.py run UNIT KEY -- CMD...          check, else run and record a pass
  ci-cache.py chain NAME -- CMD...            make ci's chains (ci-chain-%)
  ci-cache.py chain-hdl-keys NAME            "build-dir key" of each Clash run in the chain
  ci-cache.py chain-key NAME [--plan] [--show] print the key (--plan: never run Clash;
                                              --show: what went into it)
  ci-cache.py summary                         "N run, M cached" from CI_CACHE_LOG

Environment: CI_CACHE_DIR (default ~/.cache/emulation_clash/ci-cache,
shared by every worktree), CI_CACHE_LOG (make ci's list of units run and
cached). For tools/test_ci_cache.sh only: CI_CACHE_LIST_BIN_FILE (every
image generator hashes as this file), CI_CACHE_MUTATION=no-ps-encoding
(leave the PS scripts out of chain keys: the test must then fail).
"""

import datetime
import hashlib
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tarfile
import tempfile
import time

VERSION = "1"  # bump to invalidate every key
ROOT = os.path.realpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
CACHE = os.environ.get("CI_CACHE_DIR") or os.path.join(
    os.environ.get("XDG_CACHE_HOME") or os.path.expanduser("~/.cache"), "emulation_clash", "ci-cache")
MODE = os.environ.get("CI_CACHE", "on")
PRUNE_DAYS = 14
BOARDS = ("zedboard", "arty-z7-20")
SKIP_NAMES = {"dist-newstyle", ".git", "__pycache__", ".zig-cache", "zig-out"}
# build outputs that are never inputs of a chain (they are made by it, or
# reach it as the Verilog hash): skipped when a command names them
BUILD_OUTPUTS = {"verilog", "obj", "obj_dir", "frames", "images", "vivado", "zig-cache",
                 "host", "ps", "ps7", "boot", "test-logs", "ps-test"}
# tools a chain's commands name that don't change its results
NOT_INPUTS = {"tools/progress.sh", "tools/clash-slot.sh", "tools/queue.sh", "tools/ci-cache.py"}
HARNESS = ["platform/sim", "boards/sim", "build.zig", "flake.lock"]
# a build's intermediate files: nobody reads them, and some carry the time
# they were made (ca65's objects)
INTERMEDIATE = (".o", ".lbl", ".sym", ".log", ".link", ".map", ".part", ".dbg", ".d")
SUITE_SKIP = (".zig", ".cpp", ".h", ".mk", ".md", ".v", ".py", ".sh", ".ld", ".tcl", ".html")
# the parts of platform/ that are not its Haskell package (the PS app, the
# desktop side, the harness): no suite reads them
SUITE_SKIP_DIRS = {"platform/ps", "platform/host", "platform/sim"}


def say(msg):
    print(f"ci-cache: {msg}", file=sys.stderr, flush=True)


# --- hashing ----------------------------------------------------------------

_memo = {}


def hash_file(path):
    st = os.stat(path)
    m = (path, st.st_mtime_ns, st.st_size)
    if m in _memo:
        return _memo[m]
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for b in iter(lambda: f.read(1 << 20), b""):
            h.update(b)
    _memo[m] = h.hexdigest()
    return _memo[m]


def hash_dir(path, exclude=(), skip_ext=(), skip_dirs=()):
    h = hashlib.sha256()
    for d, dirs, files in os.walk(path):
        dirs[:] = sorted(x for x in dirs if x not in SKIP_NAMES and not x.startswith(".")
                         and os.path.relpath(os.path.join(d, x), ROOT) not in skip_dirs)
        for f in sorted(files):
            if f.startswith(".") or f in exclude or f.endswith(skip_ext):
                continue
            p = os.path.join(d, f)
            if os.path.isfile(p):
                h.update(f"{os.path.relpath(p, path)}\0{hash_file(p)}\n".encode())
    return h.hexdigest()


def hash_path(rel):
    p = os.path.join(ROOT, rel)
    if os.path.isdir(p):
        return "dir " + hash_dir(p)
    if os.path.isfile(p):
        return "file " + hash_file(p)
    return "missing"


class Key:
    """A key: lines of "label value", hashed; the lines are kept with a
    pass, so two keys can be compared ("why did this run?")."""

    def __init__(self, unit):
        self.lines = [f"version {VERSION}", f"unit {unit}"]

    def add(self, label, value):
        self.lines.append(f"{label} {value}")

    def path(self, rel):
        self.add(rel, hash_path(rel))

    def digest(self):
        return hashlib.sha256("\n".join(self.lines).encode()).hexdigest()


# --- the store ----------------------------------------------------------------

def commit():
    # make ci on the worker (tools/ci-remote.sh): its copy has no history
    if os.environ.get("CI_CACHE_COMMIT"):
        return os.environ["CI_CACHE_COMMIT"]
    try:
        return subprocess.run(["git", "-C", ROOT, "rev-parse", "--short", "HEAD"], capture_output=True,
                              text=True, check=True).stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return "?"


def write_json(path, obj):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    fd, tmp = tempfile.mkstemp(dir=os.path.dirname(path), prefix=".tmp-")
    with os.fdopen(fd, "w") as f:
        json.dump(obj, f, indent=1)
    os.replace(tmp, path)


def read_json(path):
    try:
        with open(path) as f:
            return json.load(f)
    except (OSError, ValueError):
        return None


def prune():
    """Entries older than PRUNE_DAYS go (at most once an hour)."""
    stamp = os.path.join(CACHE, ".pruned")
    now = time.time()
    try:
        if now - os.stat(stamp).st_mtime < 3600:
            return
    except OSError:
        pass
    os.makedirs(CACHE, exist_ok=True)
    open(stamp, "w").close()
    for sub in ("pass", "hdl", "bit"):
        d = os.path.join(CACHE, sub)
        if not os.path.isdir(d):
            continue
        for f in os.listdir(d):
            p = os.path.join(d, f)
            try:
                if now - os.stat(p).st_mtime > PRUNE_DAYS * 86400:
                    os.remove(p)
            except OSError:
                pass


def pass_path(key):
    return os.path.join(CACHE, "pass", key + ".json")


def cached(unit, key):
    if MODE != "on":
        return None
    e = read_json(pass_path(key))
    return e if e and e.get("unit") == unit else None


def record(unit, key, secs, lines=None):
    if MODE == "off":
        return
    write_json(pass_path(key), {"unit": unit, "key": key, "commit": commit(),
                                "date": datetime.datetime.now().strftime("%Y-%m-%d %H:%M"),
                                "secs": int(secs), "lines": lines or []})
    prune()


def log(what, unit):
    p = os.environ.get("CI_CACHE_LOG")
    if p:
        with open(p, "a") as f:
            f.write(f"{what} {unit}\n")


def progress_cached(typ, key):
    """make watch (tools/progress.sh cached): the item is done without a
    run, and its seconds are no estimate for one."""
    j = os.environ.get("PROGRESS_JOB")
    if j and os.path.isdir(j):
        try:
            with open(os.path.join(j, "events"), "a") as f:
                f.write(f"{int(time.time())} cached {typ} {key}\n")
        except OSError:
            pass


def report_cached(unit, e):
    say(f"{unit}: cached ({e.get('commit', '?')}, {e.get('date', '?')}; {e.get('secs', 0)} s saved)")
    log("cached", unit)


# --- hdl: the Clash runs ------------------------------------------------------

def hdl_key(dirs, files, text, unit="hdl"):
    k = Key(unit)
    for d in dirs.split():
        k.path(d)
    for f in files.split():
        k.path(f)
    k.add("text", hashlib.sha256(text.encode()).hexdigest())
    return k.digest()


def hdl_meta(key):
    return read_json(os.path.join(CACHE, "hdl", key + ".json"))


def hdl_restore(key, d, exclude):
    if MODE != "on":
        return 1
    meta = hdl_meta(key)
    tar = os.path.join(CACHE, "hdl", key + ".tar")
    if not meta or not os.path.isfile(tar):
        return 1
    if os.path.isdir(d) and hash_dir(d, exclude) == meta["verilog"]:
        say(f"{os.path.relpath(d, ROOT)}: Verilog cached ({meta['commit']}, {meta['date']}), already in place")
        return 0
    shutil.rmtree(d, ignore_errors=True)
    os.makedirs(d)
    with tarfile.open(tar) as t:
        t.extractall(d, filter="data")
    say(f"{os.path.relpath(d, ROOT)}: Verilog cached ({meta['commit']}, {meta['date']}), restored; no Clash run")
    return 0


def hdl_store(key, d, exclude):
    if MODE == "off":
        return 0
    os.makedirs(os.path.join(CACHE, "hdl"), exist_ok=True)
    fd, tmp = tempfile.mkstemp(dir=os.path.join(CACHE, "hdl"), prefix=".tmp-")
    os.close(fd)
    with tarfile.open(tmp, "w") as t:
        for f in sorted(os.listdir(d)):
            if f not in exclude and not f.startswith("."):
                t.add(os.path.join(d, f), arcname=f)
    os.replace(tmp, os.path.join(CACHE, "hdl", key + ".tar"))
    write_json(os.path.join(CACHE, "hdl", key + ".json"),
               {"key": key, "verilog": hash_dir(d, exclude), "exclude": list(exclude),
                "commit": commit(), "date": datetime.datetime.now().strftime("%Y-%m-%d %H:%M")})
    prune()
    return 0


# --- bit: the Vivado runs (make bit) -------------------------------------------
#
# Keyed on everything Vivado reads (the Makefile passes it: the Clash
# output directory whole, memory images included, the connector, the
# board's Tcl, wrappers and XDC, the top and the Vivado installation).
# Only a build that passed build.tcl's gates is stored (bit-run calls
# bit-store after it): its .bit, reports/ and build.log, and the WNS and
# WHS it printed.

BIT_FILES = ("reports", "build.log")


def bit_meta(key):
    return read_json(os.path.join(CACHE, "bit", key + ".json"))


def bit_restore(key, vdir):
    if MODE != "on":
        return 1
    meta = bit_meta(key)
    tar = os.path.join(CACHE, "bit", key + ".tar")
    if not meta or not os.path.isfile(tar):
        return 1
    os.makedirs(vdir, exist_ok=True)
    for f in os.listdir(vdir):
        if f.endswith(".bit"):
            os.remove(os.path.join(vdir, f))
    shutil.rmtree(os.path.join(vdir, "reports"), ignore_errors=True)
    with tarfile.open(tar) as t:
        t.extractall(vdir, filter="data")
    print(f"make bit: cached ({meta['commit']}, {meta['date']}; {meta.get('timing', 'timing in reports/')}): "
          f"{meta['bit']} restored, no Vivado run (CI_CACHE=off builds it)", flush=True)
    log("cached", f"bit:{os.path.relpath(vdir, ROOT)}")
    return 0


def bit_store(key, vdir):
    if MODE == "off":
        return 0
    bits = sorted(f for f in os.listdir(vdir) if f.endswith(".bit"))
    if len(bits) != 1:
        say(f"bit-store: expected one .bit in {vdir}, found {bits}: not cached")
        return 0
    timing = ""
    logf = os.path.join(vdir, "build.log")
    if os.path.isfile(logf):
        m = re.findall(r"build\.tcl: (WNS = \S+ ns, WHS = \S+ ns)", open(logf, errors="replace").read())
        timing = m[-1] if m else ""
    d = os.path.join(CACHE, "bit")
    os.makedirs(d, exist_ok=True)
    fd, tmp = tempfile.mkstemp(dir=d, prefix=".tmp-")
    os.close(fd)
    with tarfile.open(tmp, "w") as t:
        for f in bits + [x for x in BIT_FILES if os.path.exists(os.path.join(vdir, x))]:
            t.add(os.path.join(vdir, f), arcname=f)
    os.replace(tmp, os.path.join(d, key + ".tar"))
    write_json(os.path.join(d, key + ".json"),
               {"key": key, "bit": bits[0], "timing": timing, "commit": commit(),
                "date": datetime.datetime.now().strftime("%Y-%m-%d %H:%M")})
    prune()
    return 0


# --- suites -------------------------------------------------------------------

def cabal_packages():
    """name -> (dir, local build-depends) for every local package."""
    pkgs = {}
    for top in ("platform", "cpus", "systems", "demos"):
        base = os.path.join(ROOT, top)
        cands = [base] + ([os.path.join(base, x) for x in sorted(os.listdir(base))] if os.path.isdir(base) else [])
        for d in cands:
            if not os.path.isdir(d):
                continue
            for f in os.listdir(d):
                if f.endswith(".cabal"):
                    text = open(os.path.join(d, f)).read()
                    deps = set()
                    for m in re.finditer(r"build-depends:\s*((?:.*\n)(?:\s{4,}.*\n)*)", text):
                        for part in m.group(1).split(","):
                            w = re.match(r"\s*([A-Za-z0-9-]+)", part)
                            if w:
                                deps.add(w.group(1))
                    pkgs[f[:-6]] = (os.path.relpath(d, ROOT), deps)
    return pkgs


def suite_key(d, suite):
    pkgs = cabal_packages()
    rel = os.path.relpath(os.path.realpath(d), ROOT)
    here = [n for n, (pd, _) in pkgs.items() if pd == rel]
    if not here:
        return None
    seen, todo = set(), list(here)
    while todo:
        n = todo.pop()
        if n in seen or n not in pkgs:
            continue
        seen.add(n)
        todo.extend(pkgs[n][1])
    tier = "full" if os.environ.get("EMU_FULL_TESTS") == "1" else "quick"
    k = Key(f"suite:{suite}:{tier}")
    k.add("env", f"EMU_FULL_TESTS={os.environ.get('EMU_FULL_TESTS', '')} NES_CORE_ROMS={os.environ.get('NES_CORE_ROMS', '')}")
    builds = set()
    for n in sorted(seen):
        pd = pkgs[n][0]
        # the package's files a suite can read: not the Zig, C++ and make
        # files beside them (platform/ holds the PS app and the harness)
        k.add(pd, hash_dir(os.path.join(ROOT, pd), skip_ext=SUITE_SKIP, skip_dirs=SUITE_SKIP_DIRS))
        for dd, dirs, files in os.walk(os.path.join(ROOT, pd)):
            dirs[:] = [x for x in dirs if x not in SKIP_NAMES]
            for f in files:
                if f.endswith(".hs"):
                    for m in re.finditer(r'"(?:\.\./)+(build/[^"$]+?)/?"', open(os.path.join(dd, f), errors="replace").read()):
                        builds.add(m.group(1))
    for b in sorted(builds):
        p = os.path.join(ROOT, b)
        k.add(b, ("dir " + hash_dir(p, skip_ext=INTERMEDIATE)) if os.path.isdir(p) else hash_path(b))
    for f in ["flake.lock", "cabal.project", "tools/fetch-roms.sh"] + sorted(
            os.path.relpath(p, ROOT) for p in _glob("tools", r"roms-.*\.sha256$")):
        k.path(f)
    return k.digest()


def _glob(d, pat):
    full = os.path.join(ROOT, d)
    return [os.path.join(full, f) for f in sorted(os.listdir(full)) if re.match(pat, f)] if os.path.isdir(full) else []


# --- chains -------------------------------------------------------------------

def make_vars():
    """The variable assignments make passed us (MAKEFLAGS after " -- "):
    BOARD=zedboard in make ci's pool."""
    mf = os.environ.get("MAKEFLAGS", "")
    if " -- " in mf:
        return shlex.split(mf.split(" -- ", 1)[1])
    if mf.startswith("-- "):
        return shlex.split(mf[3:])
    return []


def dry(chain):
    """The chain's commands, as make -n -B prints them, independent of what
    is built: the Verilog (CLASH_V, MANIFEST, CLK_WIZ_TCL) reaches the key
    as its hash, the PS app isn't an input of layer 4 (PS_ELF), and the
    Verilator's own makefile isn't entered (VL_MAKE)."""
    env = dict(os.environ, MAKEFLAGS="", CI_CACHE="on")
    env.pop("MAKELEVEL", None)
    cmd = ["make", "-j1", "-n", "-B", "--no-print-directory", "VL_MAKE=:", "CLASH_V=", "MANIFEST=",
           "CLK_WIZ_TCL=", "PS_ELF="] + make_vars() + [f"ci-{chain}"]
    r = subprocess.run(cmd, cwd=ROOT, env=env, capture_output=True, text=True)
    if r.returncode != 0:
        return None, f"make -n failed: {r.stderr.strip().splitlines()[-1:]}"
    text = r.stdout.replace(ROOT, "@ROOT@")
    lines, cur = [], ""
    for raw in text.splitlines():
        cur += raw
        if cur.endswith("\\"):
            cur = cur[:-1] + " "
            continue
        if not cur.lstrip().startswith("#"):
            lines.append(cur)
        cur = ""
    if cur:
        lines.append(cur)
    return lines, None


LAST_LINES = []  # the last chain key's lines (chain-key --show)
USED = []  # the last chain's models it doesn't build: (system, board)


def top_of(build):
    """build/arty-z7-20/gb -> (gb, arty-z7-20); build/gb -> (gb, zedboard)"""
    parts = build.split("/")
    return (parts[2], parts[1]) if len(parts) == 3 and parts[1] in BOARDS else (parts[1], "zedboard")
TOKEN = re.compile(r"[A-Za-z0-9_@./+-]+")


def clash_line_keys(clash):
    """{build dir: key} of the Clash runs in a chain's dry-run lines."""
    keys = {}
    for l in clash:
        m = re.search(r'hdl-key --dirs "([^"]*)" --files "([^"]*)" --text "([^"]*)"', l)
        d = re.search(r"-fclash-hdldir (\S+)/verilog", l)
        if m and d:
            keys[d.group(1)] = hdl_key(*(g.replace("@ROOT@", ROOT) for g in m.groups()))
    return keys


def chain_key(chain, plan):
    """(key, None) or (None, why it's unknown). plan: never run Clash."""
    lines, why = dry(chain)
    if lines is None:
        return None, why
    clash = [l for l in lines if "clash --verilog" in l]
    tops = []
    for l in clash:
        m = re.search(r"-fclash-hdldir (\S+)/verilog", l)
        if not m:
            return None, "a Clash command without -fclash-hdldir"
        tops.append(m.group(1))
    refs = set(re.findall(r"(?:@ROOT@/)?(build/(?:[^/\s\"']+/)?[^/\s\"']+)/verilog\b", "\n".join(lines)))
    # models the chain re-simulates without building them (ci-demos): keyed
    # on their Verilog as the others; chain() brings them up to date first
    USED[:] = [top_of(b) for b in sorted(refs - set(tops))]
    if not tops and not refs:
        return None, "no model"
    mutation = os.environ.get("CI_CACHE_MUTATION", "")
    k = Key(f"chain:{chain}")
    k.add("commands", hashlib.sha256("\n".join(lines).encode()).hexdigest())
    # each Clash run's key from its own hdl-key command (make hdl's, as the
    # dry run expanded it: a BUILD=, MOVIES=0 ... override is keyed as it is
    # built); a model the chain only re-simulates, from make ci-hdl-key
    line_keys = clash_line_keys(clash)
    for b in sorted(set(tops) | refs):
        system, board = top_of(b)
        standard = b in (f"build/{board}/{system}", f"build/{system}")
        if b in line_keys:
            hk = [line_keys[b]]
        else:
            hk = subprocess.run(["make", "-s", "--no-print-directory", "ci-hdl-key", f"SYSTEM={system}", f"BOARD={board}"],
                                cwd=ROOT, env=dict(os.environ, MAKEFLAGS=""), capture_output=True, text=True)
            hk = hk.stdout.strip().splitlines()[-1:] if hk.returncode == 0 else []
        if not hk:
            return None, f"no hdl key for {system} on {board}"
        meta = hdl_meta(hk[0])
        if not meta and not plan and MODE != "off" and standard:
            # the Clash run now (stored by make hdl), so that a Verilog
            # identical to a cached one still saves the rest of the chain
            r = subprocess.run(["make", "--no-print-directory", "hdl", f"SYSTEM={system}", f"BOARD={board}"],
                               cwd=ROOT, env=dict(os.environ, CI_CACHE="on"))
            meta = hdl_meta(hk[0]) if r.returncode == 0 else None
        if not meta:
            return None, f"no cached Verilog for {system} on {board} (key {hk[0][:12]})"
        k.add(f"verilog {b}", meta["verilog"])
    scripts, gens, paths = set(), set(), set()
    for l in lines:
        for m in re.finditer(r"cabal run -v0 (\S+)", l):
            gens.add(m.group(1))
        if l in clash:
            continue  # its sources are the hdl key's, its output the Verilog hash
        for t in TOKEN.findall(l):
            for c in (t, t[2:] if t[:2] in ("-i", "-I", "-y") else None):
                if not c or "/" not in c:
                    continue
                c = c.replace("@ROOT@/", "").rstrip("/")
                if c.startswith("/") or c.startswith("..") or c in NOT_INPUTS or c.endswith(INTERMEDIATE):
                    continue
                parts = c.split("/")
                if parts[0] == "build" and (BUILD_OUTPUTS & set(parts) or not os.path.isfile(os.path.join(ROOT, c))):
                    continue  # in build/, only files made before the chain (test programs)
                if os.path.exists(os.path.join(ROOT, c)):
                    (scripts if c.endswith(".ps") else paths).add(c)
    for p in sorted(paths):
        k.path(p)
    if mutation != "no-ps-encoding":
        enc = ps_encode(sorted(scripts) + (["--fingerprint"] if any(" --ps " in l for l in lines) else []))
        if enc is None:
            return None, "ps-encode is missing or failed (make ci's ci-prep builds it)"
        for name, h in enc:
            k.add(f"ps {name}", h)
    for g in sorted(gens):
        b = list_bin(g)
        if not b:
            return None, f"no binary for the generator {g}"
        k.add(f"generator {g}", hash_file(b))
    for h in HARNESS:
        k.path(h)
    LAST_LINES[:] = k.lines
    return k.digest(), None


def ps_encode(args):
    exe = os.path.join(ROOT, "build", "host", "ps-encode")
    if not args:
        return []
    if not os.access(exe, os.X_OK):
        return None
    r = subprocess.run([exe] + args, cwd=ROOT, capture_output=True, text=True)
    if r.returncode != 0:
        return None
    out = []
    for l in r.stderr.splitlines():
        h, _, name = l.partition(" ")
        out.append((name, h))
    return out


def list_bin(exe):
    f = os.environ.get("CI_CACHE_LIST_BIN_FILE")
    if f:
        return f
    r = subprocess.run(["cabal", "list-bin", "-v0", exe], cwd=ROOT, capture_output=True, text=True)
    b = r.stdout.strip().splitlines()[-1:] if r.returncode == 0 else []
    return b[0] if b and os.path.isfile(b[0]) else None


def run_cmd(cmd):
    t0 = time.time()
    rc = subprocess.run(cmd).returncode
    return rc, time.time() - t0


def chain(name, cmd):
    unit = f"chain:{name}"
    if MODE == "off":
        return subprocess.run(cmd).returncode
    if MODE == "on":
        key, why = chain_key(name, plan=False)
        if key:
            e = cached(unit, key)
            if e:
                report_cached(f"ci-{name}", e)
                progress_cached("chain", name)
                return 0
            say(f"ci-{name}: run (no cached pass for its inputs)")
        else:
            say(f"ci-{name}: run ({why})")
        # the models it uses but doesn't build: as their Verilog is now (a
        # cached chain left its build directory as it was)
        for system, board in USED:
            subprocess.run(["make", "--no-print-directory", "hdl", f"SYSTEM={system}", f"BOARD={board}"],
                           cwd=ROOT, env=dict(os.environ, MAKEFLAGS=""))
    rc, secs = run_cmd(cmd)
    if rc == 0:
        key, why = chain_key(name, plan=True)
        if key:
            record(unit, key, secs)
        else:
            say(f"ci-{name}: passed, not cached ({why})")
    log("run", f"ci-{name}")
    return rc


def summary():
    p = os.environ.get("CI_CACHE_LOG")
    if not p or not os.path.isfile(p):
        return
    n = {"run": 0, "cached": 0}
    for l in open(p):
        w = l.split(" ", 1)[0]
        if w in n:
            n[w] += 1
    print(f"make ci: {n['run']} run, {n['cached']} cached (CI_CACHE={MODE})")


def main(argv):
    if not argv:
        print(__doc__)
        return 2
    c, a = argv[0], argv[1:]
    if c == "hdl-key":
        opts = {"--dirs": "", "--files": "", "--text": ""}
        i = 0
        while i < len(a):
            opts[a[i]] = a[i + 1]
            i += 2
        print(hdl_key(opts["--dirs"], opts["--files"], opts["--text"]))
        return 0
    if c == "bit-key":
        opts = {"--dirs": "", "--files": "", "--text": ""}
        i = 0
        while i < len(a):
            opts[a[i]] = a[i + 1]
            i += 2
        print(hdl_key(opts["--dirs"], opts["--files"], opts["--text"], unit="bit"))
        return 0
    if c == "bit-restore":
        return bit_restore(a[0], a[1])
    if c == "bit-store":
        return bit_store(a[0], a[1])
    if c == "hdl-restore":
        return hdl_restore(a[0], a[1], set(a[2:]))
    if c == "hdl-store":
        return hdl_store(a[0], a[1], set(a[2:]))
    if c == "hdl-seed":  # tools/test_ci_cache.sh: a Clash run's result, without Clash
        write_json(os.path.join(CACHE, "hdl", a[0] + ".json"),
                   {"key": a[0], "verilog": a[1], "exclude": [], "commit": "test", "date": "test"})
        return 0
    if c == "suite-key":
        k = suite_key(a[0], a[1])
        if not k:
            return 1
        print(k)
        return 0
    if c == "check":
        e = cached(a[0], a[1])
        if e:
            report_cached(a[0], e)
            return 0
        return 1
    if c == "pass":
        record(a[0], a[1], float(a[2]))
        return 0
    if c == "run":
        unit, key, cmd = a[0], a[1], a[3:]
        e = cached(unit, key)
        if e:
            report_cached(unit, e)
            return 0
        rc, secs = run_cmd(cmd)
        if rc == 0:
            record(unit, key, secs)
        log("run", unit)
        return rc
    if c == "chain":
        return chain(a[0], a[2:])
    if c == "chain-hdl-keys":
        lines, why = dry(a[0])
        if lines is None:
            print(f"unknown: {why}")
            return 1
        for d, key in sorted(clash_line_keys([l for l in lines if "clash --verilog" in l]).items()):
            print(d, key)
        return 0
    if c == "chain-key":
        key, why = chain_key(a[0], plan="--plan" in a)
        if not key:
            print(f"unknown: {why}")
            return 1
        if "--show" in a:
            print("\n".join(LAST_LINES))
        print(key)
        return 0
    if c == "summary":
        summary()
        return 0
    print(__doc__)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
