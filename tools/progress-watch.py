#!/usr/bin/env python3
"""make watch / make progress: every running make ci, make hdl, make sim
and make bit on this machine, from every worktree, with progress bars,
then the queue (tools/queue.sh).

    tools/progress-watch.py          redraw every second (Ctrl-C ends)
    tools/progress-watch.py --once   print once (for scripts and agents)
    tools/progress-watch.py --untracked   warn (stderr) about heavy work no
                                     job tracks (make ci and make bit do)

Reads the jobs tools/progress.sh writes ($PROGRESS_DIR/jobs/*/meta and
events) and the durations of earlier runs ($PROGRESS_DIR/history: the
last few real runs per item; the estimate is their median, a cached
pass's seconds left out). How exact each bar is (since 2026-11-01, side
Q's request, a job's bar is time: elapsed / (elapsed + its ETA)):
  - make ci: the steps still to come at their estimates; the running
    step's suites or chains scheduled on their pool (CI_JOBS chains,
    TEST_JOBS suites at a time; the CI_AFTER chains after it, one at a
    time), the running ones by what's left of them. A suite or chain
    served from the cache counts done.
  - a simulation: frames written / frames asked (exact).
  - Clash: elapsed / its estimate for that top and board (it prints
    nothing while it normalises).
  - Vivado: its phase from build.log (setup, synth, opt, place, phys_opt,
    route, reports, bitstream), and elapsed / its estimate.
An item past its estimate still has a tenth of (estimate + elapsed) to go.
Without a time from an earlier run, a default stands in (marked "~?");
an ETA that is mostly defaults shows as a range ("ETA ~20-50m") or
"ETA ?". The ETA always fits the line; the elapsed time and the counts
give way first.

Memory (the user, 2026-10-18): the machine's in use and available, and
each job's (and each untracked process's) whole process tree, as PSS
(shared pages split among their users, so a tree's sum is what it holds)
where /proc lets us read it, else RSS, with its biggest processes by
name. The remote worker (`make ci-remote`, the user's ~/.claude/CLAUDE.md)
gets a section of its own after this machine's: this script run there
over ssh (WATCH_REMOTE, default "worker"; empty: none), every
WATCH_REMOTE_EVERY seconds (5) in make watch.

    tools/progress-watch.py --label NAME   the machine's heading (default
                                     "desktop"; the worker's run: "worker")
"""
import os
import re
import shutil
import socket
import subprocess
import sys
import threading
import time

PDIR = os.environ.get("PROGRESS_DIR") or os.path.join(
    os.environ.get("XDG_CACHE_HOME") or os.path.expanduser("~/.cache"),
    "emulation_clash", "progress")
SELF = os.path.abspath(__file__)
QUEUE = os.environ.get("WATCH_QUEUE") or os.path.join(os.path.dirname(SELF), "queue.sh")
REMOTE = os.environ.get("WATCH_REMOTE", "worker")
REMOTE_EVERY = int(os.environ.get("WATCH_REMOTE_EVERY", "5"))
# the worker's queue.sh (its copies of the checkouts, wrun's ~/work/...)
REMOTE_QUEUE = os.environ.get("WATCH_REMOTE_QUEUE", "$HOME/work/Documents/emulation_clash/tools/queue.sh")
DEFAULT = {"suite": 60, "chain": 300, "clash": 180, "sim": 60, "bit": 900,
           "step:prep": 300, "step:tests": 900, "step:layer4": 2400, "ci": 3600}
BIT_PHASES = [("setup", None), ("synth", "synth_design"), ("opt", "opt_design"),
              ("place", "place_design"), ("phys_opt", "phys_opt_design"),
              ("route", "route_design"), ("reports", "report_"),
              ("bitstream", "write_bitstream")]


def started(pid):
    try:
        with open(f"/proc/{pid}/stat") as f:
            s = f.read()
    except OSError:
        return None
    return s.rsplit(") ", 1)[1].split()[19]


def read_kv(path):
    d = {}
    try:
        with open(path) as f:
            for line in f:
                k, _, v = line.rstrip("\n").partition("=")
                d[k] = v
    except OSError:
        pass
    return d


def load_history():
    """{type:key[@tier]: [secs, ...]}, oldest first (tools/progress.sh keeps
    the last few runs' times on a line; older files have one)."""
    h = {}
    try:
        with open(os.path.join(PDIR, "history")) as f:
            for line in f:
                w = line.split()
                if len(w) >= 2 and all(v.isdigit() for v in w[1:]):
                    h[w[0]] = [int(v) for v in w[1:]]
    except OSError:
        pass
    return h


# A time below this is a pass served from make ci's result cache (from
# before tools/progress.sh left those out of the history), not a run's.
CACHED_BELOW = {"chain": 6, "bit": 30}
RUNS = 5   # the median of the last RUNS real runs is the estimate


def median(xs):
    xs = sorted(xs)
    n = len(xs)
    return xs[n // 2] if n % 2 else (xs[n // 2 - 1] + xs[n // 2]) / 2


def expected(hist, typ, key, tier=""):
    """(seconds, runs it comes from: 0 = a default) for one item, in the
    job's test tier (a full run's times are kept apart: its suites take
    several times as long; ALL=1's chains fall back to the plain run's):
    the median of its last real runs."""
    tiers = [tier]
    if tier.endswith("all"):
        tiers.append(tier[:-3].rstrip("-"))
    for t in tiers:
        k = f"{typ}:{key}" + (f"@{t}" if t else "")
        runs = [s for s in hist.get(k, []) if s >= CACHED_BELOW.get(typ, 0)][-RUNS:]
        if runs:
            return max(median(runs), 1), len(runs)
    k = f"{typ}:{key}"
    return DEFAULT.get(k, DEFAULT.get(typ, 120)), 0


def remaining(exp, el):
    """An active item's remaining seconds: its expected time less what has
    passed, but never nothing: one past its estimate needs a tenth of
    (estimate + elapsed) more (37 min into a 4 s estimate: ~4 min)."""
    return max(exp - el, 0.1 * (exp + el))


def pool_size(typ):
    """How many items of a type run at once: make ci's chains CI_JOBS (3),
    tools/run-tests.sh's suites TEST_JOBS (a quarter of the cores)."""
    if typ == "chain":
        v = os.environ.get("CI_JOBS", "3")
    elif typ == "suite":
        v = os.environ.get("TEST_JOBS", str(max((os.cpu_count() or 4) // 4, 1)))
    else:
        v = "1"
    return int(v) if v.isdigit() and int(v) > 0 else 1


def eta_text(eta, unknown):
    """"ETA ~12:30", "ETA ~1h05"; a range when much of it comes from
    defaults ("ETA ~20-50m", "ETA ~1-2.5h"); "ETA ?" when nearly all."""
    if eta is None:
        return "ETA ?"
    share = unknown / eta if eta > 0 else 0
    if share >= 0.75:
        return "ETA ?"
    if share >= 0.25:
        lo, hi = eta - unknown / 2, eta + unknown
        if hi < 3600:
            return f"ETA ~{max(int(lo // 60), 1)}-{max(-int(-hi // 60), 2)}m"
        lo_h, hi_h = int(lo // 1800) / 2, -int(-hi // 1800) / 2
        return f"ETA ~{lo_h:g}-{hi_h:g}h" if lo_h > 0 else f"ETA <{hi_h:g}h"
    s = int(eta)
    return f"ETA ~{s // 3600}h{s % 3600 // 60:02}" if s >= 3600 else f"ETA ~{dur(s)}"


def dur(s):
    s = int(max(s, 0))
    h, m, sec = s // 3600, s % 3600 // 60, s % 60
    return f"{h}:{m:02}:{sec:02}" if h else f"{m}:{sec:02}"


def bar(p, width):
    p = min(max(p, 0.0), 1.0)
    full = int(p * width)
    return "█" * full + "░" * (width - full)


def progress_line(p, eta, counts, elapsed, width):
    """A job's bar line in `width` columns: "      ███░░  42%  ETA ~1h05
    suites 3/51 · chains 9/17  3:02:41 elapsed". The percentage and the
    ETA always show; the elapsed time goes first, then the counts, then
    the bar shrinks (to 10, or nothing in a very narrow terminal)."""
    head = f"{int(p * 100):3}%  {eta}"
    for extra in ([counts, elapsed], [counts], []):
        tail = "  ".join([head] + [e for e in extra if e])
        room = width - 6 - 1 - len(tail)
        if room >= 10:
            return f"      {bar(p, min(room, max(width - 60, 20)))} {tail}"
    return f"      {bar(p, room)} {tail}" if room > 0 else "  " + tail


def frames_written(d):
    try:
        return sum(1 for n in os.listdir(d) if n.startswith("frame_") and n.endswith(".png"))
    except OSError:
        return 0


def bit_phase(log):
    idx = 0
    try:
        with open(log, errors="replace") as f:
            for line in f:
                if line.startswith("Command: "):
                    cmd = line[9:]
                    for i, (_, marker) in enumerate(BIT_PHASES):
                        if marker and cmd.startswith(marker):
                            idx = max(idx, i)
    except OSError:
        pass
    return idx


class Job:
    def __init__(self, path, hist, now):
        self.meta = read_kv(os.path.join(path, "meta"))
        self.hist, self.now = hist, now
        self.tier = self.meta.get("tier", "")
        self.plans = {}      # type -> [names]
        self.done = {}       # (type, key) -> secs
        self.cached = set()  # (type, key): served from make ci's result cache
        self.serial = set()  # (type, key): run one at a time after the pool
        self.failed = []     # (type, key)
        self.active = {}     # (type, key) -> (t, detail)
        try:
            with open(os.path.join(path, "events")) as f:
                lines = f.read().splitlines()
        except OSError:
            lines = []
        for line in lines:
            w = line.split()
            if len(w) < 3:
                continue
            t, what, typ = int(w[0]), w[1], w[2]
            if what == "plan":
                self.plans.setdefault(typ, [])
                self.plans[typ] += [n for n in w[3:] if n not in self.plans[typ]]
            elif what == "serial":
                self.serial.update((typ, n) for n in w[3:])
            elif what == "cached" and len(w) >= 4:
                self.cached.add((typ, w[3]))
                self.done.setdefault((typ, w[3]), 0)
            elif what == "start" and len(w) >= 4:
                self.active[(typ, w[3])] = (t, " ".join(w[4:]))
            elif what in ("done", "fail") and len(w) >= 4:
                self.active.pop((typ, w[3]), None)
                secs = int(w[4]) if len(w) > 4 and w[4].isdigit() else 0
                if what == "done":
                    self.done[(typ, w[3])] = secs
                else:
                    self.failed.append((typ, w[3]))

    # The estimates (side Q's request, 2026-11-01): in seconds still to go,
    # each as (seconds, of them from defaults). A done or cached item needs
    # none; an unstarted one its whole expected time (the median of its
    # last real runs); an active one what's left of it, never nothing
    # (remaining()). A pool (make ci's chains, make test's suites) is
    # scheduled on its workers, its running items first; the job's
    # fraction is time: elapsed / (elapsed + still to go), so it can't
    # read 99% with most of its steps not started.
    def sim_frames(self, detail):
        parts = detail.split()
        if len(parts) == 2 and parts[1].isdigit() and int(parts[1]) > 0:
            return min(frames_written(parts[0]) / int(parts[1]), 1.0)
        return None

    def item_left(self, typ, key):
        if (typ, key) in self.done:
            return 0.0, 0.0
        exp, runs = expected(self.hist, typ, key, self.tier)
        if (typ, key) not in self.active:
            return float(exp), (0.0 if runs else float(exp))
        t0, detail = self.active[(typ, key)]
        el = self.now - t0
        if typ == "step":
            child = {"tests": "suite", "layer4": "chain"}.get(key)
            if child and self.plans.get(child):
                return self.pool_left(child)
        if typ == "sim":   # frames are exact: the rate so far
            f = self.sim_frames(detail)
            if f is not None and f > 0.02:
                return el * (1 - f) / f, 0.0
        left = remaining(exp, el)
        return left, (0.0 if runs else left)

    def pool_left(self, typ):
        names = [n for n in self.plans.get(typ, []) if (typ, n) not in self.done]
        pool = [n for n in names if (typ, n) not in self.serial]
        running = [self.item_left(typ, n) for n in pool if (typ, n) in self.active]
        queued = [self.item_left(typ, n) for n in pool if (typ, n) not in self.active]
        workers = sorted([r for r, _ in running] + [0.0] * max(pool_size(typ) - len(running), 0))
        for r, _ in queued:
            workers[0] += r
            workers.sort()
        after = [self.item_left(typ, n) for n in names if (typ, n) in self.serial]
        left = (workers[-1] if workers else 0.0) + sum(r for r, _ in after)
        tot = sum(r for r, _ in running + queued + after)
        unk = sum(u for _, u in running + queued + after)
        return left, (left * unk / tot if tot else 0.0)

    def item_fraction(self, typ, key):
        """Fraction of one item done (an active item by its own measure)."""
        if (typ, key) in self.done:
            return 1.0
        if (typ, key) not in self.active:
            return 0.0
        t0, detail = self.active[(typ, key)]
        if typ == "sim":
            f = self.sim_frames(detail)
            if f is not None:
                return f
        el = self.now - t0
        left, _ = self.item_left(typ, key)
        return el / (el + left) if el + left > 0 else 0.0

    def counts(self, typ, label):
        names = self.plans.get(typ, [])
        n = sum(1 for x in names if (typ, x) in self.done)
        c = sum(1 for x in names if (typ, x) in self.cached)
        return f"{label} {n}/{len(names)}" + (f" ({c} cached)" if c else "")

    def overall(self):
        """(fraction, eta seconds or None, of it from defaults, counts text)."""
        el = self.now - int(self.meta.get("since", self.now))
        steps = self.plans.get("step")
        if steps:
            # steps run one after another (CI_OVERLAP=1: tests beside
            # layer 4, so the running ones count as the longest of them)
            active = [self.item_left("step", s) for s in steps if ("step", s) in self.active]
            eta = max((r for r, _ in active), default=0.0)
            unk = max((u for _, u in active), default=0.0)
            for s in steps:
                if ("step", s) in self.active or ("step", s) in self.done:
                    continue
                child = {"tests": "suite", "layer4": "chain"}.get(s)
                r, u = self.pool_left(child) if child and self.plans.get(child) else self.item_left("step", s)
                eta, unk = eta + r, unk + u
            counts = " · ".join(self.counts(t, l) for t, l in (("suite", "suites"), ("chain", "chains"))
                                if self.plans.get(t))
        elif self.plans:   # e.g. make test: its suites
            typ = next(iter(self.plans))
            eta, unk = self.pool_left(typ)
            counts = self.counts(typ, typ + "s")
        else:
            kind, key = self.meta.get("kind", "?"), self.meta.get("key", "?")
            if (kind, key) in self.active:
                eta, unk = self.item_left(kind, key)
            else:   # between its start and its first event
                exp, runs = expected(self.hist, kind, key, self.tier)
                eta, unk = remaining(exp, el), (0.0 if runs else remaining(exp, el))
            counts = ""
        return (el / (el + eta) if el + eta > 0 else 0.0), eta, unk, counts

    def describe(self, typ, key, t0, detail, width):
        el = self.now - t0
        exp, runs = expected(self.hist, typ, key, self.tier)
        of = f"{dur(el)} of {'~' if runs else '~?'}{dur(exp)}"
        if typ == "sim":
            parts = detail.split()
            if len(parts) == 2 and parts[1].isdigit():
                n, total = frames_written(parts[0]), int(parts[1])
                return f"sim   {key:22} {bar(n / max(total, 1), 12)} frame {n}/{total}"
        if typ == "bit":
            i = bit_phase(detail)
            return f"bit   {key:22} {bar(self.item_fraction(typ, key), 12)} {BIT_PHASES[i][0]} ({i + 1}/{len(BIT_PHASES)}), {of}"
        if typ == "clash":
            return f"clash {key:22} {bar(self.item_fraction(typ, key), 12)} {of}"
        label = {"chain": "ci-", "suite": "", "step": "step "}.get(typ, typ + " ")
        return f"{typ:5} {label + key:22} {bar(self.item_fraction(typ, key), 12)} {of}"

    def render(self, width, procs=None):
        m = self.meta
        p, eta, unk, counts = self.overall()
        since = int(m.get("since", self.now))
        head = f"{m.get('kind', '?'):5} {m.get('worktree', '?')} ({m.get('branch', '?')})" + \
            (f"  [{self.tier} tier]" if self.tier else "")
        total, parts = procs.usage(int(m["pid"])) if procs and m.get("pid", "").isdigit() else (0, [])
        if procs:
            head += f"  RAM {gb(total)}"
        out = [head, progress_line(p, eta_text(eta, unk), counts, f"{dur(self.now - since)} elapsed", width)]
        big = [f"{name} {gb(k)}" for name, k in parts[:5] if k >= 102400]
        if big:
            out.append("        RAM: " + ", ".join(big))
        top = (m.get("kind"), m.get("key"))
        for (typ, key), (t0, detail) in sorted(self.active.items(), key=lambda kv: kv[1][0]):
            # the job's own line only when it has no parts to show (a lone
            # make bit: its phase; Clash; a simulation: its frames)
            if ((typ, key) == top and self.plans) or typ == "step":
                continue
            out.append("        " + self.describe(typ, key, t0, detail, width))
        if self.failed:
            out.append("        FAILED: " + ", ".join(f"{t} {k}" for t, k in self.failed))
        return out


# Heavy processes that no job of tools/progress.sh started: from a branch
# without the tracking (it reached main 2026-10-08), or run by hand. They
# hold memory the queue doesn't know about, so they are shown, and make
# ci / make bit warn about them (--untracked).
def proc_stat(pid):
    """(ppid, start ticks) of a process, or None."""
    try:
        with open(f"/proc/{pid}/stat") as f:
            s = f.read().rsplit(") ", 1)[1].split()
        return int(s[1]), int(s[19])
    except (OSError, IndexError, ValueError):
        return None


def heavy_kind(argv):
    """What a process is, if it's heavy work, else None."""
    if not argv:
        return None
    name = os.path.basename(argv[0])
    if name == "vivado" and "-mode" in argv:
        return "vivado"
    if name == "clash" and "--verilog" in argv:
        return "clash"
    if name in ("vga-sim", "arty-sim"):
        return "sim"
    if "/dist-newstyle/" in argv[0] and "/t/" in argv[0]:
        return "suite " + name
    return None


def worktree_of(pid, argv):
    for path in (os.path.realpath(f"/proc/{pid}/cwd"), argv[0]):
        m = re.search(r"/\.claude/worktrees/([^/]+)", path)
        if m:
            return m.group(1)
        if "/emulation_clash" in path:
            return "main checkout"
    return "?"


# A process's age from its start (/proc/<pid>/stat field 22, clock ticks
# since the host booted), measured against this script's own start: in an
# LXC container (the worker) /proc/uptime is the container's (lxcfs) while
# the start times count from the host's boot, so uptime - start came out
# negative there and every untracked process read 0:00 (side Q, 2026-11-01).
_T0 = time.time()
_SELF = None


def age(start_ticks):
    """Seconds since a process with this start time began, or None."""
    global _SELF
    try:
        tck = os.sysconf("SC_CLK_TCK")
    except (OSError, ValueError):
        return None
    if _SELF is None:
        st = proc_stat(os.getpid())
        _SELF = st[1] if st else False
    if _SELF is not False:
        return (_SELF - start_ticks) / tck + (time.time() - _T0)
    try:
        with open("/proc/uptime") as f:
            return float(f.read().split()[0]) - start_ticks / tck
    except (OSError, ValueError, IndexError):
        return None


def untracked(job_pids):
    """[(pid, kind, worktree, seconds)] of heavy processes outside every job."""
    found = []
    for d in os.listdir("/proc"):
        if not d.isdigit():
            continue
        try:
            with open(f"/proc/{d}/cmdline", "rb") as f:
                argv = [a.decode(errors="replace") for a in f.read().split(b"\0") if a]
        except OSError:
            continue
        kind = heavy_kind(argv)
        if not kind:
            continue
        st = proc_stat(d)
        if not st:
            continue
        # tracked if a job's process is among its ancestors
        p, seen, tracked = int(d), 0, False
        while p > 1 and seen < 64:
            if p in job_pids:
                tracked = True
                break
            ps = proc_stat(p)
            if not ps:
                break
            p, seen = ps[0], seen + 1
        if not tracked:
            found.append((int(d), kind, worktree_of(d, argv), age(st[1]) or 0))
    return sorted(found, key=lambda x: -x[3])


def gb(kib):
    return f"{kib / 1048576:.1f} GB" if kib >= 102400 else f"{kib // 1024} MB"


def machine_memory():
    """(in use, total) KiB, from /proc/meminfo (in use = total - available)."""
    m = {}
    try:
        with open("/proc/meminfo") as f:
            for line in f:
                k, _, v = line.partition(":")
                m[k] = int(v.split()[0])
    except (OSError, ValueError, IndexError):
        return None
    if "MemTotal" not in m or "MemAvailable" not in m:
        return None
    return m["MemTotal"] - m["MemAvailable"], m["MemTotal"]


class Procs:
    """This machine's processes: parents, names, and (read on demand) each
    one's memory."""

    def __init__(self):
        self.children, self.argv = {}, {}
        self._mem = {}
        for d in os.listdir("/proc"):
            if not d.isdigit():
                continue
            st = proc_stat(d)
            if st:
                self.children.setdefault(st[0], []).append(int(d))

    def tree(self, pid):
        out, todo = [], [pid]
        while todo:
            p = todo.pop()
            out.append(p)
            todo += self.children.get(p, [])
        return out

    def mem(self, pid):
        """KiB: PSS where readable (our own processes), else RSS."""
        if pid not in self._mem:
            kib = 0
            try:
                with open(f"/proc/{pid}/smaps_rollup") as f:
                    for line in f:
                        if line.startswith("Pss:"):
                            kib = int(line.split()[1])
                            break
            except (OSError, ValueError, IndexError):
                try:
                    with open(f"/proc/{pid}/statm") as f:
                        kib = int(f.read().split()[1]) * (os.sysconf("SC_PAGE_SIZE") // 1024)
                except (OSError, ValueError, IndexError):
                    pass
            self._mem[pid] = kib
        return self._mem[pid]

    def name(self, pid):
        try:
            with open(f"/proc/{pid}/cmdline", "rb") as f:
                argv = [a.decode(errors="replace") for a in f.read().split(b"\0") if a]
        except OSError:
            return "?"
        if not argv:
            return "?"
        kind = heavy_kind(argv)
        if kind:
            return kind.split()[-1] if kind.startswith("suite ") else kind
        name = os.path.basename(argv[0])
        # interpreters: the script they run (a file, not a -c string)
        if re.match(r"(python3?(\.\d+)?|bash|sh|perl)$", name):
            script = next((a for a in argv[1:] if not a.startswith("-")), None)
            if script and "/" in script and os.path.isfile(script):
                name = os.path.basename(script)
        return name

    def usage(self, pid):
        """(total KiB, [(name, KiB)] biggest first) of pid's tree."""
        by = {}
        for p in self.tree(pid):
            k = self.mem(p)
            if k:
                n = self.name(p)
                by[n] = by.get(n, 0) + k
        return sum(by.values()), sorted(by.items(), key=lambda kv: -kv[1])


def untracked_lines(found, procs=None):
    if not found:
        return []
    out = ["untracked (heavy work no job tracks: a branch without the tracking, or run by hand):"]
    for pid, kind, wt, secs in found[:12]:
        ram = f"RAM {gb(procs.usage(pid)[0])}, " if procs else ""
        out.append(f"  {kind:22} {wt:22} {dur(secs)} running, {ram}pid {pid}")
    if len(found) > 12:
        out.append(f"  ... and {len(found) - 12} more")
    return out


def live_jobs():
    """[(path, meta)] of jobs whose process lives (dead ones removed)."""
    jobs_dir = os.path.join(PDIR, "jobs")
    try:
        names = sorted(os.listdir(jobs_dir))
    except OSError:
        return []
    out = []
    for n in names:
        path = os.path.join(jobs_dir, n)
        meta = read_kv(os.path.join(path, "meta"))
        if not meta.get("pid") or started(meta["pid"]) != meta.get("start"):
            shutil.rmtree(path, ignore_errors=True)   # its process is gone
            continue
        out.append((path, meta))
    return out


def arg(name, default=None):
    a = sys.argv[1:]
    return a[a.index(name) + 1] if name in a and a.index(name) + 1 < len(a) else default


def machine_head(label):
    """"desktop (host): memory 21.4 GB of 31.2 GB in use, 9.8 GB available"."""
    m = machine_memory()
    mem = f": memory {gb(m[0])} of {gb(m[1])} in use, {gb(m[1] - m[0])} available" if m else ""
    return f"== {label} ({socket.gethostname()}){mem}"


def snapshot(width=None, label=None):
    """This machine's section: its memory, jobs, untracked work, queue."""
    now = int(time.time())
    hist = load_history()
    width = width or shutil.get_terminal_size((100, 30)).columns
    label = label or os.environ.get("WATCH_LABEL") or "desktop"
    procs = Procs()
    lines = [machine_head(label), ""]
    jobs = live_jobs()
    for path, _ in jobs:
        lines += Job(path, hist, now).render(width, procs) + [""]
    if not jobs:
        lines += ["no make ci / hdl / sim / bit running", ""]
    unt = untracked_lines(untracked({int(m["pid"]) for _, m in jobs}), procs)
    if unt:
        lines += unt + [""]
    try:
        q = subprocess.run([QUEUE, "show"], capture_output=True, text=True, timeout=10).stdout
        lines += [f"queue ({label}):"] + ["  " + l for l in q.splitlines()]
    except (OSError, subprocess.SubprocessError):
        pass
    return "\n".join(l[:width] for l in lines)


def remote_snapshot(width):
    """The worker's section: this script run there (its own jobs, queue,
    memory), or why it couldn't be."""
    cmd = [os.environ.get("WATCH_SSH", "ssh"), "-o", "BatchMode=yes", "-o", "ConnectTimeout=3", REMOTE,
           f"WATCH_REMOTE= WATCH_QUEUE={REMOTE_QUEUE} python3 - --once --label worker --width {width}"]
    try:
        with open(SELF, "rb") as f:
            r = subprocess.run(cmd, stdin=f, capture_output=True, timeout=15)
    except (OSError, subprocess.SubprocessError) as e:
        return f"== worker ({REMOTE}): unreachable ({e.__class__.__name__})"
    if r.returncode != 0:
        err = r.stderr.decode(errors="replace").strip().splitlines()[-1:] or [f"exit {r.returncode}"]
        return f"== worker ({REMOTE}): unreachable ({err[0]})"
    return r.stdout.decode(errors="replace").rstrip("\n")


class Remote:
    """make watch: the worker's section, refreshed in the background (ssh
    takes a moment; the local section redraws every second)."""

    def __init__(self, width):
        self.text, self.at, self.width = f"== worker ({REMOTE}): asking ...", None, width
        threading.Thread(target=self.loop, daemon=True).start()

    def loop(self):
        while True:
            self.text, self.at = remote_snapshot(self.width), time.time()
            time.sleep(REMOTE_EVERY)

    def show(self):
        if self.at and time.time() - self.at > 3 * REMOTE_EVERY:
            return self.text + f"\n(as of {dur(time.time() - self.at)} ago)"
        return self.text


def main():
    if "--untracked" in sys.argv[1:]:   # make ci / make bit: warn, never stop
        unt = untracked_lines(untracked({int(m["pid"]) for _, m in live_jobs()}))
        if unt:
            sys.stderr.write("make: warning: heavy work is running that the queue doesn't know about;\n"
                             "make: it holds memory beside this job (CLAUDE.md, one heavy job at a time):\n"
                             + "\n".join("make:   " + l for l in unt[1:]) + "\n")
        return
    w = arg("--width")
    width = int(w) if w and w.isdigit() else None
    label = arg("--label")
    if "--once" in sys.argv[1:]:
        text = snapshot(width, label)
        if REMOTE:
            text += "\n\n" + remote_snapshot(width or shutil.get_terminal_size((100, 30)).columns)
        print(text)
        return
    remote = Remote(shutil.get_terminal_size((100, 30)).columns) if REMOTE else None
    try:
        while True:
            text = snapshot(width, label)
            if remote:
                text += "\n\n" + remote.show()
            sys.stdout.write("\033[H\033[J" + time.strftime("%H:%M:%S") + "  make watch (Ctrl-C ends)\n\n" + text + "\n")
            sys.stdout.flush()
            time.sleep(1)
    except KeyboardInterrupt:
        print()


if __name__ == "__main__":
    main()
