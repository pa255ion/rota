#!/usr/bin/env bash
# make ci on the remote worker (side track K, the user 2026-10-18):
#
#   tools/ci-remote.sh [--start | --wait] [MAKE_ARG...]
#       (make ci-remote [ALL=1] [FULL=1]; make ci-remote-start; make ci-remote-wait)
#
# 1. copies both boards' ps7_init (the ZedBoard's from Vivado's sample, the
#    Arty's from make ps7-init: git-ignored) into this checkout's copy on
#    the worker, build/remote-ps7/, every run (a few KB; never stale), and
#    pushes this machine's result cache there (entries it lacks), so a
#    pass made here is a pass there;
# 2. starts make ci there, detached (setsid, nohup: a worker make ci takes
#    hours, longer than an ssh session or an agent's command may last),
#    with PS7_DIR pointing at those copies; its output in the worker copy's
#    build/ci-remote.log, which ends with a line "ci-remote: exit N" (its
#    pid in build/ci-remote.pid: a second start refused while it lives);
# 3. waits for that line (an ssh look every 15 s; CI_REMOTE_WAIT_MAX=<s>:
#    give up after that long, exit 124, the run going on: wait again with
#    --wait), prints make ci's summary, and pulls the worker's result cache
#    (passes and Verilog, not .bits) into this machine's, entries this
#    machine lacks only: cache keys are relative to the checkout (@ROOT@),
#    so a pass there skips the same unit here (the post-merge make ci
#    ALL=1, a later local make ci).
#
# --start: steps 1 and 2 only. --wait: step 3 only (a run started before).
# The exit status is make ci's. A second start while a run is going in the
# same checkout is refused (wrun mirrors the checkout with --delete: two
# would rewrite each other's files). make bit, prog and sd stay local
# (Vivado, the board).
set -euo pipefail
root=$(git rev-parse --show-toplevel)
cd "$root"
host=${WRUN_HOST:-worker}
wrun=${WRUN:-$HOME/.claude/bin/wrun}
[ -x "$wrun" ] || { echo "ci-remote: no wrun at $wrun (WRUN=...)" >&2; exit 2; }
mode=both
case ${1:-} in
    --start) mode=start; shift ;;
    --wait) mode=wait; shift ;;
esac

# the checkout's copy on the worker, as wrun names it
rel=${root#"$HOME"/}
remote="work/${rel#/}"
log="$remote/build/ci-remote.log"

start() {
    read -r zed arty < <(make -s --no-print-directory ps7-dirs PS7_DIR=)
    for d in "$zed" "$arty"; do
        for f in ps7_init.c ps7_init.h ps7_init.tcl; do
            [ -f "$d/$f" ] && continue
            echo "ci-remote: no $d/$f" >&2
            case $d in
                "$arty") echo "  run: make ps7-init BOARD=arty-z7-20 (Vivado, about a minute)" >&2 ;;
                *) echo "  the ZedBoard's comes with Vivado (VIVADO_SETTINGS=...)" >&2 ;;
            esac
            exit 2
        done
    done
    # a run still going here: its process (pid recorded at its start) alive
    if ssh "$host" "test -f $remote/build/ci-remote.pid && kill -0 \$(cat $remote/build/ci-remote.pid) 2>/dev/null"; then
        echo "ci-remote: a run is still going in this checkout's copy (its log: $host:$log); wait for it: $0 --wait" >&2
        exit 2
    fi
    # this machine's passes and Verilog to the worker first (entries it
    # lacks only), so what a run here (another session's post-merge make
    # ci, say) already passed is cached there too, as the worker's are
    # pulled here afterwards
    local cache=${CI_CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/emulation_clash/ci-cache}
    for sub in pass hdl; do
        [ -d "$cache/$sub" ] || continue
        rsync -a --mkpath --ignore-existing --exclude '.tmp-*' \
            "$cache/$sub/" "$host:.cache/emulation_clash/ci-cache/$sub/" ||
            echo "ci-remote: could not push this machine's $sub cache" >&2
    done
    # .gitignore first: wrun's sync (--delete) keeps build/ on the worker
    # only where the worker's copy has it (a first run deleted the copies)
    rsync -a --mkpath .gitignore "$host:$remote/"
    for b in zedboard:"$zed" arty-z7-20:"$arty"; do
        rsync -a --mkpath "${b#*:}"/ps7_init.c "${b#*:}"/ps7_init.h "${b#*:}"/ps7_init.tcl \
            "$host:$remote/build/remote-ps7/${b%%:*}/"
    done
    # the commit the passes are recorded under (+dirty: tracked files changed)
    local c b mk
    c=$(git rev-parse --short HEAD)
    git diff --quiet HEAD -- || c="$c+dirty"
    # PROGRESS_BRANCH: make watch's name for the job there (its copy has no
    # history)
    b=$(git rev-parse --abbrev-ref HEAD)
    # CI_JOBS (layer 4's chains at a time; make ci's 3 suits this machine,
    # shared and short of memory): 6 on the worker, idle and with 28 GB (a
    # worker make ci ran at a load of 2 of its 8 cores; the heavy Clash
    # runs stay limited by tools/clash-slot.sh). In the environment, not on
    # make's command line: so not in MAKEFLAGS, nor in the chains' keys
    mk=$(printf '%q ' env PS7_DIR=build/remote-ps7 "CI_CACHE_COMMIT=$c" "PROGRESS_BRANCH=$b, via ci-remote" \
        "CI_JOBS=${CI_REMOTE_JOBS:-6}" nix develop -c make ci "$@")
    # wrun hands its words to the worker's shell as one string: the whole
    # detached command quoted once more for sh -c
    local inner
    inner=$(printf '%q' "$mk; echo \"ci-remote: exit \$?\"")
    "$wrun" "rm -f build/ci-remote.log; setsid nohup sh -c $inner > build/ci-remote.log 2>&1 < /dev/null & echo \$! > build/ci-remote.pid" > /dev/null
    echo "ci-remote: make ci started on $host for $b ($c); its log: $host:$log"
}

wait_run() {
    local max=${CI_REMOTE_WAIT_MAX:-0} t0=$SECONDS last
    while :; do
        last=$(ssh "$host" "grep -E '^ci-remote: exit ' $log 2>/dev/null | tail -1" || true)
        [ -n "$last" ] && break
        if [ "$max" -gt 0 ] && [ $((SECONDS - t0)) -ge "$max" ]; then
            echo "ci-remote: still running after $max s ($host:$log); wait again: $0 --wait"
            exit 124
        fi
        sleep 15
    done
    local status=${last#ci-remote: exit }
    ssh "$host" "grep -E '^make ci: |^make: [*][*][*]' $log | tail -6" || true
    # the worker's passes and Verilog into this machine's cache (never over
    # an entry it has; .tmp- files are half-written ones)
    local cache=${CI_CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/emulation_clash/ci-cache}
    for sub in pass hdl; do
        ssh "$host" "test -d .cache/emulation_clash/ci-cache/$sub" || continue
        rsync -a --mkpath --ignore-existing --exclude '.tmp-*' \
            "$host:.cache/emulation_clash/ci-cache/$sub/" "$cache/$sub/" ||
            echo "ci-remote: could not pull the worker's $sub cache" >&2
    done
    echo "ci-remote: make ci on $host exited $status; its cache pulled"
    exit "$status"
}

case $mode in
    start) start "$@" ;;
    wait) wait_run ;;
    both) start "$@"; wait_run ;;
esac
