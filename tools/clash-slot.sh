#!/usr/bin/env bash
# Clash runs on this machine take turns (make hdl runs every Clash run
# through this; docs/TESTING.md, "Tiers and parallel runs").
#
#   tools/clash-slot.sh [--heavy] -- COMMAND...
#
# Every real system's Clash run needs 4 to 10 GB (measured 2026-10-08:
# the 800XL about 10, the NES and Game Boy 4 to 6), and three at once ran
# a make ci out of memory. So, across every worktree and session:
#   - at most CLASH_MAX (default 2) Clash runs at a time;
#   - a --heavy one (CLASH_HEAVY in the system's .mk: the 800XL, the Game
#     Gear) only one at a time;
#   - none starts while less than CLASH_MEM_MIN GB (default 6) of memory
#     is available (other sessions' work counts too).
# Locks (flock) in LOCK_DIR, released by the kernel however a run ends.
# Waiting is said once per reason. CLASH_POLL and MEMINFO are for the
# tests.
set -uo pipefail

LOCK_DIR=${LOCK_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/emulation_clash}
MAX=${CLASH_MAX:-2}
MIN=${CLASH_MEM_MIN:-6}
POLL=${CLASH_POLL:-5}
MEMINFO=${MEMINFO:-/proc/meminfo}

heavy=0
[[ ${1:-} == --heavy ]] && { heavy=1; shift; }
[[ ${1:-} == -- ]] && shift
[[ $# -gt 0 ]] || { echo "usage: tools/clash-slot.sh [--heavy] -- COMMAND..." >&2; exit 2; }
mkdir -p "$LOCK_DIR" || exit 2

# 1. a heavy run: the heavy lock first (so it never holds a slot while
#    waiting for another heavy run)
if [[ $heavy == 1 ]]; then
    exec {h}>"$LOCK_DIR/clash-heavy.lock"
    if ! flock -n "$h"; then
        echo "clash-slot: waiting for another heavy Clash run ($LOCK_DIR/clash-heavy.lock)" >&2
        flock "$h"
    fi
fi

# 2. one of the MAX slots
said=0
while :; do
    for ((i = 1; i <= MAX; i++)); do
        exec {s}>"$LOCK_DIR/clash-slot-$i.lock"
        flock -n "$s" && break 2
        exec {s}>&-
    done
    if [[ $said == 0 ]]; then
        echo "clash-slot: waiting: $MAX Clash runs already on the machine (CLASH_MAX)" >&2
        said=1
    fi
    sleep "$POLL"
done

# 3. enough memory (GB available, rounded down)
avail() { awk '/^MemAvailable:/ { print int($2 / 1048576) }' "$MEMINFO"; }
said=0
while (( $(avail) < MIN )); do
    if [[ $said == 0 ]]; then
        echo "clash-slot: waiting: $(avail) GB available, a Clash run starts at $MIN (CLASH_MEM_MIN)" >&2
        said=1
    fi
    sleep "$POLL"
done

"$@"
