#!/usr/bin/env bash
# Tests for tools/clash-slot.sh (make test runs them): at most CLASH_MAX
# runs at a time, heavy runs one at a time (beside light ones), the memory
# floor (a fake /proc/meminfo), exit codes. A private LOCK_DIR, so the
# machine's real Clash slots are never touched.
set -uo pipefail
cd "$(dirname "$0")/.."

export LOCK_DIR MEMINFO CLASH_POLL=0.1 CLASH_MAX=2 CLASH_MEM_MIN=6
LOCK_DIR=$(mktemp -d)
MEMINFO=$LOCK_DIR/meminfo
trap 'rm -rf "$LOCK_DIR"' EXIT
echo "MemAvailable:   20971520 kB" > "$MEMINFO"   # 20 GB
c=tools/clash-slot.sh
log=$LOCK_DIR/log
fails=0
check() { # name, condition...
    local name=$1; shift
    if "$@"; then echo "test_clash_slot: ok    $name"; else echo "test_clash_slot: FAIL  $name"; fails=$((fails + 1)); fi
}
# job NAME [--heavy] SECONDS: logs NAME+ at its start, NAME- at its end
job() { local n=$1 h=""; shift; [[ $1 == --heavy ]] && { h=--heavy; shift; }
        $c $h -- sh -c "echo $n+ >> $log; sleep $1; echo $n- >> $log" 2>/dev/null; }

$c -- true; check "exit 0" test $? -eq 0
$c -- sh -c 'exit 5'; check "exit 5" test $? -eq 5

# two at a time: C starts only when A (the shorter) ends
: > "$log"
job A 0.6 & sleep 0.15
job B 1.2 & sleep 0.15
job C 0.1 &
wait
check "third waits for a slot" test "$(tr '\n' ' ' < "$log")" = "A+ B+ A- C+ C- B- "

# heavy runs one at a time, a light one beside them
: > "$log"
job H1 --heavy 0.8 & sleep 0.15
job H2 --heavy 0.1 & sleep 0.15
job L 0.1 &
wait
check "heavy runs take turns, light beside" test "$(tr '\n' ' ' < "$log")" = "H1+ L+ L- H1- H2+ H2- "

# the memory floor: waits at 2 GB, starts at 8
: > "$log"
echo "MemAvailable:    2097152 kB" > "$MEMINFO"
job M 0.1 & sleep 0.5
check "waits below the floor" test ! -s "$log"
echo "MemAvailable:    8388608 kB" > "$MEMINFO"
wait
check "starts above it" test "$(tr '\n' ' ' < "$log")" = "M+ M- "

$c --heavy 2>/dev/null; check "no command refused" test $? -eq 2

if [[ $fails -gt 0 ]]; then echo "test_clash_slot: $fails failed" >&2; exit 1; fi
echo "test_clash_slot: all passed"
