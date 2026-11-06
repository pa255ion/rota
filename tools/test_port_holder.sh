#!/usr/bin/env bash
# Tests for tools/port-holder.sh (make test runs them): a free port, one
# held by another process (found through a symlink, as /dev/serial/by-id
# names are), our own open file not counted, a missing port.
set -uo pipefail
cd "$(dirname "$0")/.."

d=$(mktemp -d)
trap 'kill $holder 2>/dev/null; rm -rf "$d"' EXIT
h=tools/port-holder.sh
fails=0
check() { # name, condition...
    local name=$1; shift
    if "$@"; then echo "test_port_holder: ok    $name"; else echo "test_port_holder: FAIL  $name"; fails=$((fails + 1)); fi
}

: > "$d/tty"            # stands in for the device
ln -s "$d/tty" "$d/by-id"

$h "$d/by-id" 2>/dev/null; check "free port: exit 0" test $? -eq 0
$h "$d/missing" 2>/dev/null; check "missing port: exit 0 (link reports it)" test $? -eq 0

# another process holds it
bash -c "exec 9<>'$d/tty'; sleep 300" & holder=$!
sleep 0.3
out=$($h "$d/by-id" 2>&1); rc=$?
check "held port: exit 1" test $rc -eq 1
check "held port: names the PID" grep -q "in use by PID $holder" <<< "$out"
check "held port: names the command" grep -q "sleep 30\|bash -c" <<< "$out"
kill $holder 2>/dev/null; wait $holder 2>/dev/null

# our own open file (this script's fd) doesn't count
exec 8<>"$d/tty"
$h "$d/by-id" 2>/dev/null; check "our own fd: not another process" test $? -eq 0
exec 8>&-

$h 2>/dev/null; check "no argument: usage (2)" test $? -eq 2

if [[ $fails -gt 0 ]]; then echo "test_port_holder: $fails failed" >&2; exit 1; fi
echo "test_port_holder: all passed"
