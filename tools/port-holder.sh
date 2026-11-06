#!/usr/bin/env bash
# Is the board's serial port free? (side track K; side J's request,
# 2026-10-09)
#
#   tools/port-holder.sh PORT
#
# Two `link`s on one port steal each other's replies: a `make sd` failed
# with "pulling the saves failed: NoAnswer" while a `make load` from
# another terminal had held the same port for two hours. So every Make
# target that runs `link` on the board's port asks this first: it
# resolves PORT (the /dev/serial/by-id/... name is a symlink), looks
# through /proc/*/fd for another process with that device open, and if
# it finds one prints "<port> is in use by PID <n> (<command>)" and
# exits 1. A free port (or one that doesn't exist: link reports that
# itself) exits 0. Our own process and its parents don't count.
set -uo pipefail

port=${1:-}
[[ -n $port ]] || { echo "usage: tools/port-holder.sh PORT" >&2; exit 2; }
dev=$(readlink -f -- "$port" 2>/dev/null) || exit 0
[[ -e $dev ]] || exit 0

# our own ancestry (the make, the shell): not "another process"
declare -A mine
p=$$
while [[ -n $p && $p -gt 1 ]]; do
    mine[$p]=1
    p=$(awk '{print $4}' "/proc/$p/stat" 2>/dev/null) || break
done

found=0
# one find over every process's fds (a readlink each took longer than half
# a minute on a loaded machine, 5000 fds: a holder could be gone by then,
# side K 2026-10-18, M7's finding)
for fd in $(find /proc/[0-9]*/fd -maxdepth 1 -lname "$dev" 2>/dev/null); do
    pid=${fd#/proc/}; pid=${pid%%/*}
    [[ -n ${mine[$pid]:-} ]] && continue
    # gone already: find itself (it inherits our fds), or a holder that left
    [[ -e /proc/$pid ]] || continue
    cmd=$(tr '\0' ' ' < "/proc/$pid/cmdline" 2>/dev/null | cut -c1-120)
    echo "$port is in use by PID $pid (${cmd% }): stop it first; two links on one port steal each other's replies" >&2
    found=1
    break
done
exit $found
