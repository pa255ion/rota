#!/usr/bin/env bash
# Pick the JTAG cable that `make prog BOARD=<board>` may program (side
# track G step 2), so that with the ZedBoard and the Arty Z7-20 both
# plugged in, a bitstream never goes to the wrong board (both are
# xc7z020s: the JTAG chain can't tell them apart).
#
#   tools/jtag-pick.sh BOARD [--cables "SERIAL ..."]
#
# The connected cables come from hw_server (boards/zedboard/jtag_cables.tcl
# through tools/xsdb.sh; needs VIVADO_SETTINGS), or from --cables (for the
# tests, tools/test-jtag-pick.sh). Each board's cable serial comes from
# JTAG_SERIALS ("zedboard=210248... arty-z7-20=..."), which the Makefile
# fills from the git-ignored local/boards.mk:
#
#   JTAG_SERIAL_zedboard   = 210248XXXXXX
#   JTAG_SERIAL_arty-z7-20 = XXXXXXXXXXXX
#
# Rules: a board with a serial gets exactly that cable, if it's connected;
# without one, the only connected cable, unless that cable is another
# board's; with several cables and no serial: refuse. Prints the cables
# found on stderr and the chosen serial on stdout; exit 1 when it refuses.
set -euo pipefail

board=${1:?usage: jtag-pick.sh BOARD [--cables "SERIAL ..."]}
shift
cables=
listed=0
if [ "${1:-}" = --cables ]; then
    cables=${2:-}
    listed=1
fi

if [ $listed = 0 ]; then
    here=$(cd "$(dirname "$0")" && pwd)
    out=$("$here/xsdb.sh" "$here/../boards/zedboard/jtag_cables.tcl" </dev/null 2>&1) || true
    if ! grep -q '^jtag_cables.tcl: OK' <<<"$out"; then
        printf '%s\n' "$out" >&2
        echo "jtag-pick: couldn't list the JTAG cables (xsdb output above)" >&2
        exit 1
    fi
    cables=$(sed -n 's/^JTAG_CABLE //p' <<<"$out" | tr '\n' ' ' | sed 's/ $//')
fi

want=
others=
for kv in ${JTAG_SERIALS:-}; do
    b=${kv%%=*}
    s=${kv#*=}
    [ -n "$s" ] || continue
    if [ "$b" = "$board" ]; then want=$s; else others="$others $s"; fi
done

n=0
for c in $cables; do n=$((n + 1)); done
echo "jtag-pick: $n cable(s) connected:${cables:+ $cables}" >&2
for kv in ${JTAG_SERIALS:-}; do
    [ -n "${kv#*=}" ] && echo "jtag-pick:   local/boards.mk: ${kv%%=*} = ${kv#*=}" >&2
done

refuse() {
    echo "jtag-pick: REFUSED for BOARD=$board: $1" >&2
    exit 1
}

if [ -n "$want" ]; then
    for c in $cables; do
        if [ "$c" = "$want" ]; then
            echo "jtag-pick: BOARD=$board -> cable $c (its serial in local/boards.mk)" >&2
            echo "$c"
            exit 0
        fi
    done
    refuse "its cable $want (local/boards.mk) is not connected"
fi
# With none found, say why if it's the usual reason: a Digilent USB
# device (manufacturer "Digilent") that this user can't open for
# writing, i.e. no udev rule for it (docs/TOOLCHAIN.md).
no_access() {
    local d node
    for d in /sys/bus/usb/devices/*; do
        [ "$(cat "$d/manufacturer" 2>/dev/null)" = Digilent ] || continue
        node=/dev/bus/usb/$(printf %03d "$(cat "$d/busnum")")/$(printf %03d "$(cat "$d/devnum")")
        if [ -e "$node" ] && [ ! -w "$node" ]; then
            echo "jtag-pick: $(cat "$d/idVendor"):$(cat "$d/idProduct") \"$(cat "$d/product" 2>/dev/null)\" (serial $(cat "$d/serial" 2>/dev/null)) is on USB, but $node isn't writable for $(id -un): no udev rule gives you access (docs/TOOLCHAIN.md, \"JTAG programming needs udev rules\")" >&2
        fi
    done
}

case $n in
    0) [ $listed = 1 ] || no_access
       refuse "no JTAG cable connected" ;;
    1) c=${cables// /}
       for o in $others; do
           [ "$o" = "$c" ] && refuse "the only cable, $c, is another board's (local/boards.mk)"
       done
       echo "jtag-pick: BOARD=$board -> cable $c (the only one; no serial for $board in local/boards.mk)" >&2
       echo "$c"
       exit 0 ;;
    *) refuse "$n cables connected and no serial for $board: set JTAG_SERIAL_$board in local/boards.mk" ;;
esac
