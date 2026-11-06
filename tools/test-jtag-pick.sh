#!/usr/bin/env bash
# Tests for tools/jtag-pick.sh's rules (side track G step 2), with made-up
# cable lists: no hardware, no Vivado. Run by make test.
set -uo pipefail
pick="$(dirname "$0")/jtag-pick.sh"
fail=0
Z=210248AAAAAA
A=003017BBBBBB

# expect <name> <chosen serial | REFUSED> <board> <cables> [JTAG_SERIALS]
expect() {
    local name=$1 want=$2 board=$3 cables=$4 serials=${5:-}
    local got
    got=$(JTAG_SERIALS=$serials "$pick" "$board" --cables "$cables" 2>/dev/null) || got=REFUSED
    if [ "$got" = "$want" ]; then
        echo "test-jtag-pick: ok   $name"
    else
        echo "test-jtag-pick: FAIL $name: got '$got', want '$want'" >&2
        fail=1
    fi
}

expect "one cable, no serials: that cable"             $Z      zedboard   "$Z"
expect "no cable: refuse"                              REFUSED zedboard   ""
expect "two cables, no serials: refuse"                REFUSED zedboard   "$Z $A"
expect "two cables, own serial: that one"              $A      arty-z7-20 "$Z $A" "zedboard=$Z arty-z7-20=$A"
expect "two cables, own serial (other board)"          $Z      zedboard   "$A $Z" "zedboard=$Z arty-z7-20=$A"
expect "two cables, only the other board's serial"     REFUSED arty-z7-20 "$Z $A" "zedboard=$Z"
expect "own serial set but not connected: refuse"      REFUSED arty-z7-20 "$Z"    "arty-z7-20=$A"
expect "one cable that is the other board's: refuse"   REFUSED arty-z7-20 "$Z"    "zedboard=$Z"
expect "one cable, other board's serial is different"  $A      arty-z7-20 "$A"    "zedboard=$Z"
expect "empty serial entries are ignored"              $Z      zedboard   "$Z"    "zedboard= arty-z7-20="

if [ $fail = 0 ]; then echo "test-jtag-pick: OK"; else echo "test-jtag-pick: FAIL" >&2; exit 1; fi
