#!/usr/bin/env bash
# make sd refuses a card that wouldn't boot or would lack a system (side
# track K, M6's request, 2026-10-12): no boot image, or a system without a
# bitstream; SD_PARTIAL=1 lets it through. No board: a fake SD_BUILD, and
# BOARD_QUEUE=false stops a card that passes the check before it reaches
# the board (its "make sd: refused" is what tells the cases apart).
# Run by make test (inside nix develop).
set -uo pipefail
cd "$(dirname "$0")/.."
unset MAKEFLAGS MAKELEVEL
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
fails=0
check() { # name, condition...
    local name=$1; shift
    if "$@"; then echo "test_sd_guard: ok    $name"; else echo "test_sd_guard: FAIL  $name"; fails=$((fails + 1)); fi
}
for s in chip8 gb; do mkdir -p "$tmp/build/$s/vivado" && touch "$tmp/build/$s/vivado/${s}_zynq.bit"; done
touch "$tmp/BOOT.BIN"
sd() { make -s --no-print-directory sd SD_BUILD="$tmp/build" SD_SYSTEMS="chip8 gb" BOARD_QUEUE=false "$@" 2>&1; }

out=$(sd SD_BOOT="$tmp/BOOT.BIN")
check "complete card: passes the check" test -z "$(grep 'make sd: refused' <<< "$out")"
out=$(sd SD_BOOT="$tmp/none.bin")
check "no boot image: refused" grep -q "make sd: refused" <<< "$out"
check "no boot image: says how to build it" grep -q "make boot-image BOARD=" <<< "$out"
out=$(sd SD_BOOT="$tmp/BOOT.BIN" SD_SYSTEMS="chip8 gb nes")
check "a system without a bitstream: refused" grep -q "make sd: refused" <<< "$out"
check "names the system and its make bit" grep -q "no nes bitstream.*make bit SYSTEM=nes" <<< "$out"
check "doesn't name those that have one" test -z "$(grep 'no gb bitstream' <<< "$out")"
out=$(sd SD_BOOT="$tmp/none.bin" SD_SYSTEMS="chip8 gb nes" SD_PARTIAL=1)
check "SD_PARTIAL=1: let through" test -z "$(grep 'make sd: refused' <<< "$out")"

if [[ $fails -gt 0 ]]; then echo "test_sd_guard: $fails failed" >&2; exit 1; fi
echo "test_sd_guard: all passed"
