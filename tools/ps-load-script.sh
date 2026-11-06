#!/usr/bin/env bash
# A PS link script for the Verilator harness (vga-sim --ps, platform/sim/ps.zig)
# that loads a cartridge file as the PS app does (platform/ps/load.zig):
# hold, the whole file from link address 0 (16 KiB per line), release.
#
#   tools/ps-load-script.sh ROM OUT.ps
set -euo pipefail
rom=${1:?usage: tools/ps-load-script.sh ROM OUT.ps}
out=${2:?usage: tools/ps-load-script.sh ROM OUT.ps}
test -f "$rom" || { echo "ps-load-script.sh: no file $rom" >&2; exit 1; }
mkdir -p "$(dirname "$out")"
{
    echo "# $rom loaded from link address 0 (tools/ps-load-script.sh)"
    echo "hold 1"
    od -An -v -tx1 -w16384 "$rom" | tr -d ' ' | awk '{ printf "bytes %d %s\n", (NR - 1) * 16384, $0 }'
    echo "hold 0"
} > "$out"
