#!/usr/bin/env bash
# The PS app's ps7_init.c without the register tables of silicon revisions
# 1.0 and 2.0 (side track K, 2026-10-18: OCM budget). Vivado's generated
# ps7_init.c keeps five tables (PLL, clock, DDR, MIO, peripherals) three
# times, one per Zynq-7000 silicon revision, and picks one at run time
# (ps7GetSiliconVersion); both boards (ZedBoard rev D, Arty Z7-20) carry
# production silicon, 3.x, so the 1.0 and 2.0 tables (about 7 KiB of .data
# in the OCM) are never used. Each such table becomes a #define naming its
# 3.0 twin: the code that picks one stays as it is and compiles to the same
# 3.0 path. Nothing else in the file changes (tools/test_ps7_trim.sh).
#
#   tools/ps7-trim.sh IN.c OUT.c
set -euo pipefail
in=${1:?usage: tools/ps7-trim.sh IN.c OUT.c}
out=${2:?usage: tools/ps7-trim.sh IN.c OUT.c}
awk '
/^unsigned long ps7_[a-z]+_init_data_[12]_0\[\] = \{/ {
    name = $3
    sub(/\[\].*/, "", name)
    twin = name
    sub(/_[12]_0$/, "_3_0", twin)
    print "#define " name " " twin " /* tools/ps7-trim.sh: revisions 1.0 and 2.0 run the 3.0 table */"
    skip = 1
    next
}
skip && /^};/ { skip = 0; next }
skip { next }
{ print }
' "$in" > "$out.tmp"
mv "$out.tmp" "$out"
