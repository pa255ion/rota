#!/usr/bin/env bash
# Checks a Zynq boot image (BOOT.BIN from bootgen, side track F) against
# the boot ROM's rules for a non-secure first-stage program copied into OCM
# (UG585 v1.8.1, 6.3.2, table 6-5), so a bad image is caught on the desktop
# instead of as a dark board:
#
#   tools/check-boot-image.sh build/boot/BOOT.BIN
#
# Width detection 0xAA995566, image identification "XNLX", not encrypted,
# source offset 64-byte aligned and at or above 0x8C0, image length at most
# 192 KB and equal to the total image length, reserved word 0x038 zero,
# start of execution 64-byte aligned and below 0x30000, header checksum
# (the words 0x020 to 0x044 summed and inverted; bootgen writes 1 into
# 0x044, which v1.8.1 also calls reserved, so only the sum covers it). The image's run-time footprint (with
# .bss and stacks) is checked by the linker script (platform/ps/link.ld).
set -euo pipefail
img=${1:?usage: tools/check-boot-image.sh BOOT.BIN}
test -f "$img" || { echo "check-boot-image.sh: no such file: $img" >&2; exit 1; }

# little-endian 32-bit word at a byte offset
word() { od -A n -t u4 -j "$1" -N 4 --endian=little "$img" | tr -d ' '; }

fail=0
bad() { echo "check-boot-image.sh: $img: $*" >&2; fail=1; }
hex() { printf '0x%X' "$1"; }

[ "$(word 0x20)" -eq $((0xAA995566)) ] || bad "width detection word is $(hex "$(word 0x20)"), not 0xAA995566"
[ "$(word 0x24)" -eq $((0x584C4E58)) ] || bad "image identification is $(hex "$(word 0x24)"), not 0x584C4E58 (XNLX)"
enc=$(word 0x28)
{ [ "$enc" -ne $((0xA5C3C5A3)) ] && [ "$enc" -ne $((0x3A5C3C5A)) ]; } || bad "the image is marked encrypted"
src=$(word 0x30)
{ [ $((src % 64)) -eq 0 ] && [ "$src" -ge $((0x8C0)) ]; } || bad "source offset $(hex "$src") not 64-byte aligned at or above 0x8C0"
len=$(word 0x34)
total=$(word 0x40)
[ "$len" -le $((192 * 1024)) ] || bad "image length $len bytes is over the boot ROM's 192 KB"
[ "$len" -eq "$total" ] || bad "image length $len differs from the total image length $total (non-secure: equal)"
[ "$(word 0x38)" -eq 0 ] || bad "reserved word 0x038 is not zero"
exe=$(word 0x3C)
{ [ $((exe % 64)) -eq 0 ] && [ "$exe" -lt $((0x30000)) ]; } || bad "start of execution $(hex "$exe") not 64-byte aligned below 0x30000"
sum=0
for off in 0x20 0x24 0x28 0x2C 0x30 0x34 0x38 0x3C 0x40 0x44; do
    sum=$(( (sum + $(word $off)) & 0xFFFFFFFF ))
done
[ "$(word 0x48)" -eq $(( ~sum & 0xFFFFFFFF )) ] || bad "header checksum $(hex "$(word 0x48)") is wrong (expected $(hex $(( ~sum & 0xFFFFFFFF ))))"
size=$(stat -c %s "$img")
[ "$size" -ge $((src + len)) ] || bad "the file ($size bytes) ends before the image (offset $(hex "$src") + $len)"

[ "$fail" -eq 0 ] || exit 1
echo "check-boot-image.sh: $img: OK (first-stage image $len bytes of 192 KB, start $(hex "$exe"))"
