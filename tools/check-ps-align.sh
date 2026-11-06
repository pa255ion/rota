#!/usr/bin/env bash
# Regression check for the PS app (CLAUDE.md rule 5): the app runs with the
# MMU off, where every unaligned access is an alignment fault (ARMv7-A ARM:
# memory is Strongly-ordered without the MMU, A3.2.1 / B3.2.1). The first
# board run of the SD menu stopped dead on `ldrh r1, [r0, #11]` (a FAT
# boot-sector field at an odd offset): LLVM had been allowed unaligned
# accesses. The fix is -mcpu cortex_a9+strict_align (build.zig).
#
# The gate (exact): the compiler was told unaligned accesses are not allowed.
# LLVM records that in the assembly as the ARM build attribute
# Tag_CPU_unaligned_access (34): `.eabi_attribute 34, 0`. Missing or not 0
# fails the build: strict_align was lost.
#
# A warning (a heuristic, not a gate since 2026-10-07, the user's decision):
# loads/stores whose constant offset doesn't fit the access size. A base
# register known to hold sp plus a constant in straight-line code (set by
# `add rD, sp, #k`, or from such a register, since the last label or call,
# with no other write to it and no change to sp) is checked by the whole
# offset: LLVM splits large stack offsets in two (`add lr, sp, #256`, `add
# r0, lr, #117`, `ldrh r0, [r0, #175]` is sp + 548). Other bases can't be
# judged from here (a register may point anywhere), so what remains is
# listed for a reader, not refused: with strict alignment the compiler only
# emits a halfword or word access where it knows the address is aligned.
# (The scan had become a gate of false positives when the GBC's palette
# menu moved LLVM's stack offsets: side track I, docs/STATUS.md.)
#
#   tools/check-ps-align.sh build/ps/relay.s
set -euo pipefail
asm=${1:?usage: tools/check-ps-align.sh APP.s}

if ! grep -Eq '^[[:space:]]*\.eabi_attribute[[:space:]]+34,[[:space:]]*0[[:space:]]*(@.*)?$' "$asm"; then
    echo "check-ps-align.sh: $asm lacks .eabi_attribute 34, 0: the compiler may emit unaligned accesses (strict_align lost?)" >&2
    exit 1
fi

maybe=$(gawk '
  function forget() { for (r in spoff) delete spoff[r] }
  # a label or a call: forget all (calls clobber r0-r3, r12 and lr)
  /^[^ \t].*:/ { forget(); next }
  /^[ \t]+(bl|blx)[ \t]/ { forget(); next }
  # sp itself moves
  /^[ \t]+(push|pop|vpush|vpop)[ \t]/ { forget(); next }
  {
    known = 0
    if (match($0, /^[ \t]+add[ \t]+([a-z0-9]+), (sp|[a-z0-9]+), #(-?[0-9]+)$/, a)) {
      if (a[2] == "sp") { spoff[a[1]] = a[3] + 0; known = 1 }
      else if (a[2] in spoff) { spoff[a[1]] = spoff[a[2]] + a[3]; known = 1 }
    }
    if (match($0, /^[ \t]+(ldr|str|ldrh|strh|ldrsh|ldrd|strd)[ \t]+[^[]*\[([a-z0-9]+), #(-?[0-9]+)\]/, m)) {
      op = m[1]; base = m[2]; off = m[3] + 0
      size = 4
      if (op ~ /h$/) size = 2
      total = (base in spoff) ? spoff[base] + off : off
      if (total % size != 0) print FNR ": " $0
    }
    # a writeback (pre- or post-indexed) changes its base
    if (match($0, /\[([a-z0-9]+)(, [^]]*)?\](!|, )/, wb)) delete spoff[wb[1]]
    # the first operand of anything but a store, compare, branch or test is written
    if (!known && match($0, /^[ \t]+([a-z.0-9]+)[ \t]+([a-z0-9]+)(,|$)/, w) && w[1] !~ /^(str|cmp|cmn|tst|teq|vst|b|bx|it)/) {
      if (w[2] == "sp") forget(); else delete spoff[w[2]]
    }
  }' "$asm")
if [ -n "$maybe" ]; then
    echo "check-ps-align.sh: warning: $asm: constant offsets that don't fit the access size, through bases not known here (strict alignment is on, so the compiler knows these are aligned; listed for a reader):" >&2
    echo "$maybe" >&2
fi
echo "check-ps-align.sh: $asm: built with strict alignment (.eabi_attribute 34, 0)"
