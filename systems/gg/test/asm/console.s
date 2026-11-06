; The SDSC debug console (gg-run --console; Gg.Spec.takeConsole): the
; text below goes out on port FDh, which the hardware ignores. gg-spec
; checks it arrives.

.INCLUDE "gg.inc"

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  ld hl, text
-:
  ld a, (hl)
  or a
  jp z, pass
  out ($FD), a
  inc hl
  jr -
text:
  .DB "GG console OK", 10, 0
.ENDS
