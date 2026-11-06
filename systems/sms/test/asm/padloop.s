; The pads read over and over (M6 step 6.4): DCh and DDh 2000 times into
; RAM, then a pass. For gg-lockstep, which presses pad 1 and pad 2 buttons
; at instruction counts and compares every IN's result, spec against core.

.INCLUDE "sms.inc"

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  ld bc, 2000
-:
  in a, ($DC)
  ld ($C080), a
  in a, ($DD)
  ld ($C081), a
  dec bc
  ld a, b
  or c
  jr nz, -
  jp pass
.ENDS
