; The tooling's first ROM (M6 step 6.0): sms.inc's header, boot and
; verdict on the Master System. Checks: 1 RAM mirrored at E000h, 2 slot 2
; maps bank 3 through FFFFh, 3 slot 1 still bank 1, 4 the VDP's status
; port reads (bits 4-0 are 1).

.INCLUDE "sms.inc"

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  ld a, $5A
  ld ($C100), a
  ld a, ($E100)
  EXPECT $5A, 1
  ld a, 3
  ld (MAPSLOT2), a
  ld a, ($8000 + BANKMARK)
  EXPECT 3, 2
  ld a, ($4000 + BANKMARK)
  EXPECT 1, 3
  in a, ($BF)
  and $1F
  EXPECT $1F, 4
  jp pass
.ENDS
