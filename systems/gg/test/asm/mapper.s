; The Sega mapper and the RAM (M5 step 5.2; docs/systems/gamegear.md,
; "Machine summary"). The code stays in the first 1 KiB, which slot 0's
; bank register never moves.
;   1  FFFF = 3: slot 2 shows bank 3
;   2  FFFE = 0: slot 1 shows bank 0
;   3  FFFD = 2: slot 0 shows bank 2 above 0400h
;   4  ... but 0000h-03FFh stays bank 0
;   5  FFFF = 7 on a 4-bank ROM: bank 3 (the bank number wraps)
;   6  FFFC = 08h: cartridge RAM at 8000h, written and read back
;   7  FFFC = 00h: the ROM bank is back at 8000h
;   8  FFFC = 08h again: the RAM kept its byte
;   9  C000h-DFFFh is mirrored at E000h-FFFFh
;  10  a mapper register write also lands in RAM (DFFFh)

.INCLUDE "gg.inc"

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  ld a, 3
  ld (MAPSLOT2), a
  ld a, ($8000 + BANKMARK)
  EXPECT 3, 1

  xor a
  ld (MAPSLOT1), a
  ld a, ($4000 + BANKMARK)
  EXPECT 0, 2

  ld a, 2
  ld (MAPSLOT0), a
  ld a, (BANKMARK)
  EXPECT 2, 3
  ld a, (boot)             ; first byte of the ROM: DI (F3h)
  EXPECT $F3, 4
  xor a
  ld (MAPSLOT0), a

  ld a, 7
  ld (MAPSLOT2), a
  ld a, ($8000 + BANKMARK)
  EXPECT 3, 5

  ld a, $08
  ld (MAPCTRL), a
  ld a, $55
  ld ($8000), a
  ld a, ($8000)
  EXPECT $55, 6

  xor a
  ld (MAPCTRL), a
  ld a, ($8000 + BANKMARK)
  EXPECT 3, 7

  ld a, $08
  ld (MAPCTRL), a
  ld a, ($8000)
  EXPECT $55, 8
  xor a
  ld (MAPCTRL), a

  ld a, $A5
  ld ($C123), a
  ld a, ($E123)
  EXPECT $A5, 9

  ld a, 2
  ld (MAPSLOT2), a
  ld a, ($DFFF)
  EXPECT 2, 10

  jp pass
.ENDS
