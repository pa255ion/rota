; The Codemasters mapper (M5; docs/systems/gamegear.md, "Codemasters
; mapper"; SMS Power "Mappers", cross-checked with MAME, Genesis Plus GX
; and MEKA). This ROM has a Codemasters header (7FE0h) and no Sega one,
; so Gg.Spec.detectMapper must pick the Codemasters mapper.
;   1  power-on: slot 2 shows bank 0
;   2  power-on: slot 1 shows bank 1
;   3  8000h = 3: slot 2 shows bank 3
;   4  4000h = 2: slot 1 shows bank 2
;   5  FFFFh = 1 switches nothing (only RAM) ...
;   6  ... and lands in RAM (DFFFh)
;   7  8000h = 7 on a 4-bank ROM: bank 3 (the bank number wraps)
;   8  4000h = 80h: cartridge RAM at A000h, written and read back
;   9  ... while 8000h-9FFFh stays slot 2's ROM
;  10  ... and slot 1 keeps its bank
;  11  4000h = 01h: the ROM back at A000h, slot 1 = bank 1
;  12  4000h = 80h again: the RAM kept its byte
;  13  0000h = 1 (from RAM): slot 0 shows bank 1 at 2000h ...
;  14  ... and at 0000h too: no fixed first 1 KiB

.DEFINE CODEMASTERS
.INCLUDE "gg.inc"

.DEFINE LOWMARK $1000     ; a second marker per bank, in slot 2's lower half
.DEFINE SEEN0   $C0F0
.DEFINE SEEN1   $C0F1
.DEFINE RAMCODE $C100

; Codemasters header: banks, BCD date and time, checksum and its
; complement (the two words sum to 0), zeros.
.BANK 1 SLOT 1
.ORGA $7FE0
.SECTION "cmheader" FORCE
  .DB 4, $07, $10, $26, $12, $00
  .DW $1234, $10000 - $1234
  .DB 0, 0, 0, 0, 0, 0
.ENDS

.BANK 1 SLOT 1
.ORGA $4000
.SECTION "b1first" FORCE
  .DB $B1                 ; bank 1's first byte
.ENDS

.BANK 0 SLOT 0
.ORGA LOWMARK
.SECTION "low0" FORCE
  .DB $10
.ENDS
.BANK 1 SLOT 1
.ORGA $4000 + LOWMARK
.SECTION "low1" FORCE
  .DB $11
.ENDS
.BANK 2 SLOT 2
.ORGA $8000 + LOWMARK
.SECTION "low2" FORCE
  .DB $12
.ENDS
.BANK 3 SLOT 2
.ORGA $8000 + LOWMARK
.SECTION "low3" FORCE
  .DB $13
.ENDS

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  ld a, ($8000 + BANKMARK)
  EXPECT 0, 1
  ld a, ($4000 + BANKMARK)
  EXPECT 1, 2

  ld a, 3
  ld ($8000), a
  ld a, ($8000 + BANKMARK)
  EXPECT 3, 3

  ld a, 2
  ld ($4000), a
  ld a, ($4000 + BANKMARK)
  EXPECT 2, 4

  ld a, 1
  ld ($FFFF), a
  ld a, ($8000 + BANKMARK)
  EXPECT 3, 5
  ld a, ($DFFF)
  EXPECT 1, 6

  ld a, 7
  ld ($8000), a
  ld a, ($8000 + BANKMARK)
  EXPECT 3, 7

  ld a, $80
  ld ($4000), a
  ld a, $5A
  ld ($A000), a
  ld a, ($A000)
  EXPECT $5A, 8
  ld a, ($8000 + LOWMARK)
  EXPECT $13, 9
  ld a, ($4000 + BANKMARK)
  EXPECT 2, 10

  ld a, $01
  ld ($4000), a
  ld a, ($8000 + BANKMARK)
  EXPECT 3, 11
  ld a, ($4000 + BANKMARK)
  EXPECT 1, 11

  ld a, $80
  ld ($4000), a
  ld a, ($A000)
  EXPECT $5A, 12
  ld a, $01
  ld ($4000), a

  ; slot 0 can't be switched from code running in it: from RAM
  ld hl, slot0
  ld de, RAMCODE
  ld bc, slot0end - slot0
  ldir
  call RAMCODE
  ld a, (SEEN0)
  EXPECT 1, 13
  ld a, (SEEN1)
  EXPECT $B1, 14
  jp pass

; Position independent: copied to RAMCODE and run there.
slot0:
  ld a, 1
  ld ($0000), a
  ld a, (BANKMARK)
  ld (SEEN0), a
  ld a, ($0000)
  ld (SEEN1), a
  xor a
  ld ($0000), a
  ret
slot0end:
.ENDS
