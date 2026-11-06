; Battery saves (M5 step 5.10, exit item 2): counts its boots in cartridge
; RAM. With the Sega mapper's cartridge RAM at 8000h (FFFCh bit 3): if it
; starts with "SAVE", the byte after it (the count) goes up by one; else
; it writes "SAVE" and a count of 1. Then ROM is back at 8000h and the
; verdict (green) is drawn. Layer 4 (test/ps/save.ps, Makefile ci-gg and
; ci-arty-gg): the PS loads "SAVE" and 29h, the count comes back as 2Ah.
; On the board, the count survives a power cycle through the card.

.INCLUDE "gg.inc"

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  ld a, $08               ; cartridge RAM (bank 0) at 8000h-BFFFh
  ld (MAPCTRL), a
  ld hl, $8000
  ld de, magic
  ld b, 4
-:
  ld a, (de)
  cp (hl)
  jr nz, fresh
  inc hl
  inc de
  djnz -
  ld a, ($8004)           ; a save: one more boot
  inc a
  ld ($8004), a
  jr done
fresh:
  ld hl, magic            ; no save: "SAVE", count 1
  ld de, $8000
  ld bc, 4
  ldir
  ld a, 1
  ld ($8004), a
done:
  xor a                   ; ROM at 8000h again
  ld (MAPCTRL), a
  jp pass

magic:
  .DB "SAVE"
.ENDS
