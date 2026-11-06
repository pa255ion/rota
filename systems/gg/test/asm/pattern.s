; A static picture for layer 4 (M5 step 5.8): 32 colours, four tiles,
; the name table with both palettes and the flips, two sprites (one tall).
; make sim SYSTEM=gg ROM=build/gg/asm/pattern.gg compares the board's
; frame with the spec's (systems/gg/test/golden/vga/pattern.png).

.INCLUDE "gg.inc"

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  VDPREG 0, $04
  VDPREG 1, $00
  VDPREG 2, $FF
  VDPREG 5, $7F
  VDPREG 6, $FB
  VDPREG 7, $00
  VDPREG 8, $00
  VDPREG 9, $00
  ; CRAM
  xor a
  out ($BF), a
  ld a, $C0
  out ($BF), a
  ld hl, palette
  ld b, 64
  ld c, $BE
  otir
  ; tiles 0-3 at 0000h
  xor a
  out ($BF), a
  ld a, $40
  out ($BF), a
  ld hl, tiles
  ld b, 128
  otir
  ; the name table (3800h): entry (x + y) & 3, palette 1 from row 14,
  ; horizontal flip on odd columns, vertical flip on rows 8-11
  xor a
  out ($BF), a
  ld a, $78
  out ($BF), a
  ld d, 0                 ; y
-:
  ld e, 0                 ; x
--:
  ld a, d
  add a, e
  and 3
  out ($BE), a
  ld a, d
  cp 14
  ld a, 0
  jr c, +
  or $08
+:
  bit 0, e
  jr z, +
  or $02
+:
  ld b, a
  ld a, d
  and $FC
  cp 8
  ld a, b
  jr nz, +
  or $04
+:
  out ($BE), a
  inc e
  ld a, e
  cp 32
  jr nz, --
  inc d
  ld a, d
  cp 28
  jr nz, -
  ; sprites: two at Y 70 (one 8x16 from tiles 2-3 needs R1 bit 1: all are
  ; 8x16 then), the end marker
  xor a
  out ($BF), a
  ld a, $7F
  out ($BF), a
  ld a, 70
  out ($BE), a
  ld a, 90
  out ($BE), a
  ld a, $D0
  out ($BE), a
  ld a, $80
  out ($BF), a
  ld a, $7F
  out ($BF), a
  ld a, 80
  out ($BE), a
  ld a, 2
  out ($BE), a
  ld a, 120
  out ($BE), a
  ld a, 0
  out ($BE), a
  VDPREG 1, $42           ; display on, 8x16 sprites
  jp passkeep

palette:
  .DB $73, $01, $A8, $0C, $DD, $07, $02, $02, $37, $0D, $6C, $08, $91, $03, $C6, $0E
  .DB $FB, $09, $20, $04, $55, $0F, $8A, $0A, $BF, $05, $E4, $00, $19, $0B, $4E, $06
  .DB $73, $01, $A8, $0C, $DD, $07, $02, $02, $37, $0D, $6C, $08, $91, $03, $C6, $0E
  .DB $FB, $09, $20, $04, $55, $0F, $8A, $0A, $BF, $05, $E4, $00, $19, $0B, $4E, $06
tiles:
  .DB $FF, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $00, $00
  .DB $FF, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $00, $00
  .DB $00, $FF, $00, $00, $FF, $FF, $00, $00, $00, $FF, $00, $00, $FF, $FF, $00, $00
  .DB $00, $FF, $00, $00, $FF, $FF, $00, $00, $00, $FF, $00, $00, $FF, $FF, $00, $00
  .DB $55, $00, $FF, $55, $AA, $00, $FF, $AA, $55, $00, $FF, $55, $AA, $00, $FF, $AA
  .DB $55, $00, $FF, $55, $AA, $00, $FF, $AA, $55, $00, $FF, $55, $AA, $00, $FF, $AA
  .DB $80, $FF, $FF, $80, $40, $FF, $FF, $40, $20, $FF, $FF, $20, $10, $FF, $FF, $10
  .DB $08, $FF, $FF, $08, $04, $FF, $FF, $04, $02, $FF, $FF, $02, $01, $FF, $FF, $01
.ENDS
