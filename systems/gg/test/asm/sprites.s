; Sprite overflow and collision (M5 step 5.3; MacDonald's VDP notes).
; SAT at 3F00h (R5 = 7Fh), sprite tile 1 = all pixels colour 15.
;   1  nine sprites on one line: overflow (bit 6) ...
;   2  ... and no collision (bit 5): none overlap
;   3  two overlapping sprites: collision ...
;   4  ... and no overflow
;   5  a D0h terminator at sprite 0: no flags

.INCLUDE "gg.inc"

.MACRO VWRITE
  ld a, <\1
  out ($BF), a
  ld a, (>\1) | $40
  out ($BF), a
.ENDM

.MACRO WAITV
-:
  in a, ($7E)
  cp \1
  jr nz, -
.ENDM

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  VDPREG 0, $04
  VDPREG 1, $00
  VDPREG 5, $7F
  VDPREG 6, $FB
  ; tile 1: every pixel colour 15
  VWRITE $0020
  ld b, 32
  ld a, $FF
-:
  out ($BE), a
  djnz -
  ; sprites 0-8 at Y = 50h, X = 0, 16, ..., 128, tile 1; sprite 9: D0h
  VWRITE $3F00
  ld b, 9
  ld a, $50
-:
  out ($BE), a
  djnz -
  ld a, $D0
  out ($BE), a
  VWRITE $3F80
  ld b, 9
  ld c, 0
-:
  ld a, c
  out ($BE), a
  ld a, 1
  out ($BE), a
  ld a, c
  add a, 16
  ld c, a
  djnz -

  call frameFlags
  push af
  and $40
  EXPECT $40, 1
  pop af
  and $20
  EXPECT $00, 2

  ; two sprites, the second at X = 4: they overlap
  VWRITE $3F02
  ld a, $D0
  out ($BE), a
  VWRITE $3F82
  ld a, 4
  out ($BE), a
  call frameFlags
  push af
  and $20
  EXPECT $20, 3
  pop af
  and $40
  EXPECT $00, 4

  VWRITE $3F00
  ld a, $D0
  out ($BE), a
  call frameFlags
  and $60
  EXPECT $00, 5
  jp pass

; Display on for the lines 0 to 6Fh of one frame; A = the status flags
; collected.
frameFlags:
  VDPREG 1, $00
  WAITV $D0
  in a, ($BF)
  VDPREG 1, $40
  WAITV $70
  in a, ($BF)
  push af
  VDPREG 1, $00
  pop af
  ret
.ENDS
