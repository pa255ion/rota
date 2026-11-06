; The Master System's picture as the board shows it (M6 step 6.2): a
; checkerboard of red and white tiles over the whole 256 x 192 picture,
; the left column blanked (R0 bit 5) so it shows the backdrop, and the
; backdrop (R7 = 3: colour 19) blue, so the border around the picture is
; blue too. Its picture is the test: it ends with `passkeep`, and the
; golden (test/golden/vga/picture.png, gg-run --vga-at) is the spec's.

.INCLUDE "sms.inc"

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  in a, ($BF)
  VDPREG 1, $00           ; display off while drawing
  VDPREG 0, $24           ; mode 4, left column blank
  VDPREG 2, $FF           ; name table 3800h
  VDPREG 5, $FF           ; sprite table 3F00h
  VDPREG 6, $FB
  VDPREG 7, $03           ; backdrop: colour 16 + 3
  VDPREG 8, $00
  VDPREG 9, $00
  VDPREG 10, $FF
  xor a                   ; VRAM 0000h: tile 0 colour 2 (plane 1), tile 1 colour 1 (plane 0)
  out ($BF), a
  ld a, $40
  out ($BF), a
  ld b, 8
-:
  xor a
  out ($BE), a
  dec a
  out ($BE), a
  xor a
  out ($BE), a
  out ($BE), a
  djnz -
  ld b, 8
-:
  ld a, $FF
  out ($BE), a
  xor a
  out ($BE), a
  out ($BE), a
  out ($BE), a
  djnz -
  xor a                   ; the name table: tile (x + y) AND 1, 32 x 28
  out ($BF), a
  ld a, $78
  out ($BF), a
  ld d, 0                 ; y
--:
  ld e, 0                 ; x
-:
  ld a, d
  add a, e
  and 1
  out ($BE), a
  xor a
  out ($BE), a
  inc e
  ld a, e
  cp 32
  jr nz, -
  inc d
  ld a, d
  cp 28
  jr nz, --
  xor a                   ; no sprites
  out ($BF), a
  ld a, $7F
  out ($BF), a
  ld a, $D0
  out ($BE), a
  xor a                   ; CRAM: 0 black, 1 white, 2 red, the rest black but 19 blue
  out ($BF), a
  ld a, $C0
  out ($BF), a
  xor a
  out ($BE), a
  ld a, $3F
  out ($BE), a
  ld a, $03
  out ($BE), a
  ld b, 16
-:
  xor a
  out ($BE), a
  djnz -
  ld a, $30
  out ($BE), a
  ld b, 12
-:
  xor a
  out ($BE), a
  djnz -
  VDPREG 1, $40           ; display on
  jp passkeep
.ENDS
