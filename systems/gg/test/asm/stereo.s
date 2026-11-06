; The Game Gear's stereo (M5 step 5.10, exit item 3): a 440.4 Hz tone on
; PSG channel 0 routed by port 06h (bits 4-7: channels 0-3 on the left,
; bits 0-3: on the right), in a cycle of four phases of 120 frames (about
; 2 s) each, the window's colour showing which:
;   left only   10h  blue
;   right only  01h  red
;   both        11h  green
;   neither     00h  black
; It never halts (gg-spec's ROM runner skips it); gg-spec's "stereo" test
; measures both sides of the spec's audio in each phase, and on the board
; the tone moves left, right, centre, silence.

.INCLUDE "gg.inc"

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  in a, ($BF)             ; a fresh control-port latch
  VDPREG 1, $00           ; display off while drawing
  VDPREG 0, $04
  VDPREG 2, $FF
  VDPREG 5, $7F
  VDPREG 6, $FB
  VDPREG 7, $00
  VDPREG 8, $00
  VDPREG 9, $00
  VDPREG 10, $FF
  xor a                   ; tile 0 (VRAM 0000h): empty
  out ($BF), a
  ld a, $40
  out ($BF), a
  ld b, 32
-:
  xor a
  out ($BE), a
  djnz -
  xor a                   ; the name table (3800h): all tile 0
  out ($BF), a
  ld a, $78
  out ($BF), a
  ld bc, 32 * 28 * 2
-:
  xor a
  out ($BE), a
  dec bc
  ld a, b
  or c
  jr nz, -
  xor a                   ; no sprites
  out ($BF), a
  ld a, $7F
  out ($BF), a
  ld a, $D0
  out ($BE), a
  VDPREG 1, $40           ; display on, no interrupts (the frame flag still sets)
  ld a, $9F               ; every channel silent
  out ($7F), a
  ld a, $BF
  out ($7F), a
  ld a, $DF
  out ($7F), a
  ld a, $FF
  out ($7F), a
  ld a, $8E               ; channel 0: period FEh = 254 (440.4 Hz)
  out ($7F), a
  ld a, $0F
  out ($7F), a
  ld a, $90               ; channel 0: volume 0 (full)
  out ($7F), a
cycle:
  ld a, $10               ; left only, blue (----BBBBGGGGRRRR)
  ld hl, $0F00
  call phase
  ld a, $01               ; right only, red
  ld hl, $000F
  call phase
  ld a, $11               ; both, green
  ld hl, $00F0
  call phase
  xor a                   ; neither, black
  ld hl, $0000
  call phase
  jr cycle

; A phase: A to port 06h, colour 0 := HL, then 120 frames (the VDP's frame
; flag, status bit 7, polled: reading the status clears it).
phase:
  out ($06), a
  xor a
  out ($BF), a
  ld a, $C0
  out ($BF), a
  ld a, l
  out ($BE), a
  ld a, h
  out ($BE), a
  ld b, 120
--:
  in a, ($BF)
  and $80
  jr z, --
  djnz --
  ret
.ENDS
