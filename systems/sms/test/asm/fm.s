; The FM unit (M6 step 6.3): its detection as games do it (Phantasy Star's
; routine, SMS Power "FM Chip Detection"), then A4 on the YM2413. With the
; I/O chip off (port 3Eh bit 2), seven times: write a value to F2h (bit 0
; toggling) and read it back (bits 2-0); the unit is there if all seven
; match. Check 1: it
; is. Then F2h = 01h (FM on, PSG off), channel 0: the user instrument as a
; pure sine (the modulator at TL 63, so no FM; the carrier sustained, MULT
; 1: a clean tone for layer 4's zero-crossing detector), volume 0,
; F-number 120h, block 4, key on: 436.97 Hz
; (the manual's A4; fs = 3579545 / 72, 288 x 2^4 x fs / 2^19). The tone is
; the test: sms-spec measures its pitch. Without an FM unit (sms-run
; --no-fm) the ROM ends red with one square.
;
; The YM2413's waits (its manual): 12 master clocks after an address byte,
; 84 after a data byte; its clock is the Z80's, so T-states.

.INCLUDE "sms.inc"

.DEFINE FMFOUND $C080

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  ld a, ($C000 + $0F)     ; BOOT3E: the memory control the BIOS left
  or $04                  ; the I/O chip off
  out ($3E), a
  ld b, 7
  ld c, 0                 ; matches
-:
  ld a, b                 ; bit 0 toggling, as the games' routine
  and $01
  out ($F2), a
  ld d, a
  in a, ($F2)
  and $07
  cp d
  jr nz, +
  inc c
+:
  djnz -
  ld a, ($C000 + $0F)
  out ($3E), a            ; the I/O chip back on
  ld a, c
  ld (FMFOUND), a
  cp 7
  jr z, +
  ld a, $81
  jp finish
+:
  ld a, $01               ; FM on, PSG off
  out ($F2), a
  ld hl, notes
  ld b, (notes_end - notes) / 2
-:
  ld a, (hl)
  inc hl
  out ($F0), a            ; the register
  call wait12
  ld a, (hl)
  inc hl
  out ($F1), a            ; its value
  call wait84
  djnz -
  jp passkeep

notes:
  .DB $00, $21            ; user patch: modulator sustained, MULT 1
  .DB $01, $21            ; carrier sustained, MULT 1
  .DB $02, $3F            ; modulator TL 63 (silent: no modulation)
  .DB $03, $00
  .DB $04, $F0, $05, $F0  ; attack 15, decay 0
  .DB $06, $0F, $07, $0F  ; sustain level 0, release 15
  .DB $30, $00            ; channel 0: the user instrument, volume 0
  .DB $10, $20            ; F-number low 20h
  .DB $20, $19            ; key on, block 4, F-number bit 8
notes_end:

; Waits longer than the YM2413 needs (a CALL and RET alone are 27 T-states).
wait12:
  ret
wait84:
  push bc
  ld b, 8
-:
  djnz -
  pop bc
  ret
.ENDS
