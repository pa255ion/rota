; A short FM tune (M6 step 6.3; ours): for the ear, and its audio hash
; frozen in sms-spec once heard. Rhythm mode on (channels 6-8 the drums,
; with the manual's drum frequencies), a melody on channel 0 (instrument 2,
; guitar), a bass on channel 1 (instrument 14, acoustic bass) and chords on
; channels 2-3 (instrument 8, organ); a step every 12 frames, 16 steps,
; then it repeats. BD on steps 0, 4, 8, 12, SD on 4 and 12, HH on every
; other step. FM on, PSG off (F2h = 01h); no detection (sms-run with the
; FM unit; without one it is silent).

.INCLUDE "sms.inc"

.DEFINE STEP  $C080
.DEFINE TIMER $C081

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  ld a, $01
  out ($F2), a
  ld hl, setup
  ld b, (setup_end - setup) / 2
  call regs
  xor a
  ld (STEP), a
  ld (TIMER), a
  SETIRQ frame
  VDPREG 1, $60            ; display on, frame interrupts
  ei
-:
  halt
  jr -

frame:
  in a, ($BF)
  ld a, (TIMER)
  inc a
  cp 12
  jr nz, +
  call step
  xor a
+:
  ld (TIMER), a
  ei
  ret

; One step: key the four melodic channels off, set their notes, key on;
; then the drums for this step.
step:
  ld a, (STEP)
  ld e, a
  ld d, 0
  ld hl, song
  add hl, de
  add hl, de
  add hl, de
  add hl, de
  add hl, de
  add hl, de
  add hl, de
  add hl, de               ; 8 bytes a step: 4 x (F-number low, block/fnum8)
  ld c, 0                  ; channel
-:
  ld a, $20
  add a, c
  out ($F0), a
  call wait12
  xor a
  out ($F1), a             ; key off
  call wait84
  ld a, $10
  add a, c
  out ($F0), a
  call wait12
  ld a, (hl)
  inc hl
  out ($F1), a
  call wait84
  ld a, $20
  add a, c
  out ($F0), a
  call wait12
  ld a, (hl)
  inc hl
  or $10                   ; key on
  out ($F1), a
  call wait84
  inc c
  ld a, c
  cp 4
  jr nz, -
  ld a, $0E                ; the drums: off, then this step's
  out ($F0), a
  call wait12
  ld a, $20
  out ($F1), a
  call wait84
  ld a, (STEP)
  ld e, a
  ld d, 0
  ld hl, drums
  add hl, de
  ld a, $0E
  out ($F0), a
  call wait12
  ld a, (hl)
  out ($F1), a
  call wait84
  ld a, (STEP)
  inc a
  and 15
  ld (STEP), a
  ret

; B register pairs from HL.
regs:
  ld a, (hl)
  inc hl
  out ($F0), a
  call wait12
  ld a, (hl)
  inc hl
  out ($F1), a
  call wait84
  djnz regs
  ret

wait12:
  ret
wait84:
  push bc
  ld b, 8
-:
  djnz -
  pop bc
  ret

setup:
  .DB $0E, $20             ; rhythm mode
  .DB $16, $20, $17, $50, $18, $C0   ; the drums' F-numbers (the manual's)
  .DB $26, $05, $27, $05, $28, $01
  .DB $36, $02, $37, $11, $38, $22   ; drum volumes: BD 2; HH 1, SD 1; TOM 2, TCY 2
  .DB $30, $21             ; channel 0: guitar, volume 1
  .DB $31, $E2             ; channel 1: acoustic bass, volume 2
  .DB $32, $84, $33, $84   ; channels 2, 3: organ, volume 4
setup_end:

; F-numbers (9 bits; the manual's table, block 4): C 172 (ACh), D 192 (C0h),
; E 216 (D8h), F 229 (E5h), G 257 (101h), A 288 (120h), B 323 (143h); the
; second byte is block x 2 + F-number bit 8 (block 4: 08h/09h, block 5 for
; C' and D': 0Ah, block 2 for the bass: 04h/05h). A step: melody, bass,
; two chord notes.
song:
  .DB $AC, $08, $AC, $04, $D8, $08, $01, $09    ; C  | C | E G
  .DB $D8, $08, $AC, $04, $D8, $08, $01, $09    ; E
  .DB $01, $09, $AC, $04, $D8, $08, $01, $09    ; G
  .DB $D8, $08, $AC, $04, $D8, $08, $01, $09    ; E
  .DB $E5, $08, $E5, $04, $20, $09, $AC, $0A    ; F  | F | A C'
  .DB $20, $09, $E5, $04, $20, $09, $AC, $0A    ; A
  .DB $AC, $0A, $E5, $04, $20, $09, $AC, $0A    ; C'
  .DB $20, $09, $E5, $04, $20, $09, $AC, $0A    ; A
  .DB $01, $09, $01, $05, $43, $09, $C0, $0A    ; G  | G | B D'
  .DB $43, $09, $01, $05, $43, $09, $C0, $0A    ; B
  .DB $C0, $0A, $01, $05, $43, $09, $C0, $0A    ; D'
  .DB $43, $09, $01, $05, $43, $09, $C0, $0A    ; B
  .DB $AC, $0A, $AC, $04, $D8, $08, $01, $09    ; C' | C | E G
  .DB $01, $09, $AC, $04, $D8, $08, $01, $09    ; G
  .DB $D8, $08, $AC, $04, $D8, $08, $01, $09    ; E
  .DB $AC, $08, $AC, $04, $D8, $08, $01, $09    ; C
drums:
  .DB $31, $21, $20, $21, $39, $21, $20, $21, $31, $21, $20, $21, $39, $21, $24, $23
.ENDS
