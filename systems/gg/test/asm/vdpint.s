; The VDP's interrupts (M5 step 5.3; docs/systems/gamegear.md, "VDP
; timing": the GG Hardware Reference Manual's example, MacDonald's notes).
;   1  the frame interrupt comes on line C1h (the handler reads V = C1h)
;   2  ... with status bit 7 set
;   3  a status read clears bit 7
;   4  with R10 = 0Bh the first line interrupt of a frame comes on line 0Bh
;      ("after line 10 has been scanned")
;   5  ... and the next one 12 lines later, on line 17h
;   6  with R10 = 0 the last line interrupt of a frame is on line C0h:
;      the counter is decremented on lines 0 to 192 (C0h)

.INCLUDE "gg.inc"

.DEFINE VSEEN  $C020
.DEFINE STSEEN $C021
.DEFINE COUNT  $C022
.DEFINE V2     $C023

.MACRO WAITV
-:
  in a, ($7E)
  cp \1
  jr nz, -
.ENDM

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  VDPREG 0, $04           ; Mode 4, no line interrupt
  VDPREG 1, $00
  SETIRQ frameIrq
  xor a
  ld (COUNT), a
  WAITV $10
  in a, ($BF)             ; nothing pending
  VDPREG 1, $20           ; frame interrupt on (display off)
  ei
-:
  ld a, (COUNT)
  or a
  jr z, -
  di
  VDPREG 1, $00
  ld a, (VSEEN)
  EXPECT $C1, 1
  ld a, (STSEEN)
  and $80
  EXPECT $80, 2
  in a, ($BF)
  and $80
  EXPECT $00, 3

  ; line interrupts: R10 is reloaded on lines C1h and later
  VDPREG 10, $0B
  SETIRQ lineIrq
  xor a
  ld (COUNT), a
  WAITV $D0
  in a, ($BF)             ; clears a line interrupt left pending
  VDPREG 0, $14           ; Mode 4 + line interrupt
  ei
-:
  ld a, (COUNT)
  cp 2
  jr c, -
  di
  VDPREG 0, $04
  ld a, (VSEEN)
  EXPECT $0B, 4
  ld a, (V2)
  EXPECT $17, 5

  ; R10 = 0: a line interrupt on every line the counter is decremented
  ; on, the last one of a frame on line C0h (192)
  VDPREG 10, $00
  SETIRQ everyLine
  WAITV $D0
  in a, ($BF)
  xor a
  ld (VSEEN), a
  VDPREG 0, $14
  ei
  WAITV $10               ; through the next frame's lines 0 to C0h
  WAITV $D8
  di
  VDPREG 0, $04
  ld a, (VSEEN)
  EXPECT $C0, 6
  jp pass

everyLine:
  push af
  in a, ($7E)
  cp $D0
  jr nc, +
  ld (VSEEN), a
+:
  in a, ($BF)
  pop af
  ei
  ret

frameIrq:
  push af
  in a, ($7E)
  ld (VSEEN), a
  in a, ($BF)
  ld (STSEEN), a
  ld a, 1
  ld (COUNT), a
  pop af
  ei
  ret

lineIrq:
  push af
  push hl
  in a, ($7E)
  ld hl, COUNT
  inc (hl)
  ld l, a
  ld a, (COUNT)
  cp 1
  ld a, l
  jr nz, +
  ld (VSEEN), a
  jr ++
+:
  cp $C0
  jr nc, ++               ; only the first two
  ld (V2), a
++:
  in a, ($BF)
  pop hl
  pop af
  ei
  ret
.ENDS
