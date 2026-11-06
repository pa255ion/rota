; A pure tone (M4 step 4.4): channel 1 on the 64 kHz clock, AUDF1 = 72,
; pure tone at volume 15: 1789772.5 / (73 × 28 × 2) = 437.8 Hz (HRM 5.3's
; example). The spec and layer 4 measure it.
.include "a800.inc"
.include "xex.inc"

start:  lda #0
        sta AUDCTL
        lda #3
        sta SKCTL       ; out of initialization mode (the OS left it so too)
        lda #72
        sta AUDF1
        lda #$AF        ; pure tone, volume 15
        sta AUDC1
        jmp *
