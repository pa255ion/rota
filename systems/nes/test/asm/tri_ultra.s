; The triangle left running at timer period 0 (about 56 kHz), as games do
; to silence it (step 3.9 board check: a high-pitched hum in silence).
; Sampled at 48 kHz it would fold back to about 7.9 kHz; the audio path
; must keep it inaudible. make sim checks the S/PDIF stream is quiet.
.include "nrom.inc"

.segment "CODE"
reset:
    sei
    cld
    ldx #$FF
    txs
    lda #$40            ; frame counter: 4-step, no IRQ
    sta $4017
    lda #$04            ; triangle only
    sta $4015
    lda #$FF            ; control (length halt) set, linear counter 127
    sta $4008
    lda #$00            ; timer period 0
    sta $400A
    lda #$08            ; length load, timer high 0
    sta $400B
forever:
    jmp forever
nmi:
irq:
    rti

.segment "VECTORS"
    .word nmi, reset, irq
