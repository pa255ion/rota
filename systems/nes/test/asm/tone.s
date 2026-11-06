; A steady tone for the sound path (step 3.9, layer 4): pulse 1 alone,
; 50 % duty, constant volume 15, timer period 253: 1789773 / (16 * 254)
; = 440.40 Hz. make sim checks the S/PDIF stream's frequency.
.include "nrom.inc"

.segment "CODE"
reset:
    sei
    cld
    ldx #$FF
    txs
    lda #$40            ; frame counter: 4-step, no IRQ
    sta $4017
    lda #$01            ; pulse 1 only
    sta $4015
    lda #$BF            ; duty 2 (50 %), length halt, constant volume 15
    sta $4000
    lda #$08            ; sweep off (negate set: no muting by the target)
    sta $4001
    lda #<253
    sta $4002
    lda #>253           ; length load index 0, timer high bits
    sta $4003
forever:
    jmp forever
nmi:
irq:
    rti

.segment "VECTORS"
    .word nmi, reset, irq
