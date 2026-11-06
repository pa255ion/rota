; Controller reads (step 3.2): strobe, then ten reads of each port stored
; raw at $6010 and $6020 (bits 5 to 7 are open bus: $40, the high byte of
; the operand). Then Blargg's protocol says "done" (status 0).
.include "nrom.inc"

.segment "CODE"
reset:
    sei
    cld
    ldx #$FF
    txs
    lda #1
    sta $4016
    lda #0
    sta $4016
    ldx #0
read:
    lda $4016
    sta $6010,x
    lda $4017
    sta $6020,x
    inx
    cpx #10
    bne read
    lda #$DE
    sta $6001
    lda #$B0
    sta $6002
    lda #$61
    sta $6003
    lda #0
    sta $6004
    sta $6000
forever:
    jmp forever
nmi:
irq:
    rti

.segment "VECTORS"
    .word nmi, reset, irq
