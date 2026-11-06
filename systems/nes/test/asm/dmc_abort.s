; DMC DMA aborts for the core lockstep (side track D step 3): no result of
; its own, it exercises the cases the spec models after AccuracyCoin's
; "Explicit/Implicit DMA Abort" (nesdev wiki "DMA", "DMC DMA bugs"):
; playback stopped by $4015 or by the last byte of a 1-byte sample just
; before, at or after the moment a reload DMA would be scheduled, with
; reads and writes around it. 2048 rounds with different delays (the
; fastest rate, 54 cycles per bit, drifts against them), looping and not;
; then Blargg's protocol says "done" (status 0).
.include "nrom.inc"

round = $10           ; 16 bits
lfsr = $12

.segment "CODE"
reset:
    sei
    cld
    ldx #$FF
    txs
    lda #$80            ; running
    sta $6000
    lda #$DE
    sta $6001
    lda #$B0
    sta $6002
    lda #$61
    sta $6003
    lda #0
    sta $6004
    sta round
    sta round+1
    lda #1
    sta lfsr
    lda #$00            ; sample at $C000, 1 byte
    sta $4012
    sta $4013

loop:
    lda round+1         ; looping on odd 256-round blocks
    lsr a
    lda #$0F            ; fastest rate
    bcc :+
    lda #$4F            ; looping
:   sta $4010
    lda lfsr            ; a pseudo-random delay, up to 640 cycles, before
    asl a               ; the start: the buffer has been empty for a while,
    bcc :+              ; so the load DMA can land at any phase of the
    eor #$1D            ; 432-cycle output byte (also in the cycle a byte
:   sta lfsr            ; ends: the RP2A03G's unexpected reload DMA)
    and #$7F
    tax
    jsr delay
    lda #$10
    sta $4015           ; start (a load DMA)
    ldx #2
    jsr delay
    lda round
    and #$10
    beq :+
    nop                 ; +2 or +3 cycles: every phase
:   bit $00
    lda #$00
    sta $4015           ; stop
    lda round
    lsr a
    and #$07
    tax
    jsr delay
    lda #$10
    sta $4015           ; start again
    ldx #3
    jsr delay
    lda #$00
    sta $4015
    inc $0200           ; a read-modify-write in RAM (write cycles)
    inc round
    bne loop
    inc round+1
    lda round+1
    cmp #8              ; 2048 rounds
    bne loop

    lda #0              ; done
    sta $6000
forever:
    jmp forever

; 5 * X + 6 cycles or so (X >= 1).
delay:
    inx
:   dex
    bne :-
    rts

nmi:
irq:
    rti

.segment "VECTORS"
    .word nmi, reset, irq
