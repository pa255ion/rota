; A $4015 read clears the frame IRQ flag at the start of the next get
; cycle (side track D step 2): after a read on a get cycle, a read on the
; following put still sees the flag; after a read on a put, the next read
; (a get) doesn't. AccuracyCoin's sync routines tell get from put that
; way: SLO $4015,X reads $4015 on two consecutive cycles (the dummy read,
; then the read), and A ends up $80 or $00.
;
; The parity is unknown, so three measurements, each after the flag is
; set again: from the first to the second the code takes P + 2 cycles,
; from the second to the third P + 3 (NOP, then BIT zp). One of the two
; gaps is odd, so exactly one adjacent pair must differ. Results at
; $6010-$6012; Blargg's protocol, status 2: not exactly one pair differs
; (with the flag dropping on the next cycle either way, all are $00).
.include "nrom.inc"

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
    sta $6004           ; no text
    sta $4017           ; frame IRQ flag on (4-step, IRQ enabled; I is set)

    jsr measure         ; $6010
    sta $6010
    nop                 ; 2 cycles
    jsr measure
    sta $6011
    bit $00             ; 3 cycles
    jsr measure
    sta $6012

    ldx #0              ; differing adjacent pairs
    lda $6010
    cmp $6011
    beq :+
    inx
:   lda $6011
    cmp $6012
    beq :+
    inx
:   cpx #1
    bne fail
    lda #0              ; passed
    sta $6000
forever:
    jmp forever
fail:
    lda #2
    sta $6000
    jmp forever

; Wait for the flag (about 30 700 cycles, more than the 29 830 of a
; sequence), then read $4015 twice on consecutive cycles: A = the second
; read's frame IRQ flag, in bit 7.
measure:
    ldx #24
    ldy #0
:   dey
    bne :-
    dex
    bne :-
    lda #0
    ldx #0
    .byte $1F           ; SLO $4015,X
    .word $4015
    and #$80            ; the other bits: channels, open bus
    rts

nmi:
irq:
    rti

.segment "VECTORS"
    .word nmi, reset, irq
