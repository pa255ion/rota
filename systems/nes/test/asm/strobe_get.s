; The controllers see $4016's bit 0 only when a get cycle ends (side track
; D step 4; AccuracyCoin "Controller Strobing" 3 and 4). DEC $4016 with
; controller 1 shifted out (it reads 1s: $41) writes $41, then $40 on the
; next cycle: a 1-cycle strobe that counts only if the $41 is there when a
; get ends, so on one parity only.
;
; The parity is unknown, so three measurements (as irq_clear_get.s): the
; gaps between them differ by one cycle, so exactly one adjacent pair must
; differ. Each: shift controller 1 out, DEC $4016, read bit 0 (0: strobed,
; reloaded with no buttons; 1: not strobed). Results at $6010-$6012;
; Blargg's protocol, status 2: not exactly one pair differs (with every
; write seen at once, all are strobed).
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
    sta $6004

    jsr measure
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

measure:
    lda #1              ; strobe, then shift out more than 8 bits: 1s
    sta $4016
    lda #0
    sta $4016
    ldx #10
:   lda $4016
    dex
    bne :-
    dec $4016           ; reads $41, writes $41, then $40
    lda $4016
    and #1
    rts

nmi:
irq:
    rti

.segment "VECTORS"
    .word nmi, reset, irq
