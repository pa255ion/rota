; With the frame IRQ inhibit bit set ($4017 = $40), the frame IRQ flag is
; still set for 2 CPU cycles at the end of each 4-step sequence (29828 and
; 29829 cycles after the reset), but the IRQ line stays high (side track D
; step 4; AccuracyCoin "Frame Counter IRQ" tests I to M).
;
; Polls $4015 every 13 cycles for about 20 frames (29830 mod 13 = 8: the
; polls drift over every phase, so they meet the 2-cycle window), with I
; clear. Blargg's protocol; status 2: the flag was never seen; 3: an IRQ
; happened. Counts at $6010 (flag seen) and $6011 (IRQs).
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
    sta $6010
    sta $6011
    lda #$40
    sta $4017           ; 4-step, inhibit
    cli
    ldx #180
    ldy #0
poll:
    lda $4015           ; 4
    and #$40            ; 2
    bne seen            ; 2
next:
    dey                 ; 2
    bne poll            ; 3
    dex
    bne poll
    sei
    lda $6011
    bne irqs
    lda $6010
    beq never
    lda #0              ; passed
    sta $6000
forever:
    jmp forever
never:
    lda #2
    sta $6000
    jmp forever
irqs:
    lda #3
    sta $6000
    jmp forever
seen:
    inc $6010
    jmp next

nmi:
    rti
irq:
    inc $6011
    rti

.segment "VECTORS"
    .word nmi, reset, irq
