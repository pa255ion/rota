; APU register activation (side track D step 1; AccuracyCoin "APU Register
; Activation" 4 and 5, nesdev wiki "DMA", "Register conflicts"). The 2A03
; decodes $4000-$401F from bits 15-5 of the CPU's address and bits 4-0 of
; the DMA's, so an OAM DMA of page $40 reads the registers only while the
; CPU is halted at $4000-$401F. Blargg's protocol; status = failed check:
;
; 2  CPU halted in ROM: the DMA read $4015 (the frame IRQ flag is gone)
; 3  ... or OAM isn't all open bus ($AD: the halt cycle reads the next
;    opcode, LDA abs)
; 4  CPU halted at $4000: OAM isn't $40 up to $34, then $00 (the status
;    reads of the mirrors $4035, ... clear the frame IRQ flag, and that
;    $00 stays on the bus)
; 5  ... the byte left on the bus wasn't $00: the CPU ran RTI at $4000,
;    not BRK
;
; To halt the CPU at $4000 it runs STA ($F0),Y from $3FFE: the opcode $91
; is the PPU's I/O latch (a $2006 mirror), the operand $F0 its read buffer
; (a $2007 mirror), ($F0) = $4014; the next opcode fetch, at $4000, is the
; DMA's halt cycle. That opcode is the byte the DMA leaves on the bus.
.include "nrom.inc"

ptr = $F0

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
    sta $2000
    sta $2001
    bit $2002
vbl1:
    bit $2002
    bpl vbl1
vbl2:
    bit $2002
    bpl vbl2
    lda #1              ; strobe: both controllers read 0s from here on
    sta $4016
    lda #0
    sta $4016

    ; CPU halted in ROM
    lda #2
    sta $6000+$10       ; current check, for the failure path
    lda #0
    sta $4017           ; frame IRQ flag on
    jsr wait30k
    lda #$40
    sta $4014
    lda $4015
    and #$40
    bne :+
    jmp fail
:   lda #3
    sta $6010
    ldx #0
    lda #$AD
    jsr check_oam_from  ; X..$FF all $AD
    bcc :+
    jmp fail

    ; CPU halted at $4000
:   lda #4
    sta $6010
    lda #0
    sta $4017
    jsr wait30k         ; the flag again
    lda #$14
    sta ptr
    lda #$40
    sta ptr+1
    lda #>ran_rti       ; RTI at $4000 would return to ran_rti
    pha
    lda #<ran_rti
    pha
    lda #$04            ; P for it: I set
    pha
    lda #$24            ; $2400 = $F0, then $F0 into the read buffer
    sta $2006
    lda #$00
    sta $2006
    lda #ptr
    sta $2007
    lda #$24
    sta $2006
    lda #$00
    sta $2006
    lda $2007
    lda #$91            ; the PPU's I/O latch
    sta $2002
    ldy #0
    lda #$40
    jmp $3FFE

ran_rti:
    lda #5
    sta $6010
    jmp fail

brk_at_4000:
    ldx #$FF
    txs
    lda #0
    sta $2003
    ldx #0
:   lda $2004           ; $00-$34 = $40
    cmp #$40
    bne fail
    sta $2004           ; next address
    inx
    cpx #$35
    bne :-
    lda #0
    jsr check_oam_from  ; $35-$FF = $00
    bcs fail
    lda #0              ; passed
    sta $6000
forever:
    jmp forever

fail:
    lda $6010
    sta $6000
    jmp forever

; Carry clear if OAM bytes X to $FF all equal A (the attribute bytes keep
; bits 7-5 and 1-0 only); OAMADDR must be X.
check_oam_from:
    sta $00
    and #$E3
    sta $01
    stx $2003
@loop:
    txa
    and #3
    cmp #2
    beq @attr
    lda $00
    jmp @cmp
@attr:
    lda $01
@cmp:
    cmp $2004
    bne @bad
    lda $2004
    sta $2004
    inx
    bne @loop
    clc
    rts
@bad:
    sec
    rts

; About 30 700 cycles: past the 29 830 the frame IRQ flag takes.
wait30k:
    ldx #24
    ldy #0
:   dey
    bne :-
    dex
    bne :-
    rts

nmi:
    rti

.segment "VECTORS"
    .word nmi, reset, brk_at_4000
