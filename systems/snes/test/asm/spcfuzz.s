; spcfuzz.s (M7 step 7.9b): uploads build/snes/spc/spcfuzz.bin (the sound
; module's I/O at random) through the boot ROM's protocol as tone.s does,
; then writes random values to the four ports and reads them back, its
; reads logged in WRAM (7E:4000-7E:4FFF), for the machine's lockstep test
; (snes-core); not a pass/fail ROM.

.INCLUDE "snes.inc"
SNES_HEADER "M7 SPC FUZZ          "

.DEFINE SIZE $200
.DEFINE SEED $10
.DEFINE OUTP $14

.SECTION "main" FREE
Main:
        ; the boot ROM is ready: AAh, BBh
-       lda.w $2140
        cmp.b #$AA
        bne -
-       lda.w $2141
        cmp.b #$BB
        bne -
        ; a block to 0200h
        rep #$20
        lda.w #$0200
        sta.w $2142
        sep #$20
        lda.b #$01
        sta.w $2141
        lda.b #$CC
        sta.w $2140
-       cmp.w $2140
        bne -
        ldx.w #0
byte:   lda.l Spc,x
        sta.w $2141
        txa
        sta.w $2140
-       cmp.w $2140
        bne -
        inx
        cpx.w #SIZE
        bne byte
        rep #$20
        lda.w #$0200
        sta.w $2142
        sep #$20
        stz.w $2141
        txa
        inc a
        ora.b #$01
        sta.w $2140
        ; no wait for the boot ROM's echo: the program it starts writes the
        ; ports at once, over the echo; a while for the jump instead
        ldx.w #400
-       dex
        bne -
        ; the ports at random
        rep #$20
        lda.w #$BEEF
        sta.b SEED
        lda.w #$4000
        sta.b OUTP
        sep #$20
Loop:
        ; a write one time in 4 (so a port CONTROL cleared stays clear a while)
        jsr Next
        and.b #$03
        bne NoWrite
        jsr Next
        rep #$20
        and.w #$0003           ; X is 16 bits: B too
        tax
        sep #$20
        jsr Next
        sta.w $2140,x
NoWrite:
        jsr Next
        rep #$20
        and.w #$0003           ; X is 16 bits: B too
        tax
        sep #$20
        lda.w $2140,x
        jsr Out
        jsr Next
        rep #$20
        and.w #$001F
        tax
        sep #$20
-       dex
        bpl -
        bra Loop

Out:
        phx
        rep #$20
        ldx.b OUTP
        sep #$20
        sta.l $7E0000,x
        rep #$20
        inx
        txa
        and.w #$0FFF
        ora.w #$4000
        sta.b OUTP
        sep #$20
        plx
        rts

; xorshift16: SEED, its low byte in A (8 bits) on return
Next:
        rep #$20
        lda.b SEED
        asl a
        asl a
        asl a
        asl a
        asl a
        asl a
        asl a
        eor.b SEED
        sta.b SEED
        lsr a
        lsr a
        lsr a
        lsr a
        lsr a
        lsr a
        lsr a
        lsr a
        lsr a
        eor.b SEED
        sta.b SEED
        xba
        and.w #$FF00
        eor.b SEED
        sta.b SEED
        sep #$20
        rts

Spc:    .INCBIN "../../../../build/snes/spc/spcfuzz.bin"
.ENDS
