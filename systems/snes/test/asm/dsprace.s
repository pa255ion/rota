; dsprace.s (M7 step 7.12): uploads build/snes/spc/dsprace.bin (the SPC700
; against the DSP's samples) through the boot ROM's protocol as spcfuzz.s
; does, then reads the ports again and again (their values into WRAM,
; 7E:4000-7E:40FF), for the machine's lockstep test (snes-core); not a
; pass/fail ROM.

.INCLUDE "snes.inc"
SNES_HEADER "M7 DSP RACE          "

.DEFINE SIZE $200

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
        ; the ports, read again and again
        ldx.w #0
Loop:   lda.w $2140
        sta.l $7E4000,x
        lda.w $2141
        sta.l $7E4001,x
        inx
        inx
        txa
        and.b #$FE
        tax
        bra Loop

Spc:    .INCBIN "../../../../build/snes/spc/dsprace.bin"
.ENDS
