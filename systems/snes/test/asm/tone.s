; tone.s (M7 step 7.4): uploads build/snes/spc/tone.bin (its SPC700 code
; and data, 0200h-03FFh) through the boot ROM's protocol and runs it;
; RESULT 01h once the boot ROM has acknowledged the run command, then the
; CPU waits. snes-spec measures the 1000 Hz tone in the DSP's output.

.INCLUDE "snes.inc"
SNES_HEADER "M7 TONE TEST         "

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
        ; each byte with its index, waiting for the echo
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
        ; run at 0200h: command 0, kick = (last index + 2) | 1
        rep #$20
        lda.w #$0200
        sta.w $2142
        sep #$20
        stz.w $2141
        txa
        inc a
        ora.b #$01
        sta.w $2140
-       cmp.w $2140
        bne -
        lda.b #$01
        sta.l RESULT
-       bra -
Spc:    .INCBIN "../../../../build/snes/spc/tone.bin"
.ENDS
