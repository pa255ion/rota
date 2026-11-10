; dsp1.s (M7 step 7.16d): our DSP-1 test cartridge, with our own firmware
; (build/snes/asm/dsptest.rom, snes-dspfw: "Snes.Spec.Upd77c25Asm"'s
; dspTestFirmware). 64 rounds: each waits a different while (0 to 7
; loops), then writes X and Y to DR (16-bit words, low byte first, each
; after RQM) and reads back X + Y, the multiplier's high word (X * Y * 2)
; and X XOR Y. Check 1: a sum wrong; 2: an XOR wrong. The products go to
; WRAM 1000h on (snes-core's lockstep compares the whole machine). Then
; the timing probe: X = FFFFh, and 200 reads of SR at different moments
; (WRAM 1100h on), each of which shows how far the chip has run.

.DEFINE DSP
.INCLUDE "snes.inc"
SNES_HEADER "M7 DSP1 TEST         "

.DEFINE DR $308000
.DEFINE SR $30C000
.DEFINE XV $0010
.DEFINE YV $0012
.DEFINE S1 $0014
.DEFINE S2 $0016
.DEFINE S3 $0018

.SECTION "main" FREE
Main:
        rep #$20
        lda.w #$1234
        sta.w XV
        lda.w #$0F0F
        sta.w YV
        sep #$20
        ldy.w #$0000
round:
        ; a different while each round
        tya
        and.b #$07
        beq +
-       dec a
        bne -
+       jsr Wait
        lda.w XV
        sta.l DR
        lda.w XV+1
        sta.l DR
        jsr Wait
        lda.w YV
        sta.l DR
        lda.w YV+1
        sta.l DR
        jsr Wait
        lda.l DR
        sta.w S1
        lda.l DR
        sta.w S1+1
        jsr Wait
        lda.l DR
        sta.w S2
        lda.l DR
        sta.w S2+1
        jsr Wait
        lda.l DR
        sta.w S3
        lda.l DR
        sta.w S3+1
        rep #$20
        lda.w XV
        clc
        adc.w YV
        cmp.w S1
        bne fail1
        lda.w XV
        eor.w YV
        cmp.w S3
        bne fail2
        ; the product, kept
        tya
        asl a
        tax
        lda.w S2
        sta.w $1000,x
        ; the next X (X * 5 + 3B41h) and Y (Y * 3 + 1357h)
        lda.w XV
        asl a
        asl a
        clc
        adc.w XV
        clc
        adc.w #$3B41
        sta.w XV
        lda.w YV
        asl a
        clc
        adc.w YV
        clc
        adc.w #$1357
        sta.w YV
        sep #$20
        iny
        cpy.w #64
        beq probe
        jmp round
        ; the timing probe: X = FFFFh, then 200 reads of SR after waits of
        ; 0 to 7 loops (the firmware puts a count into SR's bits every
        ; three instructions), into WRAM 1100h on
probe:  jsr Wait
        lda.b #$FF
        sta.l DR
        sta.l DR
        ldx.w #$0000
srloop: txa
        and.b #$07
        beq +
-       dec a
        bne -
+       lda.l SR
        sta.w $1100,x
        inx
        cpx.w #200
        bne srloop
        lda.b #$01
        jmp Show
fail1:  sep #$20
        lda.b #$81
        jmp Show
fail2:  sep #$20
        lda.b #$82
        jmp Show

; The verdict (A) in RESULT and on the screen, for layer 4: the backdrop
; green (pass) or red, the display on, then a loop (no STP: the picture
; stays; the spec's test reads RESULT after its frames)
Show:   sta.l RESULT
        stz.w $2121
        cmp.b #$01
        bne +
        lda.b #$E0
        sta.w $2122
        lda.b #$03
        sta.w $2122
        bra ++
        ; a failure: the colour the word read back (the sum for check 1,
        ; the XOR for 2), its low 15 bits, for layer 4's picture to tell
+       cmp.b #$81
        bne +
        lda.w S1
        sta.w $2122
        lda.w S1+1
        and.b #$7F
        sta.w $2122
        bra ++++
+       lda.w S3
        sta.w $2122
        lda.w S3+1
        and.b #$7F
        sta.w $2122
++++
++      lda.b #$0F
        sta.w $2100
-       bra -

; RQM: the chip waits for us (SR's high byte, bit 7)
Wait:
-       lda.l SR
        bpl -
        rts
.ENDS
