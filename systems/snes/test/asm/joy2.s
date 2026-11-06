; joy2.s (M7 step 7.12): pad 2, for layer 4 (Makefile ci-arty-snes,
; ci-snes: pad 2 = 0A5h through its link bytes at 160000h, as the PS's
; relay writes them, systems/snes/test/ps/pad2.ps). The auto-read (4200h
; bit 0) gives 421Ah = 50h, 421Bh = 0Ah once its busy flag (4212h bit 0)
; has set and cleared. The verdict shows: the backdrop green (pass) or
; red, the display on; RESULT as usual, but no STP (the picture stays
; whole: snes-run --pad2 0xA5 --frames 26 --vga4 for the golden).

.INCLUDE "snes.inc"
SNES_HEADER "M7 JOY2 TEST         "

.SECTION "main" FREE
Main:
        lda.b #$01
        sta.w $4200
        ; a few frames first (the PS writes pad 2 after the core starts)
        ldy.w #4
frame:
-       lda.w $4212
        and.b #$01
        beq -
-       lda.w $4212
        and.b #$01
        bne -
        dey
        bne frame
        lda.w $421A
        cmp.b #$50
        bne fail
        lda.w $421B
        cmp.b #$0A
        bne fail
        ; pass: green
        stz.w $2121
        lda.b #$E0
        sta.w $2122
        lda.b #$03
        sta.w $2122
        lda.b #$0F
        sta.w $2100
        lda.b #$01
        sta.l RESULT
-       bra -
fail:   ; red
        stz.w $2121
        lda.b #$1F
        sta.w $2122
        stz.w $2122
        lda.b #$0F
        sta.w $2100
        lda.b #$81
        sta.l RESULT
-       bra -
.ENDS
