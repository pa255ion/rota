; nmi.s (M7 step 7.3): V-blank. Check 1: 4212h bit 7 (V-blank) comes and
; goes; 2: RDNMI (4210h) bit 7 is set in V-blank and cleared by the read;
; 3: with NMI enabled (4200h bit 7), three NMIs come, one a frame.

.INCLUDE "snes.inc"
SNES_HEADER "M7 NMI TEST          "

.SECTION "main" FREE
Main:
        ; 1: out of V-blank, then into it
-       lda.w $4212
        bmi -
-       lda.w $4212
        bpl -
        ; 2: the flag is set; a second read finds it clear
        lda.w $4210
        bpl fail2
        lda.w $4210
        bmi fail2
        ; 3
        lda.b #$80
        sta.w $4200
-       lda.l NMICOUNT
        cmp.b #3
        bcc -
        ; NMIs counted, one per frame: still 3 right after
        lda.l NMICOUNT
        cmp.b #3
        bne fail3
        lda.b #$01
        jmp Finish
fail2:  lda.b #$82
        jmp Finish
fail3:  lda.b #$83
        jmp Finish
.ENDS
