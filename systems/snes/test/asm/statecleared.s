; statecleared.s (M7 step 7.15a): as stateheld.s, but its loop reads
; 4211h, so TIMEUP is clear between the timer's firings (the restore
; routine must clear it after the timer fires during the routine).

.INCLUDE "snes.inc"
SNES_HEADER "M7 STATE CLEARED     "

.SECTION "main" FREE
Main:
        sei
        lda.b #$7F
        sta.w $4201
        lda.b #$01
        sta.w $4016
        lda.b #$05
        sta.w $2121
        lda.b #$AB
        sta.w $2122
        lda.b #100
        sta.w $4209
        stz.w $420A
        lda.b #$21
        sta.w $4200
-       lda.w $4211
        bra -
.ENDS
