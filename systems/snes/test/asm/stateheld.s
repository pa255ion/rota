; stateheld.s (M7 step 7.15a): a state for the restore test (snes-spec,
; "loading through the CPU") with what the games there leave as at power
; on: WRIO 7Fh (its bit 7 fallen: the counters latched), the pads' strobe
; on, CGRAM's flip-flop set (one byte written to 2122h), the V-IRQ timer on
; with IRQs masked, so TIMEUP stays set; then it loops for ever (no STP).

.INCLUDE "snes.inc"
SNES_HEADER "M7 STATE HELD        "

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
-       bra -
.ENDS
