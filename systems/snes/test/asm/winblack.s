; winblack.s (M7 step 7.12): the colour window forcing the main screen
; black, as a game's text box does (Rockman & Forte's box was missing: the
; window's enable was read from the wrong bit). A white backdrop; window 1
; from x 64 to 191 as the colour window (WOBJSEL 20h: its enable bit,
; not inverted); CGWSEL B0h: the main screen black inside it (bits 7-6
; = 2), the maths off (5-4 = 3). The spec's
; test (snes-spec) reads the frame: black at x 64-191, white elsewhere.
; No STP: the picture stays whole.

.INCLUDE "snes.inc"
SNES_HEADER "M7 WINDOW BLACK      "

.SECTION "main" FREE
Main:
        ; the backdrop white
        stz.w $2121
        lda.b #$FF
        sta.w $2122
        lda.b #$7F
        sta.w $2122
        ; window 1: x 64 to 191
        lda.b #64
        sta.w $2126
        lda.b #191
        sta.w $2127
        ; the colour window: window 1 enabled, not inverted
        lda.b #$20
        sta.w $2125
        stz.w $212B
        ; the main screen black inside the colour window, the maths never
        lda.b #$B0
        sta.w $2130
        stz.w $2131
        ; the display on, nothing but the backdrop
        stz.w $212C
        stz.w $212D
        lda.b #$0F
        sta.w $2100
        lda.b #$01
        sta.l RESULT
-       bra -
.ENDS
