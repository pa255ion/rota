; scroll.s (M7 step 7.12): the BG scroll registers' two writes, for the
; spec's register test (snes-spec: the registers read after the STP; they
; are write-only to a ROM). A game writes the low byte first: HOFS = new
; << 8 | (old & ~7) | (old2 & 7), old the last byte written to any scroll
; register, old2 the last written to a HOFS (its own latch; read from the
; register masked to 10 bits, bit 2 of the low byte was lost, and Rockman &
; Forte's scroll jumped back and forth). Checks: BG1HOFS 06h, 01h = 106h;
; BG1VOFS 34h, 01h = 134h; BG2HOFS 0Dh, 02h = 20Dh; BG3HOFS 0Dh, then
; BG3VOFS 52h, then BG3HOFS 02h = 255h (the latches apart: one latch for
; both would give 252h).

.INCLUDE "snes.inc"
SNES_HEADER "M7 SCROLL TEST       "

.SECTION "main" FREE
Main:
        lda.b #$06
        sta.w $210D
        lda.b #$01
        sta.w $210D
        lda.b #$34
        sta.w $210E
        lda.b #$01
        sta.w $210E
        lda.b #$0D
        sta.w $210F
        lda.b #$02
        sta.w $210F
        lda.b #$0D
        sta.w $2111
        lda.b #$52
        sta.w $2112
        lda.b #$02
        sta.w $2111
        lda.b #$01
        jmp Finish
.ENDS
