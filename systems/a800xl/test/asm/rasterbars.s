; Mid-line colour changes (M4 step 4.5, a golden and a lockstep case):
; with all DMA off, each line from VCOUNT 16 to 111 synchronises with
; WSYNC, waits a fixed time and writes COLBK eight times, every 6 cycles,
; so the background shows vertical bars whose edges sit on exact colour
; clocks
; (GTIA: a colour write shows one colour clock after its cycle's first).
; The colour steps by line, so a late or early write shows as a shifted
; edge on every line.
.include "a800.inc"
.include "xex.inc"

COLBK   = $D01A

start:  sei
        lda #0
        sta NMIEN
        sta DMACTL
frame:  lda VCOUNT
        cmp #16
        bne frame
line:   ldx VCOUNT
        txa
        asl
        sta WSYNC       ; resume at cycle 105
        ldy #7          ; 36 cycles: the first write lands near cycle 27
dly:    dey
        bne dly
        sta COLBK
        eor #$10
        sta COLBK
        eor #$20
        sta COLBK
        eor #$40
        sta COLBK
        eor #$80
        sta COLBK
        eor #$10
        sta COLBK
        eor #$20
        sta COLBK
        eor #$40
        sta COLBK
        cpx #111
        bne line
        lda #0
        sta COLBK
        jmp frame
