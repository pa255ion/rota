; A display test scene (M4 step 4.3; checked by eye, then a golden): one
; band per ANTIC mode (text 2, 4, 6, 7; bitmap 8, A, D, E, F), inverse
; text, four players across the bands with priority 1 (players over the
; playfield), and a GTIA mode 9 band switched on and off by DLIs. Set up
; through the OS's shadow registers, which its VBI copies every frame.
.include "a800.inc"
.include "xex.inc"

SDMCTL  = $022F
SDLSTL  = $0230
GPRIOR  = $026F
PCOLR0  = $02C0
COLOR0  = $02C4
VDSLST  = $0200
CHBAS   = $02F4
PMBASE  = $D407
GRACTL  = $D01D
HPOSP0  = $D000
SIZEP0  = $D008
PRIOR   = $D01B
COLBK   = $D01A

PM      = $4000         ; P/M memory, one-line resolution (2 KiB aligned)
SCREEN  = $5000

start:  lda #0
        sta SDMCTL      ; DMA off while we build
        ; screen data: an ascending ramp
        ldx #0
fill:   txa
        sta SCREEN,x
        sta SCREEN+$100,x
        sta SCREEN+$200,x
        sta SCREEN+$300,x
        inx
        bne fill
        ; players: solid bars from line 40 to 200, one byte each
        ldx #0
pmclr:  lda #0
        sta PM+$300,x
        sta PM+$400,x
        sta PM+$500,x
        sta PM+$600,x
        sta PM+$700,x
        inx
        bne pmclr
        ldx #40
pmset:  lda #$F0
        sta PM+$400,x
        lda #$3C
        sta PM+$500,x
        lda #$FF
        sta PM+$600,x
        lda #$81
        sta PM+$700,x
        inx
        cpx #200
        bne pmset
        lda #>PM
        sta PMBASE
        lda #3
        sta GRACTL
        ldx #3
pos:    lda xpos,x
        sta HPOSP0,x
        lda #1
        sta SIZEP0,x
        lda pcol,x
        sta PCOLR0,x
        dex
        bpl pos
        ldx #4
cols:   lda pfcol,x
        sta COLOR0,x
        dex
        bpl cols
        lda #1
        sta GPRIOR
        lda #<dli1
        sta VDSLST
        lda #>dli1
        sta VDSLST+1
        lda #<dlist
        sta SDLSTL
        lda #>dlist
        sta SDLSTL+1
        lda #$C0
        sta NMIEN
        lda #$3E        ; normal playfield, DL, P/M one-line, players+missiles
        sta SDMCTL
        jmp *

; the DLI before the mode 9 band turns mode 9 on; the next one turns it off
dli1:   pha
        lda #$41
        sta WSYNC
        sta PRIOR
        lda #$90
        sta COLBK
        lda #<dli2
        sta VDSLST
        pla
        rti
dli2:   pha
        lda #$01
        sta WSYNC
        sta PRIOR
        lda #$00
        sta COLBK
        lda #<dli1
        sta VDSLST
        pla
        rti

xpos:   .byte $50, $70, $98, $C0
pcol:   .byte $46, $86, $C6, $1E
pfcol:  .byte $28, $CA, $94, $3A, $00

.assert >dli1 = >dli2, error, "the DLIs share a page"

dlist:  .byte $70, $70, $70
        .byte $42, <SCREEN, >SCREEN, $02          ; mode 2 (ramp: all characters, inverse half)
        .byte $44, <SCREEN, >SCREEN               ; mode 4
        .byte $46, <SCREEN, >SCREEN, $07          ; modes 6, 7
        .byte $48, <SCREEN, >SCREEN, $08          ; mode 8
        .byte $4A, <SCREEN, >SCREEN, $0A, $0A
        .byte $4D, <SCREEN, >SCREEN, $0D, $0D, $0D
        .byte $4E, <SCREEN, >SCREEN
        .repeat 15
        .byte $0E
        .endrepeat
        .byte $CF, <SCREEN, >SCREEN               ; mode F, DLI before the mode 9 band
        .repeat 15
        .byte $0F
        .endrepeat
        .byte $8F                                 ; DLI: mode 9 ends after the next
        .repeat 16
        .byte $0F
        .endrepeat
        .byte $41, <dlist, >dlist
