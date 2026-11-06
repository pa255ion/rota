; Every register a save state's restore routine writes, set to its own
; value (side track N, N.4: the spec's restore tests save this program
; and check each comes back; a routine that skips one fails them). The
; OS's vertical blank copies its shadows to the colours, CHBASE, CHACTL,
; DMACTL, PRIOR and the display list pointer, so those go to the shadows;
; the rest straight to the chips. Then it waits, the OS's VBI running.
.include "a800.inc"
.include "xex.inc"

PCOLR0  = $02C0         ; shadows: COLPM0-3, COLPF0-3, COLBK
SDMCTL  = $022F
GPRIOR  = $026F
CHACT   = $02F3
CHBAS   = $02F4

start:  ldx #8
@col:   txa
        asl a
        asl a
        asl a
        asl a
        ora #$06
        sta PCOLR0,x    ; $86, $76, ... $06: each its own
        dex
        bpl @col
        lda #$3E        ; normal playfield, all DMA, P/M single line
        sta SDMCTL
        lda #$24        ; priority, multicolour players
        sta GPRIOR
        lda #$05
        sta CHACT
        lda #$E0
        sta CHBAS
        ; GTIA: positions, sizes, VDELAY, GRACTL, the data latches, CONSOL's
        ; speaker bit off
        ldx #7
@pos:   txa
        asl a
        asl a
        asl a
        adc #$40
        sta GTIA,x      ; HPOSP0-3, HPOSM0-3: $40, $48, ... $78
        dex
        bpl @pos
        lda #1
        sta GTIA+$08    ; SIZEP0
        lda #3
        sta GTIA+$0B    ; SIZEP3
        lda #$9C
        sta GTIA+$0C    ; SIZEM
        lda #$A5
        sta GTIA+$1C    ; VDELAY
        lda #$04        ; GRACTL: the trigger latch; no P/M DMA into GTIA,
        sta GTIA+$1D    ; so the data latches keep what the CPU writes
        ldx #4
@graf:  txa
        ora #$C0
        sta GTIA+$0D,x  ; GRAFP0-3 $C0-$C3, GRAFM $C4
        dex
        bpl @graf
        lda #$08
        sta CONSOL
        ; ANTIC: scrolling, PMBASE (the OS doesn't touch them)
        lda #$07
        sta $D404       ; HSCROL
        lda #$05
        sta $D405       ; VSCROL
        lda #$A0
        sta $D407       ; PMBASE
        ; POKEY: two linked channels and two plain ones
        lda #$58
        sta AUDCTL
        ldx #7
@aud:   txa
        asl a
        asl a
        asl a
        asl a
        ora #$03
        sta AUDF1,x     ; AUDF1 $03, AUDC1 $13, AUDF2 $23, ... AUDC4 $73
        dex
        bpl @aud
        ; the PIA: port A's high bits as outputs (sticks 2), driven $50
        lda #$38
        sta PACTL       ; direction register, CA2 output high
        lda #$F0
        sta PORTA
        lda #$3C
        sta PACTL       ; data register
        lda #$50
        sta PORTA
        jmp *
