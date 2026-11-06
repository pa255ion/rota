; The Atari 800XL boot splash (PLAN.md, side track J, step 6; docs/brand.md,
; "The Atari 800XL's layout": layout A).
;
; A stream player: demos/rota-brand (Rota.A800, Rota.A800.Rom) lays out
; the screen and every frame's records; Rota.A800.renderShot is the exact
; model of the picture, and the layer 3 tests compare every frame with it.
; The generator (make demos) writes rota_pic.inc (the first beat's screen,
; player and missile memory and display list, the streams, the fabric's
; step lists, the display-list interrupts) and rota_sound.inc.
;
; Each frame (frame k shows, frame k + 1 is prepared):
;   after VBI k      frame k + 1's records into buffers and its gradient;
;                    its fabric step, each cell written once the beam has
;                    drawn it in frame k
;   from line 224    frame k + 1's slots, then the lambdas' reveal and the
;                    sparks' and lights' graphics (done by line 32)
;   VBI k + 1        line 0's positions and sizes, the first DLI, the
;                    sound (deferred VBI); the OS copies the shadows
;   display          display-list interrupts reload colours and positions
;                    down the screen from the slots (Rota.A800.Rom.dliPlan)
;
; Started by the XEX loader after AltirraOS has booted; the OS stays on.
; After the chime the idle loop plays for ever: waiting for a key is the
; PS's job.

; OS
ATRACT  = $4D
VDSLST  = $0200
SDMCTL  = $022F
SDLSTL  = $0230
GPRIOR  = $026F
PCOLR0  = $02C0
COLOR0  = $02C4
SETVBV  = $E45C
XITVBV  = $E462
RUNAD   = $02E0

; Hardware
HPOSP0  = $D000
HPOSP1  = $D001
HPOSP2  = $D002
HPOSP3  = $D003
HPOSM0  = $D004
HPOSM1  = $D005
HPOSM2  = $D006
HPOSM3  = $D007
SIZEM   = $D00C
GRAFP0  = $D00D            ; GRAFP1 to GRAFP3 and GRAFM follow
COLPM0  = $D012
COLPM1  = $D013
COLPM2  = $D014
COLPM3  = $D015
COLPF0  = $D016
COLPF1  = $D017
COLPF2  = $D018
COLPF3  = $D019
COLBK   = $D01A
GRACTL  = $D01D
AUDF1   = $D200
PMBASE  = $D407
WSYNC   = $D40A
VCOUNT  = $D40B
NMIEN   = $D40E

PM       = $A000
MISSILES = PM + $300
PLAYERS  = PM + $400
DMA_ON   = $3E          ; normal playfield, missiles, players, one-line resolution, display list
EMPTY    = $FE          ; no sprite

.include "rota_pic.inc"

.segment "ZEROPAGE"
; slots the DLIs and the VBI read (Rota.A800.Rom: the Slot names)
stage:      .res 5
grad:       .res 32
aboveX:     .res 3
belowX:     .res 3
bandX:      .res 2
lightX:     .res 4
bandCol:    .res 1
SLOTS_N = 13            ; aboveX .. bandCol
; the main loop's
ptr:        .res 2
src:        .res 2
looks:      .res 2      ; the streams
sparks:     .res 2
lights:     .res 2
fab:        .res 2
reveal:     .res 2
snd:        .res 2
kApply:     .res 2      ; the ROM frame apply is for
gph:        .res 2      ; 14 p mod 512 (p: picture frames since the first beat)
t0:         .res 1
t1:         .res 1
t2:         .res 1
pv:         .res 1      ; a voice or side
pn:         .res 1      ; rows left
pi:         .res 1      ; a destination index
ps:         .res 1      ; a source index
tick:       .res 1      ; counted by the VBI
vt:         .res 1      ; the VBI's own (it interrupts the main loop)
seenV:      .res 1      ; the last VCOUNT the fabric's pass read
pendingB:   .res 1      ; a step's lower part is still to write
seen:       .res 1

.segment "BSS"
; frame k + 1, prepared while frame k shows
nLooks:     .res 6
nSparks:    .res 16
nLights:    .res 4
nSteps:     .res 1
nGrad:      .res 32
nSlots:     .res SLOTS_N
; what player and missile memory holds now: first scanline (EMPTY: none), rows
curP:       .res 6
curM:       .res 4
step:       .res 1      ; the fabric's step, 0 to 15

.segment "XEXMARK"
    .word $FFFF
.segment "XEXHDR1"
    .word __MAIN_START__, __MAIN_LAST__ - 1
.segment "XEXHDR2"
    .word __SCR1_START__, __SCR1_LAST__ - 1
.segment "XEXHDR3"
    .word __SCR2_START__, __SCR2_LAST__ - 1
.segment "XEXHDR4"
    .word __PM_START__, __PM_LAST__ - 1
.segment "XEXHDR5"
    .word __DL_START__, __DL_LAST__ - 1
.segment "XEXRUN"
    .word RUNAD, RUNAD + 1, start
.import __MAIN_START__, __MAIN_LAST__, __SCR1_START__, __SCR1_LAST__, __SCR2_START__, __SCR2_LAST__
.import __PM_START__, __PM_LAST__, __DL_START__, __DL_LAST__

.segment "CODE"

start:
    lda #0
    sta SDMCTL              ; dark through the lead-in (the OS copies it at its VBI)
    ldx #$7F                ; our zero page
@zp:
    sta $80, x
    dex
    bpl @zp
    sta step
    lda #EMPTY
    ldx #5
@cur:
    sta curP, x
    dex
    bpl @cur
    ldx #3
@curm:
    sta curM, x
    dex
    bpl @curm
    lda #>PM
    sta PMBASE
    ; the players' and missiles' graphics off until the display is on: with
    ; DMA off GTIA keeps showing its graphics registers on every line (a
    ; line down the middle through the lead-in, from the core's missiles)
    lda #0
    sta GRACTL
    ldx #4
@graf:
    sta GRAFP0, x
    dex
    bpl @graf
    lda #$11                ; the fifth player (missiles as PF3), players over the playfield
    sta GPRIOR
    ldx #4                  ; colours: PF0 to PF3, background; the sparks' players
@col:
    lda shadowColours, x
    sta COLOR0, x
    dex
    bpl @col
    ldx #2
@pcol:
    lda sparkColours, x
    sta PCOLR0, x
    dex
    bpl @pcol
    lda #<RotaDl
    sta SDLSTL
    lda #>RotaDl
    sta SDLSTL + 1
    ldx #11
@ptrs:
    lda streamStarts, x
    sta looks, x
    dex
    bpl @ptrs
    lda #<RotaDli0
    sta VDSLST
    lda #>RotaDli0
    sta VDSLST + 1
    lda #7                  ; our deferred VBI
    ldx #>vbi
    ldy #<vbi
    jsr SETVBV
    lda #$C0                ; DLIs (none show until the display is on) and VBIs
    sta NMIEN
    jsr prepare             ; frame 0

main:
    jsr apply               ; from line 224: frame k + 1's slots and graphics
@vbi:
    lda tick                ; until its VBI (it may have come during apply)
    cmp seen
    beq @vbi
    jsr fabricB             ; the lower part of the step just shown, ahead of the beam
    jsr prepare             ; the next frame's records
@top:
    lda VCOUNT              ; this frame's top (the VBI was at the last one's bottom)
    cmp #120
    bcs @top
    jsr fabric              ; its fabric step, behind the beam
    jmp main

shadowColours:
    .byte ROTA_COL_FABRIC, 0, 0, ROTA_COL_LIGHT, ROTA_COL_BK
sparkColours:
    .byte ROTA_COL_LEAD, ROTA_COL_HARM, ROTA_COL_BASS
streamStarts:
    .word RotaLooks, RotaSparks, RotaLights, RotaFab, RotaReveal, rota_sound

; --- the deferred VBI: line 0's positions and sizes, the first DLI ---
vbi:
    lda aboveX
    sta HPOSP0
    lda aboveX + 1
    sta HPOSP1
    lda aboveX + 2
    sta HPOSP2
    lda #0
    sta HPOSP3
    sta SIZEM
    sta ATRACT
    lda #(48 + 156 / 2)
    sta HPOSM0
    lda #(48 + 160 / 2)
    sta HPOSM1
    lda lightX
    sta HPOSM2
    lda lightX + 1
    sta HPOSM3
    lda #<RotaDli0
    sta VDSLST
    lda #>RotaDli0
    sta VDSLST + 1
    jsr sound
    inc tick
    jmp XITVBV

; --- the sound stream: (register, value) pairs into $D200 + register ---
sound:
    ldy #0
    lda (snd), y
    cmp #$FF
    beq @end
    iny
    tax
    beq @adv
    stx vt
@pair:
    lda (snd), y
    tax
    iny
    lda (snd), y
    iny
    sta AUDF1, x
    dec vt
    bne @pair
@adv:
    tya
    clc
    adc snd
    sta snd
    bcc @end
    inc snd + 1
@end:
    rts

.macro record stream, loop, buffer, size
.local @copy, @byte, @go
    ldy #0
    lda (stream), y
    cmp #$FF
    bne @go
    lda #<loop
    sta stream
    lda #>loop
    sta stream + 1
@go:
    ldy #size - 1
@byte:
    lda (stream), y
    sta buffer, y
    dey
    bpl @byte
    clc
    lda stream
    adc #size
    sta stream
    bcc @copy
    inc stream + 1
@copy:
.endmacro

; --- the next frame's records, its gradient ---
prepare:
    record looks, RotaLooksLoop, nLooks, 6
    record sparks, RotaSparksLoop, nSparks, 16
    record lights, RotaLightsLoop, nLights, 4
    record fab, RotaFabLoop, nSteps, 1
    ; the gradient: dim, or lit: phase (27 y - 14 p) / 2 from line 80
    lda nLooks + 5
    and #2
    bne @lit
    lda #ROTA_DIM
    ldx #31
@dim:
    sta nGrad, x
    dex
    bpl @dim
    jmp @gradDone
@lit:
    sec                     ; t = (27 * 80 - gph) mod 512 = (112 - gph) mod 512
    lda #112
    sbc gph
    sta t0
    lda #0
    sbc gph + 1
    and #1
    sta t1
    ldx #0
@line:
    lda t1                  ; the phase: t >> 1
    lsr a
    lda t0
    ror a
    tay
    lda RotaGradLevel, y
    tay
    lda RotaGradColour, y
    sta nGrad, x
    clc                     ; t += 27 (mod 512)
    lda t0
    adc #27
    sta t0
    lda t1
    adc #0
    and #1
    sta t1
    inx
    cpx #32
    bne @line
@gradDone:
    lda nLooks + 5          ; p counts from the first beat
    and #1
    beq @noP
    clc
    lda gph
    adc #14
    sta gph
    lda gph + 1
    adc #0
    and #1
    sta gph + 1
@noP:
    rts

; --- the fabric's step: each cell of the move from step to step + 1 once
; the beam has drawn it (its VCOUNT), all within this frame ---
fabric:
    lda nSteps
    bne @go
    rts
@go:
    ldx step
    lda RotaStepsLo, x
    sta src
    lda RotaStepsHi, x
    sta src + 1
    lda #0                  ; nothing passed yet in this frame
    sta seenV
@cell:
    ldy #0
    lda (src), y            ; VCOUNT once the beam is past it, or $FF
    cmp #$FF
    beq @done
    cmp seenV               ; the beam was past it already
    bcc @ready
    beq @ready
@wait:
    ldx VCOUNT
    stx seenV
    cmp seenV
    beq @ready
    bcs @wait               ; not there yet
@ready:
    iny
    lda (src), y            ; the cell
    sta ptr
    iny
    lda (src), y
    sta ptr + 1
    iny
    lda (src), y            ; its move
    sta @call + 1
    iny
    lda (src), y
    sta @call + 2
@call:
    jsr $FFFF
    clc                     ; the next cell
    lda src
    adc #5
    sta src
    bcc @cell
    inc src + 1
    jmp @cell
@done:
    lda #1                  ; its lower part after the VBI
    sta pendingB
    rts

; fabricB: the lower part of the step whose upper part was written last
; frame (lines from Rota.A800.Rom.splitLine), ahead of the beam; then the
; step is done
fabricB:
    lda pendingB
    bne @go
    rts
@go:
    lda #0
    sta pendingB
    ldx step
    lda RotaStepsBLo, x
    sta src
    lda RotaStepsBHi, x
    sta src + 1
    ldy #0
    lda (src), y            ; the count
    sta pn
    inc src
    bne @cell
    inc src + 1
@cell:
    lda pn
    beq @done
    ldy #0
    lda (src), y
    sta ptr
    iny
    lda (src), y
    sta ptr + 1
    iny
    lda (src), y
    sta @call + 1
    iny
    lda (src), y
    sta @call + 2
@call:
    jsr $FFFF
    clc
    lda src
    adc #4
    sta src
    bcc :+
    inc src + 1
:
    dec pn
    jmp @cell
@done:
    lda step
    clc
    adc #1
    and #15
    sta step
    rts

; --- from line 224: frame k + 1's slots, then its graphics ---
apply:
    lda tick
    sta seen
@wait:
    lda VCOUNT
    cmp #112
    bcc @wait
    ; slots: the stages, the gradient
    ldx #4
@st:
    lda nLooks, x
    sta stage, x
    dex
    bpl @st
    ldx #31
@gr:
    lda nGrad, x
    sta grad, x
    dex
    bpl @gr
    ; the display: on from the first beat
    lda nLooks + 5
    and #1
    beq @off
    lda #3                  ; the players' and missiles' DMA into GTIA
    sta GRACTL
    lda #DMA_ON
@off:
    sta SDMCTL
    ; the sparks' players: positions, then graphics
    ldx #0                  ; voice
@voice:
    stx t2
    txa
    asl a
    adc t2                  ; 3 v
    tay
    lda nSparks, y          ; y0 or EMPTY
    cmp #EMPTY
    beq @noSpark
    cmp #SPARK_TOP
    lda nSparks + 1, y
    bcs @below
    sta aboveX, x
    lda #0
    sta belowX, x
    beq @placed
@below:
    sta belowX, x
    lda #0
    sta aboveX, x
    beq @placed
@noSpark:
    lda #0
    sta aboveX, x
    sta belowX, x
@placed:
    inx
    cpx #3
    bne @voice
    lda nSparks + 9 + 1     ; the band's missiles
    sta bandX
    lda nSparks + 12 + 1
    sta bandX + 1
    lda nSparks + 15
    sta bandCol
    ldx #3
@lx:
    lda nLights, x
    sta lightX, x
    dex
    bpl @lx
    jsr players
    jsr bandMissiles
    jsr lightGraphics
    jsr revealWrites
    rts

; players: per voice clear the rows it had, draw its shape
players:
    ldx #0
@v:
    stx pv
    lda #0                  ; ptr = PLAYERS + $100 v
    sta ptr
    txa
    clc
    adc #>PLAYERS
    sta ptr + 1
    txa
    asl a
    tax                     ; curP + 2 v
    lda curP, x
    cmp #EMPTY
    beq @draw
    tay
    lda curP + 1, x
    sta pn
    lda #0
@clr:
    sta (ptr), y
    iny
    dec pn
    bne @clr
    lda #EMPTY
    sta curP, x
@draw:
    lda pv                  ; the record: nSparks + 3 v
    asl a
    adc pv
    tay
    lda nSparks, y
    cmp #EMPTY
    beq @nextV
    clc
    adc #32                 ; its first scanline
    sta curP, x
    sta pi
    lda nSparks + 2, y
    tay
    lda RotaPlayerShapeLo, y
    sta src
    lda RotaPlayerShapeHi, y
    sta src + 1
    ldy #0
    lda (src), y
    sta curP + 1, x
    sta pn
@row:
    iny
    lda (src), y
    sty ps
    ldy pi
    sta (ptr), y
    inc pi
    ldy ps
    dec pn
    bne @row
@nextV:
    ldx pv
    inx
    cpx #3
    bne @v
    rts

; bandMissiles: the band's sparks as M2 (left, bits 4-5) and M3 (right,
; bits 6-7): clear the rows each had, draw its shape (two bits a row)
bandMissiles:
    ldx #0
@s:
    stx pv
    txa
    asl a
    tax                     ; curM + 2 s
    lda curM, x
    cmp #EMPTY
    beq @draw
    tay
    lda curM + 1, x
    sta pn
    ldx pv
@clr:
    lda MISSILES, y
    and keepMask, x
    sta MISSILES, y
    iny
    dec pn
    bne @clr
    lda pv
    asl a
    tax
    lda #EMPTY
    sta curM, x
@draw:
    lda pv                  ; the record: nSparks + 9 + 3 s
    asl a
    adc pv
    adc #9
    tay
    lda nSparks, y
    cmp #EMPTY
    beq @nextS
    clc
    adc #32
    sta curM, x
    sta pi
    lda nSparks + 2, y
    tay
    lda RotaMissileShapeLo, y
    sta src
    lda RotaMissileShapeHi, y
    sta src + 1
    ldy #0
    lda (src), y
    sta curM + 1, x
    sta pn
@row:
    iny
    lda (src), y            ; 0 to 3
    sty ps
    ldx pv
    beq @m2
    clc
    adc #4                  ; M3's values follow M2's in missileBits
@m2:
    tax
    lda missileBits, x
    ldy pi
    ora MISSILES, y
    sta MISSILES, y
    inc pi
    ldy ps
    dec pn
    bne @row
@nextS:
    ldx pv
    inx
    cpx #2
    bne @s
    rts

keepMask:
    .byte $CF, $3F
missileBits:
    .byte $00, $10, $20, $30, $00, $40, $80, $C0

; lightGraphics: the two light rows (Rota.A800.lightRows), each light
; three rows from the line above, both bits (the lab's 3 x 3 square in
; one colour), M2 left and M3 right
lightGraphics:
    ldx #0
    ldy #ROTA_LIGHT_SCAN0
    jsr lightRow
    ldx #2
    ldy #ROTA_LIGHT_SCAN1
lightRow:
    lda #0
    sta t0
    lda nLights, x
    beq @noL
    lda #$30
    sta t0
@noL:
    lda nLights + 1, x
    beq @noR
    lda t0
    ora #$C0
    sta t0
@noR:
    lda t0
    sta MISSILES, y
    sta MISSILES + 1, y
    sta MISSILES + 2, y
    rts

; revealWrites: in the intro, this frame's bitmap writes (the lambdas)
revealWrites:
    lda kApply + 1
    cmp #>ROTA_INTRO
    bcc @do
    bne @count
    lda kApply
    cmp #<ROTA_INTRO
    bcs @count
@do:
    ldy #0
    lda (reveal), y
    sta pn
    inc reveal
    bne @w
    inc reveal + 1
@w:
    lda pn
    beq @count
    ldy #0
    lda (reveal), y
    sta ptr
    iny
    lda (reveal), y
    sta ptr + 1
    iny
    lda (reveal), y
    ldy #0
    sta (ptr), y
    clc
    lda reveal
    adc #3
    sta reveal
    bcc :+
    inc reveal + 1
:
    dec pn
    jmp @w
@count:
    inc kApply
    bne :+
    inc kApply + 1
:
    rts

.segment "RODATA"
.include "rota_sound.inc"
