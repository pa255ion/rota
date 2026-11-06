; The NES boot splash (PLAN.md, side track J, step 3; docs/brand.md,
; "Rendition B" and "The NES's palettes").
;
; A stream player: demos/rota-brand (Rota.Nes, Rota.Nes.Rom) lays out every
; tile and every frame's changes; Rota.Nes.Rom.replay is the exact model of
; what this file shows, and the layer 3 tests compare every frame with it.
; The generator (make demos) writes rota_pic.inc (tiles, the nametable,
; the streams, the fabric's step code) and rota_sound.inc (the APU writes).
;
; Each frame, from NMI k:
;   NMI    OAM DMA (the shadow holds frame k's sprites), the palette if it
;          changed, the nametable writes, the fabric's step (generated
;          code), rendering on or off; all prepared before the NMI
;   then   the sound (rota_sound), then frame k + 1: its background record
;          and fabric steps into buffers, its sprites into the shadow
; Each picture stream has an intro and a loop: $FF jumps to the loop.
;
; UxROM (mapper 2) with CHR RAM: bank 0 at $8000 holds the streams and the
; step code and stays selected; the fixed bank holds the rest. Background
; tiles in pattern table 0, sprites in table 1. With SPLIT defined (ca65
; -D SPLIT; the toggle the user asked for, off by default), the bottom
; half's background comes from table 1: after each NMI the main loop waits
; for sprite 0's hit on line 115 and switches the table in line 119's
; horizontal blank. After the chime the idle loop plays for ever: waiting
; for a key is the PS's job.

PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
OAMADDR   = $2003
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007
OAMDMA    = $4014

CTRL_NMI  = %10001000   ; NMI on, 8 x 8 sprites from $1000, background from $0000
CTRL_LOW  = %10011000   ; the same, background from $1000 (SPLIT's bottom half)
SPLIT_WAIT = 106        ; 5-cycle loops from sprite 0's hit to line 119's blank
MASK_ON   = %00011110   ; background and sprites, the left column too
oam       = $0200

overruns  = $FE         ; frames whose work missed the next NMI (read by the layer 3 tests)

.segment "HEADER"
    .byte "NES", $1A, 2, 0, $20, $00, 0, 0, 0, 0, 0, 0, 0, 0

.segment "ZEROPAGE"
ptr:        .res 2
bg:         .res 2      ; the streams
fab:        .res 2
spk:        .res 2
lgt:        .res 2
snd:        .res 2
t0:         .res 1
nmiCount:   .res 1
frameNmi:   .res 1
; prepared for the next NMI
mask:       .res 1      ; PPUMASK
palReady:   .res 1
ntCount:    .res 1
fabSteps:   .res 1
step:       .res 1      ; the fabric's step, 0 to 15
stepPtr:    .res 2
tableHi:    .res 1      ; the step code's pattern table: $00, or $10 (SPLIT's second pass)

.segment "BSS"
palBuf:     .res 32
ntBuf:      .res 3 * 32

.segment "CODE"

reset:
    sei
    cld
    ldx #$FF
    txs
    inx
    stx PPUCTRL
    stx PPUMASK
    stx $4010
    lda #$40
    sta $4017
    lda #0                  ; UxROM: bank 0 at $8000 (written over a 0 for bus conflicts)
    sta bankZero
    bit PPUSTATUS
@vb1:
    bit PPUSTATUS
    bpl @vb1
    lda #0                  ; zero page and RAM
    tax
@clr:
    sta $00, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    inx
    bne @clr
    lda #$FF                ; OAM shadow: every sprite hidden, sprite 0 as given
@oam:
    sta oam, x
    inx
    bne @oam
    ldx #3
@spr0:
    lda RotaSprite0, x
    sta oam, x
    dex
    bpl @spr0
@vb2:
    bit PPUSTATUS
    bpl @vb2

    ; rendering is off: the tiles, the nametable and a black palette
    lda #$00
    sta PPUADDR
    sta PPUADDR
    lda #<RotaTable0
    sta ptr
    lda #>RotaTable0
    sta ptr + 1
    ldx #>(ROTA_BG0_TILES * 16 + 255)
    jsr copyPages
    lda #$10
    sta PPUADDR
    lda #$00
    sta PPUADDR
    lda #<RotaTable1
    sta ptr
    lda #>RotaTable1
    sta ptr + 1
    ldx #>(ROTA_TABLE1_TILES * 16 + 255)
    jsr copyPages
    lda #$20
    sta PPUADDR
    lda #$00
    sta PPUADDR
    lda #<RotaNametable
    sta ptr
    lda #>RotaNametable
    sta ptr + 1
    ldx #4                  ; 960 cells and 64 attribute bytes
    jsr copyPages
    lda #$3F
    sta PPUADDR
    lda #$00
    sta PPUADDR
    lda #$0F
    ldx #32
@black:
    sta PPUDATA
    dex
    bne @black

    lda #<RotaBg
    sta bg
    lda #>RotaBg
    sta bg + 1
    lda #<RotaFab
    sta fab
    lda #>RotaFab
    sta fab + 1
    lda #<RotaSparks
    sta spk
    lda #>RotaSparks
    sta spk + 1
    lda #<RotaLights
    sta lgt
    lda #>RotaLights
    sta lgt + 1
    lda #<rota_sound
    sta snd
    lda #>rota_sound
    sta snd + 1
    jsr prepare             ; frame 0
    lda nmiCount
    sta frameNmi
    lda #CTRL_NMI
    sta PPUCTRL

main:
    lda frameNmi
@wait:
    cmp nmiCount
    beq @wait
    lda nmiCount
    sta frameNmi
.ifdef SPLIT
    lda mask                ; rendering on: the bottom half from table 1
    beq @noSplit
@hitClear:
    bit PPUSTATUS           ; last frame's hit clears at the pre-render line
    bvs @hitClear
@hit:
    bit PPUSTATUS
    bvc @hit
    ldx #SPLIT_WAIT
@delay:
    dex
    bne @delay
    lda #CTRL_LOW
    sta PPUCTRL
@noSplit:
.endif
    jsr sound
    jsr prepare
    lda nmiCount            ; another NMI meanwhile: an overrun
    cmp frameNmi
    beq main
    inc overruns
    sta frameNmi
    jmp main

; copyPages: X pages from ptr to PPUDATA (whole pages; the tail past the
; data is whatever follows, written past the tiles' end).
copyPages:
    ldy #0
@loop:
    lda (ptr), y
    sta PPUDATA
    iny
    bne @loop
    inc ptr + 1
    dex
    bne @loop
    rts

; --- the next frame: its background record, fabric steps and sprites ---
prepare:
    ; background: flags (bit 7 on, bit 6 a palette), [32 bytes], a count,
    ; (high, low, tile) per write
    ldy #0
    lda (bg), y
    cmp #$FF
    bne @bgRec
    lda #<RotaBgLoop
    sta bg
    lda #>RotaBgLoop
    sta bg + 1
    lda (bg), y
@bgRec:
    iny
    tax
    lda #0
    cpx #$80
    bcc @off
    lda #MASK_ON
@off:
    sta mask
    txa
    and #$40
    beq @noPal
    ldx #0
@pal:
    lda (bg), y
    sta palBuf, x
    iny
    inx
    cpx #32
    bne @pal
    lda #1
    sta palReady
@noPal:
    lda (bg), y
    iny
    sta ntCount
    beq @ntDone
    asl
    clc
    adc ntCount             ; 3 bytes a write
    sta t0
    ldx #0
@nt:
    lda (bg), y
    sta ntBuf, x
    iny
    inx
    cpx t0
    bne @nt
@ntDone:
    tya
    clc
    adc bg
    sta bg
    bcc :+
    inc bg + 1
:
    ; fabric: the steps
    ldy #0
    lda (fab), y
    cmp #$FF
    bne @fabRec
    lda #<RotaFabLoop
    sta fab
    lda #>RotaFabLoop
    sta fab + 1
    lda (fab), y
@fabRec:
    sta fabSteps
    inc fab
    bne :+
    inc fab + 1
:
    lda fabSteps
    beq @fabDone
    ldx step
    lda RotaStepsLo, x
    sta stepPtr
    lda RotaStepsHi, x
    sta stepPtr + 1
@fabDone:
    ; sprites
    ldx #spk
    lda #<RotaSparksLoop
    ldy #>RotaSparksLoop
    jsr oamRecord
    ldx #lgt
    lda #<RotaLightsLoop
    ldy #>RotaLightsLoop
    ; fall through

; oamRecord: the sprite stream whose pointer is at zero page X, its loop
; at YA: a count, then (OAM offset, y, tile, attributes, x).
oamRecord:
    sta t0
    lda ($00, x)
    cmp #$FF
    bne @rec
    lda t0
    sta $00, x
    tya
    sta $01, x
    lda ($00, x)
@rec:
    jsr advance
    sta t0
    cmp #0
    beq @done
@sprite:
    lda ($00, x)
    jsr advance
    tay
    lda ($00, x)
    jsr advance
    sta oam, y
    lda ($00, x)
    jsr advance
    sta oam + 1, y
    lda ($00, x)
    jsr advance
    sta oam + 2, y
    lda ($00, x)
    jsr advance
    sta oam + 3, y
    dec t0
    bne @sprite
@done:
    rts

; advance: the pointer at zero page X, one on (A kept).
advance:
    inc $00, x
    bne :+
    inc $01, x
:
    rts

; --- NMI ---
nmi:
    pha
    txa
    pha
    tya
    pha
    lda #0
    sta OAMADDR
    lda #>oam
    sta OAMDMA
    bit PPUSTATUS
    lda palReady
    bne :+
    jmp @noPal
:
    lda #$3F
    sta PPUADDR
    lda #$00
    sta PPUADDR
    .repeat 32, i
        lda palBuf + i
        sta PPUDATA
    .endrepeat
    lda #0
    sta palReady
@noPal:
    ldy ntCount
    beq @noNt
    ldx #0
@nt:
    lda ntBuf, x
    sta PPUADDR
    lda ntBuf + 1, x
    sta PPUADDR
    lda ntBuf + 2, x
    sta PPUDATA
    inx
    inx
    inx
    dey
    bne @nt
    sty ntCount
@noNt:
    lda fabSteps
    beq @noFab
    lda #$00
    sta tableHi
    jsr callStep
.ifdef SPLIT
    lda #$10
    sta tableHi
    jsr callStep
.endif
    lda step
    clc
    adc #1
    and #15
    sta step
    lda #0
    sta fabSteps
@noFab:
    lda #0
    sta PPUSCROLL
    sta PPUSCROLL
    lda #CTRL_NMI
    sta PPUCTRL
    lda mask
    sta PPUMASK
    inc nmiCount
    pla
    tay
    pla
    tax
    pla
irq:
    rti

callStep:
    jmp (stepPtr)

; --- the sound stream: (register, value) pairs into $4000 + register ---
sound:
    ldy #0
    lda (snd), y
    cmp #$FF
    beq @end
    iny
    tax
    beq @adv
    stx t0
@pair:
    lda (snd), y
    tax
    iny
    lda (snd), y
    iny
    sta $4000, x
    dec t0
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

.segment "RODATA"
bankZero:
    .byte 0

.ifdef SPLIT
.include "rota_pic_split.inc"
.else
.include "rota_pic.inc"
.endif
.segment "RODATA"
.include "rota_sound.inc"

.segment "VECTORS"
    .addr nmi, reset, irq
