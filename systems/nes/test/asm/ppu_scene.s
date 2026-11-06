; A still picture for the PPU goldens (step 3.3; later the Clash PPU's,
; steps 3.5 and 3.7): background tiles in all four palettes through the
; attribute table, fine X and Y scroll, both nametables, 8x8 sprites with
; every flip, behind-background priority, and the left-column masks off.
.include "nrom.inc"

PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
OAMADDR   = $2003
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007
OAMDMA    = $4014

.segment "ZEROPAGE"
tmp: .res 1

.segment "CODE"
reset:
    sei
    cld
    ldx #$FF
    txs
    lda #0
    sta PPUCTRL
    sta PPUMASK
    bit PPUSTATUS
vbl1:
    bit PPUSTATUS
    bpl vbl1
vbl2:
    bit PPUSTATUS
    bpl vbl2

    ; Palette
    lda #$3F
    sta PPUADDR
    lda #$00
    sta PPUADDR
    ldx #0
pal:
    lda palette,x
    sta PPUDATA
    inx
    cpx #32
    bne pal

    ; Nametables $2000 and $2400: tile = ((col/4 + row/4) & 3) + 1 above
    ; row 22, blank below; then attributes = position * $1B.
    lda #$20
    jsr fill_nt
    lda #$24
    jsr fill_nt

    ; Sprites at $0200: 16 of tile 5 over the blank rows, two rows of 8
    ; (OAM Y 176 and 208, X = 8 + 30 * (i & 7)); attributes: palette i & 3,
    ; flips and priority from bits 1 to 3 of i.
    ldx #0
    ldy #0
spr:
    tya
    and #8
    asl a
    asl a
    adc #184
    sec
    sbc #8
    sta $0200,x         ; Y: 176, or 208 for i >= 8 (drawn one line lower)
    lda #5
    sta $0201,x         ; tile
    tya
    and #3
    sta tmp
    tya
    asl a
    asl a
    asl a
    asl a
    and #$E0
    ora tmp
    sta $0202,x         ; attributes
    tya
    and #7
    sta tmp
    asl a
    asl a
    asl a
    asl a
    asl a
    sec
    sbc tmp
    sbc tmp
    clc
    adc #8
    sta $0203,x         ; X = 8 + 30 * (i & 7)
    inx
    inx
    inx
    inx
    iny
    cpy #16
    bne spr
    lda #$FF            ; the other 48 sprites off screen
hide:
    sta $0200,x
    inx
    bne hide
    lda #0
    sta OAMADDR
    lda #$02
    sta OAMDMA

    ; Scroll: X = 3 (fine X 3), Y = 5; nametable $2000.
vbl3:
    bit PPUSTATUS
    bpl vbl3
    lda #3
    sta PPUSCROLL
    lda #5
    sta PPUSCROLL
    lda #%00000000      ; NMI off, 8x8 sprites, both tables at $0000
    sta PPUCTRL
    lda #%00011110      ; BG and sprites on, left column shown
    sta PPUMASK
forever:
    jmp forever

; A = high byte of a nametable.
fill_nt:
    sta PPUADDR
    lda #0
    sta PPUADDR
    ldy #0              ; row
row:
    ldx #0              ; column
col:
    lda #0
    cpy #22
    bcs blank
    txa
    lsr a
    lsr a
    sta tmp
    tya
    lsr a
    lsr a
    clc
    adc tmp
    and #3
    clc
    adc #1
blank:
    sta PPUDATA
    inx
    cpx #32
    bne col
    iny
    cpy #30
    bne row
    ldx #0
attr:
    txa
    asl a
    asl a
    asl a
    asl a
    asl a
    sta tmp
    txa
    asl a
    asl a
    asl a
    asl a
    clc
    adc tmp
    sta tmp
    txa
    clc
    adc tmp
    adc tmp
    sta PPUDATA
    inx
    cpx #64
    bne attr
    rts

nmi:
irq:
    rti

palette:
    .byte $0F, $16, $2A, $12,  $0F, $27, $1A, $21,  $0F, $30, $14, $08,  $0F, $3C, $05, $38
    .byte $0F, $30, $16, $02,  $0F, $28, $0A, $11,  $0F, $26, $36, $06,  $0F, $19, $29, $3A

.segment "VECTORS"
    .word nmi, reset, irq

.segment "CHARS"
    ; 0: blank
    .res 16, $00
    ; 1: solid colour 1
    .res 8, $FF
    .res 8, $00
    ; 2: checkerboard of colours 1 and 2
    .byte $AA, $55, $AA, $55, $AA, $55, $AA, $55
    .byte $55, $AA, $55, $AA, $55, $AA, $55, $AA
    ; 3: stripes of colours 1, 2, 3 and 0
    .byte $C3, $C3, $C3, $C3, $C3, $C3, $C3, $C3
    .byte $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0
    ; 4: a frame in colour 3 around colour 0
    .byte $FF, $81, $81, $81, $81, $81, $81, $FF
    .byte $FF, $81, $81, $81, $81, $81, $81, $FF
    ; 5: an arrow pointing up-left (shows the flips), colours 1 to 3
    .byte $F0, $C0, $A0, $90, $08, $04, $02, $01
    .byte $FC, $E0, $D0, $C8, $04, $02, $01, $00
