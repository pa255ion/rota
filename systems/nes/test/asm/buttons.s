; Controller 1 on screen (step 3.7, input through the button path): one
; solid 8x8 sprite per button, shown while it's held, left to right A, B,
; Select, Start, Up, Down, Left, Right (X = 32 + 24 i, Y = 112). The
; layer 4 run holds BTNC (A) and BTNU (Up).
.include "nrom.inc"

PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
OAMADDR   = $2003
PPUADDR   = $2006
PPUDATA   = $2007
OAMDMA    = $4014
JOY1      = $4016

.segment "ZEROPAGE"
pad: .res 1

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
    ; palette: backdrop dark blue, sprite palette 0 = white, yellow, red
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
    ; nametable 0: all tile 0 (blank)
    lda #$20
    sta PPUADDR
    lda #$00
    sta PPUADDR
    ldx #0
    ldy #4
    lda #0
clear:
    sta PPUDATA
    inx
    bne clear
    dey
    bne clear
    ; all sprites off screen
    lda #$FF
    ldx #0
hide:
    sta $0200,x
    inx
    bne hide
    lda #0
    sta PPUCTRL
    lda #%00010110      ; sprites on, BG off, left column shown
    sta PPUMASK

frame:
    bit PPUSTATUS       ; wait for VBlank
wait:
    bit PPUSTATUS
    bpl wait
    lda #0
    sta OAMADDR
    lda #$02
    sta OAMDMA
    ; read controller 1
    lda #1
    sta JOY1
    lda #0
    sta JOY1
    ldx #8
read:
    lda JOY1
    lsr a
    ror pad             ; bit 0 (A) ends up in bit 0 after 8 reads
    dex
    bne read
    ; sprites 0 to 7
    ldx #0              ; OAM offset
    ldy #0              ; button
    lda pad
    sta $10             ; working copy
spr:
    lsr $10
    bcs on
    lda #$FF            ; off screen
    sta $0200,x
    jmp next
on:
    lda #111
    sta $0200,x         ; Y (drawn one line lower)
next:
    lda #1
    sta $0201,x         ; tile 1: solid
    tya
    and #3
    sta $0202,x         ; palette
    tya
    asl a
    asl a
    asl a
    sta $11             ; 8 i
    asl a               ; 16 i
    clc
    adc $11             ; 24 i
    adc #32
    sta $0203,x
    inx
    inx
    inx
    inx
    iny
    cpy #8
    bne spr
    jmp frame

nmi:
irq:
    rti

palette:
    .byte $01, $00, $00, $00,  $01, $00, $00, $00,  $01, $00, $00, $00,  $01, $00, $00, $00
    .byte $01, $30, $30, $30,  $01, $28, $28, $28,  $01, $16, $16, $16,  $01, $2A, $2A, $2A

.segment "VECTORS"
    .word nmi, reset, irq

.segment "CHARS"
    .res 16, $00        ; 0: blank
    .res 8, $FF         ; 1: solid (colour 1)
    .res 8, $00
