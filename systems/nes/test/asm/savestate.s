; Save states (side track N, N.7): a program that leaves at rest, in the
; vertical blank, what the restore routine must put back besides the usual:
; w set (one $2005 write in the NMI), the controllers' strobe high (every
; other frame), the DMC's sample buffer full (a 1-byte sample each NMI at
; the slowest rate: its output unit takes it up to 3424 cycles later), and
; the frame counter in each of four setups, 16 frames each: 5-step with
; its IRQ allowed (no flag), 4-step with its flag never acknowledged
; (I is set: no IRQ taken), 4-step acknowledged every frame by a $4015
; read, 4-step inhibited. In the third, $4017 is written again late in
; the NMI, just before line 243 (the first a state is taken at): the
; sequencer then counts from near 0 at rest, and the routine's own reset
; is a sequence earlier (its flag set in the routine's tail, cleared by
; its $4015 read). Rendering on, NMI on; the main loop idles.
.include "nrom.inc"

PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007

DELAY = 30

.segment "ZEROPAGE"
frame: .res 1

.segment "CODE"
reset:
    sei
    cld
    ldx #$FF
    txs
    lda #0
    sta PPUCTRL
    sta PPUMASK
    sta frame
    bit PPUSTATUS
vbl1:
    bit PPUSTATUS
    bpl vbl1
vbl2:
    bit PPUSTATUS
    bpl vbl2
    ; a backdrop and one palette
    lda #$3F
    sta PPUADDR
    lda #$00
    sta PPUADDR
    ldx #0
pal:
    lda palette,x
    sta PPUDATA
    inx
    cpx #4
    bne pal
    lda #$80            ; NMI on
    sta PPUCTRL
    lda #$0A            ; background on, left column shown
    sta PPUMASK
forever:
    jmp forever

nmi:
    pha
    txa
    pha
    bit PPUSTATUS       ; w = 0
    inc frame
    ; a new frame counter setup every 16 frames
    lda frame
    and #$0F
    bne same
    lda frame
    lsr
    lsr
    lsr
    lsr
    and #3
    tax
    lda modes,x
    sta $4017
same:
    ; setup 2: the flag acknowledged
    lda frame
    and #$30
    cmp #$20
    bne noack
    lda $4015
    ldx #DELAY          ; to just before line 243
wait:
    dex
    bne wait
    lda #$00
    sta $4017
noack:
    ; the DMC: 1 byte from $C000 at the slowest rate, no IRQ, no loop
    lda #$00
    sta $4010
    sta $4012
    sta $4013
    lda #$10
    sta $4015
    ; the strobe high on odd frames
    lda frame
    and #1
    sta $4016
    ; one $2005 write: coarse and fine X, w = 1
    lda frame
    sta PPUSCROLL
    pla
    tax
    pla
irq:
    rti

modes:
    .byte $80, $00, $00, $40
palette:
    .byte $0F, $16, $2A, $12

.segment "VECTORS"
    .word nmi, reset, irq
