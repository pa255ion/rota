; Every APU channel at once, for the lockstep of step 3.9 (the channels'
; outputs are compared every CPU cycle): pulse 1 with a falling sweep and
; a looping envelope, pulse 2 with a rising sweep, the triangle with its
; linear counter, the noise in both modes, the DMC looping a sample (with
; its DMA every byte), and register writes every frame from the NMI.
.include "nrom.inc"

.segment "ZEROPAGE"
frame: .res 1

.segment "CODE"
; The DMC sample: $C000 + 0 * 64, 2 * 16 + 1 = 33 bytes.
sample:
    .byte $00, $FF, $0F, $F0, $55, $AA, $33, $CC, $00, $FF, $81, $7E, $18, $E7, $24, $DB
    .byte $FF, $FF, $00, $00, $F0, $0F, $AA, $55, $C3, $3C, $99, $66, $01, $80, $FE, $7F
    .byte $A5

reset:
    sei
    cld
    ldx #$FF
    txs
    lda #0
    sta $2000
    sta $2001
    bit $2002
vbl1:
    bit $2002
    bpl vbl1
vbl2:
    bit $2002
    bpl vbl2
    lda #$00            ; frame counter: 4-step (its IRQ stays masked: SEI)
    sta $4017
    lda #$1F            ; all channels
    sta $4015
    ; pulse 1: duty 1, envelope loop, period 2 (decaying volume)
    lda #$62
    sta $4000
    lda #$9A            ; sweep on, period 1, negate, shift 2
    sta $4001
    lda #$80
    sta $4002
    lda #$0B
    sta $4003
    ; pulse 2: duty 3, constant volume 9, sweep up slowly
    lda #$D9
    sta $4004
    lda #$C7            ; sweep on, period 4, shift 7
    sta $4005
    lda #$40
    sta $4006
    lda #$02
    sta $4007
    ; triangle: linear counter 40
    lda #$28
    sta $4008
    lda #$60
    sta $400A
    lda #$08
    sta $400B
    ; noise: envelope period 3, mode 0, period index 4
    lda #$03
    sta $400C
    lda #$04
    sta $400E
    lda #$18
    sta $400F
    ; DMC: loop, rate 14, sample at $C000, 33 bytes
    lda #$4E
    sta $4010
    lda #$40
    sta $4011
    lda #$00
    sta $4012
    lda #$02
    sta $4013
    lda #$1F            ; start the DMC (load DMA)
    sta $4015
    lda #$80            ; NMI on
    sta $2000
forever:
    jmp forever

nmi:
    inc frame
    lda frame
    and #$07
    bne nmi_done
    ; every 8 frames: restart notes, flip the noise mode, retrigger the triangle
    lda frame
    and #$08
    asl a
    asl a
    asl a
    asl a               ; bit 7 = noise mode
    ora #$06
    sta $400E
    lda #$18
    sta $400F
    lda frame
    sta $4002
    lda #$0B
    sta $4003
    lda #$08
    sta $400B
    lda #$80
    sta $4006
    lda #$02
    sta $4007
nmi_done:
    rti
irq:
    rti

.segment "VECTORS"
    .word nmi, reset, irq
