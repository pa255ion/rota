; MMC5 (mapper 5; side track D, nesdev wiki "MMC5"): what the visual test
; ROMs (roms/nes/mmc5) don't check by numbers. Layout: mmc5.cfg (PRG bank
; n starts with $A0 + n, CHR 1 KiB bank n with $C0 + n). Blargg's
; protocol in PRG RAM page 0 (unlocked first: $5102 = 2, $5103 = 1);
; status = the failed check:
;
; 2  PRG mode 3: $5114-$5116 choose 8 KiB ROM banks at $8000, $A000, $C000
; 3  PRG mode 2: $5115 a 16 KiB bank at $8000 (its bit 0 ignored), $5116
;    an 8 KiB one at $C000
; 4  PRG mode 1: $5115 16 KiB at $8000, $5117 16 KiB at $C000
; 5  PRG mode 0: $5117 32 KiB (its bits 1-0 ignored)
; 6  PRG RAM through $8000 ($5114 bit 7 clear): pages 2 and 3 apart, and
;    page 2 at $6000 through $5113
; 7  PRG RAM write protection: $5102 not 2, writes ignored
; 8  the multiplier: $5205 * $5206
; 9  ExRAM: mode 2 read/write, mode 3 read-only, mode 0 outside
;    rendering writes 0
; 10 CHR banks through $2007 (8x8 sprites: $5120-$5127): 1 KiB mode
;    $5123 at $0C00, 8 KiB mode $5127
; 11 nametables ($5105): fill tile and attribute ($5106/$5107), ExRAM as
;    a nametable (written and read through $2007, read back at $5C00)
; 12 the scanline IRQ: $5203 = 32, enabled, background on: IRQs come,
;    $5204 shows pending and in-frame; rendering off ends the frame
.segment "HEADER"
    .byte "NES", $1A
    .byte 4, 2              ; 64 KiB PRG, 16 KiB CHR
    .byte $50, $00          ; mapper 5
    .res 8, 0

.segment "ZEROPAGE"
irqs:   .res 1
st5204: .res 1
tmp:    .res 1

.segment "BANK0"
    .byte $A0
.segment "BANK1"
    .byte $A1
.segment "BANK2"
    .byte $A2
.segment "BANK3"
    .byte $A3
.segment "BANK4"
    .byte $A4
.segment "BANK5"
    .byte $A5
.segment "BANK6"
    .byte $A6

.segment "CHARS"
    .repeat 16, I
    .byte $C0 + I
    .res $3FF, 0
    .endrepeat

.macro check n
    lda #n
    sta $6010
.endmacro

; A = the byte expected at addr, else fail
.macro expect addr, v
    lda addr
    cmp #v
    beq :+
    jmp fail
:
.endmacro

; the PPU byte at addr through $2007 (after the buffer's dummy read)
.macro ppuread addr
    bit $2002
    lda #>addr
    sta $2006
    lda #<addr
    sta $2006
    lda $2007
    lda $2007
.endmacro

.segment "CODE"
reset:
    sei
    cld
    ldx #$FF
    txs
    lda #2                  ; PRG RAM writable, page 0 at $6000
    sta $5102
    lda #1
    sta $5103
    lda #0
    sta $5113
    lda #$80                ; running
    sta $6000
    lda #$DE
    sta $6001
    lda #$B0
    sta $6002
    lda #$61
    sta $6003
    lda #0
    sta $6004               ; no text
    sta $2000
    sta $2001
    lda #$40                ; no frame IRQs
    sta $4017
    bit $2002
vbl1:
    bit $2002
    bpl vbl1
vbl2:
    bit $2002
    bpl vbl2

    ; PRG mode 3
    check 2
    lda #3
    sta $5100
    ldx #0
m3loop:
    txa
    ora #$80
    sta $5114
    txa
    ora #$A0
    sta tmp
    lda $8000
    cmp tmp
    beq :+
    jmp fail
:   inx
    cpx #7
    bne m3loop
    lda #$83
    sta $5115
    expect $A000, $A3
    lda #$85
    sta $5116
    expect $C000, $A5

    ; PRG mode 2
    check 3
    lda #2
    sta $5100
    lda #$85
    sta $5115
    expect $8000, $A4
    expect $A000, $A5
    lda #$82
    sta $5116
    expect $C000, $A2

    ; PRG mode 1 ($5117 = $FF: banks 6 and 7 at $C000)
    check 4
    lda #1
    sta $5100
    lda #$82
    sta $5115
    expect $8000, $A2
    expect $A000, $A3
    expect $C000, $A6

    ; PRG mode 0 ($5117 = $FF: banks 4 to 7)
    check 5
    lda #0
    sta $5100
    expect $8000, $A4
    expect $A000, $A5
    expect $C000, $A6
    lda #3
    sta $5100

    ; PRG RAM through $8000
    check 6
    lda #$02
    sta $5114
    lda #$5A
    sta $8100
    lda #$03
    sta $5114
    lda #$C3
    sta $8100
    lda #$02
    sta $5114
    expect $8100, $5A
    lda #$03
    sta $5114
    expect $8100, $C3
    lda #$02
    sta $5113
    lda $6100
    ldx #0
    stx $5113
    cmp #$5A
    beq :+
    jmp fail
:
    ; write protection
    check 7
    lda #0
    sta $5102
    lda #$02
    sta $5114
    lda #$11
    sta $8100
    lda #2
    sta $5102
    expect $8100, $5A

    ; the multiplier: $C3 * $7B = $5DB1
    check 8
    lda #$C3
    sta $5205
    lda #$7B
    sta $5206
    expect $5205, $B1
    expect $5206, $5D

    ; ExRAM
    check 9
    lda #2
    sta $5104
    lda #$42
    sta $5C10
    expect $5C10, $42
    lda #3
    sta $5104
    lda #$55
    sta $5C10
    expect $5C10, $42
    lda #0
    sta $5104
    lda #$55
    sta $5C10               ; outside rendering: 0
    lda #2
    sta $5104
    expect $5C10, $00

    ; CHR banks through $2007
    check 10
    lda #3
    sta $5101
    lda #5
    sta $5123
    ppuread $0C00
    cmp #$C5
    beq :+
    jmp fail
:   lda #0
    sta $5101
    lda #1
    sta $5127
    ppuread $0400
    cmp #$C9
    beq :+
    jmp fail
:
    ; nametables: CIRAM 0, CIRAM 1, ExRAM, fill
    check 11
    lda #$E4
    sta $5105
    lda #$33
    sta $5106
    lda #2
    sta $5107
    ppuread $2C00
    cmp #$33
    beq :+
    jmp fail
:   ppuread $2FC1
    cmp #$AA
    beq :+
    jmp fail
:   lda #0
    sta $5104
    bit $2002
    lda #$28
    sta $2006
    lda #$05
    sta $2006
    lda #$99
    sta $2007
    ppuread $2805
    cmp #$99
    beq :+
    jmp fail
:   lda #2
    sta $5104
    expect $5C05, $99

    ; the scanline IRQ
    check 12
    lda #0
    sta $5105
    sta irqs
    sta st5204
    lda #32
    sta $5203
    lda #$80
    sta $5204
    bit $2002
:   bit $2002
    bpl :-
    lda #0
    sta $2005
    sta $2005
    lda #$08                ; background on
    sta $2001
    cli
    ldx #0
    ldy #0
wait:
    lda irqs
    cmp #2
    bcs waited
    dex
    bne wait
    dey
    bne wait
waited:
    sei
    lda irqs
    cmp #2
    bcs :+
    jmp fail
:   lda st5204
    and #$C0
    cmp #$C0                ; pending and in-frame
    beq :+
    jmp fail
:   lda #0
    sta $2001               ; rendering off: the frame ends
    nop
    nop
    nop
    nop
    lda $5204
    and #$40
    beq :+
    jmp fail
:
    lda #0                  ; passed
    sta $6000
done:
    jmp done

fail:
    lda $6010
    sta $6000
    jmp done

irq:
    pha
    lda $5204               ; acknowledges
    sta st5204
    inc irqs
    pla
    rti
nmi:
    rti

.segment "VECTORS"
    .word nmi, reset, irq
