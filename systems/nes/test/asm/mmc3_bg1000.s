; MMC3 scanline IRQs with the background at $1000 and sprites at $0000
; (step 3.10): the counter is clocked at dot 324 of each line, not again
; at dot 5 (the PPU's idle dot 0 shows the pattern address, keeping A12
; high). nes-lockstep compares the counter after every step, so a core
; that clocks at dot 5 too is caught within a line. 16 KiB PRG at $C000:
; with the power-on banks it is mapped as NROM-128 would be.
.segment "HEADER"
    .byte "NES", $1A
    .byte 1, 1              ; 16 KiB PRG, 8 KiB CHR
    .byte $40, $00          ; mapper 4, horizontal
    .res 8, 0

.segment "ZEROPAGE"
irqs: .res 1

.segment "CODE"
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
    lda #$40                ; no frame IRQs
    sta $4017
    lda #10                 ; IRQ every 11 lines
    sta $C000
    sta $C001               ; reload
    sta $E001               ; enable
    lda #$10                ; background $1000, sprites $0000, 8x8
    sta $2000
    lda #$18                ; background and sprites on
    sta $2001
    cli
forever:
    jmp forever

irq:
    sta $E000               ; acknowledge
    sta $E001
    inc irqs
    rti
nmi:
    rti

.segment "VECTORS"
    .word nmi, reset, irq
