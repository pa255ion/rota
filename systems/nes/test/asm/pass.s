; Smallest user of Blargg's result protocol (docs/systems/nes.md, "Test
; ROMs"): signature at $6001, "running" at $6000, text at $6004, then
; status 0 (passed). Checks the hook and, from step 3.2, the spec's PRG RAM.
.include "nrom.inc"

.segment "CODE"
reset:
    sei
    cld
    ldx #$FF
    txs
    lda #$80            ; running
    sta $6000
    lda #$DE
    sta $6001
    lda #$B0
    sta $6002
    lda #$61
    sta $6003
    ldx #0
copy:
    lda text,x
    sta $6004,x
    beq done
    inx
    bne copy
done:
    lda #0              ; passed
    sta $6000
forever:
    jmp forever
nmi:
irq:
    rti

text:
    .byte "pass", $0A, $0A, "Passed", $0A, 0

.segment "VECTORS"
    .word nmi, reset, irq
