; An 8 KiB cartridge against XL banking (M4 step 4.1; HRM 2.6, 2.8).
; Prints "Pass" or "FAIL n":
;   1  TRIG3 = 1: a cartridge is present
;   2  BASIC on (PORTB bit 1 = 0) doesn't hide the cartridge
;   3  writes to the cartridge go nowhere
.include "a800.inc"

.segment "CODE"
start:  lda TRIG3               ; 1
        cmp #1
        check 1
        lda PORTB
        and #$FD
        sta PORTB
        lda marker              ; 2
        cmp #$C5
        check 2
        lda #$00                ; 3
        sta marker
        lda marker
        cmp #$C5
        check 3
        lda PORTB
        ora #$02
        sta PORTB
        jmp pass
init:   rts

marker: .byte $C5
.include "report.inc"

.segment "CARTHDR"
        .word start
        .byte 0
        .byte 4
        .word init
