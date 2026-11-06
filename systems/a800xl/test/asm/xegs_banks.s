; A 32 KiB XEGS cartridge (CAR type 12; side track Q, Q.7; A800.Media's
; cartBanks: the atari800 emulator's CARTRIDGE_XEGS_32, the HRM 8.1's
; CCTL region). Each bank holds its number at $9FF0 (offset $1FF0); the
; last bank, fixed at $A000-$BFFF, also at $BFF0. Prints "Pass" or
; "FAIL n":
;   1  TRIG3 = 1: a cartridge is present
;   2  bank 0 at power-on
;   3  a write to $D500 selects banks 1, 2, 3 (the last one too)
;   4  a write to $D5FF selects as well, the value ANDed with 3 ($FE: 2)
;   5  the window at $A000 is the last bank whatever is selected
;   6  a read of $D500 changes nothing
;   7  writes to the window go nowhere
.include "a800.inc"

.segment "BANK0"
        .res $1FF0, $FF
        .byte 0
.segment "BANK1"
        .res $1FF0, $FF
        .byte 1
.segment "BANK2"
        .res $1FF0, $FF
        .byte 2
.segment "MARK"
        .byte 3

.segment "CODE"
start:  lda TRIG3               ; 1
        cmp #1
        check 1
        lda $9FF0               ; 2
        cmp #0
        check 2
        ldx #1                  ; 3
bank:   stx $D500
        cpx $9FF0
        beq next
        lda #3
        jmp fail
next:   inx
        cpx #4
        bne bank
        lda #$FE                ; 4
        sta $D5FF
        lda $9FF0
        cmp #2
        check 4
        lda #0                  ; 5
        sta $D500
        lda $BFF0
        cmp #3
        check 5
        lda #1                  ; 6
        sta $D500
        lda $D500
        lda $9FF0
        cmp #1
        check 6
        lda #$55                ; 7
        sta $9FF0
        lda $9FF0
        cmp #1
        check 7
        jmp pass
init:   rts
.include "report.inc"

.segment "CARTHDR"
        .word start     ; $BFFA: start address
        .byte 0         ; $BFFC: 0 = a cartridge is present
        .byte 4         ; $BFFD: start the cartridge; no disk boot
        .word init      ; $BFFE: init address
