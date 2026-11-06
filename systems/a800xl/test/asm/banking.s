; XL banking through PORTB (M4 step 4.1; HRM 2.6), run from the XEX
; loader (BASIC off, no cartridge). Prints "Pass" or "FAIL n":
;   1  RAM under the OS ROM ($C000, $D800, $FFF0) keeps what was written
;      while the OS was off
;   2  writes to $C000/$D800 while the OS is on don't reach that RAM
;   3  the OS ROM comes back unchanged (the byte read before)
;   4  self-test on with the OS off: $5000 is RAM
;   5  self-test on with the OS on: writes to $5000 are lost
;   6  BASIC on: writes to $A000 are lost, BASIC off shows the RAM again
;   7  BASIC on shows its ROM ($BFFC = 0, "a cartridge is present")
;   8  a PORTB input bit is pulled up: DDR bit 0 = 0 turns the OS on
;      though the output latch says off
;   9  TRIG3 = 0: no cartridge at $A000 (the loader's stub is gone)
.include "a800.inc"
.include "xex.inc"

start:  sei
        lda #0
        sta NMIEN               ; the OS's vectors go away with it
        lda TRIG3               ; 9
        check 9
        lda $C000
        sta rom0
        ; --- OS off: write the RAM under it
        lda PORTB
        and #$FE
        sta PORTB
        lda #$AA
        sta $C000
        sta $D800
        sta $FFF0
        lda $C000               ; 1
        cmp #$AA
        check 1
        lda $D800
        cmp #$AA
        check 1
        lda $FFF0
        cmp #$AA
        check 1
        ; --- OS on: writes go nowhere, the ROM is back
        lda PORTB
        ora #$01
        sta PORTB
        lda #$55
        sta $C000
        sta $D800
        lda $C000               ; 3
        cmp rom0
        check 3
        lda PORTB
        and #$FE
        sta PORTB
        lda $C000               ; 2
        cmp #$AA
        check 2
        lda $D800
        cmp #$AA
        check 2
        ; --- self-test with the OS off: RAM
        lda #$5A
        sta $5000
        lda PORTB
        and #$7F                ; self-test bit on, OS still off
        sta PORTB
        lda $5000               ; 4
        cmp #$5A
        check 4
        lda #$C3
        sta $5000
        lda $5000
        cmp #$C3
        check 4
        ; --- self-test with the OS on: writes lost
        lda PORTB
        ora #$01
        sta PORTB
        lda #$11
        sta $5000
        lda PORTB
        ora #$80                ; self-test off
        sta PORTB
        lda $5000               ; 5
        cmp #$C3
        check 5
        ; --- BASIC
        lda #$A5
        sta $A000
        lda PORTB
        and #$FD                ; BASIC on
        sta PORTB
        lda #$5A
        sta $A000
        lda $BFFC               ; 7
        check 7
        lda PORTB
        ora #$02
        sta PORTB
        lda $A000               ; 6
        cmp #$A5
        check 6
        ; --- an input bit reads as pulled up
        lda PORTB
        and #$FE                ; OS off by the latch
        sta PORTB
        lda #$AA
        sta $C001
        lda #$38                ; PBCTL: the direction register
        sta PBCTL
        lda #$FE                ; bit 0 an input
        sta PORTB
        lda #$3C
        sta PBCTL
        lda $C001               ; 8: the OS is on again
        cmp #$AA
        beq bad8
        lda #$38                ; all outputs again, OS on
        sta PBCTL
        lda #$FF
        sta PORTB
        lda #$3C
        sta PBCTL
        lda PORTB
        ora #$01
        sta PORTB
        lda #$40
        sta NMIEN
        cli
        jmp pass
bad8:   lda #8
        jmp fail

rom0:   .byte 0
.include "report.inc"
