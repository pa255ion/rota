; Shared ending of our regression programs: `fail` prints "FAIL n" (n in
; A, as two hex digits), `pass` prints "Pass"; both then stop. `fail`
; first puts the OS back (PORTB: OS on, BASIC and self-test off; VBI on;
; IRQs on), since a check may fail while a test has it switched off.
pass:   print passmsg, passlen
        jmp *
fail:   tay
        lda PORTB
        ora #$83
        sta PORTB
        lda #$40
        sta NMIEN
        cli
        tya
        pha
        lsr
        lsr
        lsr
        lsr
        tax
        lda hexdig,x
        sta failnum
        pla
        and #$0F
        tax
        lda hexdig,x
        sta failnum+1
        print failmsg, faillen
        jmp *
passmsg: .byte "Pass", EOL
passlen = * - passmsg
failmsg: .byte "FAIL "
failnum: .byte "00", EOL
faillen = * - failmsg
hexdig:  .byte "0123456789ABCDEF"
