; muldiv.s (M7 step 7.3): the 5A22's multiply and divide units. Check 1:
; 12h * 34h = 03A8h in RDMPY after 8 cycles; 2: 1234h / 12h = 0102h,
; remainder 0010h after 16 cycles; 3: a division by 0: quotient FFFFh,
; remainder the dividend; 4: read too early (right after WRMPYB) the
; product isn't there yet.

.INCLUDE "snes.inc"
SNES_HEADER "M7 MULDIV TEST       "

.SECTION "main" FREE
Main:
        ; 1
        lda.b #$12
        sta.w $4202
        lda.b #$34
        sta.w $4203
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        rep #$20
        lda.w $4216
        cmp.w #$03A8
        bne fail1
        ; 2
        lda.w #$1234
        sta.w $4204
        sep #$20
        lda.b #$12
        sta.w $4206
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        rep #$20
        lda.w $4214
        cmp.w #$0102
        bne fail2
        lda.w $4216
        cmp.w #$0010
        bne fail2
        ; 3
        lda.w #$1234
        sta.w $4204
        sep #$20
        lda.b #$00
        sta.w $4206
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        rep #$20
        lda.w $4214
        cmp.w #$FFFF
        bne fail3
        lda.w $4216
        cmp.w #$1234
        bne fail3
        ; 4: FFh * FFh read at once is not FE01h yet
        sep #$20
        lda.b #$FF
        sta.w $4202
        sta.w $4203
        rep #$20
        lda.w $4216
        cmp.w #$FE01
        beq fail4
        sep #$20
        lda.b #$01
        jmp Finish
fail1:  sep #$20
        lda.b #$81
        jmp Finish
fail2:  sep #$20
        lda.b #$82
        jmp Finish
fail3:  sep #$20
        lda.b #$83
        jmp Finish
fail4:  sep #$20
        lda.b #$84
        jmp Finish
.ENDS
