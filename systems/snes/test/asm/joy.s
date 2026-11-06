; joy.s (M7 step 7.3): the joypads; run with pad 1 = 0A5h (B, Start,
; right... as 12 bits: 0000 1010 0101). Check 1: the auto-read (4200h bit
; 0) gives 4218h = 50h, 4219h = 0Ah once its busy flag (4212h bit 0) has
; set and cleared; 2: the serial read (4016h strobe, then 16 reads of bit 0) gives
; the same 16 bits, B first.

.INCLUDE "snes.inc"
SNES_HEADER "M7 JOY TEST          "

.SECTION "main" FREE
Main:
        lda.b #$01
        sta.w $4200
        ; the auto-read starts after V-blank begins: wait for its busy flag,
        ; then for its end
-       lda.w $4212
        and.b #$01
        beq -
-       lda.w $4212
        and.b #$01
        bne -
        ; 1
        lda.w $4218
        cmp.b #$50
        bne fail1
        lda.w $4219
        cmp.b #$0A
        bne fail1
        ; 2: shift 16 bits into 7E0010h (16-bit), MSB first
        stz.w $4200
        lda.b #$01
        sta.w $4016
        stz.w $4016
        rep #$20
        stz.w $0010
        sep #$20
        ldx.w #16
-       lda.w $4016
        lsr a
        rep #$20
        rol.w $0010
        sep #$20
        dex
        bne -
        rep #$20
        lda.w $0010
        cmp.w #$0A50
        bne fail2
        sep #$20
        lda.b #$01
        jmp Finish
fail1:  lda.b #$81
        jmp Finish
fail2:  sep #$20
        lda.b #$82
        jmp Finish
.ENDS
