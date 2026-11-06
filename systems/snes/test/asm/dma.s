; dma.s (M7 step 7.3): general DMA. Check 1: 8 bytes from ROM to WRAM
; through the WRAM port (mode 0, B address 80h) arrive in order; 2: the
; channel's count is 0 afterwards and its A address 8 past the start;
; 3: 4 words to VRAM (mode 1, 2118h/2119h): the first and the last read
; back through 2139h/213Ah, each as the first read after setting the
; address (the prefetched word; which word the reads after that return
; depends on when the prefetch reloads: *to verify* at step 7.5).

.INCLUDE "snes.inc"
SNES_HEADER "M7 DMA TEST          "

.SECTION "main" FREE
Main:
        ; WRAM port at 7E0100h
        lda.b #$00
        sta.w $2181
        lda.b #$01
        sta.w $2182
        lda.b #$00
        sta.w $2183
        lda.b #$00
        sta.w $4300
        lda.b #$80
        sta.w $4301
        rep #$20
        lda.w #Table
        sta.w $4302
        lda.w #8
        sta.w $4305
        sep #$20
        lda.b #:Table
        sta.w $4304
        lda.b #$01
        sta.w $420B
        ; 1
        ldx.w #0
-       lda.l $7E0100,x
        cmp.l Table,x
        bne fail1
        inx
        cpx.w #8
        bne -
        ; 2
        rep #$20
        lda.w $4305
        bne fail2
        lda.w $4302
        cmp.w #Table + 8
        bne fail2
        ; 3: VRAM word 1000h
        sep #$20
        lda.b #$80
        sta.w $2115
        rep #$20
        lda.w #$1000
        sta.w $2116
        lda.w #Table
        sta.w $4302
        lda.w #8
        sta.w $4305
        sep #$20
        lda.b #$01
        sta.w $4300
        lda.b #$18
        sta.w $4301
        lda.b #$01
        sta.w $420B
        rep #$20
        lda.w #$1000
        sta.w $2116
        sep #$20
        lda.w $2139
        cmp.l Table
        bne fail3
        lda.w $213A
        cmp.l Table + 1
        bne fail3
        rep #$20
        lda.w #$1003
        sta.w $2116
        sep #$20
        lda.w $2139
        cmp.l Table + 6
        bne fail3
        lda.w $213A
        cmp.l Table + 7
        bne fail3
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
Table:  .DB $11, $22, $33, $44, $55, $66, $77, $88
.ENDS
