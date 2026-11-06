; map.s (M7 step 7.3): the memory map. Check 1: WRAM's first 8 KiB at
; 000000h-001FFFh is 7E0000h; 2: 7F0000h is not 7E0000h; 3: the WRAM port
; (2180h, address 2181h-2183h) reads what a long store wrote, and its
; address counts up; 4: ROM at 808000h is ROM at 008000h; 5: the 32 KiB
; ROM mirrors at 018000h; 6: 2000h-20FFh (nothing there) reads the last
; byte on the bus (open bus).

.INCLUDE "snes.inc"
SNES_HEADER "M7 MAP TEST          "

.SECTION "main" FREE
Main:
        ; 1
        lda.b #$3C
        sta.l $7E1234
        lda.l $001234
        cmp.b #$3C
        bne fail1
        ; 2
        lda.b #$C3
        sta.l $7F1234
        lda.l $7E1234
        cmp.b #$3C
        bne fail2
        ; 3: WRAM port at 7F1234h
        lda.b #$34
        sta.l $002181
        lda.b #$12
        sta.l $002182
        lda.b #$01
        sta.l $002183
        lda.l $002180
        cmp.b #$C3
        bne fail3
        lda.b #$99
        sta.l $7F1235
        lda.l $002180
        cmp.b #$99
        bne fail3
        ; 4
        lda.l $808000
        cmp.l $008000
        bne fail4
        lda.l Mark
        cmp.b #$A7
        bne fail4
        ; 5: the same byte one 32 KiB bank up
        lda.l Mark + $10000
        cmp.b #$A7
        bne fail5
        ; 6: open bus: the last byte fetched, the address's bank (00h)
        lda.l $002000
        cmp.b #$00
        bne fail6
        lda.b #$01
        jmp Finish
fail1:  lda.b #$81
        jmp Finish
fail2:  lda.b #$82
        jmp Finish
fail3:  lda.b #$83
        jmp Finish
fail4:  lda.b #$84
        jmp Finish
fail5:  lda.b #$85
        jmp Finish
fail6:  lda.b #$86
        jmp Finish
Mark:   .DB $A7
.ENDS
