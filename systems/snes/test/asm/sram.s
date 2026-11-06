; sram.s (M7 step 7.3): LoROM with 8 KiB of battery SRAM at 700000h.
; Check 1: a byte written reads back; 2: the 8 KiB mirror through bank 70h
; (702000h is 700000h); 3: bank F0h is bank 70h.

.DEFINE SRAM
.INCLUDE "snes.inc"
SNES_HEADER "M7 SRAM TEST         "

.SECTION "main" FREE
Main:
        lda.b #$A5
        sta.l $700000
        lda.b #$5A
        sta.l $701FFF
        ; 1
        lda.l $700000
        cmp.b #$A5
        bne fail1
        lda.l $701FFF
        cmp.b #$5A
        bne fail1
        ; 2
        lda.l $702000
        cmp.b #$A5
        bne fail2
        ; 3
        lda.l $F00000
        cmp.b #$A5
        bne fail3
        lda.b #$01
        jmp Finish
fail1:  lda.b #$81
        jmp Finish
fail2:  lda.b #$82
        jmp Finish
fail3:  lda.b #$83
        jmp Finish
.ENDS
