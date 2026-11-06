; save.s (M7 step 7.11): the battery save at layer 4 (systems/snes/test/ps/
; save.ps; Makefile ci-snes, ci-arty-snes). LoROM with 8 KiB of battery
; SRAM at 700000h: the save's boot count at 700004h goes up by one (the PS
; loads "SAVE" and 29h before the game starts, and reads 2Ah back). The
; rest of SRAM is left as the save brought it.

.DEFINE SRAM
.INCLUDE "snes.inc"
SNES_HEADER "M7 SAVE TEST         "

.SECTION "main" FREE
Main:
        lda.l $700004
        inc a
        sta.l $700004
        lda.b #$01
        jmp Finish
.ENDS
