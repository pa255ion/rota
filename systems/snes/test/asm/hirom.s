; hirom.s (M7 step 7.0): the smallest HiROM cartridge. Its header must
; parse as HiROM, 64 KiB, checksum good (snes-spec); from step 7.3 on it
; runs on the spec and passes. Check 1: the ROM byte at C07000h (the low
; half of the bank, which bank 00h doesn't show) is the one put there.

.DEFINE HIROM
.INCLUDE "snes.inc"
SNES_HEADER "M7 HIROM TEST        "

.SECTION "main" FREE
Main:
        lda.l $C07000
        cmp.b #$C3
        bne fail1
        lda.b #$01
        jmp Finish
fail1:  lda.b #$81
        jmp Finish
.ENDS

.ORG $7000
.SECTION "mark" FORCE
        .DB $C3
.ENDS
