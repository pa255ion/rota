; lorom.s (M7 step 7.0): the smallest LoROM cartridge. Its header must
; parse as LoROM, 32 KiB, checksum good (snes-spec); from step 7.3 on it
; runs on the spec and passes. Check 1: a byte stored to WRAM through its
; long address reads back through the bank 00h mirror.

.INCLUDE "snes.inc"
SNES_HEADER "M7 LOROM TEST        "

.SECTION "main" FREE
Main:
        lda.b #$5A
        sta.l $7E0010
        lda.l $000010
        cmp.b #$5A
        bne fail1
        lda.b #$01
        jmp Finish
fail1:  lda.b #$81
        jmp Finish
.ENDS
