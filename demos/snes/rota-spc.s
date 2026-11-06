; rota-spc.s (SPC700, M7 step 7.13): the boot splash's sound driver, at
; 0200h, with the samples' directory and the samples (BRR) after it, from
; the generator (rota_spc.inc: Rota.Sound.Snes). Assembled by `make
; demos-snes` (wla-spc700) into build/demos/snes/rota-spc.bin (0200h-
; 03FFh), which the 65816 ROM demos/snes/rota.s uploads through the boot
; ROM and starts.
;
; It clears the echo buffer (8000h-A7FFh: EDL 5 at ESA 80h) before the
; stream turns echo writes on, then says it is ready (5Ah on port 3) and
; applies the DSP writes the 65816 hands it: the register on port 1, the
; value on port 2, then a new sequence number on port 0; it echoes the
; number on port 0 when the write is done.

.MEMORYMAP
SLOTSIZE $200
DEFAULTSLOT 0
SLOT 0 $0200
.ENDME
.ROMBANKSIZE $200
.ROMBANKS 1

; direct page: a pointer (low, high), the last sequence number
.DEFINE PTR $00
.DEFINE PTRH $01
.DEFINE LAST $02

.BANK 0 SLOT 0
.ORG $000
.SECTION "driver" FORCE
        ; the echo buffer cleared, page by page
        mov PTR,#$00
        mov PTRH,#$80
        mov a,#$00
        mov y,#$00
clear:  mov [PTR]+y,a
        inc y
        bne clear
        inc PTRH
        cmp PTRH,#$A8
        bne clear
        ; ready
        mov a,$F4
        mov LAST,a
        mov $F7,#$5A
wait:   mov a,$F4
        cmp a,LAST
        beq wait
        mov LAST,a
        mov $F2,$F5
        mov $F3,$F6
        mov $F4,a
        bra wait
.ENDS

; the directory at 0300h, the samples from 0310h
.ORG $100
.SECTION "samples" FORCE
.INCLUDE "rota_spc.inc"
.ENDS
