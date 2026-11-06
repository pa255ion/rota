; tone.s (SPC700, M7 step 7.4): voice 0 loops one BRR block, 8 samples
; high and 8 low, at pitch 0800h (16 kHz): a 1000 Hz square, direct gain
; 7Fh. Assembled by `make snes-asm` into build/snes/spc/tone.bin (0200h-
; 03FFh), which the 65816 ROM tone.s uploads through the boot ROM.

.MEMORYMAP
SLOTSIZE $200
DEFAULTSLOT 0
SLOT 0 $0200
.ENDME
.ROMBANKSIZE $200
.ROMBANKS 1

.BANK 0 SLOT 0
.ORG $000
.SECTION "code" FORCE
        mov $F2,#$6C    ; FLG: unmuted, echo writes off, no reset
        mov $F3,#$20
        mov $F2,#$5D    ; DIR: the table at 0300h
        mov $F3,#$03
        mov $F2,#$04    ; V0SRCN 0
        mov $F3,#$00
        mov $F2,#$05    ; V0ADSR1: GAIN mode
        mov $F3,#$00
        mov $F2,#$07    ; V0GAIN: direct, 7Fh
        mov $F3,#$7F
        mov $F2,#$02    ; V0PITCH 0800h
        mov $F3,#$00
        mov $F2,#$03
        mov $F3,#$08
        mov $F2,#$00    ; V0VOL 40h, 40h
        mov $F3,#$40
        mov $F2,#$01
        mov $F3,#$40
        mov $F2,#$0C    ; MVOL 40h, 40h
        mov $F3,#$40
        mov $F2,#$1C
        mov $F3,#$40
        mov $F2,#$4C    ; KON voice 0
        mov $F3,#$01
loop:   bra loop
.ENDS

.ORG $100
.SECTION "dir" FORCE
        .DW $0310, $0310
.ENDS

.ORG $110
.SECTION "brr" FORCE
        ; shift 12, filter 0, loop and end; 7 and -7 (not -8: -8 at shift 12
        ; overflows the interpolation, as fullsnes notes for the hardware)
        .DB $C3, $77, $77, $77, $77, $99, $99, $99, $99
.ENDS
