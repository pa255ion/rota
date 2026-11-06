; ppufuzz.s (M7 step 7.6): random PPU traffic for the lockstep tests
; (snes-lockstep: the Clash PPU against the spec's), not a pass/fail ROM:
; it never stops. VRAM, CGRAM and OAM are filled from 16 KiB of random
; bytes (VRAM four times, through VMAIN's four address remaps), every
; register 2101h-2133h starts random, four HDMA channels write random
; registers from random tables, and the CPU then writes random values to
; random registers at random times while the display runs (1 in 8 of the
; INIDISP writes forced blank), reads 2134h-213Fh, and latches the
; counters through WRIO.

.INCLUDE "snes.inc"
SNES_HEADER "M7 PPU FUZZ          "

.DEFINE SEED $10        ; direct page: xorshift16's state

.SEED 7616

.SECTION "rnd" FREE
Rnd:
        .DBRND 16384, 0, 255
.ENDS

.SECTION "main" FREE
Main:
        rep #$20
        lda.w #$ACE1
        sta.b SEED
        sep #$20
        ; VRAM: Rnd four times, remaps 0-3, word increments
        stz.w $2116
        stz.w $2117
        ldy.w #0
VramLoop:
        tya
        asl a
        asl a
        ora.b #$80
        sta.w $2115
        lda.b #$01
        sta.w $4300
        lda.b #$18
        sta.w $4301
        rep #$20
        lda.w #Rnd
        sta.w $4302
        lda.w #16384
        sta.w $4305
        sep #$20
        lda.b #:Rnd
        sta.w $4304
        lda.b #$01
        sta.w $420B
        iny
        cpy.w #4
        bne VramLoop
        ; CGRAM: 512 bytes
        stz.w $2121
        stz.w $4300
        lda.b #$22
        sta.w $4301
        rep #$20
        lda.w #Rnd + 1000
        sta.w $4302
        lda.w #512
        sta.w $4305
        sep #$20
        lda.b #$01
        sta.w $420B
        ; OAM: 544 bytes
        stz.w $2102
        stz.w $2103
        lda.b #$04
        sta.w $4301
        rep #$20
        lda.w #Rnd + 3000
        sta.w $4302
        lda.w #544
        sta.w $4305
        sep #$20
        lda.b #$01
        sta.w $420B
        ; the registers 2101h-2133h
        ldx.w #1
RegLoop:
        lda.l Rnd + 5000,x
        sta.w $2100,x
        inx
        cpx.w #$34
        bne RegLoop
        ; HDMA channels 1-4: direct, a random mode, a random register
        ; (01h-33h), a table at a random place in Rnd
        ldx.w #$10
HdmaLoop:
        lda.l Rnd + 6000,x
        and.b #$07
        sta.w $4300,x
        lda.l Rnd + 6001,x
        and.b #$3F
        bit.b #$20
        beq HdmaLow
        and.b #$1F
        clc
        adc.b #$14
HdmaLow:
        ora.b #$01
        sta.w $4301,x
        rep #$20
        lda.l Rnd + 6002,x
        and.w #$3FFF
        clc
        adc.w #Rnd
        sta.w $4302,x
        sep #$20
        lda.b #:Rnd
        sta.w $4304,x
        rep #$20
        txa
        clc
        adc.w #$10
        tax
        sep #$20
        cpx.w #$50
        bne HdmaLoop
        lda.b #$1E
        sta.w $420C
        ; the display on
        lda.b #$0F
        sta.w $2100

Loop:
        jsr Next
        rep #$20
        lda.b SEED
        and.w #$003F
        tax
        sep #$20
        cpx.w #$34
        bcs Rd
        jsr Next
        cpx.w #0
        bne Wr
        bit.b #$70
        bne Bright
        and.b #$8F
        bra Wr
Bright:
        and.b #$0F
Wr:
        sta.w $2100,x
        bra Delay
Rd:
        cpx.w #$3F
        bne Rd2
        lda.w $213F
        jsr Next
        sta.w $4201
        bra Delay
Rd2:
        lda.w $2100,x
Delay:
        jsr Next
        rep #$20
        lda.b SEED
        and.w #$001F
        inc a
        tax
        sep #$20
Wait:
        dex
        bne Wait
        bra Loop

; xorshift16 (x ^= x << 7, x ^= x >> 9, x ^= x << 8): SEED, and its low
; byte in A (8 bits) on return.
Next:
        rep #$20
        lda.b SEED
        asl a
        asl a
        asl a
        asl a
        asl a
        asl a
        asl a
        eor.b SEED
        sta.b SEED
        lsr a
        lsr a
        lsr a
        lsr a
        lsr a
        lsr a
        lsr a
        lsr a
        lsr a
        eor.b SEED
        sta.b SEED
        xba
        and.w #$FF00
        eor.b SEED
        sta.b SEED
        sep #$20
        rts
.ENDS
