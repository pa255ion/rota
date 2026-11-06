; busfuzz.s (M7 step 7.9a): the 5A22's bus work for the machine's lockstep
; test (snes-core: the Clash machine against the spec), not a pass/fail
; ROM: it never stops, and what it does lands in WRAM, VRAM and CGRAM,
; which the test compares. In order:
;
; 1. Multiply and divide with random operands (multiplicands of 80h and
;    up too, divisors of 0 too), each result read back at every cycle
;    while the unit works (its partial results), and a multiply started
;    while a divide runs (ignored).
; 2. DMA in every transfer mode, each with the A address counting up,
;    fixed and counting down, both ways: from ROM to the WRAM port and
;    its address registers (2180h-2183h), from the WRAM port back to
;    WRAM, from VRAM's read port to WRAM; then four channels at once.
; 3. HDMA during the display, eight channels: direct with repeat
;    entries (the fixed colour; CGRAM's address and data, mode 3, in
;    H-blank), indirect (BG1's scroll, mode 2; the WRAM port, mode 0 and
;    4), to the A bus (VRAM's read port into a table in WRAM), and a
;    channel that ends early (a 0 count). The CPU meanwhile goes on with
;    random multiplies, divides and DMAs (which HDMA interrupts), with
;    NMI and the V IRQ on.

.INCLUDE "snes.inc"
SNES_HEADER "M7 BUS FUZZ          "

.DEFINE SEED $10        ; direct page: xorshift16's state
.DEFINE TMP $12
.DEFINE OUTP $14        ; where the next result byte goes (bank 7Eh)

.SEED 7919

.SECTION "rnd" FREE
Rnd:
        .DBRND 4096, 0, 255
.ENDS

; HDMA tables (in ROM, bank 0)
.SECTION "tables" FREE
; the fixed colour: direct, mode 0, repeat entries (bit 7: a byte a line)
TabColour:
        .DB $10, $3F, $85, $5F, $9F, $DF, $FF, $81, $40, $20, $E0, $30, $1F, $00
; CGRAM 0's colour: direct, mode 3 (2121h twice, 2122h twice)
TabCg:
        .DB $08, $00, $00, $1F, $00
        .DB $88, $00, $00, $E0, $03, $00, $00, $00, $7C, $00, $00, $FF, $7F, $00, $00, $10, $42
        .DB $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
        .DB $20, $00, $00, $00, $00
        .DB $00
; BG1's horizontal scroll: indirect, mode 2 (210Dh twice)
TabScrollI:
        .DB $0C
        .DW ScrollData
        .DB $84
        .DW ScrollData + 2
        .DB $20
        .DW ScrollData + 4
        .DB $00
ScrollData:
        .DB $00, $00, $08, $00, $10, $00, $18, $00, $20, $00, $28, $00, $30, $00, $38, $00
        .DB $40, $00, $48, $00, $50, $00, $58, $00
; the WRAM port: indirect, mode 0 (a byte a line from Rnd), then mode 4
TabWramI:
        .DB $90
        .DW Rnd + 100
        .DB $10
        .DW Rnd + 200
        .DB $00
TabWram4:
        .DB $82
        .DW WramData
        .DB $08
        .DW WramData + 8
        .DB $00
WramData:
        .DB $5A, $00, $30, $7E, $A5, $10, $30, $7E, $C3, $20, $30, $7E
; a channel that ends after one entry
TabShort:
        .DB $02, $55, $00
.ENDS

.SECTION "main" FREE
Main:
        rep #$20
        lda.w #$ACE1
        sta.b SEED
        lda.w #$4000
        sta.b OUTP
        sep #$20

        ; 1. multiply and divide
        ldy.w #48
AluLoop:
        ; a new line for the V IRQ now and then (more IRQs, at many
        ; places in the CPU's cycles)
        jsr Next
        and.b #$7F
        sta.w $4209
        jsr Next
        sta.w $4202
        jsr Next
        ora.b #$80
        sta.w $4203
        ; the product as it builds, a read a cycle (more or less)
        lda.w $4216
        jsr Out
        lda.w $4217
        jsr Out
        lda.w $4216
        jsr Out
        lda.w $4217
        jsr Out
        lda.w $4214
        jsr Out
        nop
        lda.w $4216
        jsr Out
        lda.w $4217
        jsr Out
        ; a divide, its partial quotient and remainder
        jsr Next
        sta.w $4204
        jsr Next
        sta.w $4205
        jsr Next
        and.b #$1F              ; small divisors and 0
        sta.w $4206
        lda.w $4214
        jsr Out
        lda.w $4215
        jsr Out
        ; a multiply now: the divide is still busy, so it is ignored
        lda.b #$77
        sta.w $4203
        lda.w $4216
        jsr Out
        lda.w $4214
        jsr Out
        lda.w $4217
        jsr Out
        ldx.w #8
-       dex
        bne -
        lda.w $4214
        jsr Out
        lda.w $4215
        jsr Out
        lda.w $4216
        jsr Out
        lda.w $4217
        jsr Out
        dey
        beq AluDone
        jmp AluLoop
AluDone:

        ; 2. DMA: each mode, three A-address steps, to the WRAM port
        ; (80h-83h: its data and address registers) from Rnd
        stz.b TMP
DmaLoop:
        ; the WRAM port at 7E:2000 + TMP * 64
        rep #$20
        lda.b TMP
        and.w #$00FF
        asl a
        asl a
        asl a
        asl a
        asl a
        asl a
        clc
        adc.w #$2000
        sta.w $2181
        sep #$20
        lda.b #$00
        sta.w $2183
        ; DMAP: the mode (TMP & 7), the step (TMP >> 3: 0 up, 1 fixed, 2 down)
        lda.b TMP
        and.b #$07
        sta.w TMP + 1
        lda.b TMP
        lsr a
        lsr a
        lsr a
        cmp.b #1
        bne +
        lda.b #$08
        bra ++
+       cmp.b #2
        bne +
        lda.b #$10
        bra ++
+       lda.b #$00
++      ora.w TMP + 1
        sta.w $4300
        lda.b #$80
        sta.w $4301
        ; A: Rnd + 1000 + TMP * 16 (from the middle, as it may count down)
        rep #$20
        lda.b TMP
        and.w #$00FF
        asl a
        asl a
        asl a
        asl a
        clc
        adc.w #Rnd + 1000
        sta.w $4302
        jsr Next
        rep #$20
        and.w #$001F
        inc a
        sta.w $4305
        sep #$20
        lda.b #:Rnd
        sta.w $4304
        lda.b #$01
        sta.w $420B
        ; the channel's registers after it, kept
        lda.w $4302
        jsr Out
        lda.w $4303
        jsr Out
        lda.w $4305
        jsr Out
        inc.b TMP
        lda.b TMP
        cmp.b #24
        beq DmaDone
        jmp DmaLoop
DmaDone:

        ; the other way: the WRAM port (at 7E:2000) to WRAM 7E:3800, mode 0
        lda.b #$00
        sta.w $2181
        lda.b #$20
        sta.w $2182
        stz.w $2183
        lda.b #$80
        sta.w $4300
        lda.b #$80
        sta.w $4301
        rep #$20
        lda.w #$3800
        sta.w $4302
        lda.w #64
        sta.w $4305
        sep #$20
        lda.b #$7E
        sta.w $4304
        lda.b #$01
        sta.w $420B
        ; VRAM: 256 bytes of Rnd to word 0, then read back through
        ; 2139h/213Ah (mode 1, to the A bus) into 7E:3900, counting down
        stz.w $2115
        stz.w $2116
        stz.w $2117
        lda.b #$01
        sta.w $4300
        lda.b #$18
        sta.w $4301
        rep #$20
        lda.w #Rnd
        sta.w $4302
        lda.w #256
        sta.w $4305
        sep #$20
        lda.b #:Rnd
        sta.w $4304
        lda.b #$01
        sta.w $420B
        lda.b #$80
        sta.w $2115
        stz.w $2116
        stz.w $2117
        lda.b #$91              ; to the A bus, counting down, mode 1
        sta.w $4300
        lda.b #$39
        sta.w $4301
        rep #$20
        lda.w #$39FF
        sta.w $4302
        lda.w #128
        sta.w $4305
        sep #$20
        lda.b #$7E
        sta.w $4304
        lda.b #$01
        sta.w $420B
        ; four channels at once: 0-3 to the WRAM port in modes 0, 1, 2, 4
        stz.w $2181
        lda.b #$30
        sta.w $2182
        stz.w $2183
        ldx.w #0
-       txa
        lsr a
        lsr a
        lsr a
        lsr a
        cmp.b #3
        bne +
        lda.b #4
+       sta.w $4300,x
        lda.b #$80
        sta.w $4301,x
        rep #$20
        txa
        clc
        adc.w #Rnd + 2000
        sta.w $4302,x
        lda.w #9
        sta.w $4305,x
        sep #$20
        lda.b #:Rnd
        sta.w $4304,x
        rep #$20
        txa
        clc
        adc.w #$10
        tax
        sep #$20
        cpx.w #$40
        bne -
        lda.b #$0F
        sta.w $420B

        ; 3. HDMA during the display: a picture to scroll (BG1, mode 0,
        ; its map and characters from Rnd: VRAM word 0 has them already)
        lda.b #$00
        sta.w $2105
        lda.b #$00
        sta.w $2107             ; BG1's map at word 0
        lda.b #$00
        sta.w $210B             ; its characters at word 0
        lda.b #$01
        sta.w $212C
        ; 1: the fixed colour (direct, mode 0)
        lda.b #$00
        sta.w $4310
        lda.b #$32
        sta.w $4311
        rep #$20
        lda.w #TabColour
        sta.w $4312
        sep #$20
        lda.b #:TabColour
        sta.w $4314
        ; colour maths on the backdrop, so the fixed colour shows
        lda.b #$20
        sta.w $2131
        ; 2: CGRAM 0 (direct, mode 3, 2121h/2122h)
        lda.b #$03
        sta.w $4320
        lda.b #$21
        sta.w $4321
        rep #$20
        lda.w #TabCg
        sta.w $4322
        sep #$20
        lda.b #:TabCg
        sta.w $4324
        ; 3: BG1's scroll (indirect, mode 2)
        lda.b #$42
        sta.w $4330
        lda.b #$0D
        sta.w $4331
        rep #$20
        lda.w #TabScrollI
        sta.w $4332
        sep #$20
        lda.b #:TabScrollI
        sta.w $4334
        lda.b #:ScrollData
        sta.w $4337
        ; 4: the WRAM port (indirect, mode 0), from 7E:5000
        stz.w $2181
        lda.b #$50
        sta.w $2182
        stz.w $2183
        lda.b #$40
        sta.w $4340
        lda.b #$80
        sta.w $4341
        rep #$20
        lda.w #TabWramI
        sta.w $4342
        sep #$20
        lda.b #:TabWramI
        sta.w $4344
        lda.b #:Rnd
        sta.w $4347
        ; 5: the WRAM port and its address (indirect, mode 4)
        lda.b #$44
        sta.w $4350
        lda.b #$80
        sta.w $4351
        rep #$20
        lda.w #TabWram4
        sta.w $4352
        sep #$20
        lda.b #:TabWram4
        sta.w $4354
        lda.b #:WramData
        sta.w $4357
        ; 6: VRAM's read port to the A bus (direct, mode 0): the "table"
        ; is in WRAM at 7E:6000, its counts first
        lda.b #$06
        sta.l $7E6000
        lda.b #$00
        sta.l $7E6007
        lda.b #$80
        sta.w $4360
        lda.b #$39
        sta.w $4361
        rep #$20
        lda.w #$6000
        sta.w $4362
        sep #$20
        lda.b #$7E
        sta.w $4364
        ; 7: a channel that ends after one entry
        lda.b #$00
        sta.w $4370
        lda.b #$26
        sta.w $4371
        rep #$20
        lda.w #TabShort
        sta.w $4372
        sep #$20
        lda.b #:TabShort
        sta.w $4374
        lda.b #$FE
        sta.w $420C
        ; the display on; NMI and the V IRQ (line 100) on
        lda.b #$0F
        sta.w $2100
        lda.b #100
        sta.w $4209
        stz.w $420A
        lda.b #$A0
        sta.w $4200
        cli

        ; the CPU meanwhile: random multiplies, divides and small DMAs
Loop:
        jsr Next
        sta.w $4202
        jsr Next
        sta.w $4203
        lda.w $4216
        jsr Out
        jsr Next
        sta.w $4204
        jsr Next
        sta.w $4206
        jsr Next
        and.b #$03
        bne NoDma
        ; a DMA of a few bytes from Rnd to the WRAM port (channel 0)
        lda.b #$00
        sta.w $4300
        lda.b #$80
        sta.w $4301
        rep #$20
        lda.b SEED
        and.w #$03FF
        clc
        adc.w #Rnd
        sta.w $4302
        lda.w #12
        sta.w $4305
        sep #$20
        lda.b #:Rnd
        sta.w $4304
        lda.b #$01
        sta.w $420B
NoDma:
        lda.w $4214
        jsr Out
        jmp Loop

; A result byte to 7E:OUTP (wrapping within 4000h-4FFFh).
Out:
        phx
        rep #$20
        ldx.b OUTP
        sep #$20
        sta.l $7E0000,x
        rep #$20
        inx
        txa
        and.w #$0FFF
        ora.w #$4000
        sta.b OUTP
        sep #$20
        plx
        rts

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
