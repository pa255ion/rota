; ppuregs.s (M7 step 7.5): the PPU's register behaviour the picture
; depends on. Check 1: a VRAM write during the display (not V-blank, not
; forced blank) is dropped, one in V-blank lands; 2: OPVCT (213Dh) reads
; its low byte, then bit 8 (here 0), and STAT78's read resets that order;
; 3: 33 sprites on one line set STAT77's range-over flag (bit 6) in the
; next frame; 4, 5: on line 0 with the display on, from master clock 1120
; (the sprite unit reading OAM and VRAM for line 1) OAM and VRAM writes
; are dropped; 6: earlier in line 0 a VRAM write lands. The writes are
; timed by the H+V timer's flag (4211h, polled with interrupts off).

.INCLUDE "snes.inc"
SNES_HEADER "M7 PPUREGS TEST      "

.SECTION "main" FREE
Main:
        ; the display on (brightness 15), so writes outside V-blank drop
        lda.b #$0F
        sta.w $2100
        stz.w $2115
        ; into the display: wait for line 50 (latched V counter)
-       lda.w $2137
        lda.w $213F         ; resets the read order
        lda.w $213D
        cmp.b #50
        bcc -
        rep #$20
        lda.w #$0300
        sta.w $2116
        sep #$20
        lda.b #$5A
        sta.w $2118         ; dropped: the display is drawing
        ; V-blank: write word 0301h
-       lda.w $4212
        bpl -
        rep #$20
        lda.w #$0301
        sta.w $2116
        sep #$20
        lda.b #$A5
        sta.w $2118
        ; read back, still in V-blank: word 0300h (prefetched) is 0
        rep #$20
        lda.w #$0300
        sta.w $2116
        sep #$20
        lda.w $2139
        beq +
        jmp fail1
+
        rep #$20
        lda.w #$0301
        sta.w $2116
        sep #$20
        lda.w $2139
        cmp.b #$A5
        beq +
        jmp fail1
+
        ; 2: latch, then low byte, then bit 8 (0 on lines below 256)
        lda.w $2137
        lda.w $213F
        lda.w $213D
        lda.w $213D
        and.b #$01
        beq +
        jmp fail2
+
        ; 3: 33 sprites at y = 100 (OAM's low table: x, y, tile, attr)
        lda.b #$80
        sta.w $2100         ; forced blank: OAM writes land anywhere
        stz.w $2102
        stz.w $2103
        ldx.w #0
-       stz.w $2104         ; x 0
        lda.b #100
        sta.w $2104         ; y
        stz.w $2104
        stz.w $2104
        inx
        cpx.w #33
        bne -
        ; the rest of the 128 off the lines (y = 240)
-       stz.w $2104
        lda.b #240
        sta.w $2104
        stz.w $2104
        stz.w $2104
        inx
        cpx.w #128
        bne -
        lda.b #$0F
        sta.w $2100
        ; wait out a frame past line 101
-       lda.w $4212
        bmi -
-       lda.w $4212
        bpl -
        lda.w $213E
        and.b #$40
        bne +
        jmp fail3
+
        ; 4: line 0, dot 281 (master clock 1138; the writes about 150
        ; clocks later, still in line 0): OAM entry 0's x, y
        lda.b #$80
        sta.w $2115
        stz.w $2102
        stz.w $2103
        rep #$20
        lda.w #281
        jsr AtLine0
        lda.b #$55
        sta.w $2104
        lda.b #$66
        sta.w $2104
        ; 5: the same in the next frame: VRAM word 0400h's low byte
        rep #$20
        lda.w #$0400
        sta.w $2116
        lda.w #281
        jsr AtLine0
        lda.b #$77
        sta.w $2118
        ; 6: line 0, dot 10 (master clock 54): VRAM word 0401h's low byte
        rep #$20
        lda.w #$0401
        sta.w $2116
        lda.w #10
        jsr AtLine0
        lda.b #$88
        sta.w $2118
        stz.w $4200
        ; read back in forced blank
        lda.b #$80
        sta.w $2100
        stz.w $2102
        stz.w $2103
        lda.w $2138
        beq +
        jmp fail4
+
        lda.w $2138
        cmp.b #100
        beq +
        jmp fail4
+
        rep #$20
        lda.w #$0400
        sta.w $2116
        sep #$20
        lda.w $2139
        beq +
        jmp fail5
+
        lda.w $213A
        beq +
        jmp fail5
+
        rep #$20
        lda.w #$0401
        sta.w $2116
        sep #$20
        lda.w $2139
        cmp.b #$88
        beq +
        jmp fail6
+
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

; With A (16 bits) the H position in dots: waits until the H+V timer
; flags it on line 0, then returns with A 8 bits.
AtLine0:
        sta.w $4207
        stz.w $4209
        sep #$20
        stz.w $4200
        lda.w $4211
        lda.b #$30
        sta.w $4200
-       lda.w $4211
        bpl -
        rts
.ENDS
