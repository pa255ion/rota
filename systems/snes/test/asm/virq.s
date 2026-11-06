; virq.s (M7 step 7.3): the V-IRQ timer. Check 1: with VTIME = 100 and
; V-IRQ enabled, IRQs come (IRQCOUNT counts two); 2: in the handler 4211h
; read with bit 7 set; 3: with IRQs masked, TIMEUP (4211h bit 7) sets
; again, and the V counter latched just after (2137h, 213Dh) is line 100.

.INCLUDE "snes.inc"
SNES_HEADER "M7 VIRQ TEST         "

.SECTION "main" FREE
Main:
        lda.b #100
        sta.w $4209
        stz.w $420A
        lda.b #$20
        sta.w $4200
        cli
-       lda.l IRQCOUNT
        cmp.b #2
        bcc -
        sei
        ; 2
        lda.l IRQFLAG
        bpl fail2
        ; 3
        lda.w $4211
-       lda.w $4211
        bpl -
        lda.w $2137
        lda.w $213D
        cmp.b #100
        bne fail3
        lda.b #$01
        jmp Finish
fail2:  lda.b #$82
        jmp Finish
fail3:  lda.b #$83
        jmp Finish
.ENDS
