; The cassette's answer under interrupts (side track Q, the cassette's
; depth 1; A800.Spec.Machine.casRead). The answer is a program the CPU
; runs from the $D5 page; an interrupt between two of its instructions
; reads the next opcode twice before it is taken, and the program must
; not change at those reads (it once ended at its RTS's opcode: an NMI
; just before it returned through $FF). Boots from tape as cas_boot.s,
; then puts a display list interrupt on every line of the OS's screen and
; reads the tape's other records itself through SIOV (from the first the
; boot left, record 0 or 1, to the last): each must come in done (Y = 1),
; numbered in order (its first data byte). Prints "Pass", or
; "FAIL n": n the status if a read wasn't done, else $80 + the number of
; the record that came instead of the next.
.include "a800.inc"
DOSVEC  = $0A
DOSINI  = $0C
SDLSTL  = $0230
VDSLST  = $0200
DDEVIC  = $0300
DUNIT   = $0301
DCOMND  = $0302
DSTATS  = $0303
DBUFLO  = $0304
DBUFHI  = $0305
DTIMLO  = $0306
DBYTLO  = $0308
DBYTHI  = $0309
DAUX1   = $030A
DAUX2   = $030B
SIOV    = $E459
NREC    = 100
dl      = $F0                           ; zero page: the display list's address
count   = $F2

.segment "CODE"
hdr:    .byte 0                         ; flags
        .byte <((data - hdr) / 128)     ; records to boot: the code only
        .word hdr                       ; load address
        .word init                      ; init address
cont:   lda #<main                      ; load + 6: the boot continues here
        sta DOSVEC
        lda #>main
        sta DOSVEC+1
        clc
        rts
init:   rts

main:   lda SDLSTL                      ; a DLI on the OS's LMS line and its 23 mode 2 lines
        sta dl
        lda SDLSTL+1
        sta dl+1
        ldy #3                          ; the LMS instruction
        lda (dl),y
        ora #$80
        sta (dl),y
        ldy #6                          ; after its address: the 23 mode 2 lines
mark:   lda (dl),y
        ora #$80
        sta (dl),y
        iny
        cpy #29
        bne mark
        lda #<dli
        sta VDSLST
        lda #>dli
        sta VDSLST+1
        lda #$C0
        sta NMIEN
        lda #$FF                        ; the first read sets the count (the OS's boot may
        sta count                       ; have read a record past its header's count)
read:   lda #$60
        sta DDEVIC
        lda #1
        sta DUNIT
        lda #$52
        sta DCOMND
        lda #$40
        sta DSTATS
        lda #<buf
        sta DBUFLO
        lda #>buf
        sta DBUFHI
        lda #35
        sta DTIMLO
        lda #131
        sta DBYTLO
        lda #0
        sta DBYTHI
        sta DAUX1
        sta DAUX2
        jsr SIOV
        cpy #1
        bne badst
        lda buf+3                       ; the first data byte: the record's number
        ldx count
        cpx #$FF
        bne seq
        cmp #2                          ; the first: 0 or 1
        bcs bad
        sta count
        bcc more
seq:    inx
        stx count
        cmp count
        bne bad
more:   lda count
        cmp #NREC - 1
        bne read
        lda #$40                        ; DLIs off again
        sta NMIEN
        jmp pass
badst:  lda #$40                        ; FAIL: the status (Y) if not done
        sta NMIEN
        tya
        jmp fail
bad:    lda #$40                        ; else the record's number + $80 ... of the one expected
        sta NMIEN
        lda buf+3
        ora #$80
        jmp fail

dli:    pha
        lda count
        pla
        rti

.include "report.inc"
buf:    .res 131
        .res ((* - hdr + 127) / 128) * 128 - (* - hdr), $EA
data:                                   ; NREC records after the code, each its number first
.repeat NREC, i
        .byte i
        .res 127, $A5
.endrep
