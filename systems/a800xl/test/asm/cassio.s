; The tape through SIOV without the OS's boot (side track Q, cassettes on
; the board; A800.Spec.Machine.casRead, A800.Core.casAnswer): the OS's
; cassette boot waits for the tape's leader (over 800 frames before the
; first record), too long for a layer 4 run, so this executable, with
; cas_data.cas in the recorder, reads it itself: 20 records, each done (Y
; = 1, DSTATS 1) with its number as its first data byte, then the
; end-of-file record (control byte $FE), then a read with no record left
; (Y = 144). Prints "Pass", or "FAIL n" (n: the record, $80 + it for a
; wrong number, $E0 the end-of-file record, $F0 the read past the end).
.include "a800.inc"
.include "xex.inc"
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
count   = $F2

start:  lda #0
        sta count
next:   jsr read
        cpy #1
        bne bad
        lda DSTATS
        cmp #1
        bne bad
        lda buf+3
        cmp count
        bne badnum
        inc count
        lda count
        cmp #20
        bne next
        jsr read                        ; the end-of-file record
        cpy #1
        bne badeof
        lda buf+2
        cmp #$FE
        bne badeof
        jsr read                        ; no record left
        cpy #144
        bne badend
        jmp pass
bad:    lda count
        jmp fail
badnum: lda count
        ora #$80
        jmp fail
badeof: lda #$E0
        jmp fail
badend: lda #$F0
        jmp fail

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
        jmp SIOV

.include "report.inc"
buf:    .res 131
