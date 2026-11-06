; A cassette boot file (side track Q, the cassette's depth 1: the OS's
; cassette routine answered from the file, A800.Spec.Machine.casTrap).
; The OS boots it from tape when START is held at power-on and a key is
; pressed: it reads the first record, takes the header below (a flag,
; the number of 128-byte records, the load and the init address), loads
; the rest, calls the load address + 6 (carry clear: booted), then the
; init address through DOSINI, then jumps through DOSVEC. Prints "Pass"
; from there: proof that every record came in.
.include "a800.inc"
DOSVEC  = $0A
DOSINI  = $0C

.segment "CODE"
hdr:    .byte 0                         ; flags
        .byte <((last - hdr + 127) / 128) ; records
        .word hdr                       ; load address
        .word init                      ; init address
cont:   lda #<main                      ; load + 6: the boot continues here
        sta DOSVEC
        lda #>main
        sta DOSVEC+1
        clc
        rts
init:   rts
main:   lda marker                      ; the last record's byte: all loaded
        cmp #$C5
        beq ok
        lda #1
        jmp fail
ok:     jmp pass
.include "report.inc"
        .res 300, $EA                   ; padding: the file spans several records
marker: .byte $C5
last:
