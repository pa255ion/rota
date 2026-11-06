; The smallest 8 KiB cartridge: the OS starts it after its own start-up
; (header byte $BFFD bit 2), it prints "Pass" and stops.
.include "a800.inc"

.segment "CODE"
start:  print msg, msglen
loop:   jmp loop
init:   rts

msg:    .byte "Pass", EOL
msglen = * - msg

.segment "CARTHDR"
        .word start     ; $BFFA: start address
        .byte 0         ; $BFFC: 0 = a cartridge is present
        .byte 4         ; $BFFD: start the cartridge; no disk boot
        .word init      ; $BFFE: init address
