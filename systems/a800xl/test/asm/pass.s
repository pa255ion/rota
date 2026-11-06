; The smallest XEX: prints "Pass" through the OS's screen editor and
; stops. Checks the loader path end to end (A800.Media, the loader stub,
; the screen hook).
.include "a800.inc"
.include "xex.inc"

start:  print msg, msglen
loop:   jmp loop

msg:    .byte "Pass", EOL
msglen = * - msg
