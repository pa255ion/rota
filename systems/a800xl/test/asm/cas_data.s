; A plain tape for cassio.s (side track Q, cassettes on the board): 20
; records, record n's first data byte n, the rest $A5 (a800-cas cuts the
; block into 128-byte records and adds an end-of-file record). Not a boot
; tape: nothing here runs.
.segment "CODE"
.repeat 20, i
        .byte i
        .res 127, $A5
.endrep
