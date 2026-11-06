; The XEX headers for xex.cfg: the marker and MAIN's range, then a
; segment writing RUNAD = start.
.import __MAIN_START__, __MAIN_LAST__
.segment "XEXHDR"
        .word $FFFF, __MAIN_START__, __MAIN_LAST__ - 1
.segment "XEXRUN"
        .word RUNAD, RUNAD + 1, __MAIN_START__
.segment "CODE"
