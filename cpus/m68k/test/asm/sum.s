| The sum of 99 down to 0 in D0, then STOP: m68k-lib's golden program
| (cpus/m68k/test/Lib.hs: 203 instructions, 1820 cycles). Assembled by
| GNU as (m68k binutils, the dev shell) at 0x1000.
        .text
        .org    0
start:  moveq   #0,%d0
        moveq   #99,%d1
loop:   add.l   %d1,%d0
        dbra    %d1,loop
        stop    #0x2700
