; spcfuzz.s (SPC700, M7 step 7.9b): the sound module's I/O at random, for
; the machine's lockstep test (snes-core), not a pass/fail program. An
; 16-bit LFSR picks, again and again: timers 0 and 1's dividers (0
; included; timer 2 runs at 0 throughout, so its 256 steps count) and
; their enables through CONTROL (with its port clears and the boot ROM
; switched in and out; timers 0 and 2 stay on);
; a timer's counter read (it clears); a DSP register written or read
; through F2h/F3h; the CPU's ports read; F8h/F9h; the boot ROM's area
; read. Every value read goes to a log in ARAM (1000h-1FFFh) and to a
; port (F4h-F7h), so the CPU sees it. Assembled by `make snes-asm` into
; build/snes/spc/spcfuzz.bin (0200h-03FFh), which the 65816 ROM
; spcfuzz.s uploads through the boot ROM.

.MEMORYMAP
SLOTSIZE $200
DEFAULTSLOT 0
SLOT 0 $0200
.ENDME
.ROMBANKSIZE $200
.ROMBANKS 1

; direct page: the LFSR (16 bits), the log's pointer (low, high)
.DEFINE RND $00
.DEFINE RNDH $01
.DEFINE LOGL $02
.DEFINE LOGH $03

.BANK 0 SLOT 0
.ORG $000
.SECTION "code" FORCE
        mov RND,#$5A
        mov RNDH,#$3C
        mov LOGL,#$00
        mov LOGH,#$10
        mov $FC,#$00            ; timer 2 at divider 0 (256 steps) throughout
        mov $F1,#$87            ; the boot ROM in, timers on
        ; first a calibration: timer 2's counter read in a tight loop for
        ; about 18,000 cycles, the loop's count logged at each step (a
        ; step's time to a few cycles)
        mov x,#8
tcal:   mov y,#0
tcal2:  mov a,$FF
        beq +
        mov a,y
        push x
        call !log
        pop x
+       dbnz y,tcal2
        dec x
        bne tcal
loop:   call !rnd
        and a,#$07
        mov x,a
        cmp x,#0
        beq dividers
        cmp x,#1
        beq control
        cmp x,#2
        beq tread
        cmp x,#3
        beq dspw
        cmp x,#4
        beq dspr
        cmp x,#5
        beq ports
        cmp x,#6
        beq ram89
        jmp !iplread
dividers:
        ; rarely, so a divider of 0 (256 steps) lasts long enough to count
        call !rnd
        and a,#$07
        beq +
        jmp !loop
+       call !rnd
        and a,#$01              ; timers 0 and 1 (timer 2 keeps 0)
        mov x,a
        call !rnd
        and a,#$03              ; small dividers, 0 (256) one in 4
        mov $FA+x,a
        jmp !loop
control:
        call !rnd
        or a,#$05               ; timers 0 and 2 stay on (no restart)
        mov $F1,a
        jmp !loop
tread:  call !rnd
        and a,#$03
        cmp a,#3
        bne +
        mov a,#2
+       mov x,a
        mov a,$FD+x
        call !log
        jmp !loop
dspw:   call !rnd
        and a,#$7F
        mov $F2,a
        call !rnd
        mov y,a
        ; FLG keeps echo writes off (the echo would write over this program)
        mov a,$F2
        cmp a,#$6C
        bne +
        mov a,y
        or a,#$20
        mov y,a
+       mov $F3,y
        jmp !loop
dspr:   call !rnd
        mov $F2,a
        mov a,$F3
        call !log
        jmp !loop
ports:  call !rnd
        and a,#$03
        mov x,a
        mov a,$F4+x
        call !log
        jmp !loop
ram89:  call !rnd
        mov $F8,a
        mov a,$F9
        call !log
        mov a,$F8
        call !log
        jmp !loop
iplread:
        call !rnd
        and a,#$3F
        mov x,a
        mov a,!$FFC0+x
        call !log
        jmp !loop

; eight LFSR steps (16-bit Galois, taps B400h, period 65535): a fresh
; low byte in A (one step would give each byte the last one shifted)
rnd:    push y
        mov y,#8
-       lsr RNDH
        ror RND
        bcc +
        eor RNDH,#$B4
+       dbnz y,-
        pop y
        mov a,RND
        ret

; A to the log (1000h-1FFFh) and to a port (the log's pointer mod 4)
log:    mov y,#0
        mov [LOGL]+y,a
        push a
        mov a,LOGL
        and a,#$03
        mov x,a
        pop a
        mov $F4+x,a
        inc LOGL
        bne +
        inc LOGH
        mov a,LOGH
        cmp a,#$20
        bne +
        mov LOGH,#$10
+       ret
.ENDS
