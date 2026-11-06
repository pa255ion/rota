; Our own SPC700 boot ROM (IPL), 64 bytes at FFC0h-FFFFh (M7 step 7.4; the
; user's decision, 2026-10-15: never Nintendo's, which is never fetched or
; committed; the user's own dump, /snes/ipl.bin, replaces this one when the
; card has it). Written from the upload protocol in fullsnes ("SNES APU
; Main CPU Communication Port", "Uploader"); its interface is fixed by what
; programs rely on, so it keeps:
;  - the entry points: FFC0h (reset; programs jump there to start again)
;    and FFC9h (announce AAh, BBh and wait for the kick);
;  - the zero page cleared (0001h-00EFh) and SP = EFh;
;  - the handshake: AAh/BBh in ports 0/1, the kick CCh, the address in
;    ports 2/3, command (port 1) 0 = run, else a block: the bytes come with
;    their index in port 0, each echoed back; a later kick (index + 2 or
;    more) starts the next command;
;  - the hand-over: A = X = Y = 0, the address at 0000h.
; Assembled by `make snes-asm` (wla-spc700) into build/snes/ipl.bin.

.MEMORYMAP
SLOTSIZE $40
DEFAULTSLOT 0
SLOT 0 $FFC0
.ENDME
.ROMBANKSIZE $40
.ROMBANKS 1

.BANK 0 SLOT 0
.ORG $00
.SECTION "ipl" FORCE
; reset: the zero page from EFh down to 01h, the stack at 01EFh
        mov a,#$00
        mov x,#$EF
        mov sp,x
clear:  mov (x),a
        dec x
        bne clear
; FFC9h: ready
ready:  mov $F4,#$AA
        mov $F5,#$BB
kick:   cmp $F4,#$CC
        bne kick
; a command: the address from ports 2-3, port 1 the command
command:
        movw ya,$F6
        movw $00,ya
        movw ya,$F4
        mov $F4,a
        mov a,y
        mov x,a
        beq run
; a block: index 0 first, then each byte with its index
        mov y,#$00
first:  cmp y,$F4
        bne first
byte:   mov a,$F5
        mov $F4,y
        mov [$00]+y,a
        inc y
        bne poll
        inc $01
; the next index (equal: a byte), the last one still there (wait), or a
; kick past it (the next command)
poll:   cmp y,$F4
        beq byte
        bpl poll
        bra command
run:    jmp [!$0000+x]
.ENDS

.ORG $3E
.SECTION "vector" FORCE
        .DW $FFC0
.ENDS
