; dsprace.s (SPC700, M7 step 7.12): the SPC700 against the DSP's samples,
; for the machine's lockstep test (snes-core), not a pass/fail program.
; Since 7.12 the Clash DSP makes a sample beside the SPC700 ("Snes.Apu"),
; and the sample must still see ARAM and the registers as the spec's does
; (made before the cycle's access and every later one). Four voices loop
; one BRR block at a high pitch (the DSP reads its bytes often) with the
; echo on (ESA 40h, EDL 1: 4000h-47FFh, written back each sample); then,
; again and again: the BRR block's data bytes rewritten (the DSP's reads
; mid-sample: the write log), the echo buffer read and written along its
; length (the echo bytes' wait), DSP registers written (pitch, volume; a
; KON now and then: the queue) and read (ENVX, OUTX: the read's wait),
; every value read sent to port F4h. Assembled by `make snes-asm` into
; build/snes/spc/dsprace.bin (0200h-03FFh), which the 65816 ROM
; dsprace.s uploads through the boot ROM.

.MEMORYMAP
SLOTSIZE $200
DEFAULTSLOT 0
SLOT 0 $0200
.ENDME
.ROMBANKSIZE $200
.ROMBANKS 1

; direct page: the echo sweep's offset (low, high), a counter
.DEFINE EL $00
.DEFINE EH $01
.DEFINE CNT $02

.BANK 0 SLOT 0
.ORG $000
.SECTION "code" FORCE
        mov $F2,#$6C    ; FLG: unmuted, echo writes on, no reset
        mov $F3,#$00
        mov $F2,#$5D    ; DIR: the table at 0300h
        mov $F3,#$03
        mov $F2,#$6D    ; ESA 40h: the echo buffer at 4000h
        mov $F3,#$40
        mov $F2,#$7D    ; EDL 1: 2 KiB
        mov $F3,#$01
        mov $F2,#$0D    ; EFB 40h
        mov $F3,#$40
        mov $F2,#$2C    ; EVOL 30h, 30h
        mov $F3,#$30
        mov $F2,#$3C
        mov $F3,#$30
        mov $F2,#$4D    ; EON: voices 0-3
        mov $F3,#$0F
        mov $F2,#$0F    ; FIR tap 0 40h
        mov $F3,#$40
        mov $F2,#$0C    ; MVOL 40h, 40h
        mov $F3,#$40
        mov $F2,#$1C
        mov $F3,#$40
        ; voices 0-3: source 0, GAIN direct 7Fh, volume 30h, pitch 3000h
        mov x,#$00
voice:  mov a,x
        or a,#$04       ; SRCN
        mov $F2,a
        mov $F3,#$00
        mov a,x
        or a,#$05       ; ADSR1: GAIN mode
        mov $F2,a
        mov $F3,#$00
        mov a,x
        or a,#$07       ; GAIN
        mov $F2,a
        mov $F3,#$7F
        mov a,x
        mov $F2,a       ; VOL L
        mov $F3,#$30
        inc a
        mov $F2,a       ; VOL R
        mov $F3,#$30
        inc a
        mov $F2,a       ; PITCH low
        mov $F3,#$00
        inc a
        mov $F2,a       ; PITCH high
        mov $F3,#$30
        mov a,x
        clrc
        adc a,#$10
        mov x,a
        cmp x,#$40
        bne voice
        mov $F2,#$4C    ; KON voices 0-3
        mov $F3,#$0F
        mov EL,#$00
        mov EH,#$40
        mov CNT,#$00
loop:
        ; the BRR block's data bytes (not its header) rewritten
        mov x,#$08
brr:    mov a,!$0310+x
        eor a,#$11
        mov !$0310+x,a
        dec x
        bne brr
        ; the echo buffer: four bytes read and written back changed
        mov y,#$00
echo:   mov a,[EL]+y
        mov $F4,a
        eor a,#$5A
        mov [EL]+y,a
        inc y
        cmp y,#$04
        bne echo
        mov a,EL
        clrc
        adc a,#$04
        mov EL,a
        mov a,EH
        adc a,#$00
        cmp a,#$48
        bne +
        mov a,#$40
+       mov EH,a
        ; registers: voice 0's pitch and volume written, ENVX and OUTX read
        mov a,CNT
        mov $F2,#$02
        mov $F3,a
        mov $F2,#$00
        mov $F3,a
        mov $F2,#$08
        mov a,$F3
        mov $F5,a
        mov $F2,#$19
        mov a,$F3
        mov $F6,a
        ; a KON of voices 0-3 every 64th time
        inc CNT
        mov a,CNT
        and a,#$3F
        bne +
        mov $F2,#$4C
        mov $F3,#$0F
+       jmp !loop
.ENDS

.ORG $100
.SECTION "dir" FORCE
        .DW $0310, $0310
.ENDS

.ORG $110
.SECTION "brr" FORCE
        ; shift 12, filter 0, loop and end
        .DB $C3, $77, $12, $34, $77, $99, $AB, $CD, $99
.ENDS
