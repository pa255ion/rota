; The 800XL's XEX loader (M4; docs/systems/atari800xl.md, "Loading").
;
; A 256-byte ROM page at $D500-$D5FF (the cartridge control area, unused
; by the standard cartridges), visible while an executable is attached.
; A stub cartridge at $A000-$BFFF, all $FF but its header, makes the OS
; start it like a cartridge after its own start-up (start $D500, init
; LDR_RTS, "start the cartridge, no disk boot"). The loader then turns
; the stub and BASIC off, so the program sees the machine as after a DOS
; boot without BASIC (RAMTOP $C0, a fresh E: screen, TRIG3 = 0), reads
; the file through the data port and loads it as Atari DOS 2 does: each
; segment to its addresses, INITAD called after a segment that changed
; it, RUNAD at the end (the first segment's start if no segment set it).
;
; Port (the last two bytes of the page; the hardware answers there):
;   read  $D5FE  the next byte of the file ($FF past the end)
;   read  $D5FF  bit 7: the whole file has been read
;   write $D5FE  the stub cartridge goes away (RD5 low)
;   write $D5FF  done: the page and the port go away at the CPU's next
;                read outside $D500-$D5FF (the final JMP (RUNAD) reads
;                RUNAD from page 2)
;
; Zero page: $43-$48 (the DOS file manager's, unused without DOS).

PORTB   = $D301
CIOV    = $E456
TRAMSZ  = $06
RAMTOP  = $6A
RUNAD   = $02E0
INITAD  = $02E2
ICCOM   = $0342
ICBAL   = $0344
ICBLL   = $0348
ICAX1   = $034A
ICAX2   = $034B
BASICF  = $03F8         ; XL OS: non-zero = BASIC stays off on reset

XDATA   = $D5FE
XSTAT   = $D5FF

ptr     = $43
last    = $45
first   = $47           ; the first segment's start (2 bytes)

.segment "LOADER"

start:  lda PORTB       ; BASIC off, then the stub cartridge
        ora #$02
        sta PORTB
        sta BASICF
        sta XDATA
        lda #0
        sta TRAMSZ
        lda #$C0
        sta RAMTOP
        ldx #0          ; reopen E: below the new RAMTOP
        lda #12         ; CLOSE
        sta ICCOM
        jsr CIOV
        ldx #0
        lda #3          ; OPEN "E:", read/write
        sta ICCOM
        lda #<ename
        sta ICBAL
        lda #>ename
        sta ICBAL+1
        lda #12
        sta ICAX1
        lda #0
        sta ICAX2
        jsr CIOV
        lda #<LDR_RTS   ; no run address yet
        sta RUNAD
        lda #>LDR_RTS
        sta RUNAD+1
        sta first+1     ; no first segment yet (high byte $D5)

segment:
        bit XSTAT
        bmi done
        jsr word        ; start address (or the $FFFF marker)
        lda ptr
        and ptr+1
        cmp #$FF
        beq segment
        lda first+1     ; remember the first segment's start
        cmp #>LDR_RTS
        bne :+
        lda ptr
        sta first
        lda ptr+1
        sta first+1
:       lda XDATA       ; end address
        sta last
        lda XDATA
        sta last+1
        lda #<LDR_RTS   ; INITAD unchanged = nothing to call
        sta INITAD
        lda #>LDR_RTS
        sta INITAD+1
        ldy #0
copy:   lda XDATA
        sta (ptr),y
        lda ptr
        cmp last
        bne next
        lda ptr+1
        cmp last+1
        beq copied
next:   inc ptr
        bne copy
        inc ptr+1
        jmp copy
copied: lda INITAD
        cmp #<LDR_RTS
        bne init
        lda INITAD+1
        cmp #>LDR_RTS
        beq segment
init:   jsr callinit
        jmp segment
callinit:
        jmp (INITAD)

done:   lda RUNAD       ; no RUNAD: the first segment's start
        cmp #<LDR_RTS
        bne run
        lda RUNAD+1
        cmp #>LDR_RTS
        bne run
        lda first
        sta RUNAD
        lda first+1
        sta RUNAD+1
run:    sta XSTAT
        jmp (RUNAD)

word:   lda XDATA
        sta ptr
        lda XDATA
        sta ptr+1
        rts

ename:  .byte "E:", $9B

.segment "LDRRTS"
LDR_RTS:
        rts
.export LDR_RTS
.assert start = $D500, error, "the stub cartridge's header starts the loader at $D500"
