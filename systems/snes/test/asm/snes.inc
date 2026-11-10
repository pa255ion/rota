; Shared definitions for our own Super Nintendo regression ROMs (M7 step
; 7.0; CLAUDE.md rule 5). Assembled by `make snes-asm` (WLA-DX: wla-65816,
; wlalink) into build/snes/asm/<name>.sfc.
;
; NMI and IRQ (both modes) count in NMICOUNT (7E0002h) and IRQCOUNT
; (7E0003h); the IRQ handler acknowledges (reads 4211h) and keeps what it
; read in IRQFLAG (7E0004h). Reset clears them. A ROM that defines SRAM
; before including this file has 8 KiB of battery SRAM (type 02h); one that
; defines DSP is a DSP cartridge (type 03h, ROM + DSP: the DSP-1's DR at
; 30:8000h, SR at 30:C000h; step 7.16).
;
; The verdict convention, as the Game Gear's: RESULT (WRAM 7E0000h) is 00h
; while the test runs; at the end it is 01h (pass) or 80h + n (check n
; failed), then the ROM stops the CPU (STP). The Snes.Spec tests (step 7.3
; on) run a ROM until that STP and read RESULT. Drawing the verdict on the
; screen comes with the PPU (step 7.5).
;
; A ROM is LoROM, 32 KiB (one bank at 8000h), unless it defines HIROM
; before including this file: then HiROM, 64 KiB (bank C0h, code at
; C08000h, also seen at 008000h). Either way the code starts at RESET
; in emulation mode, as after a power-on. Each ROM writes its header
; with `SNES_HEADER "TITLE"` after the include (21 characters, padded
; with spaces as real headers are) (WLA-DX checks the
; header's checksum by itself).

.IFDEF HIROM
.MEMORYMAP
SLOTSIZE $10000
DEFAULTSLOT 0
SLOT 0 $0000
.ENDME
.ROMBANKSIZE $10000
.ROMBANKS 1
.HIROM
.ELSE
.MEMORYMAP
SLOTSIZE $8000
DEFAULTSLOT 0
SLOT 0 $8000
.ENDME
.ROMBANKSIZE $8000
.ROMBANKS 1
.LOROM
.ENDIF

.MACRO SNES_HEADER
.SNESHEADER
ID "SNES"
NAME "\1"
SLOWROM
.IFDEF HIROM
ROMSIZE $06
.ELSE
ROMSIZE $05
.ENDIF
.IFDEF SRAM
CARTRIDGETYPE $02
SRAMSIZE $03
.ELSE
.IFDEF DSP
CARTRIDGETYPE $03
SRAMSIZE $00
.ELSE
CARTRIDGETYPE $00
SRAMSIZE $00
.ENDIF
.ENDIF
COUNTRY $01
LICENSEECODE $00
VERSION $00
.ENDSNES
.ENDM

.SNESNATIVEVECTOR
COP Halt
BRK Halt
ABORT Halt
NMI Nmi
IRQ Irq
.ENDNATIVEVECTOR

.SNESEMUVECTOR
COP Halt
ABORT Halt
NMI Nmi
RESET Reset
IRQBRK Irq
.ENDEMUVECTOR


.DEFINE RESULT $7E0000
.DEFINE NMICOUNT $7E0002
.DEFINE IRQCOUNT $7E0003
.DEFINE IRQFLAG $7E0004

.IFDEF HIROM
; labels in bank C0h, where all of HiROM's bank 0 shows (its upper half
; also at 00h, where the vectors point)
.BASE $C0
.ENDIF
.BANK 0 SLOT 0
.IFDEF HIROM
.ORG $8000
.ELSE
.ORG $0000
.ENDIF

.SECTION "snes.inc" SEMIFREE
; Any interrupt or BRK here is a failure: verdict 80h + 7Fh.
Halt:
        sep #$20
        lda.b #$FF
        sta.l RESULT
        stp

; Into native mode with 8-bit A and 16-bit X/Y, the stack at 1FFFh, the
; data bank 0 and the direct page 0, the display in forced blank (as
; programs start: VRAM takes writes only then or in V-blank), RESULT
; cleared, then the ROM's Main.
Reset:
        sei
        clc
        xce
        rep #$38
        ldx.w #$1FFF
        txs
        lda.w #$0000
        tcd
        sep #$20
        pha
        plb
        lda.b #$80
        sta.l $002100
        lda.b #$00
        sta.l RESULT
        sta.l NMICOUNT
        sta.l IRQCOUNT
        sta.l IRQFLAG
        jml Main

; NMI and IRQ count (8-bit A for the handler, P and A restored).
Nmi:
        sep #$20
        pha
        lda.l NMICOUNT
        inc a
        sta.l NMICOUNT
        pla
        rti

Irq:
        sep #$20
        pha
        lda.l $004211
        sta.l IRQFLAG
        lda.l IRQCOUNT
        inc a
        sta.l IRQCOUNT
        pla
        rti

; Ends the test: A (8 bits) = 01h passed, 80h + n check n failed.
Finish:
        sep #$20
        sta.l RESULT
        stp
.ENDS
