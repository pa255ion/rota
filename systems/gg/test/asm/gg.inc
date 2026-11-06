; Shared definitions for our own Game Gear regression ROMs (M5 step 5.0;
; CLAUDE.md rule 5). Assembled by `make gg-asm` (WLA-DX: wla-z80, wlalink)
; into build/gg/asm/<name>.gg.
;
; The verdict convention: RESULT (C000h) is 00h while the test runs; at
; the end it is 01h (pass) or 80h + n (check n failed), then the ROM does
; DI / HALT. The Gg.Spec tests run a ROM until that HALT and read RESULT.
; On the screen (M5 step 5.10, for the board): `finish` draws the verdict,
; the whole window green on a pass, red on a fail with n small white
; squares on its third row (n = the failed check, at most 18); the layer 4
; goldens (test/golden/vga/<rom>.png) are the spec's pictures of it. A ROM
; whose own picture or sound is the test ends with `passkeep` instead.
;
; The Master System's ROMs (M6, systems/sms/test/asm/sms.inc) use this file
; too: sms.inc defines SMS first, which changes the header's region code (4,
; Master System export) and the verdict's colours (SMS CRAM: one byte per
; colour, --BBGGRR); everything else is shared. Without SMS the Game Gear's
; ROMs are byte for byte what they were before M6.
;
; Every ROM is 64 KiB (four 16 KiB banks) with the Sega header; bank n
; holds the byte n at offset 2000h of the bank (BANKMARK), so a test can
; see which bank a slot maps.

.MEMORYMAP
SLOTSIZE $4000
DEFAULTSLOT 0
SLOT 0 $0000
SLOT 1 $4000
SLOT 2 $8000
.ENDME

.ROMBANKMAP
BANKSTOTAL 4
BANKSIZE $4000
BANKS 4
.ENDRO

; Sega header at 7FF0h ("TMR SEGA", checksum, region 7: Game Gear
; international, size code E: 64 KiB).
; A ROM that defines CODEMASTERS before including this file leaves the
; Sega header out and brings its own Codemasters header (7FE0h).
.IFNDEF CODEMASTERS
.SMSHEADER
  PRODUCTCODE 0, 0, 0
  VERSION 0
.IFDEF SMS
  REGIONCODE 4
.ELSE
  REGIONCODE 7
.ENDIF
  RESERVEDSPACE $FF, $FF
  ROMSIZE $E
.ENDSMS
.ENDIF

.DEFINE RESULT    $C000
.DEFINE BOOT3E    $C00F   ; SMS: C000h as the boot found it (the BIOS's port 3Eh copy)
.DEFINE IRQVEC    $C010   ; RST 38h jumps here: by default IN A,(BFh) / EI / RET
.DEFINE BANKMARK  $2000

; The Sega mapper (SMS Power, "Mappers").
.DEFINE MAPCTRL   $FFFC   ; bit 3: cartridge RAM at 8000h, bit 2: its bank
.DEFINE MAPSLOT0  $FFFD
.DEFINE MAPSLOT1  $FFFE
.DEFINE MAPSLOT2  $FFFF

; Each bank's marker byte.
.BANK 0 SLOT 0
.ORGA BANKMARK
.SECTION "mark0" FORCE
  .DB 0
.ENDS
.BANK 1 SLOT 1
.ORGA $4000 + BANKMARK
.SECTION "mark1" FORCE
  .DB 1
.ENDS
.BANK 2 SLOT 2
.ORGA $8000 + BANKMARK
.SECTION "mark2" FORCE
  .DB 2
.ENDS
.BANK 3 SLOT 2
.ORGA $8000 + BANKMARK
.SECTION "mark3" FORCE
  .DB 3
.ENDS

; Install \1 as the interrupt handler (JP \1 at IRQVEC).
.MACRO SETIRQ
  ld a, $C3
  ld (IRQVEC), a
  ld hl, \1
  ld (IRQVEC + 1), hl
.ENDM

; VDP register \1 := \2.
.MACRO VDPREG
  ld a, \2
  out ($BF), a
  ld a, $80 | \1
  out ($BF), a
.ENDM

; Check that A equals \1, else fail with check number \2.
.MACRO EXPECT
  cp \1
  jp nz, fail\@
  jr ok\@
fail\@:
  ld a, $80 + \2
  jp finish
ok\@:
.ENDM

.BANK 0 SLOT 0
.ORGA $0000
.SECTION "boot" FORCE
boot:
  di
  im 1
  ld sp, $DFF0
.IFDEF SMS
  ld a, (RESULT)          ; the BIOS's last port 3Eh value, kept at C000h
  ld (BOOT3E), a
.ENDIF
  xor a
  ld (RESULT), a
  ld a, $DB               ; in a, ($BF): acknowledge the VDP
  ld (IRQVEC), a
  ld a, $BF
  ld (IRQVEC + 1), a
  ld a, $FB               ; ei
  ld (IRQVEC + 2), a
  ld a, $C9               ; ret
  ld (IRQVEC + 3), a
  jp main
.ENDS

.ORGA $0038
.SECTION "irq" FORCE
  jp IRQVEC
.ENDS

.ORGA $0066
.SECTION "nmi" FORCE
  retn
.ENDS

; End of a test: A is the verdict, also drawn on the screen.
.SECTION "finish" FREE
finish:
  ld (RESULT), a
  di
  call showResult
  halt
pass:
  ld a, $01
  jr finish
; A pass that leaves the screen and the PSG as the ROM set them.
passkeep:
  ld a, $01
  ld (RESULT), a
  di
  halt

; The verdict on the screen (RESULT): tile 0 empty, tile 1 a 6 x 6 square
; in colour 1, the name table all tile 0 but for the fail squares, no
; sprites, colour 0 green (pass) or red (fail), colour 1 white.
showResult:
  in a, ($BF)             ; a fresh control-port latch
  VDPREG 1, $00           ; display off while drawing
  VDPREG 0, $04
  VDPREG 2, $FF
  VDPREG 5, $7F
  VDPREG 6, $FB
  VDPREG 7, $00
  VDPREG 8, $00
  VDPREG 9, $00
  VDPREG 10, $FF
  xor a                   ; VRAM 0000h: tile 0, then tile 1
  out ($BF), a
  ld a, $40
  out ($BF), a
  ld b, 32
-:
  xor a
  out ($BE), a
  djnz -
  ld c, 8                 ; tile 1, rows 0-7: plane 0 7Eh on rows 1-6
--:
  ld a, c
  cp 8
  jr z, +
  cp 1
  jr z, +
  ld a, $7E
  jr ++
+:
  xor a
++:
  out ($BE), a
  xor a
  out ($BE), a
  out ($BE), a
  out ($BE), a
  dec c
  jr nz, --
  xor a                   ; the name table (3800h): all tile 0
  out ($BF), a
  ld a, $78
  out ($BF), a
  ld bc, 32 * 28 * 2
-:
  xor a
  out ($BE), a
  dec bc
  ld a, b
  or c
  jr nz, -
  ld a, (RESULT)          ; a fail: n squares from window row 2, column 1
  cp $01
  jr z, +
  and $7F
  jr z, +
  cp 19
  jr c, ++
  ld a, 18
++:
  ld b, a
  ld a, $4E               ; 3800h + 2 * (32 * 5 + 7)
  out ($BF), a
  ld a, $79
  out ($BF), a
-:
  ld a, $01
  out ($BE), a
  xor a
  out ($BE), a
  djnz -
+:
  xor a                   ; no sprites: D0h at the table's start (3F00h)
  out ($BF), a
  ld a, $7F
  out ($BF), a
  ld a, $D0
  out ($BE), a
  xor a                   ; CRAM: colour 0, colour 1
  out ($BF), a
  ld a, $C0
  out ($BF), a
  ld a, (RESULT)
  cp $01
.IFDEF SMS
  ld a, $0C               ; green (--BBGGRR)
  jr z, +
  ld a, $03               ; red
+:
  out ($BE), a
  ld a, $3F               ; white
  out ($BE), a
.ELSE
  ld a, $F0               ; green (----BBBBGGGGRRRR, low byte first)
  jr z, +
  ld a, $0F               ; red
+:
  out ($BE), a
  xor a
  out ($BE), a
  ld a, $FF               ; white
  out ($BE), a
  ld a, $0F
  out ($BE), a
.ENDIF
  VDPREG 1, $40           ; display on, no interrupts
  ret
.ENDS
