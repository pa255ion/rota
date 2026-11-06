; The boot splash's Master System demo (M6; docs/brand.md, "Rendition B",
; "The Master System as built"). Every picture and sound is data from the
; generator (demos/rota-brand: Rota.Sms, Rota.Sms.Rom, Rota.Sound.Gg), in
; rota_data.inc; this file only plays it, as the Game Gear's
; demos/gg/rota.s does. Assembled by `make demos` (WLA-DX) into
; build/demos/rota.sms: 32 KiB, the Sega header (export), no mapper
; writes.
;
; Init (display off): the VDP's registers, VRAM cleared, the name table
; (the whole 32 x 24 cells), picture frame 0's tiles, the sprite patterns
; (tiles 0 on: the background, over 256 tiles, comes after them), the
; sprite table empty, colour RAM 16 on (the backdrop,
; then the sprite palette). Then each frame interrupt (line 193) plays one
; record of each stream: the sound (bytes to port 7Fh), the colours (CRAM
; writes, one byte an entry; the display goes on at the first beat), the
; tiles, the sparks' and the lights' sprite entries, all before the next
; frame's first line. A record: a count, then that many operations; FEh
; and an address jump to the stream's idle loop; FFh ends a stream (the
; sound).

.MEMORYMAP
SLOTSIZE $8000
DEFAULTSLOT 0
SLOT 0 $0000
.ENDME

.ROMBANKMAP
BANKSTOTAL 1
BANKSIZE $8000
BANKS 1
.ENDRO

; Region 4: Master System export; size code C: 32 KiB.
.SMSHEADER
  PRODUCTCODE 0, 0, 0
  VERSION 0
  REGIONCODE 4
  RESERVEDSPACE $FF, $FF
  ROMSIZE $C
.ENDSMS

.ENUM $C000
pSound  dw
pCram   dw
pTiles  dw
pSparks dw
pLights dw
.ENDE

.MACRO VDPREG ARGS reg, value
  ld a, value
  out ($BF), a
  ld a, $80 | reg
  out ($BF), a
.ENDM

; The VDP's address (and its write command) from the two bytes at (HL).
.MACRO SETADDR
  ld a, (hl)
  out ($BF), a
  inc hl
  ld a, (hl)
  out ($BF), a
  inc hl
.ENDM

.BANK 0 SLOT 0
.ORG $0000
.SECTION "boot" FORCE
  di
  im 1
  ld sp, $DFF0
  jp init
.ENDS

.ORG $0038
.SECTION "irq" FORCE
  push af
  in a, ($BF)             ; the frame interrupt: the status read acknowledges it
  pop af
  ei
  reti
.ENDS

.ORG $0066
.SECTION "nmi" FORCE
  retn                    ; Pause: nothing to do
.ENDS

.SECTION "main" FREE
init:
  VDPREG 0, $06           ; mode 4, 192 lines
  VDPREG 1, $20           ; display off, frame interrupts on
  VDPREG 2, $FF           ; name table 3800h
  VDPREG 3, $FF
  VDPREG 4, $FF
  VDPREG 5, $FF           ; sprite table 3F00h
  VDPREG 6, $FB           ; sprite patterns from tile 0
  VDPREG 7, $00           ; the backdrop: colour 16
  VDPREG 8, $00
  VDPREG 9, $00
  VDPREG 10, $FF
  ; VRAM cleared
  xor a
  out ($BF), a
  ld a, $40
  out ($BF), a
  ld bc, $4000
-:
  xor a
  out ($BE), a
  dec bc
  ld a, b
  or c
  jr nz, -
  ; the name table: 24 rows of 32 entries, the whole screen
  xor a
  out ($BF), a
  ld a, $78
  out ($BF), a
  ld hl, NameTable
  ld d, 24
-:
  ld bc, (64 << 8) | $BE
  otir
  dec d
  jr nz, -
  ; picture frame 0's tiles
  ld hl, InitTiles
  ld c, (hl)
  inc hl
  ld b, (hl)
  inc hl
-:
  push bc
  call tileOp
  pop bc
  dec bc
  ld a, b
  or c
  jr nz, -
  ; the sprite patterns at tile 0 (0000h)
  xor a
  out ($BF), a
  ld a, $40
  out ($BF), a
  ld hl, Patterns
  ld d, N_PATTERNS
-:
  ld bc, (32 << 8) | $BE
  otir
  dec d
  jr nz, -
  ; the sprite table: every slot hidden, then the list's end
  xor a
  out ($BF), a
  ld a, $7F
  out ($BF), a
  ld b, SPARK_SLOTS + LIGHT_SLOTS
-:
  ld a, $E0
  out ($BE), a
  djnz -
  ld a, $D0
  out ($BE), a
  ; colour RAM 16 on: the backdrop, the sprite palette (a byte each)
  ld a, 16
  out ($BF), a
  ld a, $C0
  out ($BF), a
  ld hl, SpriteCram
  ld bc, (10 << 8) | $BE
  otir
  ; the streams
  ld hl, GgSound
  ld (pSound), hl
  ld hl, GgCram
  ld (pCram), hl
  ld hl, GgTiles
  ld (pTiles), hl
  ld hl, GgSparks
  ld (pSparks), hl
  ld hl, GgLights
  ld (pLights), hl
  in a, ($BF)             ; no frame interrupt pending from the init
  ei
frame:
  halt                    ; the frame interrupt (line 193)
  call playSound
  ld hl, (pCram)
  call record
  jr z, +
-:
  call cramOp
  djnz -
+:
  ld (pCram), hl
  ld hl, (pTiles)
  call record
  jr z, +
-:
  push bc
  call tileOp
  pop bc
  djnz -
+:
  ld (pTiles), hl
  ld hl, (pSparks)
  call record
  jr z, +
-:
  call satOp
  djnz -
+:
  ld (pSparks), hl
  ld hl, (pLights)
  call record
  jr z, +
-:
  call satOp
  djnz -
+:
  ld (pLights), hl
  jr frame

; A record's count into B (Z if none), HL past it; FEh: jump to the loop.
record:
  ld a, (hl)
  cp $FE
  jr nz, +
  inc hl
  ld a, (hl)
  inc hl
  ld h, (hl)
  ld l, a
  jr record
+:
  inc hl
  ld b, a
  or a
  ret

; The sound: a count, then that many bytes to the PSG; FFh: the end.
playSound:
  ld hl, (pSound)
  ld a, (hl)
  cp $FF
  ret z
  inc hl
  or a
  jr z, +
  ld b, a
  ld c, $7F
  otir
+:
  ld (pSound), hl
  ret

; A colour (i, the byte, unused) or a register (80h | r, value, 0).
cramOp:
  ld a, (hl)
  bit 7, a
  jr nz, +
  out ($BF), a
  ld a, $C0
  out ($BF), a
  inc hl
  ld a, (hl)
  out ($BE), a
  inc hl
  inc hl
  ret
+:
  inc hl
  ld c, a
  ld a, (hl)
  out ($BF), a
  ld a, c
  out ($BF), a
  inc hl
  inc hl
  ret

; A tile: the VDP's write address, then the library graphic's address.
tileOp:
  SETADDR
  ld e, (hl)
  inc hl
  ld d, (hl)
  inc hl
  push hl
  ex de, hl
  ld bc, (32 << 8) | $BE
  otir
  pop hl
  ret

; A sprite entry: slot, Y, X, pattern (Y at 3F00h + slot, X and the
; pattern at 3F80h + 2 slot).
satOp:
  ld a, (hl)
  ld c, a
  out ($BF), a
  ld a, $7F
  out ($BF), a
  inc hl
  ld a, (hl)
  out ($BE), a
  ld a, c
  add a, a
  add a, $80
  out ($BF), a
  ld a, $7F
  out ($BF), a
  inc hl
  ld a, (hl)
  out ($BE), a
  inc hl
  ld a, (hl)
  out ($BE), a
  inc hl
  ret
.ENDS

.SECTION "data" FREE
.INCLUDE "rota_data.inc"
.ENDS
