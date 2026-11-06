; The Game Boy boot splash (PLAN.md, side track J, step 2; docs/brand.md,
; "Rendition B").
;
; A stream player: demos/rota-brand (Rota.Gb, Rota.Gb.Rom) lays out every
; tile and every frame's changes; Rota.Gb.Rom.replay is the exact model of
; what this file shows, and the layer 3 tests compare every frame with it.
; The generator (make demos) writes rota_pic.inc (tiles, the first
; tilemap, the fabric's step table, the picture streams) and
; rota_sound.inc (the APU writes).
;
; Each frame, from the vertical blank interrupt (ROM frame k):
;   in the vertical blank  OAM by DMA (the shadow holds frame k's
;                          sprites), LCDC and BGP and the tilemap writes
;                          (RotaBg), the fabric's steps (RotaFab, each
;                          redrawing the slots in RotaSteps)
;   then                   the sound (RotaSound), then the sparks' and
;                          lights' records of frame k + 1 into the shadow
; Each picture stream has an intro and a loop: a record count of $FF
; jumps to the loop's start. The sound stream's $FF ends it.
;
; 32 KiB, no mapper. Background tiles with LCDC bit 4 clear (index i < 128
; at $9000 + 16i, the rest at $8800); sprite tiles at $8000. After the
; chime the idle loop plays for ever: waiting for a key is the PS's job.

DEF rLCDC EQU $FF40
DEF rLY   EQU $FF44
DEF rSTAT EQU $FF41
DEF rSCY  EQU $FF42
DEF rSCX  EQU $FF43
DEF rDMA  EQU $FF46
DEF rBGP  EQU $FF47
DEF rOBP0 EQU $FF48
DEF rIF   EQU $FF0F
DEF rIE   EQU $FFFF

DEF LCDC_BASE EQU %10000001   ; LCD on, BG tiles at $8800/$9000, BG on
DEF LCDC_MAP1 EQU %00001000
DEF LCDC_OBJ  EQU %00000010
DEF BGP_ON    EQU $1B         ; 0 ink, 1 teal, 2 amber, 3 paper (Rota.Gb)
DEF BGP_DARK  EQU $FF
DEF OBP0_ON   EQU $90         ; 1 paper, 2 amber, 3 teal


SECTION "shadow oam", WRAM0[$C000]
wOam:    ds 160
SECTION "wram vars", WRAM0[$C100]
wSaveSp: ds 2
wCount:  ds 1
SECTION "wram debug", WRAM0[$D0F0]  ; read by the layer 3 tests
wOverruns: ds 1         ; frames whose vertical-blank work ran into line 0
wMaxLines: ds 1         ; the most lines (from 144) that work took (10: into line 153)
wMaxAt:    ds 3         ; the most lines after: the wake-up, OAM DMA, the tilemap
wLines:    ds 1         ; this frame's lines

; PROFILE n: the most lines since 144 so far at this point, in wMaxAt + n.
MACRO PROFILE
    ldh a, [rLY]
    sub 144
    jr nc, .lines\@
    add 154
.lines\@:
    ld b, a
    ld a, [wMaxAt + \1]
    cp b
    jr nc, .keep\@
    ld a, b
    ld [wMaxAt + \1], a
.keep\@:
ENDM

SECTION "hram", HRAM
hDma:    ds 10          ; the OAM DMA routine (copied from DmaCode)
hBg:     ds 2
hFab:    ds 2
hSpk:    ds 2
hLgt:    ds 2
hSnd:    ds 2
hStep:   ds 1           ; the fabric's step, 0 to 15

SECTION "vblank", ROM0[$40]
    reti                ; the main loop wakes from halt

SECTION "header", ROM0[$100]
    nop
    jp Start
    ds $150 - @, 0      ; rgbfix fills the header

SECTION "main", ROM0
Start:
    di
    ld sp, $FFFE
.waitVBlank:            ; the LCD may only be turned off in vertical blank
    ldh a, [rLY]
    cp 144
    jr c, .waitVBlank
    xor a
    ldh [rLCDC], a

    ld hl, $8000        ; all of VRAM: 0
    ld bc, $2000
    call Clear
    ld hl, wOam
    ld bc, 160
    call Clear

    ld hl, RotaBgTiles  ; background tiles: 0 to 127 at $9000, the rest at $8800
    ld de, $9000
    ld bc, 128 * 16
    call Copy
    ld de, $8800
    ld bc, (ROTA_BG_TILES - 128) * 16
    call Copy
    ld hl, RotaObjTiles
    ld de, $8000
    ld bc, ROTA_OBJ_TILES * 16
    call Copy
    ld de, $9800        ; the first beat's tilemap, in both maps
    call CopyMap
    ld de, $9C00
    call CopyMap

    ld hl, DmaCode
    ld c, LOW(hDma)
    ld b, DmaEnd - DmaCode
.dma:
    ld a, [hl+]
    ldh [c], a
    inc c
    dec b
    jr nz, .dma

    ld a, LOW(RotaBg)
    ldh [hBg], a
    ld a, HIGH(RotaBg)
    ldh [hBg + 1], a
    ld a, LOW(RotaFab)
    ldh [hFab], a
    ld a, HIGH(RotaFab)
    ldh [hFab + 1], a
    ld a, LOW(RotaSparks)
    ldh [hSpk], a
    ld a, HIGH(RotaSparks)
    ldh [hSpk + 1], a
    ld a, LOW(RotaLights)
    ldh [hLgt], a
    ld a, HIGH(RotaLights)
    ldh [hLgt + 1], a
    ld a, LOW(RotaSound)
    ldh [hSnd], a
    ld a, HIGH(RotaSound)
    ldh [hSnd + 1], a
    ld a, ROTA_STEP0
    ldh [hStep], a
    xor a
    ld [wOverruns], a
    ld [wMaxLines], a
    ld [wMaxAt], a
    ld [wMaxAt + 1], a
    ld [wMaxAt + 2], a
    ldh [rSCX], a
    ldh [rSCY], a
    call Sprites        ; frame 0's sprites into the shadow

    ld a, BGP_DARK
    ldh [rBGP], a
    ld a, OBP0_ON
    ldh [rOBP0], a
    ld a, LCDC_BASE
    ldh [rLCDC], a
    xor a
    ldh [rIF], a
    ld a, 1             ; vertical blank interrupt only
    ldh [rIE], a
    ei

Main:
    halt
    nop
    ; --- the vertical blank ---
    PROFILE 0
    ld a, HIGH(wOam)
    call hDma
    PROFILE 1
    call Background
    PROFILE 2
    call Fabric
    ldh a, [rSTAT]      ; still in the vertical blank? (LY reads 0 for
    and 3               ; most of line 153, so the mode tells)
    cp 1
    jr z, .inTime
    ld hl, wOverruns
    inc [hl]
    jr .measured
.inTime:
    ldh a, [rLY]        ; lines used (line 153 reads as 0: 10)
    sub 144
    jr nc, .ly
    add 154
.ly:
    ld b, a
    ld [wLines], a
    ld a, [wMaxLines]
    cp b
    jr nc, .measured
    ld a, b
    ld [wMaxLines], a
.measured:
    ; --- the rest of the frame ---
    call Sound
    call Sprites        ; the next frame's sprites
    jr Main

; Clear: bc bytes at hl to 0.
Clear:
    xor a
    ld [hl+], a
    dec bc
    ld a, b
    or c
    jr nz, Clear
    ret

; Copy: bc bytes from hl to de.
Copy:
    ld a, [hl+]
    ld [de], a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, Copy
    ret

; CopyMap: RotaMap's 20 x 18 into the map at de.
CopyMap:
    ld hl, RotaMap
    ld b, 18
.row:
    ld c, 20
.cell:
    ld a, [hl+]
    ld [de], a
    inc de
    dec c
    jr nz, .cell
    ld a, e
    add 12
    ld e, a
    jr nc, .nc
    inc d
.nc:
    dec b
    jr nz, .row
    ret

DmaCode:
    ldh [rDMA], a
    ld a, 40
.wait:
    dec a
    jr nz, .wait
    ret
DmaEnd:

; --- the tilemap stream: per frame a ctrl byte (bit 7 map 1 shown, bit 6
; display on), then the address of the frame's writes as generated code
; (ld a, n / ld [nn], a ... ret), or 0; a ctrl byte of $FF jumps to the
; loop ---
Background:
    ldh a, [hBg]
    ld l, a
    ldh a, [hBg + 1]
    ld h, a
    ld a, [hl+]
    cp $FF
    jr nz, .record
    ld hl, RotaBgLoop
    ld a, [hl+]
.record:
    ld b, a
    ld c, LCDC_BASE
    bit 7, b
    jr z, .map0
    ld c, LCDC_BASE | LCDC_MAP1
.map0:
    bit 6, b
    ld a, BGP_DARK
    jr z, .dark
    ld a, c
    or LCDC_OBJ
    ld c, a
    ld a, BGP_ON
.dark:
    ldh [rBGP], a
    ld a, c
    ldh [rLCDC], a
    ld a, [hl+]
    ld e, a
    ld a, [hl+]
    ld d, a
    ld a, l
    ldh [hBg], a
    ld a, h
    ldh [hBg + 1], a
    ld a, d
    or e
    ret z
    ld h, d
    ld l, e
    jp hl               ; the code returns for us

; --- the fabric: per frame the number of steps; each step from s to
; s + 1 copies the looks RotaSteps lists for s into their slots: with sp
; on the list, a count of horizontal slots (4 bytes each: rows 3 and 4),
; their (slot, look) addresses, then a count of whole slots and theirs.
; RotaFabric sits in one 256-byte page, so the looks' addresses step by
; inc e. ---
Fabric:
    ldh a, [hFab]
    ld l, a
    ldh a, [hFab + 1]
    ld h, a
    ld a, [hl+]
    cp $FF
    jr nz, .record
    ld hl, RotaFabLoop
    ld a, [hl+]
.record:
    ld b, a
    ld a, l
    ldh [hFab], a
    ld a, h
    ldh [hFab + 1], a
    ld a, b
    and a
    ret z
    ld [wCount], a
    ld [wSaveSp], sp
.step:
    ldh a, [hStep]
    add a
    ld l, a
    ld h, HIGH(RotaSteps)   ; RotaSteps is page-aligned
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    ld sp, hl
    pop bc
    ld a, c
    and a
    jr z, .whole
.h:
    pop hl
    pop de
    REPT 4
        ld a, [de]
        ld [hl+], a
        inc e
    ENDR
    dec c
    jr nz, .h
.whole:
    pop bc
    ld a, c
    and a
    jr z, .stepDone
.w:
    pop hl
    pop de
    REPT 16
        ld a, [de]
        ld [hl+], a
        inc e
    ENDR
    dec c
    jr nz, .w
.stepDone:
    ldh a, [hStep]
    inc a
    and 15
    ldh [hStep], a
    ld a, [wCount]
    dec a
    ld [wCount], a
    jr nz, .step
    ld hl, wSaveSp
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    ld sp, hl
    ret

; --- the sound stream: (register, value) pairs into $FF00 + register ---
Sound:
    ldh a, [hSnd]
    ld l, a
    ldh a, [hSnd + 1]
    ld h, a
    ld a, [hl+]
    cp $FF
    ret z               ; ended: stay on the end marker
    and a
    jr z, .done
    ld b, a
.pair:
    ld a, [hl+]
    ld c, a
    ld a, [hl+]
    ldh [c], a
    dec b
    jr nz, .pair
.done:
    ld a, l
    ldh [hSnd], a
    ld a, h
    ldh [hSnd + 1], a
    ret

; --- the sparks and lights streams: a count, then (OAM offset, y, x,
; tile, attributes) per sprite, into the shadow ---
Sprites:
    ld c, LOW(hSpk)
    ld de, RotaSparksLoop
    call OamRecord
    ld c, LOW(hLgt)
    ld de, RotaLightsLoop
    ; fall through

; OamRecord: the stream whose pointer is at HRAM c, its loop at de.
OamRecord:
    ldh a, [c]
    ld l, a
    inc c
    ldh a, [c]
    ld h, a
    ld a, [hl+]
    cp $FF
    jr nz, .record
    ld h, d
    ld l, e
    ld a, [hl+]
.record:
    and a
    jr z, .done
    ld b, a
.sprite:
    ld a, [hl+]
    ld e, a
    ld d, HIGH(wOam)
    REPT 4
        ld a, [hl+]
        ld [de], a
        inc e
    ENDR
    dec b
    jr nz, .sprite
.done:
    ld a, h
    ldh [c], a
    dec c
    ld a, l
    ldh [c], a
    ret

SECTION "data", ROM0
INCLUDE "rota_pic.inc"
INCLUDE "rota_sound.inc"
