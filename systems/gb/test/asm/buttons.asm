; Regression ROM for the input path (M2 step 2.7): shows the eight buttons
; as squares on row 8, columns 2, 4, ..., 16 of the BG map (Right, Left,
; Up, Down, A, B, Select, Start); a square is black while its button is
; held. Every VBlank it reads P1 (directions, then buttons) and updates the
; map. With buttons held from outside (Gb.Spec.setJoypad, or the board
; buttons in simulation: make sim SYSTEM=gb ROM=build/gb/asm/buttons.gb
; SIM_ARGS="--btn 0x09"), the screen is compared with a golden.

DEF rP1   EQU $FF00
DEF rLCDC EQU $FF40
DEF rLY   EQU $FF44
DEF rBGP  EQU $FF47

SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $150 - @, 0

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE
.waitVblank:                    ; switch the LCD off only in VBlank
    ldh a, [rLY]
    cp 144
    jr c, .waitVblank
    xor a
    ldh [rLCDC], a
    ; tile 0: white (already 0), tile 1: black
    ld hl, $8010
    ld b, 16
    ld a, $FF
.tile:
    ld [hli], a
    dec b
    jr nz, .tile
    ; clear the BG map at $9800
    ld hl, $9800
    ld bc, $400
.clear:
    xor a
    ld [hli], a
    dec bc
    ld a, b
    or c
    jr nz, .clear
    ld a, $E4
    ldh [rBGP], a
    ld a, $91                   ; LCD on, BG on, tiles at $8000, map at $9800
    ldh [rLCDC], a
.frame:
.wait144:
    ldh a, [rLY]
    cp 144
    jr nz, .wait144
    ld a, $20                   ; directions
    ldh [rP1], a
    ldh a, [rP1]
    ldh a, [rP1]
    and $0F
    ld e, a
    ld a, $10                   ; buttons
    ldh [rP1], a
    ldh a, [rP1]
    ldh a, [rP1]
    and $0F
    swap a
    or e
    cpl                         ; bit set = held: 0 Right ... 7 Start
    ld e, a
    ld hl, $9800 + 8 * 32 + 2
    ld b, 8
.square:
    srl e
    ld a, 0
    adc a, 0                    ; a = the bit shifted out
    ld [hli], a
    inc l
    dec b
    jr nz, .square
.wait145:
    ldh a, [rLY]
    cp 145
    jr nz, .wait145
    jr .frame
