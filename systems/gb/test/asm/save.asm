; Regression ROM for battery saves (side track I, phase 1;
; docs/systems/gameboy.md): an MBC1+RAM+BATTERY cartridge with 8 KiB of
; RAM that counts its boots. At start it enables cartridge RAM, and if
; $A000 doesn't hold "SAVE" (a blank save) writes it with a count of 0;
; then it adds one to the 16-bit count at $A004 (little-endian), disables
; RAM again and shows the count as five decimal digits on row 8 of the
; screen, then loops. So with saves working, the number on the board goes
; up by one at every power-on, reset or reload of the cartridge.
;
; Layer 4 (make ci, ci-gb): systems/gb/test/ps/save.ps writes a save with
; count 41 while the core is held, releases it, and reads cartridge RAM
; back over the PS link: "SAVE", 42.
;
; Assembled by `make gb-asm` (rgbasm, rgblink, rgbfix) into
; build/gb/asm/save.gb; never hand-patched (CLAUDE.md rule 5).

DEF rLCDC EQU $FF40
DEF rLY   EQU $FF44
DEF rBGP  EQU $FF47

DEF COUNT_ROW EQU 8
DEF COUNT_COL EQU 7

SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $147 - @, 0              ; logo, title: rgbfix
    db $03                      ; MBC1+RAM+BATTERY
    db $00                      ; 32 KiB ROM
    db $02                      ; 8 KiB RAM
    ds $150 - @, 0              ; checksums: rgbfix

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE
    ld a, $0A                   ; cartridge RAM on
    ld [$0000], a
    ld hl, $A000
    ld de, Magic
    ld b, 4
.check:
    ld a, [de]
    cp [hl]
    jr nz, .blank
    inc de
    inc hl
    dec b
    jr nz, .check
    jr .count
.blank:                         ; no save yet: the magic, a count of 0
    ld hl, $A000
    ld de, Magic
    ld b, 4
.magic:
    ld a, [de]
    ld [hli], a
    inc de
    dec b
    jr nz, .magic
    xor a
    ld [hli], a
    ld [hl], a
.count:
    ld hl, $A004
    ld a, [hl]
    add a, 1
    ld [hli], a
    ld c, a
    ld a, [hl]
    adc a, 0
    ld [hl], a
    ld b, a                     ; bc = the new count
    xor a                       ; cartridge RAM off
    ld [$0000], a

.waitVblank:                    ; switch the LCD off only in VBlank
    ldh a, [rLY]
    cp 144
    jr c, .waitVblank
    xor a
    ldh [rLCDC], a
    ; tiles 1 to 10: the digits 0 to 9 (tile 0 stays white)
    ld hl, $8010
    ld de, Digits
    ld a, 80
.font:
    push af
    ld a, [de]
    inc de
    ld [hli], a
    ld [hli], a
    pop af
    dec a
    jr nz, .font
    ; clear the BG map at $9800
    ld hl, $9800
    push bc
    ld bc, $400
.clear:
    xor a
    ld [hli], a
    dec bc
    ld a, b
    or c
    jr nz, .clear
    pop bc
    ; the count in decimal: subtract each power of ten as often as it goes
    ld hl, $9800 + COUNT_ROW * 32 + COUNT_COL
    ld de, 10000
    call Digit
    ld de, 1000
    call Digit
    ld de, 100
    call Digit
    ld de, 10
    call Digit
    ld a, c
    inc a
    ld [hl], a
    ld a, $E4
    ldh [rBGP], a
    ld a, $91                   ; LCD on, BG on, tiles at $8000, map at $9800
    ldh [rLCDC], a
.forever:
    jr .forever

; One digit of bc for the power of ten de: its tile at [hl+], bc reduced.
Digit:
    ld a, 1                     ; tile of "0"
.sub:
    push af
    ld a, c
    sub e
    ld c, a
    ld a, b
    sbc d
    ld b, a
    jr c, .under
    pop af
    inc a
    jr .sub
.under:                         ; one too many: add it back
    ld a, c
    add e
    ld c, a
    ld a, b
    adc d
    ld b, a
    pop af
    ld [hli], a
    ret

Magic:
    db "SAVE"

; 8 x 8, one bit per pixel (each row twice: both bit planes, colour 3)
Digits:
    db $3C, $66, $6E, $76, $66, $66, $3C, $00
    db $18, $38, $18, $18, $18, $18, $7E, $00
    db $3C, $66, $06, $0C, $30, $60, $7E, $00
    db $3C, $66, $06, $1C, $06, $66, $3C, $00
    db $0C, $1C, $3C, $6C, $7E, $0C, $0C, $00
    db $7E, $60, $7C, $06, $06, $66, $3C, $00
    db $3C, $66, $60, $7C, $66, $66, $3C, $00
    db $7E, $06, $0C, $18, $30, $30, $30, $00
    db $3C, $66, $66, $3C, $66, $66, $3C, $00
    db $3C, $66, $66, $3E, $06, $66, $3C, $00
