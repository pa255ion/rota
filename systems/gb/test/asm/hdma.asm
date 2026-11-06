; Regression ROM for the Game Boy Color's VRAM DMA (side track I, phase 3;
; Pan Docs, "LCD VRAM DMA transfers"). No public test ROM pins it down, so
; this checks what the spec and the core implement:
;
;   1. HDMA5 reads $FF at start (no transfer);
;   2. general purpose DMA, LCD off: 2 blocks from WRAM to VRAM bank 0,
;      HDMA5 $FF after, the 32 bytes there and the byte after untouched;
;   3. the same into bank 1 (VBK = 1) from ROM: bank 0 untouched;
;   4. HBlank DMA, LCD on: 3 blocks; HDMA5 reads bit 7 clear while it
;      runs and $FF once done (within 8 lines); the 48 bytes there;
;   5. HBlank DMA stopped by a write with bit 7 clear: HDMA5 then reads
;      bit 7 set with blocks left, and stays so;
;   6. general purpose DMA's time: 16 blocks take 15 x 8 M-cycles more
;      than 1 block (TIMA at 4 M-cycles a count: 30, +/- 1);
;   7. in double speed 15 x 16 M-cycles (60, +/- 1): the same time.
;
; Mooneye's convention: LD B,B with B, C, D, E, H, L = 3, 5, 8, 13, 21, 34
; passes; failing, all six = $42 and A = the failed check's number.
;
; Assembled by `make gb-asm`; header byte 0x143 = $C0 (Color only).

DEF rDIV   EQU $FF04
DEF rTIMA  EQU $FF05
DEF rTAC   EQU $FF07
DEF rLCDC  EQU $FF40
DEF rLY    EQU $FF44
DEF rKEY1  EQU $FF4D
DEF rVBK   EQU $FF4F
DEF rHDMA1 EQU $FF51
DEF rHDMA2 EQU $FF52
DEF rHDMA3 EQU $FF53
DEF rHDMA4 EQU $FF54
DEF rHDMA5 EQU $FF55
DEF rIE    EQU $FFFF
DEF rP1    EQU $FF00

SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $143 - @, 0
    db $C0                      ; CGB only
    ds $150 - @, 0

SECTION "Table", ROM0[$200]
Table:
    db $11, $22, $33, $44, $55, $66, $77, $88, $99, $AA, $BB, $CC, $DD, $EE, $FF, $01

SECTION "Main", ROM0[$300]
Start:
    di
    ld sp, $FFFE
    xor a
    ldh [rIE], a
    ld a, $30
    ldh [rP1], a

    ; 1. nothing running
    ld a, 1
    ld [wTest], a
    ldh a, [rHDMA5]
    cp $FF
    jp nz, Failed

    ; the source: $C000-$C0FF = i xor $5A
    ld hl, $C000
.fill:
    ld a, l
    xor $5A
    ld [hl+], a
    ld a, l
    and a
    jr nz, .fill

    ; 2. general purpose DMA, LCD off: $C000 -> $8000, 2 blocks
    ld a, 2
    ld [wTest], a
    call LcdOff
    xor a
    ld [$8020], a
    ld a, $C0
    ldh [rHDMA1], a
    xor a
    ldh [rHDMA2], a
    ld a, $80
    ldh [rHDMA3], a
    xor a
    ldh [rHDMA4], a
    ld a, $01                   ; 2 blocks
    ldh [rHDMA5], a
    ldh a, [rHDMA5]
    cp $FF
    jp nz, Failed
    ld hl, $8000
    ld b, 32
    call CheckPattern
    jp nz, Failed
    ld a, [$8020]
    and a
    jp nz, Failed

    ; 3. into bank 1, from ROM: Table -> $9000
    ld a, 3
    ld [wTest], a
    xor a
    ld [$9000], a               ; bank 0
    ld a, 1
    ldh [rVBK], a
    ld a, HIGH(Table)
    ldh [rHDMA1], a
    ld a, LOW(Table)
    ldh [rHDMA2], a
    ld a, $10
    ldh [rHDMA3], a
    xor a
    ldh [rHDMA4], a
    ldh [rHDMA5], a             ; 1 block
    ld hl, $9000
    ld de, Table
    ld b, 16
.t3:
    ld a, [de]
    cp [hl]
    jp nz, Failed
    inc de
    inc hl
    dec b
    jr nz, .t3
    xor a
    ldh [rVBK], a
    ld a, [$9000]
    and a
    jp nz, Failed

    ; 4. HBlank DMA, LCD on: $C000 -> $8100, 3 blocks
    ld a, 4
    ld [wTest], a
    ld a, $91
    ldh [rLCDC], a
    ld a, $C0
    ldh [rHDMA1], a
    xor a
    ldh [rHDMA2], a
    ld a, $81
    ldh [rHDMA3], a
    xor a
    ldh [rHDMA4], a
    ld a, $82                   ; 3 blocks, HBlank
    ldh [rHDMA5], a
    ldh a, [rHDMA5]
    bit 7, a
    jp nz, Failed
    ld c, 8                     ; done within 8 lines
.t4:
    ldh a, [rHDMA5]
    cp $FF
    jr z, .t4done
    call NextLine
    dec c
    jr nz, .t4
    jp Failed
.t4done:
    call LcdOff
    ld hl, $8100
    ld b, 48
    call CheckPattern
    jp nz, Failed

    ; 5. HBlank DMA stopped: $C000 -> $8800, 128 blocks
    ld a, 5
    ld [wTest], a
    ld a, $91
    ldh [rLCDC], a
    ld a, $C0
    ldh [rHDMA1], a
    xor a
    ldh [rHDMA2], a
    ld a, $88
    ldh [rHDMA3], a
    xor a
    ldh [rHDMA4], a
    ld a, $FF
    ldh [rHDMA5], a
    call NextLine
    call NextLine
    call NextLine
    xor a
    ldh [rHDMA5], a             ; stop
    ldh a, [rHDMA5]
    ld c, a
    bit 7, a
    jp z, Failed
    cp $FF
    jp z, Failed
    cp $FE                      ; at least one block ran
    jp nc, Failed
    call NextLine
    call NextLine
    ldh a, [rHDMA5]
    cp c
    jp nz, Failed
    call LcdOff

    ; 6. the time of general purpose DMA, normal speed
    ld a, 6
    ld [wTest], a
    call Measure
    ld a, e
    sub d                       ; 16 blocks - 1 block
    sub 29
    cp 3
    jp nc, Failed

    ; 7. double speed
    ld a, 7
    ld [wTest], a
    ld a, 1
    ldh [rKEY1], a
    stop
    nop
    call Measure
    ld a, e
    sub d
    sub 59
    cp 3
    jp nc, Failed

    ld b, 3
    ld c, 5
    ld d, 8
    ld e, 13
    ld h, 21
    ld l, 34
    ld b, b
.done:
    jr .done

Failed:
    ld a, [wTest]
    ld b, $42
    ld c, b
    ld d, b
    ld e, b
    ld h, b
    ld l, b
    ld b, b
.done:
    jr .done

; TIMA counts (4 M-cycles each) of a general purpose DMA of 1 block (d)
; and of 16 blocks (e), $C000 -> $8000, LCD off.
Measure:
    ld a, $05                   ; TIMA on, every 4 M-cycles
    ldh [rTAC], a
    ld a, $00
    call OneDma
    ld d, a
    ld a, $0F
    call OneDma
    ld e, a
    xor a
    ldh [rTAC], a
    ret

; a: HDMA5's length; out a: TIMA after the DMA, from 0 with DIV reset
; just before.
OneDma:
    ld b, a
    ld a, $C0
    ldh [rHDMA1], a
    xor a
    ldh [rHDMA2], a
    ld a, $80
    ldh [rHDMA3], a
    xor a
    ldh [rHDMA4], a
    ldh [rDIV], a
    ldh [rTIMA], a
    ld a, b
    ldh [rHDMA5], a
    ldh a, [rTIMA]
    ret

; Z set if b bytes at hl are i xor $5A (i = the low byte of the offset).
CheckPattern:
    ld c, 0
.loop:
    ld a, c
    xor $5A
    cp [hl]
    ret nz
    inc hl
    inc c
    dec b
    jr nz, .loop
    xor a
    ret

; Wait for VBlank, then turn the LCD off.
LcdOff:
    ldh a, [rLCDC]
    bit 7, a
    jr z, .off
.wait:
    ldh a, [rLY]
    cp 144
    jr nz, .wait
    xor a
    ldh [rLCDC], a
.off:
    ret

; Wait until LY changes.
NextLine:
    ldh a, [rLY]
    ld b, a
.wait:
    ldh a, [rLY]
    cp b
    jr z, .wait
    ret

SECTION "Vars", WRAM0[$C800]
wTest: ds 1
