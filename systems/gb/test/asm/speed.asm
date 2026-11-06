; Regression ROM for the Game Boy Color's double speed (side track I,
; phase 3 step 3c; Pan Docs, "KEY1"): Blargg's interrupt_time is a devcart
; build whose verdict can't be read, so this checks what the spec and the
; core implement:
;
;   1. KEY1 reads $7E at start (normal speed, not armed), $7F once armed;
;   2. STOP switches: KEY1 reads $FE (double speed, the arm cleared);
;   3. the LCD keeps its speed: one frame (LY 144 to LY 144) takes twice
;      the CPU's loop passes in double speed as in normal speed (+/- 3);
;   4. STOP again: KEY1 reads $7E, a frame the same passes as at first
;      (+/- 3).
;
; Mooneye's convention: LD B,B with B, C, D, E, H, L = 3, 5, 8, 13, 21, 34
; passes; failing, all six = $42 and A = the failed check's number.
;
; Assembled by `make gb-asm` (title from the file name); the header byte
; 0x143 = $C0 (Color only) is set here, rgbfix only fixes the checksums.

DEF rLY   EQU $FF44
DEF rKEY1 EQU $FF4D
DEF rIE   EQU $FFFF
DEF rP1   EQU $FF00

SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $143 - @, 0
    db $C0                      ; CGB only
    ds $150 - @, 0

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE
    xor a
    ldh [rIE], a
    ld a, $30
    ldh [rP1], a

    ; 1. normal speed, not armed; armed
    ld a, 1
    ld [wTest], a
    ldh a, [rKEY1]
    cp $7E
    jp nz, Failed
    ld a, 1
    ldh [rKEY1], a
    ldh a, [rKEY1]
    cp $7F
    jp nz, Failed

    ; a frame at normal speed: hl = passes
    call Frame
    ld a, h
    ld [wN1], a
    ld a, l
    ld [wN1 + 1], a

    ; 2. the switch
    ld a, 2
    ld [wTest], a
    stop
    nop
    ldh a, [rKEY1]
    cp $FE
    jp nz, Failed

    ; 3. a frame in double speed: about twice the passes
    ld a, 3
    ld [wTest], a
    call Frame
    ld b, h
    ld c, l                     ; bc = N2
    ld a, [wN1]
    ld h, a
    ld a, [wN1 + 1]
    ld l, a
    add hl, hl                  ; hl = 2 N1
    call Near
    jp nz, Failed

    ; 4. back to normal speed
    ld a, 4
    ld [wTest], a
    ld a, 1
    ldh [rKEY1], a
    stop
    nop
    ldh a, [rKEY1]
    cp $7E
    jp nz, Failed
    call Frame
    ld b, h
    ld c, l
    ld a, [wN1]
    ld h, a
    ld a, [wN1 + 1]
    ld l, a
    call Near
    jp nz, Failed

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

; Z set if bc is within 3 of hl.
Near:
    ld a, l
    sub c
    ld e, a
    ld a, h
    sbc b
    ld d, a                     ; de = hl - bc
    bit 7, d
    jr z, .pos
    ld a, e                     ; negative: de = -de
    cpl
    ld e, a
    ld a, d
    cpl
    ld d, a
    inc de
.pos:
    ld a, d
    and a
    ret nz                      ; 256 or more off
    ld a, e
    cp 4
    jr c, .ok
    or 1                        ; Z clear
    ret
.ok:
    xor a                       ; Z set
    ret

; The CPU's loop passes from one LY = 144 to the next (a frame): hl.
Frame:
.sync:                          ; wait until LY is 144 ...
    ldh a, [rLY]
    cp 144
    jr nz, .sync
.leave:                         ; ... and has moved on
    ldh a, [rLY]
    cp 144
    jr z, .leave
    ld hl, 0
.count:                         ; 10 M-cycles a pass
    inc hl
    ldh a, [rLY]
    cp 144
    jr nz, .count
    ret

SECTION "Vars", WRAM0
wTest: ds 1
wN1:   ds 2
