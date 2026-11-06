; Regression ROM for the HALT bug (M2 step 2.1), the two cases Pan Docs
; describes ("halt", "halt bug"), with Mooneye's result convention: B, C,
; D, E, H, L = 3, 5, 8, 13, 21, 34 at LD B,B for a pass, all $42 for a
; failure.
;
; 1. IME = 0 and an interrupt pending: HALT doesn't halt and PC fails to
;    advance, so the byte after it runs twice.
; 2. EI right before HALT, an interrupt pending: the interrupt is taken,
;    but returns to the HALT, which then waits for the next interrupt; so
;    the handler runs twice before the code after HALT.

DEF rTIMA EQU $FF05
DEF rTMA  EQU $FF06
DEF rTAC  EQU $FF07
DEF rIF   EQU $FF0F
DEF rIE   EQU $FFFF

SECTION "Timer interrupt", ROM0[$50]
    inc c
    reti

SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $150 - @, 0

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE
    xor a
    ldh [rTAC], a
    ld a, $04                   ; timer interrupt enabled and pending
    ldh [rIE], a
    ldh [rIF], a

    ; case 1
    ld b, 0
    halt
    inc b                       ; runs twice
    ld a, b
    cp 2
    jp nz, Failure

    ; case 2: the timer overflows every 16 increments of 4 M-cycles
    ld a, $F0
    ldh [rTMA], a
    ldh [rTIMA], a
    ld a, $04
    ldh [rIF], a                ; still pending
    ld c, 0
    ld a, $05                   ; timer on, 262144 Hz
    ldh [rTAC], a
    ei
    halt                        ; handler, back here, halt, handler
    di
    ld a, c
    cp 2
    jp nz, Failure

    ld b, 3
    ld c, 5
    ld d, 8
    ld e, 13
    ld h, 21
    ld l, 34
    ld b, b
.pass:
    jr .pass

Failure:
    ld a, $42
    ld b, a
    ld c, a
    ld d, a
    ld e, a
    ld h, a
    ld l, a
    ld b, b
.fail:
    jr .fail
