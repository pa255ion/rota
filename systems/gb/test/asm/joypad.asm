; Regression ROM for the joypad (M2 step 2.6): reads P1 with the direction
; keys selected, the buttons selected, both and neither, then clears IF and
; waits for the joypad interrupt request (IF bit 4), which needs an input
; line to go low. Results, then LD B,B and a loop:
;   B = P1 (directions), C = P1 (buttons), D = P1 (both), E = P1 (neither),
;   H = IF & $10 after the wait.
; The tests hold keys from outside (Gb.Spec.setJoypad / the core's kiKeys)
; and check the values against Pan Docs ("Joypad Input").

DEF rP1 EQU $FF00
DEF rIF EQU $FF0F

SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $150 - @, 0

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE
    ld a, $20                   ; P15 high, P14 low: directions
    ldh [rP1], a
    ldh a, [rP1]
    ld b, a
    ld a, $10                   ; P14 high, P15 low: buttons
    ldh [rP1], a
    ldh a, [rP1]
    ld c, a
    xor a                       ; both
    ldh [rP1], a
    ldh a, [rP1]
    ld d, a
    ld a, $30                   ; neither
    ldh [rP1], a
    ldh a, [rP1]
    ld e, a
    ld a, $10                   ; buttons, for the interrupt
    ldh [rP1], a
    xor a
    ldh [rIF], a
.wait:
    ldh a, [rIF]
    and $10
    jr z, .wait
    ld h, a
    ld b, b
.forever:
    jr .forever
