; Regression ROM for side track O (O.1, frame-synchronous input): with the
; direction keys selected, polls P1 and, whenever its low nibble changes
; (and once at the start), sends two bytes over the serial port: the
; nibble, then LY (the line the change was seen on). The harness collects
; them on the byte channel. With the movie register's sync bit set, a key
; pressed mid-frame reaches the core at its frame start (Gb.Machine:
; the core's frame-done pulse), seen at LY 144 ($90); without it, at the
; line where it was pressed.

DEF rP1 EQU $FF00
DEF rSB EQU $FF01
DEF rSC EQU $FF02
DEF rLY EQU $FF44

SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $150 - @, 0

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE
    ld a, $20                   ; P15 high, P14 low: the directions
    ldh [rP1], a
    ld c, $FF                   ; no nibble yet: the first one is sent
Loop:
    ldh a, [rP1]
    ldh a, [rP1]                ; (read twice: the lines settle)
    and $0F
    cp c
    jr z, Loop
    ld c, a
    ldh a, [rLY]
    ld b, a
    ld a, c
    call Send
    ld a, b
    call Send
    jr Loop

; A over the serial port (internal clock), waiting for the transfer's end
Send:
    ldh [rSB], a
    ld a, $81
    ldh [rSC], a
.wait:
    ldh a, [rSC]
    bit 7, a
    jr nz, .wait
    ret
