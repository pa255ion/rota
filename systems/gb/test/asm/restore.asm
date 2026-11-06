; Regression ROM for loading a state through the CPU (side track L,
; PLAN.md L.6c): what the restore routine has to line up, all moving at
; the moment of a save. The LCD switched off and on by the game (the PPU's
; dot phase 3: the routine must switch it too), OAM filled with a pattern
; (put back by OAM DMA), the PPU's registers set, the timer at its fastest
; (TAC 5: TIMA every 4 M-cycles) with its interrupt and VBlank's on, two
; sound channels playing. The main loop keeps DIV and TIMA in WRAM
; (0xC110, 0xC111) and counts (0xC100, HRAM 0xFF90); the handlers count
; their interrupts (0xC200 timer, 0xC201 VBlank). It never reads the sound
; unit, which the routine restores only approximately. No pass/fail.

SECTION "VBlank", ROM0[$40]
    jp VBlank

SECTION "Timer", ROM0[$50]
    jp Timer

SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $150 - @, 0

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE
    xor a
    ldh [$FF40], a              ; LCD off: OAM free
    ld hl, $FE00
    ld b, 160
.oam:
    ld a, l
    add a                       ; 3 × index + 7
    add l
    add 7
    ld [hl+], a
    dec b
    jr nz, .oam
    ld a, $E4
    ldh [$FF47], a              ; BGP
    ld a, $D2
    ldh [$FF48], a              ; OBP0
    ld a, $1B
    ldh [$FF49], a              ; OBP1
    ld a, 13
    ldh [$FF42], a              ; SCY
    ld a, 37
    ldh [$FF43], a              ; SCX
    ld a, 90
    ldh [$FF4A], a              ; WY
    ld a, 60
    ldh [$FF4B], a              ; WX
    ld a, 77
    ldh [$FF45], a              ; LYC
    ld a, $08
    ldh [$FF41], a              ; STAT: mode 0 interrupt source (IE doesn't take it)
    ; sound: on, both sides; CH2 and CH4 without length, CH1 with
    ld a, $80
    ldh [$FF26], a
    ld a, $77
    ldh [$FF24], a
    ld a, $FF
    ldh [$FF25], a
    ld a, $80 | 20
    ldh [$FF11], a
    ld a, $F3
    ldh [$FF12], a
    xor a
    ldh [$FF13], a
    ld a, $C7
    ldh [$FF14], a
    ld a, $40 | 10
    ldh [$FF16], a
    ld a, $A1
    ldh [$FF17], a
    ld a, $80
    ldh [$FF18], a
    ld a, $86
    ldh [$FF19], a
    ld a, $F0
    ldh [$FF21], a
    ld a, $31
    ldh [$FF22], a
    ld a, $80
    ldh [$FF23], a
    ; the timer at its fastest, its interrupt and VBlank's
    ld a, $40
    ldh [$FF06], a              ; TMA
    ld a, $05
    ldh [$FF07], a              ; TAC
    ld a, $05
    ldh [$FFFF], a              ; IE: VBlank, timer
    ld a, $93
    ldh [$FF40], a              ; LCD on, BG, OBJ
    ei
.loop:
    ld a, [$C100]
    inc a
    ld [$C100], a
    ldh a, [$FF04]
    ld [$C110], a
    ldh a, [$FF05]
    ld [$C111], a
    ldh a, [$FF90]
    dec a
    ldh [$FF90], a
    jr .loop

VBlank:
    push af
    ld a, [$C201]
    inc a
    ld [$C201], a
    pop af
    reti

Timer:
    push af
    ld a, [$C200]
    inc a
    ld [$C200], a
    pop af
    reti
