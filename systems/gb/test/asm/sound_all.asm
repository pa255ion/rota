; Regression ROM for sound (M2 step 2.9): exercises every channel feature
; in turn so that the lockstep tests (spec APU against the Clash APU)
; compare all of them: CH1 with sweep up and down and envelopes, CH2 with
; each duty, CH3 with a wave pattern at each output level, CH4 in 15- and
; 7-bit mode, panning (NR51) and master volume (NR50) changes, length
; timers, then APU power off and on. Each phase lasts a few frames
; (waits on LY). It loops forever afterwards; there is no pass/fail.

DEF rLY   EQU $FF44
DEF rNR10 EQU $FF10
DEF rNR11 EQU $FF11
DEF rNR12 EQU $FF12
DEF rNR13 EQU $FF13
DEF rNR14 EQU $FF14
DEF rNR21 EQU $FF16
DEF rNR22 EQU $FF17
DEF rNR23 EQU $FF18
DEF rNR24 EQU $FF19
DEF rNR30 EQU $FF1A
DEF rNR31 EQU $FF1B
DEF rNR32 EQU $FF1C
DEF rNR33 EQU $FF1D
DEF rNR34 EQU $FF1E
DEF rNR41 EQU $FF20
DEF rNR42 EQU $FF21
DEF rNR43 EQU $FF22
DEF rNR44 EQU $FF23
DEF rNR50 EQU $FF24
DEF rNR51 EQU $FF25
DEF rNR52 EQU $FF26

SECTION "Header", ROM0[$100]
    nop
    jp Start
IF DEF(DOUBLE_SPEED)
    ds $143 - @, 0
    db $C0                      ; CGB only (sound_ds.asm)
ENDC
    ds $150 - @, 0

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE
IF DEF(DOUBLE_SPEED)
    ; sound_ds.asm: the same in the Game Boy Color's double speed
    xor a
    ldh [$FFFF], a              ; IE
    ld a, $30
    ldh [$FF00], a              ; P1: no buttons (STOP)
    ld a, 1
    ldh [$FF4D], a              ; KEY1: arm the switch
    stop
    nop
ENDC
    ld a, $80
    ldh [rNR52], a
    ld a, $77
    ldh [rNR50], a
    ld a, $FF
    ldh [rNR51], a

    ; CH1: sweep up, envelope down
    ld a, $16                   ; pace 1, up, step 6
    ldh [rNR10], a
    ld a, $80
    ldh [rNR11], a
    ld a, $F3
    ldh [rNR12], a
    ld a, $00
    ldh [rNR13], a
    ld a, $85
    ldh [rNR14], a
    call Wait3
    ; CH1: sweep down, envelope up, length timer on
    ld a, $2D                   ; pace 2, down, step 5
    ldh [rNR10], a
    ld a, $D0                   ; 75 % duty, length 16
    ldh [rNR11], a
    ld a, $1A                   ; volume 1, up, pace 2
    ldh [rNR12], a
    ld a, $C7
    ldh [rNR14], a              ; trigger, length on
    call Wait3

    ; CH2: each duty
    ld a, $F0
    ldh [rNR22], a
    ld a, $40
    ldh [rNR23], a
    ld b, 4
    ld c, $00
.duty:
    ld a, c
    ldh [rNR21], a
    ld a, $87
    ldh [rNR24], a
    call Wait1
    ld a, c
    add $40
    ld c, a
    dec b
    jr nz, .duty

    ; CH3: a wave pattern, each output level
    xor a
    ldh [rNR30], a              ; DAC off while writing wave RAM
    ld hl, $FF30
    ld b, 16
    ld a, $0F
.wave:
    ld [hli], a
    add $11
    dec b
    jr nz, .wave
    ld a, $80
    ldh [rNR30], a
    ld a, $00
    ldh [rNR33], a
    ld b, 3
    ld c, $20
.level:
    ld a, c
    ldh [rNR32], a
    ld a, $86
    ldh [rNR34], a
    call Wait1
    ld a, c
    add $20
    ld c, a
    dec b
    jr nz, .level

    ; CH4: 15-bit, then 7-bit
    ld a, $F1
    ldh [rNR42], a
    ld a, $21
    ldh [rNR43], a
    ld a, $80
    ldh [rNR44], a
    call Wait1
    ld a, $39                   ; 7-bit
    ldh [rNR43], a
    ld a, $80
    ldh [rNR44], a
    call Wait1

    ; panning and master volume
    ld a, $5A
    ldh [rNR51], a
    ld a, $35
    ldh [rNR50], a
    call Wait1

    ; power off and on
    xor a
    ldh [rNR52], a
    call Wait1
    ld a, $80
    ldh [rNR52], a
.forever:
    jr .forever

; Wait for 1 or 3 frames (LY == 144 edges).
Wait3:
    call Wait1
    call Wait1
Wait1:
.not144:
    ldh a, [rLY]
    cp 144
    jr nz, .not144
.is144:
    ldh a, [rLY]
    cp 144
    jr z, .is144
    ret
