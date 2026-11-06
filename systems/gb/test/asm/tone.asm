; Regression ROM for sound (M2 step 2.9): a steady square wave on CH1,
; 50 % duty, full volume, no envelope, both sides; period value 1750, so
; 131072 / (2048 - 1750) = 439.8 Hz (Pan Docs, "FF13 - NR13"). The tests
; measure the frequency of the mixed output (spec) and of the S/PDIF stream
; from the generated Verilog (layer 4).

DEF rNR11 EQU $FF11
DEF rNR12 EQU $FF12
DEF rNR13 EQU $FF13
DEF rNR14 EQU $FF14
DEF rNR50 EQU $FF24
DEF rNR51 EQU $FF25
DEF rNR52 EQU $FF26

SECTION "Header", ROM0[$100]
    nop
    jp Start
IF DEF(DOUBLE_SPEED)
    ds $143 - @, 0
    db $C0                      ; CGB only (tone_ds.asm)
ENDC
    ds $150 - @, 0

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE
IF DEF(DOUBLE_SPEED)
    ; tone_ds.asm: the same in the Game Boy Color's double speed, as
    ; sound_all.asm (the tone keeps its pitch; the core does twice the
    ; M-cycles per dot: side track L's speed limit, PLAN.md L.4)
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
    ldh [rNR52], a              ; APU on
    ld a, $77
    ldh [rNR50], a              ; full master volume
    ld a, $11
    ldh [rNR51], a              ; CH1 left and right
    ld a, $80
    ldh [rNR11], a              ; 50 % duty
    ld a, $F0
    ldh [rNR12], a              ; volume 15, no envelope
    ld a, $D6
    ldh [rNR13], a              ; period $6D6 = 1750
    ld a, $86
    ldh [rNR14], a              ; trigger, no length timer
.forever:
    jr .forever
