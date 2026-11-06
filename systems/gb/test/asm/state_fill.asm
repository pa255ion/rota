; Regression ROM for save states (side track L, PLAN.md L.6): every
; memory a state holds gets a pattern of its own, so a state saved after
; the fill and loaded into a core that hasn't reached it shows any
; memory the load leaves out. LCD off at once (no wait: the Clash
; simulation is slow); 256 bytes at each end of VRAM (0x8000, 0x9F00:
; low byte XOR 0x5A) and of WRAM (0xC000, 0xDF00: low byte + high
; byte), HRAM 0xFF80-0xFFFD (low byte XOR 0xC3); then a loop counting in
; WRAM 0xC100 and HRAM 0xFF90, so the state keeps moving. No pass/fail.

SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $150 - @, 0

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE
    xor a
    ldh [$FF40], a              ; LCD off: VRAM free
    ld hl, $8000
    call FillVram
    ld hl, $9F00
    call FillVram
    ld hl, $C000
    call FillWram
    ld hl, $DF00
    call FillWram
    ld hl, $FF80
.hram:
    ld a, l
    xor $C3
    ld [hl+], a
    ld a, l
    cp $FE
    jr nz, .hram
.loop:
    ld a, [$C100]
    inc a
    ld [$C100], a
    ldh a, [$FF90]
    dec a
    ldh [$FF90], a
    jr .loop

; 256 bytes from HL (its low byte 0)
FillVram:
    ld a, l
    xor $5A
    ld [hl+], a
    ld a, l
    and a
    jr nz, FillVram
    ret

FillWram:
    ld a, l
    add h
    ld [hl+], a
    ld a, l
    and a
    jr nz, FillWram
    ret
