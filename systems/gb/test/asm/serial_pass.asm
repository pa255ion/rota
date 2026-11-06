; Regression ROM for the test hooks (M2 step 2.0; docs/systems/gameboy.md):
; prints "Passed\n" over the serial port the way Blargg's ROMs do (byte in
; SB, then 0x81 to SC: start, internal clock), waiting for each transfer
; to finish, then signals a Mooneye pass (B, C, D, E, H, L = 3, 5, 8, 13,
; 21, 34) at the LD B,B breakpoint and loops forever.
;
; Assembled by `make gb-asm` (rgbasm, rgblink, rgbfix) into
; build/gb/asm/serial_pass.gb; never hand-patched (CLAUDE.md rule 5).

DEF rSB EQU $FF01
DEF rSC EQU $FF02

SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $150 - @, 0              ; logo, title and checksums: rgbfix

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE
    ld hl, Message
.next:
    ld a, [hli]
    and a
    jr z, .done
    ldh [rSB], a
    ld a, $81
    ldh [rSC], a
.wait:                          ; SC bit 7 clears when the byte is out
    ldh a, [rSC]
    bit 7, a
    jr nz, .wait
    jr .next
.done:
    ld b, 3
    ld c, 5
    ld d, 8
    ld e, 13
    ld h, 21
    ld l, 34
    ld b, b                     ; Mooneye breakpoint
.forever:
    jr .forever

Message:
    db "Passed\n", 0
