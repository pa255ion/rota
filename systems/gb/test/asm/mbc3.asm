; Regression ROM for MBC3 (side track I, phase 2; docs/systems/gameboy.md):
; the pinned Mooneye release has no MBC3 tests, so this one checks what
; the spec ("Gb.Spec.Machine") and the core implement from Pan Docs,
; "MBC3":
;
;   1. ROM banks 1 to 7 at 0x4000 (each holds its number), bank 0 -> 1;
;   2. RAM banks 0 to 3 keep their own bytes;
;   3. the clock: registers written and latched (0 then 1 to 0x6000) read
;      back as written; latched values stay until the next latch;
;   4. 23:59:59 on day 511 + one second = 00:00:00, day 0, day carry set;
;   5. the halt bit stops it;
;   6. a seconds value above 59 (63) counts to 0 without carrying into
;      the minutes.
;
; Each wait is about 1.3 s of machine time (a second is 2^20 M-cycles);
; writing the seconds restarts the second, so the waits are exact.
; Mooneye's convention: LD B,B with B, C, D, E, H, L = 3, 5, 8, 13, 21, 34
; passes; failing, the same with all six = 0x42 and A = the test number.
;
; Assembled by `make gb-asm` into build/gb/asm/mbc3.gb; never hand-patched
; (CLAUDE.md rule 5).

SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $147 - @, 0
    db $10                      ; MBC3+TIMER+RAM+BATTERY
    db $02                      ; 128 KiB ROM: 8 banks
    db $03                      ; 32 KiB RAM: 4 banks
    ds $150 - @, 0

SECTION "Main", ROM0[$150]
Start:
    di
    ld sp, $FFFE

    ; 1. ROM banks
    ld a, 1
    ld [wTest], a
    ld b, 1
.rom:
    ld a, b
    ld [$2000], a
    ld a, [$4000]
    cp b
    jp nz, Failed
    inc b
    ld a, b
    cp 8
    jr nz, .rom
    xor a                       ; bank 0 selects bank 1
    ld [$2000], a
    ld a, [$4000]
    cp 1
    jp nz, Failed

    ; 2. RAM banks
    ld a, 2
    ld [wTest], a
    ld a, $0A
    ld [$0000], a
    ld b, 0
.ramw:
    ld a, b
    ld [$4000], a
    add a, $10
    ld [$A000], a
    inc b
    ld a, b
    cp 4
    jr nz, .ramw
    ld b, 0
.ramr:
    ld a, b
    ld [$4000], a
    add a, $10
    ld c, a
    ld a, [$A000]
    cp c
    jp nz, Failed
    inc b
    ld a, b
    cp 4
    jr nz, .ramr

    ; 3. clock registers: written, latched, read back
    ld a, 3
    ld [wTest], a
    ld hl, Set1
    call SetClock
    call Latch
    ld hl, Set1
    call CheckClock
    call Wait                   ; the latched values don't move
    ld hl, Set1
    call CheckClock

    ; 4. one second later: everything rolls over, day carry
    ld a, 4
    ld [wTest], a
    call Latch
    ld hl, Want4
    call CheckClock

    ; 5. halted: nothing moves
    ld a, 5
    ld [wTest], a
    ld hl, Set5
    call SetClock
    call Wait
    call Latch
    ld hl, Set5
    call CheckClock

    ; 6. seconds = 63: wraps to 0, the minutes stay
    ld a, 6
    ld [wTest], a
    ld hl, Set6
    call SetClock
    call Wait
    call Latch
    ld hl, Want6
    call CheckClock

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

; The clock's five registers from [hl] (DH first, so a halt bit set or
; cleared covers the rest), seconds last (it restarts the second).
SetClock:
    ld a, $0C
    ld [$4000], a
    ld a, [hl+]                 ; S, M, H, DL, DH in memory order
    push af
    ld a, [hl+]
    push af
    ld a, [hl+]
    push af
    ld a, [hl+]
    push af
    ld a, [hl]                  ; DH
    ld [$A000], a
    ld a, $0B
    ld [$4000], a
    pop af
    ld [$A000], a
    ld a, $0A
    ld [$4000], a
    pop af
    ld [$A000], a
    ld a, $09
    ld [$4000], a
    pop af
    ld [$A000], a
    ld a, $08
    ld [$4000], a
    pop af
    ld [$A000], a
    ret

Latch:
    xor a
    ld [$6000], a
    inc a
    ld [$6000], a
    ret

; The five latched registers against [hl]: S, M, H, DL, DH.
CheckClock:
    ld b, $08
.next:
    ld a, b
    ld [$4000], a
    ld a, [$A000]
    cp [hl]
    jp nz, Failed
    inc hl
    inc b
    ld a, b
    cp $0D
    jr nz, .next
    ret

; About 1.3 s: 3 x 65536 passes of 7 M-cycles (dec de 2, ld a, d 1,
; or e 1, jr nz 3) = 1376256 M-cycles, between 1 and 2 seconds.
Wait:
    ld c, 3
.outer:
    ld de, 0
.inner:
    dec de
    ld a, d
    or e
    jr nz, .inner
    dec c
    jr nz, .outer
    ret

Set1:  db 59, 59, 23, $FF, $01  ; 23:59:59, day 511
Want4: db 0, 0, 0, $00, $80     ; 00:00:00, day 0, carry
Set5:  db 10, 7, 3, $21, $40    ; halted
Set6:  db 63, 5, 1, $02, $00    ; running
Want6: db 0, 5, 1, $02, $00

SECTION "Vars", WRAM0
wTest: ds 1

SECTION "Bank1", ROMX[$4000], BANK[1]
    db 1
SECTION "Bank2", ROMX[$4000], BANK[2]
    db 2
SECTION "Bank3", ROMX[$4000], BANK[3]
    db 3
SECTION "Bank4", ROMX[$4000], BANK[4]
    db 4
SECTION "Bank5", ROMX[$4000], BANK[5]
    db 5
SECTION "Bank6", ROMX[$4000], BANK[6]
    db 6
SECTION "Bank7", ROMX[$4000], BANK[7]
    db 7
