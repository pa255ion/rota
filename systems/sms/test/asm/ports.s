; The Master System II's ports (M6 step 6.1; MacDonald's SMS/GG hardware
; notes, docs/systems/mastersystem.md). Run without a BIOS: the machine
; starts as the BIOS leaves it. Checks:
;  1 C000h held ABh at boot (the BIOS's copy of port 3Eh)
;  2 port 00h reads FFh (the SMS 2: 00h-3Fh read FFh; no Game Gear registers)
;  3 port 3Eh reads FFh
;  4 port DCh, nothing pressed: FFh (pad 1, pad 2's up and down)
;  5 port DDh, nothing pressed: FFh (pad 2, no Reset button, TH high)
;  6 port C0h = DCh, 7 port C1h = DDh, 8 port FFh = DDh (even/odd decode)
;  9, 10 export: TH-A and TH-B as outputs read back as written (high, low)
; 11 the I/O chip off (3Eh bit 2): port DDh reads FFh even with TH low
; 12 the cartridge off (3Eh bit 6, from code in RAM): 0000h-BFFFh read FFh
; 13 and back on: the cartridge again
; (Whether an FM unit answers F2h is fm.s's check: the machines differ.)

.INCLUDE "sms.inc"

.DEFINE ROMOFF $C080
.DEFINE ROMON  $C083

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  ld a, (BOOT3E)
  EXPECT $AB, 1
  in a, ($00)
  EXPECT $FF, 2
  in a, ($3E)
  EXPECT $FF, 3
  in a, ($DC)
  EXPECT $FF, 4
  in a, ($DD)
  EXPECT $FF, 5
  in a, ($C0)
  EXPECT $FF, 6
  in a, ($C1)
  EXPECT $FF, 7
  in a, ($FF)
  EXPECT $FF, 8
  ld a, %11110101         ; TH-A, TH-B outputs (bits 1, 3 clear), high (5, 7)
  out ($3F), a
  in a, ($DD)
  and $C0
  EXPECT $C0, 9
  ld a, %01010101         ; and low
  out ($3F), a
  in a, ($DD)
  and $C0
  EXPECT $00, 10
  ld a, $AB | $04         ; the I/O chip off
  out ($3E), a
  in a, ($DD)
  EXPECT $FF, 11
  ld a, $AB
  out ($3E), a
  ld a, $FF               ; TH lines back to inputs
  out ($3F), a
  ld hl, ramcode          ; the cartridge test runs from RAM
  ld de, $C100
  ld bc, ramcode_end - ramcode
  ldir
  call $C100
  ld a, (ROMOFF)
  EXPECT $FF, 12
  ld a, (ROMOFF + 1)
  EXPECT $FF, 12
  ld a, (ROMOFF + 2)
  EXPECT $FF, 12
  ld a, (ROMON)
  EXPECT 'T', 13
  jp pass

; From RAM: the cartridge off, read 0000h, 7FF0h and BFFFh, the cartridge
; back on, read 7FF0h ("TMR SEGA").
ramcode:
  ld a, $AB | $40
  out ($3E), a
  ld a, ($0000)
  ld (ROMOFF), a
  ld a, ($7FF0)
  ld (ROMOFF + 1), a
  ld a, ($BFFF)
  ld (ROMOFF + 2), a
  ld a, $AB
  out ($3E), a
  ld a, ($7FF0)
  ld (ROMON), a
  ret
ramcode_end:
.ENDS
