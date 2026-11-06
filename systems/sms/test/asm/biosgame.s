; The cartridge for our stand-in BIOS (bios.s; M6 step 6.1). Run with
; bios.sms as the BIOS image; without one it fails check 2. Checks:
;  1 C000h held ABh at boot (the BIOS's copy of port 3Eh)
;  2 the BIOS ran (C0F0h 'B')
;  3 while it ran, the cartridge was off (C0F1h is the BIOS's 00h, not 'T')
;  4 the BIOS is off now: 0000h reads the cartridge (F3h, gg.inc's DI)

.INCLUDE "sms.inc"

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  ld a, (BOOT3E)
  EXPECT $AB, 1
  ld a, ($C0F0)
  EXPECT 'B', 2
  ld a, ($C0F1)
  EXPECT $00, 3
  ld a, ($0000)
  EXPECT $F3, 4
  jp pass
.ENDS
