; Our own stand-in for the Master System's BIOS (M6 step 6.1; never
; Sega's, which is never fetched or committed): what the tests use where
; the board takes the user's own dump. Assembled into build/sms/asm/
; bios.sms (16 KiB, no header), run as the BIOS image with biosgame.sms
; as the cartridge.
;
; As the real one (MacDonald, "BIOS information"): it starts with the BIOS
; on and the cartridge off, copies its hand-over code to RAM, and from
; there writes port 3Eh's cartridge value (ABh), keeps a copy at C000h,
; and jumps to the cartridge at 0000h. On the way it leaves, for the
; cartridge's checks: C0F0h 'B' (it ran), C0F1h what 7FF0h read while the
; cartridge was off (its own byte, 00h: not "TMR SEGA"'s 'T').

.MEMORYMAP
SLOTSIZE $4000
DEFAULTSLOT 0
SLOT 0 $0000
.ENDME

.ROMBANKMAP
BANKSTOTAL 1
BANKSIZE $4000
BANKS 1
.ENDRO

.EMPTYFILL $00

.BANK 0 SLOT 0
.ORGA $0000
.SECTION "bios" FORCE
  di
  im 1
  ld sp, $DFF0
  ld a, ($7FF0)           ; the cartridge is off: our own byte
  ld ($C0F1), a
  ld a, 'B'
  ld ($C0F0), a
  ld hl, handover
  ld de, $C100
  ld bc, handover_end - handover
  ldir
  jp $C100
handover:
  ld a, $AB
  ld ($C000), a
  out ($3E), a            ; the cartridge on, the BIOS off
  jp $0000
handover_end:
.ENDS
