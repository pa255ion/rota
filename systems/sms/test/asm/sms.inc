; Shared definitions for our own Master System regression ROMs (M6 step
; 6.0; CLAUDE.md rule 5). Assembled by `make sms-asm` (WLA-DX) into
; build/sms/asm/<name>.sms.
;
; One copy with the Game Gear's: this file sets SMS and includes gg.inc
; (systems/gg/test/asm), which then gives the header region 4 (Master
; System export) and the verdict's colours in SMS CRAM. The verdict
; convention is gg.inc's: RESULT (C000h) 01h on a pass, 80h + n on a fail
; of check n, then DI / HALT, with the verdict on the screen (green, or red
; with n squares); `passkeep` for a ROM whose picture or sound is the test.

.DEFINE SMS
.INCLUDE "../../../gg/test/asm/gg.inc"
