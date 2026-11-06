; Regression ROM for the speed limit in the Game Boy Color's double speed
; (side track L, PLAN.md L.4): tone.asm after a speed switch, so the core
; does twice the M-cycles per dot. The tone stays 439.8 Hz at 100 %; the
; layer 4 runs measure it at the menu's speeds.

DEF DOUBLE_SPEED EQU 1
INCLUDE "tone.asm"
