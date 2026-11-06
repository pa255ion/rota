; Regression ROM for sound in the Game Boy Color's double speed (side
; track I, phase 3 step 4d): sound_all.asm after a speed switch, so the
; lockstep tests compare the APU's double-speed rules (the frame
; sequencer on DIV bit 13, the pulse dividers on every other M-cycle, the
; wave divider once, the noise 2 dots per M-cycle). No pass/fail.

DEF DOUBLE_SPEED EQU 1
INCLUDE "sound_all.asm"
