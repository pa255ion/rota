; A tone for layer 4 (M5 step 5.8): PSG channel 0 at period 254
; (3579545 / 32 / 254 = 440.4 Hz), full volume, the others silent, both
; sides (port 06h at its reset value). make sim SYSTEM=gg ROM=build/gg/asm/tone.gg
; SIM_ARGS="--expect-tone 440.4 --expect-hdmi-tone 440.4".

.INCLUDE "gg.inc"

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  ld a, $BF               ; channel 1 off
  out ($7F), a
  ld a, $DF               ; channel 2 off
  out ($7F), a
  ld a, $FF               ; noise off
  out ($7F), a
  ld a, $8E               ; channel 0: period low 4 bits = E
  out ($7F), a
  ld a, $0F               ; high bits: period FEh = 254
  out ($7F), a
  ld a, $90               ; channel 0: volume 0 (full)
  out ($7F), a
  jp passkeep
.ENDS
