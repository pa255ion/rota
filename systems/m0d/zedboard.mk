# Board settings for `make bit SYSTEM=m0d` (see the Makefile).
TOP_MODULE      = M0d.Top
BOARD_XDC_EXTRA = vga.xdc hdmi.xdc
# M0d.Top reuses the test pattern of M0b (and, through it, M0a).
HDL_SRC_EXTRA   = systems/m0b/src systems/m0a/src

# make sim: 18 frames, so that the ADV7511 set-up (200 ms boot wait, about
# 12 frames, then I2C until frame time 15) is long over; frame 17 is compared on VGA
# (test/golden/frame_0017.png, via --expect), on HDMI and the I2C log
# (goldens made by m0d-unit, cabal test m0d-unit --test-options=--accept).
FRAMES   ?= 18
SIM_HDMI_GOLDEN ?= systems/m0d/test/golden/hdmi_0017.png
SIM_I2C_GOLDEN  ?= systems/m0d/test/golden/i2c_writes.txt
