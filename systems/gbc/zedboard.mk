# Board settings for `make hdl|bit|sim SYSTEM=gbc` (see the Makefile): the
# Game Boy Color (side track I, phase 3 step 6), gbc.bit. Its source is the
# Game Boy's (systems/gb/src, top Gb.TopCgb: the core's CGB model); this
# directory holds only these settings. Tests run with SYSTEM=gb.
#
#   ROM=<file.gbc>  cartridge for make sim / make resim (default:
#                   cgb-acid2), up to 8 MiB in the harness's simulated DDR
#                   at Gb.Top.romBase, as SYSTEM=gb.
TOP_MODULE      = Gb.TopCgb
HDL_SRC_EXTRA   = systems/gb/src
BOARD_XDC_EXTRA = vga.xdc hdmi.xdc hdmi_audio.xdc usb.xdc
# the Clash flags come from the Game Boy's package (the Makefile sets
# SYSTEM_CABAL after this file: override keeps this value)
override SYSTEM_CABAL = systems/gb/gb.cabal

ROM ?= roms/gbc/cgb-acid2/cgb-acid2.gbc

SIM_SYS_ARGS = --ddr-file 0x02000000=$(abspath $(ROM))

# make sim compares its last frame with the VGA and HDMI goldens of the ROM,
# if there are any (systems/gb/test/golden/vga/<rom>.png, made by
# gb-golden from the spec's colour screens), and the I2C writes with the
# Game Boy's (the same HDMI-with-audio table).
SIM_GOLDEN ?= $(wildcard systems/gb/test/golden/vga/$(basename $(notdir $(ROM))).png)
SIM_HDMI_GOLDEN ?= $(if $(SIM_GOLDEN),$(wildcard $(subst /golden/vga/,/golden/hdmi/,$(SIM_GOLDEN))))
SIM_I2C_GOLDEN  ?= systems/gb/test/golden/i2c_writes.txt

# With the PS: boards/zedboard/gbc_zynq.v (as gb_zynq.v) is the synthesis
# top; the same relay app (system ID 6: the menu folder /gbc).
BUILD_TOP     = gbc_zynq
BOARD_WRAPPER = gbc_zynq.v
BOARD_IP_TCL  = ps7_hp_rst.tcl
PS_APP        = relay
