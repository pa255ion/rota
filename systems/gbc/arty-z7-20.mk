# Board settings for `make hdl|sim|bit|prog SYSTEM=gbc BOARD=arty-z7-20`
# (side track I, phase 3): the Game Boy Color on the Arty, gbc.bit. Its
# source is the Game Boy's (systems/gb/src, top Gb.ArtyTopCgb); the
# board's ends are Gb.ArtyTop's (systems/gb/arty-z7-20.mk). Tests run with
# SYSTEM=gb.
#
#   ROM=<file.gbc>  cartridge for make sim / make resim (default:
#                   cgb-acid2), in the harness's simulated DDR at
#                   Gb.Machine.romBase.
TOP_MODULE      = Gb.ArtyTopCgb
HDL_SRC_EXTRA   = systems/gb/src
BOARD_XDC_EXTRA = hdmi_tx.xdc audio.xdc
BOARD_WRAPPER   = arty_mmcm.v tmds_out.v gbc_zynq.v
# the Clash flags come from the Game Boy's package (the Makefile sets
# SYSTEM_CABAL after this file: override keeps this value)
override SYSTEM_CABAL = systems/gb/gb.cabal

ROM ?= roms/gbc/cgb-acid2/cgb-acid2.gbc
SIM_SYS_ARGS = --ddr-file 0x02000000=$(abspath $(ROM))

# The full-colour golden of the ROM, if there is one (TMDS carries the
# 8-bit stream losslessly; the VGA goldens have the DAC's 4 bits).
SIM_GOLDEN ?= $(wildcard systems/gb/test/golden/rgb/$(basename $(notdir $(ROM))).png)

# With the PS: the wrapper boards/arty-z7-20/gbc_zynq.v (the Arty's PS7
# with HP0) is the synthesis top; the relay built for the Arty runs on it.
BUILD_TOP     = gbc_zynq
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay

