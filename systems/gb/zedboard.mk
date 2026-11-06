# Board settings for `make hdl|bit|sim SYSTEM=gb` (see the Makefile).
#
#   ROM=<file.gb>   cartridge for make sim / make resim (default:
#                   dmg-acid2), up to 8 MiB: the harness puts it in its
#                   simulated DDR at Gb.Top.romBase (side track I, phase
#                   2). The bitstream holds no cartridge: the PS loads it.
TOP_MODULE      = Gb.Top
BOARD_XDC_EXTRA = vga.xdc hdmi.xdc hdmi_audio.xdc usb.xdc

ROM ?= roms/gb/dmg-acid2/dmg-acid2.gb

# The cartridge in the harness's DDR (platform/sim/axi.zig), where
# Gb.Top.romBase says.
SIM_SYS_ARGS = --ddr-file 0x02000000=$(abspath $(ROM))

# make sim compares its last frame with the VGA golden of the ROM, if there
# is one (systems/gb/test/golden/vga/<rom>.png, made by gb-golden from the
# spec goldens), the HDMI side with its HDMI golden, and the I2C writes
# (the HDMI-with-audio table, Platform.Video.Adv7511.Table).
SIM_GOLDEN ?= $(wildcard systems/gb/test/golden/vga/$(basename $(notdir $(ROM))).png)
SIM_HDMI_GOLDEN ?= $(if $(SIM_GOLDEN),$(wildcard $(subst /golden/vga/,/golden/hdmi/,$(SIM_GOLDEN))))
SIM_I2C_GOLDEN  ?= systems/gb/test/golden/i2c_writes.txt

# With the PS: the board wrapper boards/zedboard/gb_zynq.v (PS7 IP with
# the HP0 port, ps7_hp.tcl: the cartridge is in DDR) is the synthesis top, and the relay app (platform/ps/relay.zig)
# forwards desktop keys (make keys SYSTEM=gb) and cartridges (make load
# SYSTEM=gb ROM=…) from the USB-UART.
BUILD_TOP     = gb_zynq
BOARD_WRAPPER = gb_zynq.v
BOARD_IP_TCL  = ps7_hp_rst.tcl
PS_APP        = relay
