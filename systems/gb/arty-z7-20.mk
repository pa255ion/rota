# Board settings for `make hdl|sim|bit|prog SYSTEM=gb BOARD=arty-z7-20`
# (side track G phase 2; see the Makefile): the Game Boy on the Arty, its
# cartridge in DDR through HP0, the picture on the Arty's HDMI output, no
# sound yet.
#
#   ROM=<file.gb>   cartridge for make sim / make resim (default:
#                   dmg-acid2), in the harness's simulated DDR at
#                   Gb.Top.romBase, as on the ZedBoard.
TOP_MODULE      = Gb.ArtyTop
BOARD_XDC_EXTRA = hdmi_tx.xdc audio.xdc
BOARD_WRAPPER   = arty_mmcm.v tmds_out.v gb_zynq.v

ROM ?= roms/gb/dmg-acid2/dmg-acid2.gb
SIM_SYS_ARGS = --ddr-file 0x02000000=$(abspath $(ROM))

# The VGA golden of the ROM, if there is one: TMDS carries the same 8-bit
# stream losslessly.
SIM_GOLDEN ?= $(wildcard systems/gb/test/golden/vga/$(basename $(notdir $(ROM))).png)

# With the PS: the wrapper boards/arty-z7-20/gb_zynq.v (the Arty's PS7 with
# HP0) is the synthesis top; the relay built for the Arty runs on it.
BUILD_TOP     = gb_zynq
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay

