# Board settings for `make hdl|lint|sim|resim SYSTEM=gg BOARD=zedboard`
# (the second board; M5 step 5.8, docs/systems/gamegear.md): the Game Gear
# with its cartridge ROM in DDR, the picture on VGA and on HDMI through the
# ADV7511, the PSG on S/PDIF into it; the ROM in DDR, the PS link (keys,
# the SD menu's /gg, loading; step 5.9).
#
#   ROM=<file.gg>   the cartridge: its ROM in DDR (up to 1 MiB; default: the
#                   layer 4 test picture)
TOP_MODULE      = Gg.Top
BOARD_XDC_EXTRA = vga.xdc hdmi.xdc hdmi_audio.xdc usb.xdc
HDL_SRC_EXTRA   = cpus/z80/src

ROM ?= build/gg/asm/pattern.gg

# The power-on cartridge word (mapper, banks; Gg.Machine.cartImage), until
# the PS writes one: made per board ($(BUILD)/images), and make hdl puts it
# where Gg.Machine reads it (build/gg/, HDL_IMAGE_DIR) and next to the
# Verilog, so the two boards' builds don't race on one file.
HDL_IMAGE_DIR = build/gg
HDL_PREREQ = $(BUILD)/images/cart.bin
$(BUILD)/images/cart.bin: FORCE
	@test -f "$(ROM)" || { echo "make: ROM=$(ROM) not found (make gg-asm, tools/fetch-roms.sh?)" >&2; exit 1; }
	mkdir -p $(BUILD)/images
	$(CABAL) run -v0 gg-image -- "$(ROM)" $(BUILD)/images
	@echo "gg: cartridge word from $(ROM)"
# Clash on the whole machine (Z80, VDP, PSG) needs several GB: one such run
# at a time on the machine (the workflow audit's lock).
CLASH_HEAVY = 1

# The ROM in the harness's DDR (the media region), as the Game Boy's.
SIM_SYS_ARGS += --ddr-file 0x02000000=$(abspath $(ROM))

# The VGA golden of the ROM (the same as the Arty's: 12-bit colours widen
# exactly), and the ADV7511's init: the HDMI-with-audio table, as the Game
# Boy's (its golden, read only).
SIM_GOLDEN ?= $(wildcard systems/gg/test/golden/vga/$(basename $(notdir $(ROM))).png)
SIM_I2C_GOLDEN = systems/gb/test/golden/i2c_writes.txt

# The bitstream's top: the wrapper boards/zedboard/gg_zynq.v (the PS7 with
# HP0 for the ROM in DDR), as the Game Boy's.
BUILD_TOP     = gg_zynq
BOARD_WRAPPER = gg_zynq.v
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay
