# Board settings for `make hdl|lint|sim|resim SYSTEM=gg` (the Arty Z7-20,
# the default board; M5 step 5.8, docs/systems/gamegear.md): the Game Gear
# with its cartridge ROM in DDR, the picture on HDMI OUT with the PSG in
# HDMI's audio and on the jack; the ROM in DDR, the PS link (keys, the
# SD menu's /gg, loading; step 5.9).
#
#   ROM=<file.gg>   the cartridge: its ROM in DDR (up to 1 MiB; default: the
#                   layer 4 test picture)
TOP_MODULE      = Gg.ArtyTop
BOARD_XDC_EXTRA = hdmi_tx.xdc audio.xdc
BOARD_WRAPPER   = arty_mmcm.v tmds_out.v gg_zynq.v
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

# make sim compares its last frame with the ROM's golden, if there is one
# (systems/gg/test/golden/vga/<rom>.png: the spec's frame as the board
# shows it, `gg-run ROM --vga-at K FILE`).
SIM_GOLDEN ?= $(wildcard systems/gg/test/golden/vga/$(basename $(notdir $(ROM))).png)

# The bitstream's top: the wrapper boards/arty-z7-20/gg_zynq.v (the
# Arty's PS7 with HP0 for the ROM in DDR); the relay runs on the PS (the
# PS link: keys, the menu, loading).
BUILD_TOP     = gg_zynq
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay
