# Board settings for `make hdl|sim|bit|prog SYSTEM=nes BOARD=arty-z7-20`
# (side track G phase 2; see the Makefile): the NES on the Arty, the
# picture on its HDMI output, cartridges loaded over the PS link (or one
# built in with ROM=…), no sound yet.
TOP_MODULE      = Nes.ArtyTop
BOARD_XDC_EXTRA = hdmi_tx.xdc audio.xdc
BOARD_WRAPPER   = arty_mmcm.v tmds_out.v nes_zynq.v
HDL_SRC_EXTRA   = cpus/mos6502/src

ROM ?= build/nes/asm/ppu_scene.nes

# The built-in cartridge's block RAM images (CHR, the configuration), made
# as for the ZedBoard, in this board's own directory (Nes.Top's
# chrImage/cartImage name build/nes/, HDL_IMAGE_DIR: make hdl copies them
# there); its PRG ROM in the harness's DDR (--ddr-file, below).
HDL_IMAGE_DIR = build/nes
HDL_PREREQ = $(BUILD)/images/chr.bin $(BUILD)/images/cart.bin
$(HDL_PREREQ) &: FORCE
	@test -f "$(ROM)" || { echo "make: ROM=$(ROM) not found (tools/fetch-roms.sh, make nes-asm?)" >&2; exit 1; }
	mkdir -p $(BUILD)/images
	$(CABAL) run -v0 nes-image -- "$(ROM)" $(BUILD)/images
	@echo "nes: block RAM images from $(ROM)"

# make sim compares its last frame with the VGA golden of the ROM, if there
# is one (systems/nes/test/golden/vga/<rom>.png, made by nes-golden from the
# spec goldens), the HDMI side with its HDMI golden, and the I2C writes
# (the HDMI-with-audio table, as the Game Boy: test/golden/i2c_writes.txt).
# A run with SIM_ARGS="--expect-tone HZ" checks the sound (tone.s).
# The VGA golden of the ROM, if there is one. The NES's colours are 8 bits
# a channel and its goldens the VGA DAC's 4, so the Arty's frames are
# compared on the top 4 bits (--golden-4bit; the TMDS stream itself is
# still checked symbol by symbol).
SIM_GOLDEN ?= $(wildcard systems/nes/test/golden/vga/$(basename $(notdir $(ROM))).png)
SIM_SYS_ARGS = --golden-4bit 1 --ddr-file 0x02000000=$(abspath $(ROM))

# With the PS: the wrapper boards/arty-z7-20/nes_zynq.v (the Arty's PS7
# with HP0, side track D: PRG ROM in DDR) is the synthesis top; the relay
# built for the Arty runs on it.
BUILD_TOP     = nes_zynq
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay
