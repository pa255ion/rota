# Board settings for `make hdl|sim|bit|prog SYSTEM=a800xl BOARD=arty-z7-20`
# (M4, the Arty port; see the Makefile): the 800XL on the Arty, the
# picture on its HDMI output, mono sound on its jack, programs loaded over
# the PS link (or one built in with XEX=…).
#
#   XEX=<file.xex>  an executable baked into the simulation (64 KiB at
#                   most; default: none, BASIC's Ready prompt)
#   CART=<file>     a cartridge instead (.car or a raw .rom; side track
#                   Q): its kind in the configuration, its image in the
#                   harness's DDR at the media region (--ddr-file), as
#                   the board reads it (A800.CartDdr); or a tape (.cas,
#                   cassettes on the board), alone or with XEX=
TOP_MODULE      = A800.ArtyTop
BOARD_XDC_EXTRA = hdmi_tx.xdc audio.xdc
BOARD_WRAPPER   = arty_mmcm.v tmds_out.v a800xl_zynq.v
HDL_SRC_EXTRA   = cpus/mos6502/src
# about 10 GB at its peak: one such Clash run at a time (the Makefile)
CLASH_HEAVY     = 1

XEX ?=
CART ?=

# The block RAM images, made as for the ZedBoard, in this board's own
# directory (A800.System.imageFiles names build/a800xl/, HDL_IMAGE_DIR:
# make hdl copies them there).
HDL_IMAGE_DIR = build/a800xl
A800_IMAGES = $(addprefix $(BUILD)/images/,os.bin basic.bin cart.bin loader.bin xex.bin cfg.bin)
HDL_PREREQ = $(A800_IMAGES)
$(A800_IMAGES) &: FORCE build/a800xl/xexload.bin
	@test -f roms/a800xl/os/altirraos_xl.rom || { echo "make: roms/a800xl/os/ missing (tools/fetch-roms.sh)" >&2; exit 1; }
	@test -z "$(XEX)" || test -f "$(XEX)" || { echo "make: XEX=$(XEX) not found (make a800-asm?)" >&2; exit 1; }
	@test -z "$(CART)" || test -f "$(CART)" || { echo "make: CART=$(CART) not found (make a800-asm?)" >&2; exit 1; }
	mkdir -p $(BUILD)/images
	$(CABAL) run -v0 a800-image -- roms/a800xl/os/altirraos_xl.rom roms/a800xl/os/altirra_basic.rom \
	  build/a800xl/xexload.bin $(BUILD)/images $(XEX) $(CART)
	@echo "a800xl: block RAM images$(if $(XEX), with $(XEX))$(if $(CART), with the cartridge $(CART))"

# make sim compares its last frame with the VGA golden of the program, if
# there is one (systems/a800xl/test/golden/vga/<name>.png; ready.png
# without XEX). The goldens are the VGA DAC's 4 bits a channel, so the
# Arty's frames are compared on the top 4 bits (--golden-4bit; the TMDS
# stream itself is still checked symbol by symbol), as the NES's.
SIM_GOLDEN ?= $(wildcard systems/a800xl/test/golden/vga/$(if $(XEX)$(CART),$(basename $(notdir $(XEX)$(CART))),ready).png)
SIM_SYS_ARGS = --golden-4bit 1 $(if $(CART),--ddr-file 0x02000000=$(abspath $(BUILD)/images/cart.raw))

# With the PS: the wrapper boards/arty-z7-20/a800xl_zynq.v (the Arty's
# PS7) is the synthesis top; the relay built for the Arty runs on it.
BUILD_TOP     = a800xl_zynq
# (HP0 since side track Q, 2026-10-21: the cartridge from DDR)
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay
