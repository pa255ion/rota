# Board settings for `make hdl|lint|sim|resim|bit SYSTEM=sms` (the Arty
# Z7-20, the default board; M6 step 6.6, docs/systems/mastersystem.md): the
# Master System with its cartridge ROM (and a BIOS) in DDR, the picture on
# HDMI OUT, the PSG and the FM unit in HDMI's audio and on the jack; the PS
# link (keys, the SD menu's /sms, loading, saves).
#
#   ROM=<file.sms>   the cartridge: its ROM in DDR (up to 1 MiB; default:
#                    the layer 4 test picture)
#   BIOS=<file>      a BIOS image in DDR after it (0x02100000; your own
#                    dump, never committed; default: none)
TOP_MODULE      = Sms.ArtyTop
BOARD_XDC_EXTRA = hdmi_tx.xdc audio.xdc
BOARD_WRAPPER   = arty_mmcm.v tmds_out.v sms_zynq.v
HDL_SRC_EXTRA   = cpus/z80/src systems/gg/src

ROM ?= build/sms/asm/picture.sms
BIOS ?=

# The power-on cartridge word (mapper, BIOS banks, ROM banks;
# Sms.Machine.cartImage), until the PS writes one: per board, as the Game
# Gear's.
HDL_IMAGE_DIR = build/sms
HDL_PREREQ = $(BUILD)/images/cart.bin
$(BUILD)/images/cart.bin: FORCE
	@test -f "$(ROM)" || { echo "make: ROM=$(ROM) not found (make sms-asm, tools/fetch-roms.sh?)" >&2; exit 1; }
	mkdir -p $(BUILD)/images
	$(CABAL) run -v0 gg-image -- "$(ROM)" $(BUILD)/images $(if $(BIOS),--bios "$(BIOS)")
	@echo "sms: cartridge word from $(ROM)$(if $(BIOS), and the BIOS $(BIOS))"
# Clash on the whole machine (Z80, VDP, PSG, YM2413): a heavy run.
CLASH_HEAVY = 1

# The ROM (and the BIOS) in the harness's DDR (the media region).
# The harness takes one DDR file: with a BIOS, one image of both (the
# cartridge padded to its 1 MiB, then the BIOS at 0x02100000).
SMS_DDR = $(if $(BIOS),$(BUILD)/images/ddr.bin,$(ROM))
SIM_SYS_ARGS += --ddr-file 0x02000000=$(abspath $(SMS_DDR))
ifneq ($(BIOS),)
HDL_PREREQ += $(BUILD)/images/ddr.bin
$(BUILD)/images/ddr.bin: FORCE
	@test -f "$(BIOS)" || { echo "make: BIOS=$(BIOS) not found" >&2; exit 1; }
	mkdir -p $(BUILD)/images
	cp -f "$(ROM)" $@.part && truncate -s 1M $@.part && cat "$(BIOS)" >> $@.part && mv -f $@.part $@
endif

# make sim compares its last frame with the ROM's golden, if there is one
# (systems/sms/test/golden/vga/<rom>.png: the spec's 640 x 480 picture,
# `sms-run ROM --vga-at K FILE`).
SIM_GOLDEN ?= $(wildcard systems/sms/test/golden/vga/$(basename $(notdir $(ROM))).png)

# The bitstream's top: the wrapper boards/arty-z7-20/sms_zynq.v (the
# Arty's PS7 with HP0); the relay runs on the PS.
BUILD_TOP     = sms_zynq
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay
