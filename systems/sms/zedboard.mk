# Board settings for `make hdl|lint|sim|resim|bit SYSTEM=sms BOARD=zedboard`
# (the second board; M6 step 6.6, docs/systems/mastersystem.md): the Master
# System with its cartridge ROM (and a BIOS) in DDR, the picture on VGA and
# on HDMI through the ADV7511, the PSG and the FM unit on S/PDIF into it;
# the PS link (keys, the SD menu's /sms, loading, saves).
#
#   ROM=<file.sms>   the cartridge (default: the layer 4 test picture)
#   BIOS=<file>      a BIOS image after it in DDR (your own; default: none)
TOP_MODULE      = Sms.Top
BOARD_XDC_EXTRA = vga.xdc hdmi.xdc hdmi_audio.xdc usb.xdc
HDL_SRC_EXTRA   = cpus/z80/src systems/gg/src

ROM ?= build/sms/asm/picture.sms
BIOS ?=

# The power-on cartridge word (mapper, BIOS banks, ROM banks; Sms.Machine.cartImage), until
# the PS writes one: made per board ($(BUILD)/images), and make hdl puts it
# where Sms.Machine reads it (build/sms/, HDL_IMAGE_DIR) and next to the
# Verilog, so the two boards' builds don't race on one file.
HDL_IMAGE_DIR = build/sms
HDL_PREREQ = $(BUILD)/images/cart.bin
$(BUILD)/images/cart.bin: FORCE
	@test -f "$(ROM)" || { echo "make: ROM=$(ROM) not found (make sms-asm, tools/fetch-roms.sh?)" >&2; exit 1; }
	mkdir -p $(BUILD)/images
	$(CABAL) run -v0 gg-image -- "$(ROM)" $(BUILD)/images $(if $(BIOS),--bios "$(BIOS)")
	@echo "sms: cartridge word from $(ROM)$(if $(BIOS), and the BIOS $(BIOS))"
# Clash on the whole machine (Z80, VDP, PSG, YM2413) needs several GB: one such run
# at a time on the machine (the workflow audit's lock).
CLASH_HEAVY = 1

# The ROM in the harness's DDR (the media region), as the Game Boy's.
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

# The VGA golden of the ROM (the same as the Arty's), and the ADV7511's
# init: the HDMI-with-audio table, as the Game Boy's (its golden, read only).
SIM_GOLDEN ?= $(wildcard systems/sms/test/golden/vga/$(basename $(notdir $(ROM))).png)
SIM_I2C_GOLDEN = systems/gb/test/golden/i2c_writes.txt

# The bitstream's top: the wrapper boards/zedboard/sms_zynq.v (the PS7 with
# HP0 for the ROM in DDR), as the Game Gear's.
BUILD_TOP     = sms_zynq
BOARD_WRAPPER = sms_zynq.v
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay
