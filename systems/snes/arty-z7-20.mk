# Board settings for `make hdl|lint|sim|resim|bit SYSTEM=snes` (the Arty
# Z7-20, the default board; M7 step 7.10, docs/systems/snes.md): the Super
# Nintendo with its ROM in DDR, the picture line-locked on HDMI OUT, the
# S-DSP's stereo in HDMI's audio and on the jack; the PS link (keys, the
# menu, loading).
#
#   ROM=<file.sfc>   the cartridge: its ROM in DDR (up to 8 MiB; default:
#                    a krom picture test, fetched by tools/fetch-roms.sh)
TOP_MODULE      = Snes.ArtyTop
BOARD_XDC_EXTRA = hdmi_tx.xdc audio.xdc
BOARD_WRAPPER   = arty_mmcm.v tmds_out.v snes_zynq.v
HDL_SRC_EXTRA   = cpus/w65c816/src cpus/spc700/src

ROM ?= roms/snes/krom/8x8BG1Map2BPP32x328PAL.sfc

# The power-on cartridge word (Snes.Machine.cartImage), the boot ROM
# (Snes.Core.iplImage) and the ROM without a copier header for the
# harness's DDR: per board (snes-image).
HDL_IMAGE_DIR = build/snes
HDL_PREREQ = $(BUILD)/images/cart.bin $(BUILD)/images/ipl.mem
$(BUILD)/images/ipl.mem: $(BUILD)/images/cart.bin
$(BUILD)/images/cart.bin: FORCE build/snes/ipl.bin
	@test -f "$(ROM)" || { echo "make: ROM=$(ROM) not found (make snes-asm, tools/fetch-roms.sh?)" >&2; exit 1; }
	mkdir -p $(BUILD)/images
	$(CABAL) run -v0 snes-image -- "$(ROM)" $(BUILD)/images
	@echo "snes: cartridge word from $(ROM)"
# Clash on the whole machine (the 65C816, the PPU, the SPC700, the S-DSP):
# a heavy run.
CLASH_HEAVY = 1

# The ROM in the harness's DDR (the media region).
SIM_SYS_ARGS += --ddr-file 0x02000000=$(abspath $(BUILD)/images/rom.bin)

# make sim compares its last frame with the ROM's golden, if there is one
# (systems/snes/test/golden/vga/<rom>.png: the spec's 640 x 480 picture,
# `snes-run ROM --vga-at K FILE`).
SIM_GOLDEN ?= $(wildcard systems/snes/test/golden/vga/$(basename $(notdir $(ROM))).png)

# The bitstream's top: the wrapper boards/arty-z7-20/snes_zynq.v (the
# Arty's PS7 with HP0); the relay runs on the PS.
BUILD_TOP     = snes_zynq
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay

# Step 7.14b: the core's register is its record's bits ("Snes.Core",
# shiftChain), so Clash specialises the record's generic BitPack whole, past
# its default limit of 20; that code names a wire `packed`, no keyword in
# the Verilog-2005 Clash writes, which Verilator then reads as such (as
# side track L's Game Boy did)
CLASH_EXTRA = -fclash-spec-limit=100
VERILATOR_EXTRA = +1364-2005ext+v
