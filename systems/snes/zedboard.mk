# Board settings for `make hdl|lint|sim|resim|bit SYSTEM=snes BOARD=zedboard`
# (the second board; M7 step 7.10, docs/systems/snes.md): the Super
# Nintendo with its ROM in DDR, the picture line-locked on VGA and on HDMI
# through the ADV7511, the S-DSP's stereo on S/PDIF into it; the PS link
# (keys, the menu, loading).
#
#   ROM=<file.sfc>   the cartridge: its ROM in DDR (up to 8 MiB; default:
#                    a krom picture test, fetched by tools/fetch-roms.sh)
TOP_MODULE      = Snes.Top
BOARD_XDC_EXTRA = vga.xdc hdmi.xdc hdmi_audio.xdc usb.xdc
HDL_SRC_EXTRA   = cpus/w65c816/src cpus/spc700/src

ROM ?= roms/snes/krom/8x8BG1Map2BPP32x328PAL.sfc

# The power-on cartridge word (Snes.Machine.cartImage), the boot ROM
# (Snes.Core.iplImage) and the ROM without a copier header for the
# harness's DDR: per board (snes-image), as the Arty's.
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

# The VGA golden of the ROM: the Arty's picture on the 4-bit VGA pins
# (systems/snes/test/golden/vga4/<rom>.png, `snes-run ROM --frames N+1
# --vga4 FILE`), and the ADV7511's init: the HDMI-with-audio table, as the
# Game Boy's (its golden, read only). The init waits 200 ms after reset:
# runs of 25 frames or more.
SIM_GOLDEN ?= $(wildcard systems/snes/test/golden/vga4/$(basename $(notdir $(ROM))).png)
SIM_I2C_GOLDEN = systems/gb/test/golden/i2c_writes.txt

# The bitstream's top: the wrapper boards/zedboard/snes_zynq.v (the PS7
# with HP0 for the ROM in DDR), as the Master System's.
BUILD_TOP     = snes_zynq
BOARD_WRAPPER = snes_zynq.v
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay

# Step 7.14b: the core's register is its record's bits ("Snes.Core",
# shiftChain), so Clash specialises the record's generic BitPack whole, past
# its default limit of 20; that code names a wire `packed`, no keyword in
# the Verilog-2005 Clash writes, which Verilator then reads as such (as
# side track L's Game Boy did)
CLASH_EXTRA = -fclash-spec-limit=100
VERILATOR_EXTRA = +1364-2005ext+v

# Step 7.15: how save states load. SNES_STATES=direct (the default, step
# 7.14: the whole core's record written back as a chain), cpu (the
# 65C816's restore routine from a ROM overlay, only the sound module's
# record written back: fewer LUTs, docs/systems/snes.md), none (no state
# hooks, as the ZedBoard's top)
SNES_STATES ?= direct
CLASH_EXTRA += $(if $(filter cpu,$(SNES_STATES)),-DSNES_STATES_CPU)$(if $(filter none,$(SNES_STATES)), -DSNES_STATES_NONE)
