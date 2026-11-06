# Board settings for `make hdl|sim|bit SYSTEM=chip8 BOARD=arty-z7-20`
# (side track G; see the Makefile): CHIP-8 with the program baked in
# (loader stage A) or loaded through the PS link, on the Arty's buttons and
# HDMI output, with the Arty's PS (side track G phase 2).
#
#   ROM=<file.ch8>        program baked into the bitstream / simulation
#                         (default: the IBM logo test ROM)
#   RAM_PRESET="0x1FF=1"  extra RAM bytes
TOP_MODULE      = Chip8.ArtyTop
BOARD_XDC_EXTRA = hdmi_tx.xdc audio.xdc
BOARD_WRAPPER   = arty_mmcm.v tmds_out.v chip8_zynq.v
# With the PS (side track G phase 2): the wrapper boards/arty-z7-20/
# chip8_zynq.v (the Arty's PS7 from its ps7.tcl) is the synthesis top,
# and the relay app (built with -Dboard=arty-z7-20, the Arty's generated
# ps7_init) runs on it: make keys / make load over the Arty's UART.
BUILD_TOP       = chip8_zynq
BOARD_IP_TCL    = ps7.tcl
PS_APP          = relay

ROM        ?= roms/chip8/timendus/2-ibm-logo.ch8
RAM_PRESET ?=

# The RAM image, made per board; make hdl puts it where Chip8.Top.ramImage
# (build/chip8/ram.bin) and the Verilog expect it (HDL_IMAGE_DIR, the
# Makefile), so both boards' builds can run at the same time.
HDL_IMAGE_DIR = build/chip8
HDL_PREREQ = $(BUILD)/images/ram.bin
$(BUILD)/images/ram.bin: FORCE
	@test -f "$(ROM)" || { echo "make: ROM=$(ROM) not found (tools/fetch-roms.sh?)" >&2; exit 1; }
	mkdir -p $(@D)
	$(CABAL) run -v0 chip8-ram -- $@ "$(ROM)" $(RAM_PRESET)
	@echo "chip8: RAM image from $(ROM) $(RAM_PRESET)"

# Faster instructions in simulation (SW0 here, the ZedBoard's SW5).
SIM_ARGS ?= --sw 0x1

# make sim compares its last frame with the CHIP-8 VGA golden of the ROM,
# if there is one: TMDS carries the same 8-bit stream losslessly.
SIM_GOLDEN ?= $(wildcard systems/chip8/test/golden/vga/$(basename $(notdir $(ROM))).png)
