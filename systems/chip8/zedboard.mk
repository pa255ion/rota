# Board settings for `make hdl|bit|sim SYSTEM=chip8` (see the Makefile).
#
#   ROM=<file.ch8>        program baked into the bitstream / simulation
#                         (default: the IBM logo test ROM)
#   RAM_PRESET="0x1FF=1"  extra RAM bytes, e.g. the menu choice of 5-quirks
#                         and 6-keypad
TOP_MODULE      = Chip8.Top
BOARD_XDC_EXTRA = vga.xdc oled.xdc pmod_ja1.xdc hdmi.xdc usb.xdc

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

# Faster instructions in simulation (SW5), so that a few seconds of
# simulated time finish the test ROMs' screens.
SIM_ARGS ?= --sw 0x20

# make sim compares its last frame with the layer 3 VGA golden of the ROM,
# if there is one (systems/chip8/test/golden/vga/<rom>.png).
SIM_GOLDEN ?= $(wildcard systems/chip8/test/golden/vga/$(basename $(notdir $(ROM))).png)
# ... and the HDMI side with its HDMI golden (made by chip8-golden from the
# VGA one) and the I2C writes (the same register table as M0d's).
SIM_HDMI_GOLDEN ?= $(if $(SIM_GOLDEN),$(wildcard $(subst /golden/vga/,/golden/hdmi/,$(SIM_GOLDEN))))
SIM_I2C_GOLDEN  ?= systems/m0d/test/golden/i2c_writes.txt

# With the PS: the board wrapper boards/zedboard/chip8_zynq.v (PS7 IP from
# ps7.tcl) is the synthesis top, and the relay app (platform/ps/relay.zig)
# forwards desktop keys from the USB-UART to PS_KEYS (make keys).
BUILD_TOP     = chip8_zynq
BOARD_WRAPPER = chip8_zynq.v
BOARD_IP_TCL  = ps7.tcl
PS_APP        = relay
