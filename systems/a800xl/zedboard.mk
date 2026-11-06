# Board settings for `make hdl|bit|sim SYSTEM=a800xl` (see the Makefile;
# M4 steps 4.7 and 4.8).
#
#   XEX=<file.xex>  an executable baked into the simulation (64 KiB at
#                   most; default: none, BASIC's Ready prompt)
TOP_MODULE      = A800.Top
BOARD_XDC_EXTRA = vga.xdc hdmi.xdc hdmi_audio.xdc usb.xdc
# The shared 6502 package
HDL_SRC_EXTRA   = cpus/mos6502/src
# about 10 GB at its peak: one such Clash run at a time (the Makefile)
CLASH_HEAVY     = 1

XEX ?=

# The block RAM images, made per board; make hdl puts them where A800.Top
# reads them (A800.Top.imageFiles: build/a800xl/, HDL_IMAGE_DIR) and where
# the Verilog expects them.
HDL_IMAGE_DIR = build/a800xl
A800_IMAGES = $(addprefix $(BUILD)/images/,os.bin basic.bin cart.bin loader.bin xex.bin cfg.bin)
HDL_PREREQ = $(A800_IMAGES)
$(A800_IMAGES) &: FORCE build/a800xl/xexload.bin
	@test -f roms/a800xl/os/altirraos_xl.rom || { echo "make: roms/a800xl/os/ missing (tools/fetch-roms.sh)" >&2; exit 1; }
	@test -z "$(XEX)" || test -f "$(XEX)" || { echo "make: XEX=$(XEX) not found (make a800-asm?)" >&2; exit 1; }
	mkdir -p $(BUILD)/images
	$(CABAL) run -v0 a800-image -- roms/a800xl/os/altirraos_xl.rom roms/a800xl/os/altirra_basic.rom \
	  build/a800xl/xexload.bin $(BUILD)/images $(XEX)
	@echo "a800xl: block RAM images$(if $(XEX), with $(XEX))"

# make sim compares its last frame with the VGA golden of the program, if
# there is one (systems/a800xl/test/golden/vga/<name>.png; ready.png
# without XEX), the HDMI side with its HDMI golden, and the I2C writes
# (the HDMI-with-audio table).
SIM_GOLDEN ?= $(wildcard systems/a800xl/test/golden/vga/$(if $(XEX),$(basename $(notdir $(XEX))),ready).png)
SIM_HDMI_GOLDEN ?= $(if $(SIM_GOLDEN),$(wildcard $(subst /golden/vga/,/golden/hdmi/,$(SIM_GOLDEN))))
SIM_I2C_GOLDEN  ?= systems/a800xl/test/golden/i2c_writes.txt

# With the PS (step 4.8): the board wrapper boards/zedboard/a800xl_zynq.v
# (PS7 IP from ps7.tcl) is the synthesis top, and the relay app
# (platform/ps/relay.zig) runs the SD card menu (/a800xl) and the
# keyboard.
BUILD_TOP     = a800xl_zynq
BOARD_WRAPPER = a800xl_zynq.v
BOARD_IP_TCL  = ps7.tcl
PS_APP        = relay
