# Board settings for `make hdl|bit|sim SYSTEM=nes` (see the Makefile).
#
#   ROM=<file.nes>  cartridge for the simulation (its CHR and configuration
#                   also baked into the bitstream; default: our
#                   ppu_scene.s), mappers 0/1/2/3/4/7, PRG up to 512 KiB
#                   (in DDR: the harness's, --ddr-file), CHR up to 256 KiB
TOP_MODULE      = Nes.Top
BOARD_XDC_EXTRA = vga.xdc hdmi.xdc hdmi_audio.xdc usb.xdc
# The shared 6502 package
HDL_SRC_EXTRA   = cpus/mos6502/src

ROM ?= build/nes/asm/ppu_scene.nes

# The block RAM images, made per board; make hdl puts them where Nes.Top
# reads them (Nes.Top.chrImage, cartImage: build/nes/, HDL_IMAGE_DIR) and
# where the Verilog expects them.
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
SIM_GOLDEN ?= $(wildcard systems/nes/test/golden/vga/$(basename $(notdir $(ROM))).png)
SIM_HDMI_GOLDEN ?= $(if $(SIM_GOLDEN),$(wildcard $(subst /golden/vga/,/golden/hdmi/,$(SIM_GOLDEN))))
SIM_I2C_GOLDEN  ?= systems/nes/test/golden/i2c_writes.txt
# The whole file in the harness's DDR (platform/sim/axi.zig) at
# Nes.Top.romBase: PRG ROM is read from there (side track D).
SIM_SYS_ARGS = --ddr-file 0x02000000=$(abspath $(ROM))

# With the PS (step 3.8): the board wrapper boards/zedboard/nes_zynq.v
# (PS7 IP with HP0 from ps7_hp.tcl, side track D) is the synthesis top, and the relay app
# (platform/ps/relay.zig) runs the SD card menu (/nes) and forwards desktop
# keys (make keys SYSTEM=nes) and cartridges (make load SYSTEM=nes ROM=…)
# from the USB-UART.
BUILD_TOP     = nes_zynq
BOARD_WRAPPER = nes_zynq.v
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay
