# Board settings for `make hdl|bit|sim SYSTEM=m0e` (see the Makefile):
# side track C's DDR test (M0e.Top). Needs the PS: the board wrapper
# boards/zedboard/m0e_zynq.v with its own PS7 configuration
# (ps7_hp.tcl: the shared one plus the HP0 port), and the relay app,
# which runs the test for `make ddr-test` (platform/ps/ddrtest.zig).
TOP_MODULE      = M0e.Top
BOARD_XDC_EXTRA = vga.xdc usb.xdc

BUILD_TOP     = m0e_zynq
BOARD_WRAPPER = m0e_zynq.v
BOARD_IP_TCL  = ps7_hp.tcl
PS_APP        = relay

# make sim: the PS link script sets a run up (systems/m0e/test/ps/), the
# harness's AXI slave answers it, and the result bytes are checked.
FRAMES ?= 2
SIM_PS   ?= systems/m0e/test/ps/ddr.ps
SIM_ARGS ?= --expect-ps-hex $(abspath systems/m0e/test/golden/ps_bytes.hex)
