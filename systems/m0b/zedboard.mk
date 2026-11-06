# Board settings for `make bit SYSTEM=m0b` (see the Makefile).
TOP_MODULE      = M0b.Top
BOARD_XDC_EXTRA = vga.xdc
# M0b.Top reuses the LED logic of M0a.Top.
HDL_SRC_EXTRA   = systems/m0a/src
