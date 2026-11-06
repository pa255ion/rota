# Board settings for `make bit SYSTEM=m0c` (see the Makefile).
TOP_MODULE      = M0c.Top
BOARD_XDC_EXTRA = oled.xdc
# M0c.Top reuses the LED logic of M0a.Top.
HDL_SRC_EXTRA   = systems/m0a/src
