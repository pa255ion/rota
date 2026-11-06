# Board settings for `make hdl|bit SYSTEM=m0a BOARD=arty-z7-20` (side
# track G; see the Makefile). The MMCM is hand-written Verilog behind a
# Clash blackbox (Platform.Board.ArtyZ7), read as a board wrapper.
TOP_MODULE      = M0a.ArtyTop
BOARD_XDC_EXTRA = rgb_led.xdc
BOARD_WRAPPER   = arty_mmcm.v
