# Board settings for `make hdl|sim|bit SYSTEM=m0b BOARD=arty-z7-20` (side
# track G; see the Makefile): the test pattern on the Arty's HDMI output.
# The MMCM and the TMDS serialisers are hand-written Verilog behind Clash
# blackboxes (Platform.Board.ArtyZ7), read as board wrappers.
TOP_MODULE      = M0b.ArtyTop
BOARD_XDC_EXTRA = rgb_led.xdc hdmi_tx.xdc
BOARD_WRAPPER   = arty_mmcm.v tmds_out.v
# M0b.ArtyTop reuses the LED logic of M0a.Top and M0a.ArtyTop.
HDL_SRC_EXTRA   = systems/m0a/src
