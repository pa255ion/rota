# ZedBoard USB-OTG PHY reset (side track E): the TUSB1210's RESETB is wired
# to a PL pin, not to the PS, so the PHY stays in reset (no 60 MHz ULPI
# clock, the USB controller can't run) unless the bitstream drives it high.
# Every top with the PS app (PS_APP = relay) ties OTG_RESETN to 1 in its
# wrapper (boards/zedboard/*_zynq.v); found on the board 2026-10-04 ("USB: no
# PHY" with or without a device).
#
# Derived from reference/zedboard_master.xdc ("OTG-RESETN", G17, bank 35 =
# VADJ 2.5 V); tools/check-xdc.py checks it. OTG_VBUSOC (L16, the Vbus
# switch's over-current flag) stays unconstrained.

set_property PACKAGE_PIN G17 [get_ports {OTG_RESETN}]
set_property IOSTANDARD LVCMOS25 [get_ports {OTG_RESETN}]
