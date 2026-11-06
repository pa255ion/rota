# Arty Z7-20 base constraints: clock, user LEDs, push buttons, switches.
# Every top-level design for this board reads this file; optional I/O
# groups live in their own files next to it (hdmi_tx.xdc, ...), as on the
# ZedBoard (boards/zedboard/zedboard.xdc).
#
# Derived from reference/arty_z7_20_master.xdc (Digilent, Rev. B). Every
# PACKAGE_PIN and IOSTANDARD below is copied from that file;
# tools/check-xdc.py checks both mechanically. Changes from the master:
#
#   * Port names are the master's (clk, led[3:0], btn[3:0], sw[1:0]);
#     Clash emits a BitVector port as a bus of the same name.
#   * One property per line instead of the master's -dict form, like the
#     ZedBoard files.
#   * Every bank on this board is 3.3 V: everything is LVCMOS33 (the
#     HDMI pairs TMDS_33), as in the master.
#   * No create_clock here: Clash writes <top>.sdc with the clk clock
#     (from Dom125's period), and build.tcl reads it. The MMCM output
#     clocks are derived by Vivado automatically.
#
# Port names must match the Synthesize annotation of each Arty top.

# ---------------------------------------------------------------------------
# Clock source: 125 MHz (master: 8.00 ns), bank 35
# ---------------------------------------------------------------------------
set_property PACKAGE_PIN H16 [get_ports {clk}]
set_property IOSTANDARD LVCMOS33 [get_ports {clk}]

# ---------------------------------------------------------------------------
# User LEDs LD0..LD3 (the RGB LEDs LD4/LD5 are not used yet)
# ---------------------------------------------------------------------------
set_property PACKAGE_PIN R14 [get_ports {led[0]}]
set_property PACKAGE_PIN P14 [get_ports {led[1]}]
set_property PACKAGE_PIN N16 [get_ports {led[2]}]
set_property PACKAGE_PIN M14 [get_ports {led[3]}]
set_property IOSTANDARD LVCMOS33 [get_ports {led[*]}]

# ---------------------------------------------------------------------------
# Push buttons BTN0..BTN3
# ---------------------------------------------------------------------------
set_property PACKAGE_PIN D19 [get_ports {btn[0]}]
set_property PACKAGE_PIN D20 [get_ports {btn[1]}]
set_property PACKAGE_PIN L20 [get_ports {btn[2]}]
set_property PACKAGE_PIN L19 [get_ports {btn[3]}]
set_property IOSTANDARD LVCMOS33 [get_ports {btn[*]}]

# ---------------------------------------------------------------------------
# Slide switches SW0, SW1
# ---------------------------------------------------------------------------
set_property PACKAGE_PIN M20 [get_ports {sw[0]}]
set_property PACKAGE_PIN M19 [get_ports {sw[1]}]
set_property IOSTANDARD LVCMOS33 [get_ports {sw[*]}]

# ---------------------------------------------------------------------------
# Timing exceptions
# ---------------------------------------------------------------------------
# Buttons and switches are asynchronous; every one goes through
# Platform.Input.Sync before use. LEDs are only looked at by people.
set_false_path -from [get_ports {btn[*] sw[*]}]
set_false_path -to   [get_ports {led[*]}]
