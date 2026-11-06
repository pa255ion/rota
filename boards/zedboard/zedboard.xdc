# ZedBoard base constraints: clock, user LEDs, push buttons, DIP switches.
# Every top-level design for this board reads this file. Optional I/O groups
# live in their own files next to it (vga.xdc, ...) and are only read by
# the tops that use them, so that no top gets "no ports matched" critical
# warnings for pins it doesn't have.
#
# Derived from reference/zedboard_master.xdc (Avnet/Digilent master, rev.
# 6 June 2016). Every PACKAGE_PIN below is copied from that file;
# tools/check-xdc.py checks this mechanically. Changes from the master:
#
#   * Port names: LD0..LD7 and SW0..SW7 become the buses LD[7:0] and
#     SW[7:0] (Clash emits a BitVector port as a bus). LDn / SWn = bit n.
#     GCLK and BTNC/D/L/R/U keep their master names.
#   * IOSTANDARD is set per port instead of the master's bank-wide
#     get_iobanks lines: bank-wide lines silently do nothing for ports
#     placed after them, and give a critical warning for an empty bank.
#   * Banks 34/35 use LVCMOS25: VADJ (J18) is 2.5 V on this board. The
#     master defaults them to LVCMOS18. Banks 13/33 are fixed at 3.3 V.
#   * No create_clock here: Clash writes <top>.sdc with the GCLK clock
#     (from Dom100's period), and build.tcl reads it. The MMCM output
#     clocks are derived by Vivado automatically.
#
# Port names must match the Synthesize annotation of each <Name>.Top.

# ---------------------------------------------------------------------------
# Clock source, bank 13 (3.3 V): 100 MHz oscillator
# ---------------------------------------------------------------------------
set_property PACKAGE_PIN Y9  [get_ports {GCLK}]
set_property IOSTANDARD LVCMOS33 [get_ports {GCLK}]

# ---------------------------------------------------------------------------
# User LEDs LD0..LD7, bank 33 (3.3 V)
# ---------------------------------------------------------------------------
set_property PACKAGE_PIN T22 [get_ports {LD[0]}]
set_property PACKAGE_PIN T21 [get_ports {LD[1]}]
set_property PACKAGE_PIN U22 [get_ports {LD[2]}]
set_property PACKAGE_PIN U21 [get_ports {LD[3]}]
set_property PACKAGE_PIN V22 [get_ports {LD[4]}]
set_property PACKAGE_PIN W22 [get_ports {LD[5]}]
set_property PACKAGE_PIN U19 [get_ports {LD[6]}]
set_property PACKAGE_PIN U14 [get_ports {LD[7]}]
set_property IOSTANDARD LVCMOS33 [get_ports {LD[*]}]

# ---------------------------------------------------------------------------
# Push buttons, bank 34 (VADJ = 2.5 V)
# ---------------------------------------------------------------------------
set_property PACKAGE_PIN P16 [get_ports {BTNC}]
set_property PACKAGE_PIN R16 [get_ports {BTND}]
set_property PACKAGE_PIN N15 [get_ports {BTNL}]
set_property PACKAGE_PIN R18 [get_ports {BTNR}]
set_property PACKAGE_PIN T18 [get_ports {BTNU}]
set_property IOSTANDARD LVCMOS25 [get_ports {BTNC BTND BTNL BTNR BTNU}]

# ---------------------------------------------------------------------------
# DIP switches SW0..SW7, bank 35 (VADJ = 2.5 V)
# ---------------------------------------------------------------------------
set_property PACKAGE_PIN F22 [get_ports {SW[0]}]
set_property PACKAGE_PIN G22 [get_ports {SW[1]}]
set_property PACKAGE_PIN H22 [get_ports {SW[2]}]
set_property PACKAGE_PIN F21 [get_ports {SW[3]}]
set_property PACKAGE_PIN H19 [get_ports {SW[4]}]
set_property PACKAGE_PIN H18 [get_ports {SW[5]}]
set_property PACKAGE_PIN H17 [get_ports {SW[6]}]
set_property PACKAGE_PIN M15 [get_ports {SW[7]}]
set_property IOSTANDARD LVCMOS25 [get_ports {SW[*]}]

# ---------------------------------------------------------------------------
# Timing exceptions
# ---------------------------------------------------------------------------
# Buttons and switches are asynchronous; every one goes through
# Platform.Input.Sync before use. LEDs are only looked at by people.
set_false_path -from [get_ports {BTNC BTND BTNL BTNR BTNU SW[*]}]
set_false_path -to   [get_ports {LD[*]}]
