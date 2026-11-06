# ZedBoard VGA connector: 12-bit resistor-ladder DAC plus syncs, bank 33
# (3.3 V). Read in addition to zedboard.xdc by tops that drive VGA.
#
# Derived from reference/zedboard_master.xdc; tools/check-xdc.py checks
# every PACKAGE_PIN against it. The master names the colour pins
# VGA_R1..VGA_R4 (same for G and B); here they are the buses VGA_R[3:0],
# VGA_G[3:0], VGA_B[3:0] with VGA_Rn = VGA_R[n-1]. That VGA_R4 is the MSB
# of the ladder is NOT yet confirmed against the ZedBoard schematic: check
# it during the M0b monitor test (a reversed order shows up as non-monotonic
# grey/colour ramps).

set_property PACKAGE_PIN V20  [get_ports {VGA_R[0]}]
set_property PACKAGE_PIN U20  [get_ports {VGA_R[1]}]
set_property PACKAGE_PIN V19  [get_ports {VGA_R[2]}]
set_property PACKAGE_PIN V18  [get_ports {VGA_R[3]}]

set_property PACKAGE_PIN AB22 [get_ports {VGA_G[0]}]
set_property PACKAGE_PIN AA22 [get_ports {VGA_G[1]}]
set_property PACKAGE_PIN AB21 [get_ports {VGA_G[2]}]
set_property PACKAGE_PIN AA21 [get_ports {VGA_G[3]}]

set_property PACKAGE_PIN Y21  [get_ports {VGA_B[0]}]
set_property PACKAGE_PIN Y20  [get_ports {VGA_B[1]}]
set_property PACKAGE_PIN AB20 [get_ports {VGA_B[2]}]
set_property PACKAGE_PIN AB19 [get_ports {VGA_B[3]}]

set_property PACKAGE_PIN AA19 [get_ports {VGA_HS}]
set_property PACKAGE_PIN Y19  [get_ports {VGA_VS}]

set_property IOSTANDARD LVCMOS33 [get_ports {VGA_R[*] VGA_G[*] VGA_B[*] VGA_HS VGA_VS}]

# The outputs are registered in DomPix (40 ns period) and feed a passive
# DAC and a monitor; pin-to-pin skew of a few ns is irrelevant at that
# rate, so no output delay is modelled.
set_false_path -to [get_ports {VGA_R[*] VGA_G[*] VGA_B[*] VGA_HS VGA_VS}]
