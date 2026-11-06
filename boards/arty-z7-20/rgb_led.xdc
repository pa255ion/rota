# Arty Z7-20: the green part of RGB LED LD4, for tops that use it (the
# M0a heartbeat). Derived from reference/arty_z7_20_master.xdc; checked
# by tools/check-xdc.py. Driven at full strength (no PWM): bright, but
# only a status light.
set_property PACKAGE_PIN G17 [get_ports {led4_g}]
set_property IOSTANDARD LVCMOS33 [get_ports {led4_g}]
set_false_path -to [get_ports {led4_g}]
