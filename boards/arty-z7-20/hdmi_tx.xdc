# Arty Z7-20 HDMI TX connector (the source side; the master's "HDMI TX
# Signals"): three TMDS data pairs and the TMDS clock pair, driven by
# OBUFDS buffers (boards/arty-z7-20/tmds_out.v). Derived from
# reference/arty_z7_20_master.xdc; tools/check-xdc.py checks every pin
# and IOSTANDARD. Both halves of each pair are listed, as in the master.
# HPD, CEC and DDC (SCL/SDA) are not used: DVI needs none of them to
# show a picture.
set_property PACKAGE_PIN L16 [get_ports {hdmi_tx_clk_p}]
set_property PACKAGE_PIN L17 [get_ports {hdmi_tx_clk_n}]
set_property PACKAGE_PIN K17 [get_ports {hdmi_tx_d_p[0]}]
set_property PACKAGE_PIN K18 [get_ports {hdmi_tx_d_n[0]}]
set_property PACKAGE_PIN K19 [get_ports {hdmi_tx_d_p[1]}]
set_property PACKAGE_PIN J19 [get_ports {hdmi_tx_d_n[1]}]
set_property PACKAGE_PIN J18 [get_ports {hdmi_tx_d_p[2]}]
set_property PACKAGE_PIN H18 [get_ports {hdmi_tx_d_n[2]}]
set_property IOSTANDARD TMDS_33 [get_ports {hdmi_tx_clk_p hdmi_tx_clk_n hdmi_tx_d_p[*] hdmi_tx_d_n[*]}]

# The serialisers' outputs leave from OSERDESE2 registers in the IOB at a
# fixed delay; the sink recovers the clock from the clock pair, so there
# is no system-synchronous output timing to check.
set_false_path -to [get_ports {hdmi_tx_clk_p hdmi_tx_clk_n hdmi_tx_d_p[*] hdmi_tx_d_n[*]}]
