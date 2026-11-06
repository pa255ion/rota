# ZedBoard HDMI audio: the ADV7511's S/PDIF input, bank 33 (fixed 3.3 V).
# Read in addition to hdmi.xdc by tops that send audio (PLAN.md step 2.9;
# logic in Platform.Audio.Spdif).
#
# Derived from reference/zedboard_master.xdc ("HD-SPDIF", U15);
# tools/check-xdc.py checks it. HD_SPDIFO (the chip's S/PDIF output) and
# HD_INT stay unconstrained.

set_property PACKAGE_PIN U15  [get_ports {HD_SPDIF}]
set_property IOSTANDARD LVCMOS33 [get_ports {HD_SPDIF}]

# HD_SPDIF leaves an ODDR (in the IOB) clocked by the pixel clock, but the
# chip recovers its own clock from the stream: no timing relation to check.
set_false_path -to [get_ports {HD_SPDIF}]
