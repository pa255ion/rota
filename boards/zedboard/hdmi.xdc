# ZedBoard HDMI: the ADV7511 transmitter's video input and I2C, bank 33
# (fixed 3.3 V). Read in addition to zedboard.xdc by tops that drive HDMI
# (PLAN.md M0d step 4; logic in Platform.Board.Zedboard).
#
# Derived from reference/zedboard_master.xdc; tools/check-xdc.py checks
# every PACKAGE_PIN against it. The master names the data pins HD_D0 ..
# HD_D14 and, by a typo, HS_D15; here they are the bus HD_D[15:0]. (The
# HW user's guide's table lists HD-D1 at AA12 and bank 35; the master,
# which wins, has AA13 and bank 33. docs/ZEDBOARD.md.)
#
# Not used, so not constrained: HD_INT (the ADV7511 interrupt; HPD is
# polled over I2C instead), HD_SPDIF and HD_SPDIFO (audio: hdmi_audio.xdc).

set_property PACKAGE_PIN W18  [get_ports {HD_CLK}]

set_property PACKAGE_PIN Y13  [get_ports {HD_D[0]}]
set_property PACKAGE_PIN AA13 [get_ports {HD_D[1]}]
set_property PACKAGE_PIN AA14 [get_ports {HD_D[2]}]
set_property PACKAGE_PIN Y14  [get_ports {HD_D[3]}]
set_property PACKAGE_PIN AB15 [get_ports {HD_D[4]}]
set_property PACKAGE_PIN AB16 [get_ports {HD_D[5]}]
set_property PACKAGE_PIN AA16 [get_ports {HD_D[6]}]
set_property PACKAGE_PIN AB17 [get_ports {HD_D[7]}]
set_property PACKAGE_PIN AA17 [get_ports {HD_D[8]}]
set_property PACKAGE_PIN Y15  [get_ports {HD_D[9]}]
set_property PACKAGE_PIN W13  [get_ports {HD_D[10]}]
set_property PACKAGE_PIN W15  [get_ports {HD_D[11]}]
set_property PACKAGE_PIN V15  [get_ports {HD_D[12]}]
set_property PACKAGE_PIN U17  [get_ports {HD_D[13]}]
set_property PACKAGE_PIN V14  [get_ports {HD_D[14]}]
set_property PACKAGE_PIN V13  [get_ports {HD_D[15]}]

set_property PACKAGE_PIN U16  [get_ports {HD_DE}]
set_property PACKAGE_PIN V17  [get_ports {HD_HSYNC}]
set_property PACKAGE_PIN W17  [get_ports {HD_VSYNC}]

set_property PACKAGE_PIN AA18 [get_ports {HD_SCL}]
set_property PACKAGE_PIN Y16  [get_ports {HD_SDA}]

set_property IOSTANDARD LVCMOS33 [get_ports {HD_CLK HD_D[*] HD_DE HD_HSYNC HD_VSYNC HD_SCL HD_SDA}]

# --- Video timing ----------------------------------------------------------
# HD_CLK comes from an ODDR (D1 = 0, D2 = 1) clocked by the pixel clock, so
# it is that clock inverted. Data, DE and syncs leave from IOB flip-flops on
# the pixel clock's rising edge; the ADV7511 samples them on HD_CLK's rising
# edge, half a period later. Its input needs tVSU = 1 ns set-up and
# tVHLD = 0.7 ns hold (ADV7511 Hardware User's Guide Rev. D, AC
# specifications; measured at 0.9 V; movable by 0xBA, which we leave at
# "no delay"). Board trace skew between HD_CLK and the data lines isn't
# known; the half-period launch gives about 18 ns of margin either way.
# The ODDR is found from the port (tops with audio have a second ODDR, on
# HD_SPDIF).
create_generated_clock -name hd_clk \
    -source [get_pins -of_objects [filter [all_fanin -flat -only_cells -startpoints_only [get_ports {HD_CLK}]] {REF_NAME == ODDR}] -filter {REF_PIN_NAME == C}] \
    -divide_by 1 -invert [get_ports {HD_CLK}]
set_output_delay -clock hd_clk -max  1.0 [get_ports {HD_D[*] HD_DE HD_HSYNC HD_VSYNC}]
set_output_delay -clock hd_clk -min -0.7 [get_ports {HD_D[*] HD_DE HD_HSYNC HD_VSYNC}]
set_property IOB TRUE [get_ports {HD_D[*] HD_DE HD_HSYNC HD_VSYNC}]

# --- I2C -------------------------------------------------------------------
# 100 kHz, open drain, synchronised on the way in (Platform.Bus.I2c): no
# timing relation to the pixel clock. The weak FPGA pull-ups are only a
# fallback for the board's own I2C pull-ups.
set_property PULLUP TRUE [get_ports {HD_SCL HD_SDA}]
set_false_path -to   [get_ports {HD_SCL HD_SDA}]
set_false_path -from [get_ports {HD_SCL HD_SDA}]
