# ZedBoard onboard OLED (UG-2832HSWEG04, SSD1306, 4-wire SPI, write only),
# bank 13 (fixed 3.3 V). Read in addition to zedboard.xdc by tops that
# drive the OLED (Platform.Video.Oled).
#
# Derived from reference/zedboard_master.xdc; tools/check-xdc.py checks
# every PACKAGE_PIN against it. CS# is tied low on the board, so there is
# no chip-select pin. OLED_VDD and OLED_VBAT drive P-FET gates with pull-ups
# (docs/ZEDBOARD.md): 0 switches a supply on.

set_property PACKAGE_PIN U10  [get_ports {OLED_DC}]
set_property PACKAGE_PIN U9   [get_ports {OLED_RES}]
set_property PACKAGE_PIN AB12 [get_ports {OLED_SCLK}]
set_property PACKAGE_PIN AA12 [get_ports {OLED_SDIN}]
set_property PACKAGE_PIN U11  [get_ports {OLED_VBAT}]
set_property PACKAGE_PIN U12  [get_ports {OLED_VDD}]

set_property IOSTANDARD LVCMOS33 [get_ports {OLED_DC OLED_RES OLED_SCLK OLED_SDIN OLED_VBAT OLED_VDD}]

# All six are registered in DomPix. SCLK runs at the pixel clock / 8
# (about 3.1 MHz, 8 clocks of 39.72 ns per bit), and SDIN and D/C# change
# 4 clocks (159 ns) before each rising SCLK edge, far more than the
# SSD1306's 15 ns setup time, so no output delay is modelled.
set_false_path -to [get_ports {OLED_DC OLED_RES OLED_SCLK OLED_SDIN OLED_VBAT OLED_VDD}]
