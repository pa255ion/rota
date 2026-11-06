# Pmod JA, pin 1 only (bank 13, fixed 3.3 V): the CHIP-8 beeper output
# (docs/systems/chip8.md, "Sound"), a 440 Hz square wave while the sound
# timer runs. Nothing else is connected to the pin on the board; a piezo or
# small amplified speaker between JA1 and GND (JA5) makes it audible.
#
# Derived from reference/zedboard_master.xdc; tools/check-xdc.py checks
# the pin against it.

set_property PACKAGE_PIN Y11 [get_ports {JA1}]
set_property IOSTANDARD LVCMOS33 [get_ports {JA1}]

# Registered in DomPix; an audio-rate square wave, so no output delay.
set_false_path -to [get_ports {JA1}]
