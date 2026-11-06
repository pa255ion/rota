# Arty Z7-20 mono audio (side track G phase 2, step 6): the PWM line into
# the board's low-pass filter and amplifier, and the amplifier's enable
# (AUD_SD, high = on). Derived from reference/arty_z7_20_master.xdc;
# checked by tools/check-xdc.py. Both driven as plain outputs, as
# Digilent's out-of-box design for the board does (Platform.Audio.Pwm).
set_property PACKAGE_PIN R18 [get_ports {aud_pwm}]
set_property PACKAGE_PIN T17 [get_ports {aud_sd}]
set_property IOSTANDARD LVCMOS33 [get_ports {aud_pwm aud_sd}]
# the filter takes the stream's average; no timing to the pins
set_false_path -to [get_ports {aud_pwm aud_sd}]
