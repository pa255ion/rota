# Program the ZedBoard's PL over JTAG (PROG USB port, J17).
#
#   vivado -mode batch -source boards/zedboard/program.tcl -tclargs <file.bit> [<cable serial>]
#
# Normally run through `make prog SYSTEM=<name>`. Needs the board in JTAG
# boot mode (JP8/JP9/JP10 on GND) and the Digilent cable's udev rules
# (docs/TOOLCHAIN.md). Success: the blue DONE LED (LD12) lights.
# The bitstream is volatile; power-cycling the board clears it.
#
# Side track G: with a cable serial (make prog passes the one
# tools/jtag-pick.sh chose), only that cable is opened; without one, the
# script refuses when more than one cable is connected.

if {[llength $argv] < 1 || [llength $argv] > 2} {
    puts stderr "usage: program.tcl <file.bit> \[<cable serial>\]"
    exit 2
}
set bit [lindex $argv 0]
set serial [lindex $argv 1]
if {![file isfile $bit]} {
    puts stderr "program.tcl: no such bitstream: $bit"
    exit 1
}

open_hw_manager
connect_hw_server -allow_non_jtag
if {[llength [get_hw_targets -quiet]] == 0} {
    if {![info exists progPort]} {
        set progPort "the PROG USB port (J17)"
    }
    puts stderr "program.tcl: hw_server sees no JTAG cable. Check that the board is on and\
                 $progPort is connected; if it appears as 0403:6014 on USB,\
                 your user can't open it (udev rules, see docs/TOOLCHAIN.md). After a fix,\
                 replug the board and kill any running hw_server."
    exit 1
}
if {$serial ne ""} {
    set target [get_hw_targets -quiet "*/$serial"]
    if {[llength $target] != 1} {
        puts stderr "program.tcl: no JTAG cable with serial $serial; hw_server sees: [get_hw_targets]"
        exit 1
    }
} elseif {[llength [get_hw_targets]] > 1} {
    puts stderr "program.tcl: several JTAG cables ([get_hw_targets]) and no serial given; refusing to guess"
    exit 1
} else {
    set target [lindex [get_hw_targets] 0]
}
puts "program.tcl: cable $target"
open_hw_target $target

# The Zynq JTAG chain holds the ARM DAP and the PL; only program the PL.
set devs [get_hw_devices xc7z020*]
if {[llength $devs] != 1} {
    puts stderr "program.tcl: expected one xc7z020 on the chain, found: [get_hw_devices]"
    exit 1
}
set dev [lindex $devs 0]
current_hw_device $dev
set_property PROGRAM.FILE $bit $dev
program_hw_devices $dev

refresh_hw_device -quiet $dev
if {[catch {get_property REGISTER.CONFIG_STATUS.BIT14_DONE_PIN $dev} done]} {
    puts "program.tcl: can't read the DONE status register; check LD12 by eye"
} elseif {$done != 1} {
    puts stderr "program.tcl: DONE pin not high after programming"
    exit 1
}
puts "program.tcl: OK, programmed $dev with $bit"
close_hw_target
disconnect_hw_server
close_hw_manager
