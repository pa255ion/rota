# Program the ZedBoard for a design that uses the PS (xsdb, over JTAG):
#
#   xsdb boards/zedboard/prog_ps.tcl <file.bit> <ps7_init.tcl | -> <app.elf> [<cable serial>]
#
# Normally run through `make prog SYSTEM=<name>` (tools/xsdb.sh). Order as in
# Vitis's own launch scripts: system reset, PL bitstream, ps7_init (clocks,
# MIO, DDR, UART1), ps7_post_config (PS-PL level shifters on, PL resets
# released), then the app into OCM, started on core 0. Everything is
# volatile: power-cycling the board clears it. JTAG boot mode
# (JP8/JP9/JP10 on GND), as for make prog without a PS app.
#
# With "-" for ps7_init (make prog PS_INIT=app, side track F) both ps7
# steps are left to the app, which runs them itself when it finds DDR not
# set up: the start-up it does when the boot ROM loads it from the SD
# card, tried over JTAG.

# Side track G: with a cable serial (make prog passes the one
# tools/jtag-pick.sh chose), only targets behind that cable are used.
if {[llength $argv] < 3 || [llength $argv] > 4} {
    puts stderr "usage: prog_ps.tcl <file.bit> <ps7_init.tcl | -> <app.elf> \[<cable serial>\]"
    exit 2
}
lassign $argv bit ps7init elf serial
set onCable [expr {$serial ne "" ? " && jtag_cable_serial == \"$serial\"" : ""}]
foreach f [list $bit $ps7init $elf] {
    if {$f ne "-" && ![file isfile $f]} {
        puts stderr "prog_ps.tcl: no such file: $f"
        exit 1
    }
}

# Select a target by filter, retrying for up to 4 s: right after connect,
# hw_server already lists the targets but fills in their cable properties
# (jtag_cable_serial) a moment later, so a filter on the cable can miss
# at first (seen on the Arty, 2026-10-06).
proc selectTarget {filter} {
    for {set i 0} {$i < 20} {incr i} {
        if {![catch {targets -set -nocase -filter $filter} err]} {
            return ""
        }
        after 200
    }
    return $err
}

connect
if {[set err [selectTarget "name =~ \"APU*\"$onCable"]] ne ""} {
    puts stderr "prog_ps.tcl: no APU target over JTAG ($err). Is the board on and\
                 J17 (PROG) connected? See docs/TOOLCHAIN.md for USB access."
    exit 1
}
rst -system
after 1000
# Halt core 0 before anything else: after the reset the boot ROM boots
# from whatever the boot-mode jumpers select, and on a board set to QSPI
# (the Arty's JP4 as shipped, with Digilent's demo in the flash) that
# program is running, so ps7_init's memory reads fail ("Cannot read
# memory if not stopped", the Arty, 2026-10-06) and its loader could
# configure the PL behind our back. In JTAG mode the core is already
# halted, and stop's error is ignored.
if {[set err [selectTarget "name =~ \"*A9*#0\"$onCable"]] ne ""} {
    puts stderr "prog_ps.tcl: no Cortex-A9 #0 target ($err)"
    exit 1
}
catch {stop}
puts "prog_ps.tcl: programming the PL with [file tail $bit]"
if {$serial ne ""} {
    # the PL on this cable, not the first one xsdb finds
    if {[set err [selectTarget "name =~ \"xc7z*\"$onCable"]] ne ""} {
        puts stderr "prog_ps.tcl: no PL target on cable $serial ($err)"
        exit 1
    }
}
fpga -file $bit
if {[set err [selectTarget "name =~ \"*A9*#0\"$onCable"]] ne ""} {
    puts stderr "prog_ps.tcl: no Cortex-A9 #0 target ($err)"
    exit 1
}
if {$ps7init ne "-"} {
    source $ps7init
    ps7_init
    ps7_post_config
} else {
    puts "prog_ps.tcl: no ps7_init here, the app runs it"
}
puts "prog_ps.tcl: loading [file tail $elf]"
dow $elf
con
after 200
puts "prog_ps.tcl: OK, [file tail $elf] running on core 0"
disconnect
exit 0
