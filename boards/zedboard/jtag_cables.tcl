# List the JTAG cables hw_server sees, one "JTAG_CABLE <serial>" line each
# (side track G step 2). Read-only: connects, lists, disconnects; nothing
# is programmed or reset. Run by tools/jtag-pick.sh through tools/xsdb.sh,
# which picks the cable that make prog may use.
#
#   xsdb boards/zedboard/jtag_cables.tcl

proc cableSerials {} {
    set serials {}
    foreach t [targets -target-properties] {
        if {[dict exists $t jtag_cable_serial]} {
            set s [dict get $t jtag_cable_serial]
            if {$s ne ""} {
                lappend serials $s
            }
        }
    }
    return [lsort -unique $serials]
}

# hw_server finds the cables a moment after connect (right after another
# xsdb has exited, the first reading can come back empty), so read until
# two readings 300 ms apart agree and aren't empty, for up to 3 s.
connect
set prev [cableSerials]
for {set i 0} {$i < 10} {incr i} {
    after 300
    set serials [cableSerials]
    if {[llength $serials] && $serials eq $prev} {
        break
    }
    set prev $serials
}
foreach s $serials {
    puts "JTAG_CABLE $s"
}
disconnect
puts "jtag_cables.tcl: OK"
exit 0
