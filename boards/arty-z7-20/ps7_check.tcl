# Check the Arty Z7-20's generated ps7_init on the board (side track G
# phase 2, step 1's gate), over JTAG with xsdb, no PL design needed:
# system reset, ps7_init + ps7_post_config from build/arty-z7-20/ps7/,
# then the DDR controller's state and a DDR test across all 512 MiB:
#
#   * address lines: a distinct word at 0x00100000 + 2^b for b = 2..28
#     (and at the base), all written first, then all read back: a stuck
#     or shorted address line makes two of them alias;
#   * data lines: walking ones and zeros through all 32 bits at one word;
#   * a 16 KiB block of a pseudo-random pattern, written and read back.
#
# Below 0x00100000 is left alone (OCM and the boot ROM's area).
#
#   xsdb boards/arty-z7-20/ps7_check.tcl <ps7_init.tcl> [<cable serial>]
#
# Normally `make ps7-check BOARD=arty-z7-20` (layer 6: ask first), which
# picks the cable by serial like make prog. Volatile: power-cycling clears
# everything.

if {[llength $argv] < 1 || [llength $argv] > 2} {
    puts stderr "usage: ps7_check.tcl <ps7_init.tcl> \[<cable serial>\]"
    exit 2
}
lassign $argv ps7init serial
if {![file isfile $ps7init]} {
    puts stderr "ps7_check.tcl: no such file: $ps7init (make ps7-init BOARD=arty-z7-20)"
    exit 1
}
set onCable [expr {$serial ne "" ? " && jtag_cable_serial == \"$serial\"" : ""}]

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
    puts stderr "ps7_check.tcl: no APU target over JTAG ($err)"
    exit 1
}
rst -system
after 1000
if {[set err [selectTarget "name =~ \"*A9*#0\"$onCable"]] ne ""} {
    puts stderr "ps7_check.tcl: no Cortex-A9 #0 target ($err)"
    exit 1
}
catch {stop}
source $ps7init
puts "ps7_check.tcl: ps7_init -> [ps7_init]"
puts "ps7_check.tcl: ps7_post_config -> [ps7_post_config]"

set fails 0
proc check {what got want} {
    global fails
    if {$got != $want} {
        incr fails
        if {$fails <= 10} {
            puts stderr [format "ps7_check.tcl: FAIL %s: read 0x%08X, wrote 0x%08X" $what $got $want]
        }
    }
}

# DDR controller: mode_sts_reg (0xF8006054) bits 2:0, 1 = normal operation;
# ddrc_ctrl (0xF8006000) bit 0, soft reset released.
set ctrl [mrd -value 0xF8006000]
set mode [expr {[mrd -value 0xF8006054] & 7}]
puts [format "ps7_check.tcl: DDRC ctrl 0x%08X, operating mode %d (1 = normal)" $ctrl $mode]
if {($ctrl & 1) == 0 || $mode != 1} {
    puts stderr "ps7_check.tcl: FAIL the DDR controller is not in normal operation"
    exit 1
}

set base 0x00100000
# address lines
set addrs [list $base]
for {set b 2} {$b <= 28} {incr b} {
    lappend addrs [expr {$base + (1 << $b)}]
}
set i 0
foreach a $addrs {
    mwr $a [expr {0xA5000000 | $i}]
    incr i
}
set i 0
foreach a $addrs {
    check [format "address 0x%08X" $a] [mrd -value $a] [expr {0xA5000000 | $i}]
    incr i
}
puts "ps7_check.tcl: address lines: [llength $addrs] words up to 0x[format %08X [lindex $addrs end]]"

# data lines
set a [expr {$base + 0x40}]
for {set b 0} {$b < 32} {incr b} {
    foreach v [list [expr {1 << $b}] [expr {~(1 << $b) & 0xFFFFFFFF}]] {
        mwr $a $v
        check [format "data bit %d" $b] [mrd -value $a] $v
    }
}
puts "ps7_check.tcl: data lines: 64 patterns"

# a 16 KiB block
set n 4096
set blk [expr {$base + 0x01000000}]
set x 0x12345678
set words {}
for {set k 0} {$k < $n} {incr k} {
    set x [expr {($x * 1103515245 + 12345) & 0xFFFFFFFF}]
    lappend words $x
}
mwr $blk $words
set back [mrd -value $blk $n]
for {set k 0} {$k < $n} {incr k} {
    check [format "block word %d" $k] [lindex $back $k] [lindex $words $k]
}
puts "ps7_check.tcl: block: $n words at 0x[format %08X $blk]"

disconnect
if {$fails} {
    puts stderr "ps7_check.tcl: FAIL $fails mismatch(es)"
    exit 1
}
puts "ps7_check.tcl: OK, the Arty's ps7_init brings DDR up (512 MiB, 16 bit)"
exit 0
