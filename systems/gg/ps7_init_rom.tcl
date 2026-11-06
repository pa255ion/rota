# The Game Gear's ROM into DDR over JTAG (make gg-prog, M5 step 5.10's
# board check while the PS app doesn't know the Game Gear yet; step 5.9's
# PS part waits for side track I). make gg-prog writes a small script that
# sets gg_ps7_init (the board's real ps7_init.tcl) and gg_rom (the ROM)
# and sources this file; prog_ps.tcl sources that script as its ps7_init.
#
# ps7_init sets up DDR; ps7_post_config releases the PL's reset
# (FCLK_RESET0_N, the Game Gear's PS_RESETN). The ROM goes into DDR at the
# media base (Gg.Machine.romBase) between the two, so the machine's first
# fetch already finds it. Read back and compared before the reset is
# released: a bad write stops here instead of showing as a broken game.

source $gg_ps7_init
rename ps7_post_config gg_real_ps7_post_config

proc ps7_post_config {} {
    global gg_rom
    set base 0x02000000
    set size [file size $gg_rom]
    puts "ps7_init_rom.tcl: [file tail $gg_rom] ($size bytes) into DDR at $base"
    dow -data $gg_rom $base
    set f [open $gg_rom rb]
    set want [read $f]
    close $f
    set n [expr {$size / 4}]
    binary scan $want "i$n" words
    set got [mrd -value $base $n]
    for {set i 0} {$i < $n} {incr i} {
        set w [expr {[lindex $words $i] & 0xFFFFFFFF}]
        set g [expr {[lindex $got $i] & 0xFFFFFFFF}]
        if {$w != $g} {
            puts stderr [format "ps7_init_rom.tcl: DDR read-back differs at 0x%08X: 0x%08X, want 0x%08X" \
                [expr {$base + 4 * $i}] $g $w]
            exit 1
        }
    }
    puts "ps7_init_rom.tcl: read back, identical"
    gg_real_ps7_post_config
}
