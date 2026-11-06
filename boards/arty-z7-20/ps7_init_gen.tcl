# Generate the Arty Z7-20's ps7_init (side track G phase 2, step 1):
# the PS7 IP of ps7.tcl, its output products, and from them Vivado's
# ps7_init.c, ps7_init.h (linked into the PS app for the SD boot) and
# ps7_init.tcl (run by xsdb in make prog). The ZedBoard takes these files
# from Vivado's "zed" sample; there is none for the Arty.
#
#   vivado -mode batch -source boards/arty-z7-20/ps7_init_gen.tcl -tclargs <out_dir>
#
# Normally run through `make ps7-init BOARD=arty-z7-20`. Also writes
# <out_dir>/zynq_ps_ports.txt (the IP's top-level ports, to compare with
# the ZedBoard's: the wrappers must fit both) and <out_dir>/clocks.txt
# (the clock frequencies Vivado actually achieved).

if {[llength $argv] != 1} {
    puts stderr "usage: ps7_init_gen.tcl <out_dir>"
    exit 2
}
set outDir [file normalize [lindex $argv 0]]
set ipDir [file join $outDir ip]
file delete -force $ipDir
file mkdir $ipDir

create_project -in_memory -part xc7z020clg400-1
source [file join [file dirname [file normalize [info script]]] ps7.tcl]
set ip [get_ips zynq_ps]
generate_target all $ip

set gen [file join $ipDir zynq_ps]
foreach f {ps7_init.c ps7_init.h ps7_init.tcl} {
    if {![file isfile [file join $gen $f]]} {
        puts stderr "ps7_init_gen.tcl: Vivado wrote no $f"
        exit 1
    }
    file copy -force [file join $gen $f] [file join $outDir $f]
}

# The IP's ports, from its instantiation template.
set veo [glob -nocomplain -directory $gen *.veo]
set out [open [file join $outDir zynq_ps_ports.txt] w]
if {[llength $veo]} {
    set f [open [lindex $veo 0]]
    foreach line [split [read $f] \n] {
        if {[regexp {^\s*\.(\w+)\(} $line -> port]} {
            puts $out $port
        }
    }
    close $f
}
close $out

set out [open [file join $outDir clocks.txt] w]
foreach p {CRYSTAL_PERIPHERAL_FREQMHZ ACT_APU_PERIPHERAL_FREQMHZ
           UIPARAM_DDR_FREQ_MHZ ACT_UART_PERIPHERAL_FREQMHZ ACT_SDIO_PERIPHERAL_FREQMHZ
           ACT_USB0_PERIPHERAL_FREQMHZ UIPARAM_DDR_BUS_WIDTH UIPARAM_DDR_PARTNO
           UART0_UART0_IO SD0_SD0_IO SD0_GRP_CD_IO USB0_USB0_IO USB0_RESET_IO} {
    if {![catch {get_property CONFIG.PCW_$p $ip} v]} {
        puts $out "PCW_$p = $v"
    }
}
close $out
puts "ps7_init_gen.tcl: OK, ps7_init.c/.h/.tcl in $outDir"
