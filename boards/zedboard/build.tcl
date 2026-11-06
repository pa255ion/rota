# Non-project-mode Vivado build for the ZedBoard: Clash output -> .bit.
#
#   vivado -mode batch -source boards/zedboard/build.tcl -tclargs \
#       [-top <module>] [-wrapper <file.v>]... [-ip <file.tcl>]... \
#       <clash_top_dir> <out_dir> <clash_connector.tcl> <xdc>...
#
# -top       synthesis top (default: Clash's top entity); with -wrapper, the
#            hand-written board wrapper around it, e.g. chip8_zynq
# -wrapper   extra hand-written Verilog (boards/zedboard/*.v)
# -ip        Tcl that creates more IP (for example ps7.tcl); it runs in the
#            same throwaway project as Clash's IP, with $ipDir set
#
# clash_top_dir   directory holding the top entity's clash-manifest.json
#                 (build/<system>/verilog/<Module>.topEntity)
# out_dir         build/<system>/vivado; reports go to <out_dir>/reports
# clash_connector clashConnector.tcl from clash-lib's data files (make hdl
#                 copies it next to the Verilog, see the Makefile)
# xdc             board constraints, at least boards/zedboard/zedboard.xdc
#
# Normally run through `make bit SYSTEM=<name>`. The build fails (non-zero
# exit) on: any critical warning, setup or hold slack < 0, unconstrained
# paths reported by check_timing, or DRC errors (write_bitstream refuses).

# Another board's build.tcl (boards/arty-z7-20/build.tcl) sets its part
# and sources this file.
if {![info exists part]} {
    set part xc7z020clg484-1
}

set topOverride ""
set wrappers {}
set ipScripts {}
while {[llength $argv] && [string match -* [lindex $argv 0]]} {
    set argv [lassign $argv opt val]
    switch -- $opt {
        -top     { set topOverride $val }
        -wrapper { lappend wrappers $val }
        -ip      { lappend ipScripts $val }
        default  { puts stderr "build.tcl: unknown option $opt"; exit 2 }
    }
}
if {[llength $argv] < 4} {
    puts stderr "usage: build.tcl \[-top m\] \[-wrapper f.v\]... \[-ip f.tcl\]... <clash_top_dir> <out_dir> <clash_connector.tcl> <xdc>..."
    exit 2
}
lassign $argv topDir outDir connector
set xdcFiles [lrange $argv 3 end]
set reports [file join $outDir reports]
file mkdir $outDir $reports [file join $outDir ip]

proc fail msg {
    puts stderr "build.tcl: FAIL: $msg"
    exit 1
}

# --- Clash output and IP -------------------------------------------------
source $connector
clash::readMetadata $topDir
set top $clash::topEntity

# IP (Clash's clock wizard, plus any -ip scripts such as ps7.tcl) is created
# and generated in this in-memory project, which then also runs synthesis.
# The part is given explicitly so the IP is generated for the ZedBoard's
# device. (Closing the project and reading the .xci files into a fresh one,
# as clash::createAndReadIp does, crashed Vivado 2026.1 once a PS7 IP was
# among them.)
create_project -in_memory -part $part
set ipDir [file join $outDir ip]
clash::createIp -dir $ipDir
foreach ipScript $ipScripts {
    source $ipScript
}
set ips [get_ips]
if {[llength $ips]} {
    set_property GENERATE_SYNTH_CHECKPOINT false [get_files [get_property IP_FILE $ips]]
    generate_target synthesis $ips
}

# Memory images loaded by $readmemb (Clash's blockRamFile) are found
# relative to the working directory, which is $outDir.
foreach mem [glob -nocomplain -directory $topDir *.bin] {
    file copy -force $mem $outDir
    puts "build.tcl: memory image [file tail $mem]"
}

clash::readHdl
foreach w $wrappers {
    read_verilog $w
}
if {$topOverride ne ""} {
    set top $topOverride
}
# Clash's <top>.sdc carries create_clock for the input clock ports.
clash::readXdc {early normal late}
foreach xdc $xdcFiles {
    read_xdc $xdc
}

# --- Synthesis and implementation ---------------------------------------
synth_design -top $top -part $part

# Synchroniser flops: Platform.Input.Sync names them sync_ff1/sync_ff2,
# Clash's resetSynchronizer (inside Platform.Clocks) resetSynchronizer*.
# ASYNC_REG keeps each chain in one slice and out of timing optimisation.
set syncFfs [get_cells -quiet -hierarchical -filter \
    {IS_SEQUENTIAL && (NAME =~ *sync_ff* || NAME =~ *resetSynchronizer*)}]
if {[llength $syncFfs]} {
    set_property ASYNC_REG TRUE $syncFfs
}
puts "build.tcl: ASYNC_REG on [llength $syncFfs] synchroniser flop(s)"

write_checkpoint -force [file join $outDir post_synth.dcp]
report_utilization -file [file join $reports utilization_synth.rpt]

opt_design
place_design
phys_opt_design
route_design
write_checkpoint -force [file join $outDir post_route.dcp]

# --- Reports and gates --------------------------------------------------
report_timing_summary -max_paths 20 -file [file join $reports timing_summary.rpt]
report_utilization                -file [file join $reports utilization.rpt]
report_clock_interaction          -file [file join $reports clock_interaction.rpt]
report_clocks                     -file [file join $reports clocks.rpt]
report_io                         -file [file join $reports io.rpt]
report_drc                        -file [file join $reports drc.rpt]
check_timing -verbose             -file [file join $reports check_timing.rpt]

set wns [get_property SLACK [get_timing_paths -delay_type max]]
set whs [get_property SLACK [get_timing_paths -delay_type min]]
puts "build.tcl: WNS = $wns ns, WHS = $whs ns"
if {$wns eq "" || $whs eq ""} {
    fail "no timed paths (missing clock constraint?)"
}
if {$wns < 0} { fail "setup timing not met (WNS = $wns ns)" }
if {$whs < 0} { fail "hold timing not met (WHS = $whs ns)" }

# check_timing categories that mean part of the design isn't timed.
foreach {check what} {
    no_clock            "registers without a clock"
    unconstrained_internal_endpoints "unconstrained internal endpoints"
    no_input_delay      "inputs without input delay or false path"
    no_output_delay     "outputs without output delay or false path"
    multiple_clock      "registers with multiple clocks"
    loops               "combinational loops"
} {
    # The report has a line "N. checking <check> (<count>)". For the I/O
    # delay checks that count also includes ports that have a false path
    # instead of a delay (our asynchronous buttons/switches and the LEDs,
    # by design), so for those use the "... with no input/output delay
    # specified" line, which counts only ports with neither.
    set rpt [check_timing -override_defaults $check -return_string]
    if {$check in {no_input_delay no_output_delay}} {
        set re {There are (\d+) (?:input )?ports with no (?:input|output) delay specified}
    } else {
        set re "checking $check \\((\\d+)\\)"
    }
    if {![regexp $re $rpt -> cnt]} {
        fail "check_timing: can't parse the $check result"
    }
    if {$cnt > 0} {
        fail "check_timing: $cnt $what (see reports/check_timing.rpt)"
    }
}

set crit [get_msg_config -count -severity {CRITICAL WARNING}]
if {$crit > 0} {
    fail "$crit critical warning(s); see the Vivado log"
}

# Compressed (the user, 2026-10-16: seven uncompressed bitstreams took 42%
# of the 64 MiB SD card image); PCAP and JTAG take it as it is (UG585 6.4).
set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]
write_bitstream -force [file join $outDir $top.bit]
puts "build.tcl: OK, wrote [file join $outDir $top.bit]"
