# Out-of-context implementation of one Clash top (M7 step 7.6: the PPU,
# Snes.PpuOoc): no pins, the 25.175 MHz pixel clock its only constraint,
# on the Arty Z7-20's part (xc7z020clg400-1, as boards/arty-z7-20/build.tcl).
# Synthesis, then placement and routing, so the worst path is the routed
# one. Run by `make snes-ooc` (in the queue's bit slot), from its output
# directory. Its numbers go in docs/systems/snes.md.
#
# Arguments: the Clash output directory, the top's name, the reports'
# directory, and "hier" to keep the hierarchy (each Clash component its
# own row in utilization-hier.txt; a little bigger than flattened).

lassign $argv hdl top out mode
set part xc7z020clg400-1
file mkdir $out

read_verilog [glob [file join $hdl *.v]]
if {$mode eq "hier"} {
  synth_design -top $top -part $part -mode out_of_context -flatten_hierarchy none
} else {
  synth_design -top $top -part $part -mode out_of_context
}
create_clock -name clk -period 39.720 [get_ports CLK]
report_utilization -file [file join $out synth-utilization.txt]
report_utilization -hierarchical -file [file join $out synth-utilization-hier.txt]
# (a design over the part's size, as the whole machine with its hierarchy
# kept: the synthesis reports only)
if {[info exists ::env(OOC_SYNTH_ONLY)]} { puts "ooc.tcl: OK"; return }
opt_design
place_design
route_design
report_utilization -file [file join $out utilization.txt]
report_utilization -hierarchical -file [file join $out utilization-hier.txt]
report_timing_summary -file [file join $out timing.txt]
report_timing -max_paths 10 -nworst 1 -file [file join $out paths.txt]
puts "ooc.tcl: OK"
