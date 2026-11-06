# Out-of-context implementation of the Clash 68000 (side track P, P.4's
# last gate; `make m68k-ooc`): cpus/m68k/hdl/M68kTop.hs's Verilog, no pins,
# a 50 MHz clock its only constraint (the slack gives the Fmax: a
# transaction is four clocks, an 8 MHz 68000 needs 8), on the Arty Z7-20's
# part (xc7z020clg400-1). Synthesis, then placement and routing, so the
# worst path is the routed one. Run in the queue's bit slot, from its
# output directory; the numbers go in docs/cpus/m68k.md.
#
# Arguments: the Clash output directory, the reports' directory, the
# top's name (m68k_top, or P.4g's m68k_top2), and "hier" to keep the
# hierarchy (a row per Clash component in utilization-hier.txt).

lassign $argv hdl out top mode
if {$top eq ""} { set top m68k_top }
set part xc7z020clg400-1
file mkdir $out

read_verilog [glob [file join $hdl *.v]]
if {$mode eq "hier"} {
  synth_design -top $top -part $part -mode out_of_context -flatten_hierarchy none
} else {
  synth_design -top $top -part $part -mode out_of_context
}
create_clock -name clk -period 20.000 [get_ports clk]
report_utilization -file [file join $out synth-utilization.txt]
opt_design
place_design
route_design
report_utilization -file [file join $out utilization.txt]
report_utilization -hierarchical -file [file join $out utilization-hier.txt]
report_timing_summary -file [file join $out timing.txt]
report_timing -max_paths 10 -nworst 1 -file [file join $out paths.txt]
puts "ooc.tcl: OK"
