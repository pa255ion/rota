# Program the Arty Z7-20 for a design that uses the PS (xsdb, over JTAG):
# the ZedBoard's boards/zedboard/prog_ps.tcl (same arguments: bitstream,
# ps7_init.tcl, app, cable serial), which is not board-specific; make prog
# BOARD=arty-z7-20 passes the Arty's generated ps7_init.tcl (make
# ps7-init) and the PS app built for this board (-Dboard=arty-z7-20).

source [file join [file dirname [file normalize [info script]]] .. zedboard prog_ps.tcl]
