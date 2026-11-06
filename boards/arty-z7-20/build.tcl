# Non-project-mode Vivado build for the Arty Z7-20: the ZedBoard's
# boards/zedboard/build.tcl (same arguments, same gates) with this board's
# part. Normally run through `make bit SYSTEM=<name> BOARD=arty-z7-20`.
#
# Part: xc7z020clg400-1, from Digilent's Vivado board file
# (reference/digilent-board-files/board.xml, part_name).

set part xc7z020clg400-1
source [file join [file dirname [file normalize [info script]]] .. zedboard build.tcl]
