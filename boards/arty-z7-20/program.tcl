# Program the Arty Z7-20's PL over JTAG: the ZedBoard's
# boards/zedboard/program.tcl (same argument), which only programs the
# xc7z020 on the chain. Normally run through
# `make prog SYSTEM=<name> BOARD=arty-z7-20`.

set progPort "the Arty's PROG/UART micro-USB port"
source [file join [file dirname [file normalize [info script]]] .. zedboard program.tcl]
