# The Arty Z7-20's PS7 with the HP0 port (side track G phase 2, step 3:
# the Game Boy's cartridge in DDR), as boards/zedboard/ps7_hp.tcl: the
# board's configuration (ps7.tcl) plus S_AXI_HP0, 64 bits, clocked by the
# PL (S_AXI_HP0_ACLK = the pixel clock). Sourced by build.tcl (-ip
# ps7_hp.tcl).
#
# The HP port's own registers (AFI, 64-bit mode by default) are left at
# their reset values by the Arty's ps7_init, which make ps7-init
# generates from ps7.tcl (without HP0); the port needs nothing more, as
# on the ZedBoard, whose sample ps7_init doesn't touch them either.

source [file join [file dirname [file normalize [info script]]] ps7.tcl]

set_property -dict [list \
    CONFIG.PCW_USE_S_AXI_HP0 {1} \
    CONFIG.PCW_S_AXI_HP0_DATA_WIDTH {64} \
] [get_ips zynq_ps]
