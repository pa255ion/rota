# The Zynq PS7 with the HP0 port, for side track C's DDR test (M0e,
# boards/zedboard/m0e_zynq.v; sourced by build.tcl, -ip ps7_hp.tcl).
#
# The shared configuration (ps7.tcl: EMIO GPIO for the PS link, UART1,
# SD 0, the ZedBoard's MIO and DDR) unchanged, plus S_AXI_HP0: 64 bits,
# clocked by the PL (S_AXI_HP0_ACLK = the pixel clock; DS187 v1.21 table
# 19 allows any rate up to 250 MHz). Kept apart from ps7.tcl until side
# track C merges, because enabling HP0 adds ports every wrapper would
# have to tie off (CLAUDE.md, "Parallel sessions").
#
# The HP port's own registers (AFI, 64-bit mode by default) and the DDR
# controller's port arbitration are left at the values the ZedBoard
# ps7_init sets (Vivado's sample, as for every system).

source [file join [file dirname [info script]] ps7.tcl]

set_property -dict [list \
    CONFIG.PCW_USE_S_AXI_HP0 {1} \
    CONFIG.PCW_S_AXI_HP0_DATA_WIDTH {64} \
] [get_ips zynq_ps]
