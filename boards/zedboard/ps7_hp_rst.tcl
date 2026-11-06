# The Zynq PS7 with the HP0 port and the PS's PL reset, for the Game Boy's
# ZedBoard tops (boards/zedboard/gb_zynq.v, gbc_zynq.v; sourced by
# build.tcl, -ip ps7_hp_rst.tcl, from systems/gb/zedboard.mk and
# systems/gbc/zedboard.mk).
#
# ps7_hp.tcl unchanged, plus FCLK_RESET0_N (PCW_EN_RST0_PORT), the design's
# reset too (the wrappers' PS_RESETN, Platform.Clocks.pixelClockGenPs): the
# PS holds it while the PL is configured and releases it only after the
# PS-PL level shifters are on (ps7_post_config, and after a PCAP load).
# Without it the Game Boy's first HP0 read could start while only the
# PS-to-PL direction is on: it sees ARREADY, the PS never sees ARVALID, and
# the read waits forever (the white screen after a power-on, found on the
# Arty by side track G phase 3, which enabled it in the Arty's ps7.tcl).
# Its own file so that no other ZedBoard design changes (side track K
# agreed, 2026-10-10). ps7_init is unchanged.

source [file join [file dirname [info script]] ps7_hp.tcl]

set_property -dict [list \
    CONFIG.PCW_EN_RST0_PORT {1} \
] [get_ips zynq_ps]
