# The Zynq PS7 of the Arty Z7-20 as a Vivado IP (side track G phase 2),
# the counterpart of boards/zedboard/ps7.tcl: sourced by build.tcl (-ip
# ps7.tcl) inside its throwaway IP project, $ipDir is where IP goes, and
# by ps7_init_gen.tcl, which has Vivado write this configuration's
# ps7_init.c/.h/.tcl (the ZedBoard uses Vivado's "zed" sample; there is
# none for the Arty). Module name: zynq_ps, as on the ZedBoard, with the
# same PL-facing ports, so wrappers have the same shape.
#
# 1. Digilent's PS7 preset for this board, as Vivado would apply it from
#    the board file (reference/digilent-board-files/preset.xml,
#    "ps7_preset", all 771 parameters): 50 MHz crystal, CPU 650 MHz,
#    DDR3 MT41J256M16 on a 16-bit bus at 525 MHz with Digilent's measured
#    trace delays, UART0 on MIO 14/15, SD 0 on MIO 40..45 with card
#    detect on MIO 47, USB 0 on MIO 28..39 with the PHY's reset on
#    MIO 46, Ethernet, QSPI; banks 500/501 at 3.3/1.8 V.
# 2. Then ours, as on the ZedBoard: 64 EMIO GPIO lines (the PS link,
#    Platform.Ps.Link), no GP0 master and no fabric clock or reset ports
#    (the PL has its own clock), and the UART reference at 50 MHz instead
#    of the preset's 100, so the PS app's baud divisors are the same on
#    both boards (platform/ps/hw.zig; IO PLL 1000 MHz / 20).
#
# Digilent's delays for DQS lanes 2 and 3 are slightly negative (-0.009,
# -0.033 ns), which Vivado flags as critical warnings PSU-1/PSU-2 ("might
# fail"); build.tcl fails on any critical warning. Those lanes don't exist
# on the 16-bit bus, so the values are kept and only those two IDs are
# lowered to warnings.

set_msg_config -id {PSU-1} -new_severity {WARNING}
set_msg_config -id {PSU-2} -new_severity {WARNING}

create_ip -name processing_system7 -vendor xilinx.com -library ip \
    -module_name zynq_ps -dir $ipDir

set presetFile [file join [file dirname [file normalize [info script]]] \
    reference digilent-board-files preset.xml]
set f [open $presetFile]
set xml [read $f]
close $f
set from [string first {preset_proc_name="ps7_preset"} $xml]
set to [string first {</ip_preset>} $xml $from]
set preset {}
foreach {_ n v} [regexp -all -inline {<user_parameter name="([^"]+)" value="([^"]*)"} \
                     [string range $xml $from $to]] {
    lappend preset $n $v
}
if {[llength $preset] == 0} {
    error "ps7.tcl: no ps7_preset parameters in $presetFile"
}
set_property -dict $preset [get_ips zynq_ps]

# The SD card's write protect: the ZedBoard's socket switch is on MIO 46,
# the Arty has none (and MIO 46 is its USB PHY's reset). platform/ps/
# sd.zig refuses writes while the controller reports protection (SD host
# spec 2.2.9 bit 19), so the input is made explicit rather than left to
# whatever the preset's "no WP" (an unconnected EMIO select, SD_WP_CD_SEL
# 0x37) feeds it: routed through EMIO as the IP's SDIO0_WP port, active
# high (1 = protected: tying it high made the card read-only on the
# board, 2026-10-06), which every Arty wrapper ties low (writable).
set_property -dict [list \
    CONFIG.PCW_SD0_GRP_WP_ENABLE {1} \
    CONFIG.PCW_SD0_GRP_WP_IO {EMIO} \
] [get_ips zynq_ps]

set_property -dict [list \
    CONFIG.PCW_GPIO_EMIO_GPIO_ENABLE {1} \
    CONFIG.PCW_GPIO_EMIO_GPIO_IO {64} \
    CONFIG.PCW_UART_PERIPHERAL_FREQMHZ {50} \
    CONFIG.PCW_USE_M_AXI_GP0 {0} \
    CONFIG.PCW_EN_CLK0_PORT {0} \
    CONFIG.PCW_EN_RST0_PORT {1} \
] [get_ips zynq_ps]

# FCLK_RESET0_N (enabled above, EN_RST0_PORT) is every Arty PS design's
# reset (the wrappers' PS_RESETN): the PS holds it while the PL is
# configured and releases it only after the PS-PL level shifters are on
# (ps7_post_config, and pcap.zig after a PCAP load). Without it a design
# that starts an AXI read on HP0 the moment it comes up could do so
# while only the PS-to-PL direction is on: it sees ARREADY, the PS never
# sees ARVALID, and the read waits forever (the Game Boy's white screen
# after a power-on, the Arty, 2026-10-06).
