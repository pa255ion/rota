# The Zynq PS7 as a Vivado IP, for tops that use the PS (sourced by
# build.tcl, -ip ps7.tcl, inside its throwaway IP project; $ipDir is where
# IP goes). Module name: zynq_ps ("ps7" is taken by the PS7 primitive).
#
# Only what the PL design needs: 64 EMIO GPIO lines (0 to 31 PS -> PL, 32
# to 63 PL -> PS: the PS link, Platform.Ps.Link), and no AXI ports or
# fabric clocks. SD 0 (the SD card slot, MIO 40 to 45, CD on MIO 47, WP on
# MIO 46) is enabled here only for consistency: ps7_init sets it up, and
# the PS app drives it (platform/ps/sd.zig). The PS registers themselves
# (clocks, MIO, DDR, UART1, SD 0) are set at programming time by the ZedBoard ps7_init.tcl that
# ships with Vivado (data/embeddedsw/sample_sdt/zed/sdt/), not from this
# configuration; the MIO and DDR settings below just keep the two
# consistent (ZedBoard: bank 500 at 3.3 V, bank 501 at 1.8 V, UART1 on MIO
# 48/49, 512 MiB of MT41K128M16 on a 32-bit bus; docs/ZEDBOARD.md).

create_ip -name processing_system7 -vendor xilinx.com -library ip \
    -module_name zynq_ps -dir $ipDir
set_property -dict [list \
    CONFIG.PCW_PRESET_BANK0_VOLTAGE {LVCMOS 3.3V} \
    CONFIG.PCW_PRESET_BANK1_VOLTAGE {LVCMOS 1.8V} \
    CONFIG.PCW_CRYSTAL_PERIPHERAL_FREQMHZ {33.333333} \
    CONFIG.PCW_UIPARAM_DDR_PARTNO {MT41K128M16 JT-125} \
    CONFIG.PCW_UIPARAM_DDR_BUS_WIDTH {32 Bit} \
    CONFIG.PCW_UART1_PERIPHERAL_ENABLE {1} \
    CONFIG.PCW_UART1_UART1_IO {MIO 48 .. 49} \
    CONFIG.PCW_GPIO_EMIO_GPIO_ENABLE {1} \
    CONFIG.PCW_GPIO_EMIO_GPIO_IO {64} \
    CONFIG.PCW_SD0_PERIPHERAL_ENABLE {1} \
    CONFIG.PCW_SD0_SD0_IO {MIO 40 .. 45} \
    CONFIG.PCW_SD0_GRP_CD_ENABLE {1} \
    CONFIG.PCW_SD0_GRP_CD_IO {MIO 47} \
    CONFIG.PCW_SD0_GRP_WP_ENABLE {1} \
    CONFIG.PCW_SD0_GRP_WP_IO {MIO 46} \
    CONFIG.PCW_USE_M_AXI_GP0 {0} \
    CONFIG.PCW_EN_CLK0_PORT {0} \
    CONFIG.PCW_EN_RST0_PORT {0} \
] [get_ips zynq_ps]
