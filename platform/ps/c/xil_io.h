/* Empty stand-in for Xilinx's xil_io.h (side track F). Vivado's generated
 * ps7_init.c (Makefile PS7_INIT_C) includes it but uses nothing from it;
 * we link that file into the bare-metal PS app without Xilinx's BSP.
 * Hand-written C, the thin shim CLAUDE.md rule 9 allows. */
#ifndef XIL_IO_H
#define XIL_IO_H
#endif
