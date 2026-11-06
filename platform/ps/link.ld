/* Bare-metal Cortex-A9 image in on-chip memory (OCM). The low 192 KiB of
 * OCM are mapped at 0x00000000, both after ps7_init (make prog: xsdb loads
 * the ELF there and jumps to _start) and when the boot ROM hands over to
 * a first-stage program (side track F: BOOT.BIN on the SD card; UG585
 * 6.3.12). Everything, stacks included, must fit those 192 KiB (the
 * image alone is limited to 192 KB, UG585 6.3). DDR is only used once
 * ps7_init has run (USB DMA memory). The exception vectors (relay.zig)
 * sit at 0x0, where VBAR points. Since M1 (side K, 2026-11-02) main turns
 * the MMU and the L1 caches on after ps7_init (mmu.zig: the OCM, .ddr and
 * the DDR .bss cached, the rest strongly ordered as before); the app is
 * still built with +strict_align (Makefile). The app's .bss lives in DDR
 * (bss_ddr, cleared by main after ps7_init), all but ps7_init.c's, which
 * stays in the OCM with what _start clears. */
/* The entry is the reset vector at 0x0 (a branch to _start): the boot
 * header's start of execution must be 64-byte aligned (UG585 table 6-5),
 * and bootgen takes it from here. */
ENTRY(_vectors)
MEMORY {
  ocm (rwx) : ORIGIN = 0x00000000, LENGTH = 192K
  /* the PS app's data in DDR (docs/ARCHITECTURE.md, "DDR regions in use";
     side L, K agreed 2026-10-11): big buffers that don't fit the OCM */
  ddr (rw)  : ORIGIN = 0x00200000, LENGTH = 1M
  /* the app's .bss (M1): cleared by main once ps7_init has run */
  bss_ddr (rw) : ORIGIN = 0x00300000, LENGTH = 1M
}
SECTIONS {
  .text : { KEEP(*(.vectors)) KEEP(*(.text._start)) *(.text*) } > ocm
  .rodata : { *(.rodata*) } > ocm
  .data : { *(.data*) } > ocm
  /* ps7_init.c's .bss (if any) in the OCM: it runs before DDR is up */
  .bss (NOLOAD) : { __bss_start = .; *ps7_init*(.bss .bss.* COMMON) __bss_end = .; } > ocm
  . = ALIGN(16);
  /* 16 KiB of stack (frames are up to 2 KiB; _start paints it and the
     relay reports its high-water mark over the UART), then 1 KiB for the
     exception handlers */
  __stack_bottom = .;
  . = . + 16K;
  __stack_top = .;
  . = . + 1K;
  __fault_stack_top = .;
  /* fails the link if the stacks don't fit the OCM */
  ASSERT(. <= 192K, "the PS app's image, .bss and stacks exceed the 192 KiB of OCM")
  /* NOLOAD: not in BOOT.BIN nor loaded by xsdb, and nothing zeroes it at
     start: every variable here (linksection(".ddr")) is written before
     it is read. DDR is set up by ps7_init before the app starts (FSBL or
     xsdb), and uncached (MMU off): no hot loops over it. lld still
     gives it an empty PT_LOAD: bootgen leaves it out of BOOT.BIN, and
     xsdb's dow (after ps7_init) zeroing it is harmless (K, 2026-10-11). */
  .ddr (NOLOAD) : { __ddr_start = .; *(.ddr*) __ddr_end = .; } > ddr
  ASSERT(__ddr_end - __ddr_start <= 1M, "the PS app's data in DDR exceeds its 1 MiB")
  /* the rest of .bss: in DDR, zeroed by main after ps7_init (relay.zig) */
  .bss_ddr (NOLOAD) : { __bssd_start = .; *(.bss*) *(COMMON) . = ALIGN(4); __bssd_end = .; } > bss_ddr
  ASSERT(__bssd_end - __bssd_start <= 1M, "the PS app's .bss exceeds its 1 MiB of DDR")
  /DISCARD/ : { *(.ARM.exidx*) *(.ARM.attributes) }
}
