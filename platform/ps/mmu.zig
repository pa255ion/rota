//! The Cortex-A9's MMU and L1 caches (side track K, PLAN.md's M1, the
//! user 2026-11-02): a flat first-level table of 1 MiB sections, every
//! virtual address its physical one. Only the CPU's own memory is cached
//! (normal, write-back, write-allocate): the OCM's low megabyte (code,
//! .rodata, .data, OCM .bss, the stacks), the PS app's `.ddr` (0x00200000)
//! and its `.bss` in DDR (0x00300000). Everything else stays strongly
//! ordered, as with the MMU off before: the USB host's DMA memory
//! (0x00100000), the running system's media, the screenshot and the
//! bitstream buffers (the PL and the DMA engines read and write them), the
//! PL's AXI ranges and the PS's registers. So no DMA path needs cache
//! maintenance or a new barrier (docs/ARCHITECTURE.md, "DDR regions in
//! use"). The L2 cache (PL310) stays off.
//!
//! Sources: ARMv7-A ARM (DDI 0406C) B3.5 (short-descriptor sections),
//! B4.1 (SCTLR, TTBR0, TTBCR, DACR), B4.2 (cache and TLB maintenance);
//! Cortex-A9 TRM (DDI 0388) 4.3 (the L1 data cache: 32 KiB, 4 ways, 32-byte
//! lines); Zynq-7000 TRM (UG585) 3.2 and 4.1 (the cache and the memory map).

const std = @import("std");

/// The first-level table: 4096 sections, 16 KiB aligned (B3.5.4), in the
/// app's DDR data (`.ddr`, after ps7_init); built once by `enable`.
var table: [4096]u32 align(16384) linksection(".ddr") = undefined;

/// Section descriptors (B3.5.1): bits 1:0 = 10, B (2), C (3), XN (4), the
/// domain (8:5, 0), AP[1:0] (11:10) = 11 (full access), TEX (14:12).
pub const normal_wb: u32 = 0x1C0E; // TEX 001, C, B: write-back, write-allocate
pub const strongly_ordered: u32 = 0x0C12; // TEX 000, C 0, B 0, XN

/// The megabytes the CPU's own memory takes, cached: the OCM mapped low,
/// the app's `.ddr`, its `.bss` in DDR (link.ld).
pub fn cached(mb: u32) bool {
    return mb == 0x000 or mb == 0x002 or mb == 0x003;
}

/// One section's descriptor: its base address and attributes.
pub fn descriptor(mb: u32) u32 {
    return (mb << 20) | (if (cached(mb)) normal_wb else strongly_ordered);
}

/// The table, then the MMU, the L1 instruction and data caches and the
/// branch predictor on. Called once, from `main`, after ps7_init (the
/// table lives in DDR) and after the DDR `.bss` is cleared.
pub fn enable() void {
    for (&table, 0..) |*e, i| e.* = descriptor(@intCast(i));
    const base = @intFromPtr(&table);
    asm volatile (
    // the data cache's contents are unknown after reset: invalidate it by
    // set and way (256 sets, 4 ways: DCISW, way in 31:30, set in 12:5)
        \\ mov r2, #0
        \\1:
        \\ mov r3, #0
        \\2:
        \\ orr r1, r3, r2
        \\ mcr p15, 0, r1, c7, c6, 2
        \\ add r3, r3, #0x20
        \\ cmp r3, #0x2000
        \\ blo 2b
        \\ add r2, r2, #0x40000000
        \\ cmp r2, #0
        \\ bne 1b
        // the instruction cache, the branch predictor and the TLBs
        \\ mov r1, #0
        \\ mcr p15, 0, r1, c7, c5, 0
        \\ mcr p15, 0, r1, c7, c5, 6
        \\ mcr p15, 0, r1, c8, c7, 0
        \\ dsb
        // TTBCR 0 (TTBR0 alone), TTBR0 the table (walks non-cacheable),
        // DACR: domain 0 a client (the descriptors' AP checked)
        \\ mcr p15, 0, r1, c2, c0, 2
        \\ mcr p15, 0, %[base], c2, c0, 0
        \\ mov r1, #1
        \\ mcr p15, 0, r1, c3, c0, 0
        \\ isb
        // SCTLR: M (0), C (2), Z (11), I (12); A (1) stays clear
        \\ mrc p15, 0, r1, c1, c0, 0
        \\ ldr r3, =0x1805
        \\ orr r1, r1, r3
        \\ mcr p15, 0, r1, c1, c0, 0
        \\ dsb
        \\ isb
        :
        : [base] "r" (base),
        : .{ .r1 = true, .r2 = true, .r3 = true, .memory = true });
}

/// Whether the MMU is on (SCTLR.M), for the start-up line.
pub fn on() bool {
    const sctlr = asm volatile ("mrc p15, 0, %[r], c1, c0, 0"
        : [r] "=r" (-> u32),
    );
    return sctlr & 1 != 0;
}

test "the cached megabytes and the attributes" {
    try std.testing.expectEqual(@as(u32, 0x0000_1C0E), descriptor(0));
    try std.testing.expectEqual(@as(u32, 0x0010_0C12), descriptor(1)); // the USB host's DMA memory
    try std.testing.expectEqual(@as(u32, 0x0020_1C0E), descriptor(2));
    try std.testing.expectEqual(@as(u32, 0x0030_1C0E), descriptor(3));
    try std.testing.expectEqual(@as(u32, 0x0200_0C12), descriptor(0x020)); // the system's media
    try std.testing.expectEqual(@as(u32, 0x4000_0C12), descriptor(0x400)); // the PL's AXI GP0
    try std.testing.expectEqual(@as(u32, 0xE000_0C12), descriptor(0xE00)); // the PS's peripherals
}
