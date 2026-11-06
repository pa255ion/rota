/* The Clash 68000 as a C library (emulation_clash, side track P, P.5;
 * docs/cpus/m68k.md): Verilator's model of the core, bus-cycle exact
 * (checked against the reference model over Tom Harte's SingleStepTests
 * and random programs), its bus answered by callbacks. Link with
 * libm68k.so (make m68k-sim: build/m68k/sim/lib, its include/).
 *
 *   m68k *m = m68k_new(read, write, user);
 *   m68k_reset(m);                  // SSP from address 0, PC from 4
 *   m68k_run(m, 1000000);           // until STOP, or about a million clocks
 *   printf("%llu clocks, %llu instructions, D0 %u\n",
 *          m68k_clocks(m), m68k_instructions(m), m68k_reg(m, 0));
 *   m68k_free(m);
 *
 * Clocks are the CPU's (a bus cycle four, no wait states), counted from
 * the first instruction after the reset; instructions don't include
 * interrupt or trace processing (counted in the clocks).
 */
#ifndef M68K_H
#define M68K_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct M68k m68k;

/* A read at a 24-bit address with its function code (1 user data, 2 user
 * program, 5 supervisor data, 6 supervisor program); a byte read returns
 * the byte in the low 8 bits. */
typedef uint16_t (*m68k_read_fn)(void *user, uint32_t addr, uint8_t fc, bool is_byte);
/* A write; a byte write's value is in the low 8 bits. */
typedef void (*m68k_write_fn)(void *user, uint32_t addr, uint8_t fc, bool is_byte, uint16_t value);
/* The interrupt acknowledge for a level: the vector number (0-255), or -1
 * for the autovector (24 + level). */
typedef int32_t (*m68k_iack_fn)(void *user, uint8_t level);

m68k *m68k_new(m68k_read_fn read, m68k_write_fn write, void *user);
void m68k_free(m68k *m);
/* Without one, every interrupt takes its autovector. */
void m68k_set_iack(m68k *m, m68k_iack_fn iack);
/* The level on IPL2-IPL0: 0 none, 1-6 against SR's mask, 7 non-maskable. */
void m68k_set_ipl(m68k *m, uint8_t level);

/* The reset: supervisor mode, SSP and PC from the vectors, the counts
 * zeroed. */
void m68k_reset(m68k *m);
/* One instruction (with an interrupt's processing before it): its clocks. */
uint64_t m68k_step(m68k *m);
/* Until STOP (with no interrupt to wake it), a halt (a double fault), or
 * the first instruction start at or past `limit` clocks: the clocks run. */
uint64_t m68k_run(m68k *m, uint64_t limit);

uint64_t m68k_clocks(m68k *m);
uint64_t m68k_instructions(m68k *m);
bool m68k_stopped(m68k *m);
bool m68k_halted(m68k *m);
/* 0-7 D0-D7, 8-15 A0-A7 (A7 the active stack pointer), 16 USP, 17 SSP,
 * 18 SR, 19 PC, 20 IR. */
uint32_t m68k_reg(m68k *m, uint8_t n);

#ifdef __cplusplus
}
#endif

#endif
