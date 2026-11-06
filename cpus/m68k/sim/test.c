/* P.5's C check (make m68k-sim-test): a C program linked against
 * libm68k.so runs m68k-lib's golden program (the sum of 99 down to 0 in
 * D0, then STOP) and checks the model's counts: 203 instructions, 1820
 * clocks, D0 4950. C because it is the library's C users' view
 * (CLAUDE.md, rule 9: a test of the C ABI). */
#include <stdio.h>
#include <string.h>

#include "m68k.h"

static uint8_t ram[1 << 16];

static uint16_t rd(void *user, uint32_t addr, uint8_t fc, bool is_byte) {
  (void)user;
  (void)fc;
  addr &= 0xFFFF;
  return is_byte ? ram[addr] : (uint16_t)(ram[addr] << 8 | ram[(addr + 1) & 0xFFFF]);
}

static void wr(void *user, uint32_t addr, uint8_t fc, bool is_byte, uint16_t v) {
  (void)user;
  (void)fc;
  addr &= 0xFFFF;
  if (is_byte) {
    ram[addr] = (uint8_t)v;
  } else {
    ram[addr] = (uint8_t)(v >> 8);
    ram[(addr + 1) & 0xFFFF] = (uint8_t)v;
  }
}

int main(void) {
  static const uint8_t vectors[] = {0, 0, 0x80, 0, 0, 0, 0x10, 0};
  static const uint8_t sum[] = {0x70, 0x00, 0x72, 0x63, 0xD0, 0x81, 0x51, 0xC9,
                                0xFF, 0xFC, 0x4E, 0x72, 0x27, 0x00};
  memcpy(ram, vectors, sizeof vectors);
  memcpy(ram + 0x1000, sum, sizeof sum);
  m68k *m = m68k_new(rd, wr, NULL);
  if (!m) return 1;
  m68k_reset(m);
  m68k_run(m, 1000000);
  unsigned long long clocks = m68k_clocks(m), instr = m68k_instructions(m);
  uint32_t d0 = m68k_reg(m, 0), pc = m68k_reg(m, 19);
  bool stopped = m68k_stopped(m);
  m68k_free(m);
  printf("m68k-ctest: %llu clocks, %llu instructions, D0 %u, PC 0x%x, %s\n", clocks, instr, d0, pc,
         stopped ? "stopped" : "not stopped");
  if (clocks != 1820 || instr != 203 || d0 != 4950 || pc != 0x100E || !stopped) {
    printf("m68k-ctest: FAIL (want 1820 clocks, 203 instructions, D0 4950, PC 0x100e, stopped)\n");
    return 1;
  }
  printf("m68k-ctest: OK\n");
  return 0;
}
