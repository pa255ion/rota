# Third-party files and data

Everything not listed here is this project's own work, under the MIT
licence in [LICENSE](LICENSE).

## In this repository

### Digilent board files (MIT)

`boards/arty-z7-20/reference/digilent-board-files/` (`board.xml`,
`part0_pins.xml`, `preset.xml`) are Digilent's Vivado board files for the
Arty Z7-20, copied unchanged with their MIT licence header (Copyright (c)
2021 Digilent, Inc.).

`boards/arty-z7-20/reference/arty_z7_20_master.xdc` is Digilent's master
constraints file for the Arty Z7-20, from
<https://github.com/Digilent/digilent-xdc> (MIT licence). The board's
own constraints files in `boards/arty-z7-20/` take their pins from it.

### SameBoy's CGB palette tables (Expat)

The Game Boy Color's palettes for original Game Boy cartridges
(`platform/ps/gbc_palette.zig`: the title checksums, the palette
assignments and the palettes themselves) are copied as data from
SameBoy's open boot ROM, `BootROMs/cgb_boot.asm`
(<https://github.com/LIJI32/SameBoy>):

    Expat License

    Copyright (c) 2015-2026 Lior Halphon

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

### font8x8 (public domain)

The 8×8 text font (`platform/src/Platform/Video/Font8x8.hs`) is Daniel
Hepper's font8x8 (<https://github.com/dhepper/font8x8>), public domain,
itself based on IBM's public domain VGA fonts.

## Not in this repository

- **The ZedBoard's master constraints file** (`zedboard_master.xdc`) is
  Avnet's, and its terms don't allow publishing it. Download it yourself
  into `boards/zedboard/reference/` for `make check-xdc` to check the
  ZedBoard's pins; without it, that check is skipped.
- **Test ROMs, homebrew and reference models** are downloaded by
  `tools/fetch-roms.sh`, each from its authors' own release pages and
  under its own licence. That includes Blargg's tests, mooneye, the
  acid2 tests, AccuracyCoin, SingleStepTests, Acid800, AltirraOS (from
  the atari800 project) and the homebrew games listed in the script.
  Nuked-OPLL (GPL-2.0) is downloaded only to build a test oracle,
  `make opll-oracle`. No code of it is in this repository; the FM model
  takes values, not code, cross-checked with emu2413 (MIT).
- **Hardware facts** (register values, timings) come from public
  documentation: Analog Devices' ADV7511 guides, Pan Docs, the nesdev
  wiki, the Altirra Hardware Reference Manual, SMS Power, fullsnes and
  others, as the docs cite them. Data taken from other projects is
  listed above.
