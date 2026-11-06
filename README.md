# rota

Retro game console emulators written in [Clash](https://clash-lang.org/)
(Haskell for hardware) for two Digilent Zynq-7000 boards, the **Arty
Z7-20** and the **ZedBoard**, plus the same models as desktop emulators.

On the boards, these are hardware re-implementations, not software
emulators. Each system is described in Haskell/Clash, compiled to
Verilog and synthesized into the Zynq's programmable logic. The ARM side
only helps: it loads games from the SD card, runs the menus and reads a
USB keyboard. It never emulates anything.

Every system starts as a pure Haskell reference model (its "spec"),
tested against public test ROMs. The hardware is then checked against
the spec cycle by cycle in simulation, before it ever reaches a board.

## Systems

| System | On the boards | On the desktop |
|---|---|---|
| CHIP-8 | yes | yes |
| Game Boy, Game Boy Color | yes, with battery saves and save states | yes |
| NES (mappers 0-5, 7; MMC3, MMC5) | yes | yes |
| Atari 800XL (XEX, cartridges, cassettes) | yes | yes, with the real-speed tape recorder |
| Sega Game Gear, Master System (with FM sound) | yes | yes, with a debugger |
| Super Nintendo | in progress | yes |

The shared CPUs are in `cpus/`: the 6502 (NES, 800XL), the Z80 (Game
Gear, Master System), the 65816 and SPC700 (SNES) and a 68000.

## The desktop emulator

`make desktop ROM=game.gb` runs any system's model in an SDL3 window, by
the file's extension. `make install` installs it as `rota`, with:

- a menu (Esc): speed, scaling, save states, input movies, screenshots;
- hold TAB to fast-forward, `[` and `]` to save and load a state;
- `rota --debug GAME` with `rota-dbg` in a terminal: a debugger for the
  Game Gear and Master System (registers, disassembly, breakpoints,
  stepping, memory).

## Building

Everything builds inside the Nix dev shell (`nix develop`): GHC and
Clash, Verilator, Zig, SDL3 and the assemblers. Vivado is needed only
for the board's bitstreams.

```sh
nix develop
tools/fetch-roms.sh          # the free test ROMs and homebrew
make test SYSTEM=gb          # the Haskell tests of one system
make sim SYSTEM=nes ROM=…    # the hardware in Verilator, frames as PNGs
make desktop ROM=game.gg     # the desktop emulator
```

`docs/TESTING.md` describes the test layers. `docs/ARCHITECTURE.md`
covers how cores, the platform layer and the boards fit together. Each
system has its own notes in `docs/systems/`.

## Games and ROMs

No game, ROM or BIOS image is part of this repository. `tools/fetch-roms.sh`
downloads only freely available test suites and homebrew, each from its
authors' own pages and under its own licence. Commercial games are yours
to dump from cartridges you own. The ZedBoard's master constraints file
is Avnet's and isn't redistributable: download it into
`boards/zedboard/reference/` to check the ZedBoard's pins.

## Credits

Developed with the help of Claude Code (Anthropic). Third-party data and
files are listed in [THIRD_PARTY.md](THIRD_PARTY.md).

## Licence

MIT: see [LICENSE](LICENSE).
