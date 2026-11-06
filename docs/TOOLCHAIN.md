# Toolchain

## Versions (in the dev shell, 2026-10-02)

| Tool | Version | Source |
|---|---|---|
| GHC | 9.10.3 | `flake.nix` (nixos-26.05) |
| Clash | 1.8.5 | `flake.nix` (`clash-ghc`) |
| cabal-install | 3.16.1.0 | `flake.nix` |
| Verilator | 5.048 | `flake.nix` |
| Zig | 0.16.0 (+ zls 0.16.0); `zig c++` (clang + libc++) also compiles the Verilator model | `flake.nix` |
| SDL | 3.4.10 (`make play`) | `flake.nix` |
| Python | 3, with tkinter (`tools/*.py`) | `flake.nix` |
| RGBDS | 1.0.1 (`rgbasm`, `rgblink`, `rgbfix`; Game Boy regression ROMs, `make gb-asm`) | `flake.nix` |
| dosfstools, mtools, util-linux | mkfs.fat, mtools 4.0.49, sfdisk 2.42.3 (FAT test images for the PS app, `tools/mkfat-test.sh`) | `flake.nix` |
| cc65 | 2.18 (`ca65`, `ld65`; NES regression ROMs, `make nes-asm`, M3 step 3.0) | `flake.nix` |
| p7zip | 7-Zip 17.05 (`7z`: Holy Mapperel's release archive, `tools/fetch-roms.sh`, M3 step 3.10) | `flake.nix` |
| Vivado | 2026.1 | `~/Applications/AMD/Vivado/2026.1`, outside Nix |
| GHDL, Yosys | present | the user's profile, not the flake; optional, unused by the build |

## Haskell side: Nix dev shell

`flake.nix` (pinned by `flake.lock`) provides one dev shell with:

- `ghcWithPackages`: `clash-ghc`, `clash-prelude`, `clash-lib`,
  `ghc-typelits-natnormalise`, `ghc-typelits-extra`,
  `ghc-typelits-knownnat`, `tasty`, `tasty-hedgehog`, `tasty-golden`,
  `tasty-hunit`, `hedgehog`, `JuicyPixels`, `vector`, `bytestring`,
  `containers`
- `cabal-install`, `haskell-language-server`
- `verilator`, `sdl3` (nixpkgs' SDL2 is only sdl2-compat over SDL3),
  `pkg-config`, `gnumake`
- `python3` with `tkinter` (`tools/keys.py`, `tools/check-xdc.py`,
  `tools/test_link.py`)
- `poppler-utils` (`pdftotext`/`pdftoppm`, for the board manuals)
- `zig`, `zls` (Verilator harness, PS code, desktop link)
- `rgbds` (assembler, linker, header fixer for our own Game Boy
  regression ROMs in `systems/gb/test/asm/`, M2 step 2.0)
- `dosfstools`, `mtools`, `util-linux` (FAT test images for the PS app's
  SD card reader, `tools/mkfat-test.sh`, M1b stage C)
- `cc65` (`ca65` assembler and `ld65` linker for our own NES regression
  ROMs in `systems/nes/test/asm/`, layout `nrom.cfg`, M3 step 3.0); `aeson`
  in the GHC package set (SingleStepTests JSON, `mos6502-spec`)
- `wla-dx` (`wla-z80` assembler and `wlalink` linker for our own Game
  Gear and Z80 regression ROMs in `systems/gg/test/asm/`, `make gg-asm`;
  M5 step 5.0)
- `p7zip` (`7z`, to unpack Holy Mapperel's release in `tools/fetch-roms.sh`,
  M3 step 3.10)

```sh
cd ~/Documents/emulation_clash
nix develop
cabal build all
```

### Zig code: `build.zig` (side track H)

Every Zig program and test is built by `build.zig` at the repo root (no
package dependencies), called by the Makefile: Make stays the front door,
and Clash, Verilator's C++ model, Vivado, xsdb, bootgen and the shell and
Python tools stay in it. The Makefile passes `--cache-dir build/zig-cache`
and a `--prefix`, and each step installs where the Makefile put it before:

| `zig build` step | Make target | Output |
|---|---|---|
| `ps-app -Dps7-init-c=… [-Dps-app=relay]` | `make ps-app` (and `hdl`, `ci`) | `build/ps/relay.elf`, `relay.s`, `ps7_init.s` (installed only if `tools/check-ps-align.sh` passes on both) |
| `link` | `make link` (and `keys`, `load`, `sd`, …) | `build/host/link` |
| `sim -Dverilator-obj=DIR` | `make sim`, `resim` | `DIR/vga-sim` (DIR = `build/<system>/obj_dir`, Make's Verilator model) |
| `play -Dverilator-obj=DIR -Dsdl3-libdir=…` | `make play` | `DIR/vga-play` |
| `test` | `make test-zig` (after the FAT/SD test images) | every Zig unit test, from the repo root |

Each compilation has the same modules as the command line it replaced, so
the outputs are the same bytes (checked for `relay.elf`, `BOOT.BIN` and
`link`, 2026-10-04). `zig build --help` lists the steps and options.

Clash interactive: `cabal run clashi -- -isystems/chip8/src` (or `clashi`
inside the shell). In `clashi`, `:verilog` generates HDL for the loaded
`topEntity`.

### Clash compiler flags

Every Clash package's `.cabal` uses the standard Clash GHC options:

```
ghc-options: -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver
             -fplugin GHC.TypeLits.Extra.Solver -fexpose-all-unfoldings
             -fno-worker-wrapper -fconstraint-solver-iterations=10
```

and `default-extensions` from the Clash starter project (`DataKinds`,
`NoImplicitPrelude`, `TypeFamilies`, `TypeApplications`, and so on).
Both live in a `common clash-common` stanza. `make hdl` runs `clash`
directly on the sources (`systems/<name>/src` plus `platform/src`) and
reads its `-X` and GHC flags from that stanza in
`systems/<name>/<name>.cabal`, so every system's `.cabal` must have it.

## Vivado

Vivado is installed outside Nix and used only in batch mode, through
`make bit` / `make prog` (run them outside `nix develop`):

```sh
make bit  SYSTEM=<name>   # boards/zedboard/build.tcl
make prog SYSTEM=<name>   # boards/zedboard/program.tcl
```

If a `vivado` command is already on `PATH` the Makefile uses it as is;
otherwise it sources `VIVADO_SETTINGS` (default
`~/Applications/AMD/Vivado/2026.1/Vivado/settings64.sh`) first. The Nix
shell and the Vivado environment are never mixed.

NixOS notes:

- Vivado expects an FHS layout: its own launcher starts with `#!/bin/bash`,
  which doesn't exist on NixOS. On the development machine the user's
  profile provides an FHS wrapper, `vivado` → `vivado-env`, which sources
  `settings64.sh` itself. Don't source `settings64.sh` in front of it: that
  puts Vivado's raw launcher first on `PATH` and fails with
  "bad interpreter".
- Vivado 2026.1 refuses to start a design run without a license, even for
  the free edition that covers the XC7Z020 ("a valid license was not
  found"; `vivado -version` still works because it skips the check). The
  free BASIC license is installed at `~/.Xilinx/Xilinx.lic` (valid until
  2027-10-02).
- The FHS environment (`vivado-env`, user-maintained, outside this repo)
  must provide every shared library Vivado loads at start-up. Fixed by the
  user on 2026-10-02; full builds now run. The two problems were: `libncurses.so.5` must be found by the loader (the
  file exists in `/usr/lib`, but under its soname only `libncursesw.so.5`
  is in the loader cache, so a lookup by the name `libncurses.so.5`
  fails), and `libtinfo.so.6` must exist (it's missing). Symptom:
  "couldn't load file libxv_commontasks.so: libncurses.so.5: cannot open
  shared object file", then "libtinfo.so.6" once the first is solved.
  Quick check after any change to the env:
  `vivado -mode batch -nojournal -nolog -source <file with: puts ok>`.
- JTAG programming needs udev rules for the Digilent FTDI cable, normally
  installed by `install_drivers` in the Vivado tree. On NixOS, add the
  equivalent rules through `services.udev.packages` / `extraRules` instead
  (vendor `0403`, product `6014`). The rule must actually give your user
  access: `TAG+="uaccess"` only works in a rules file that sorts **before**
  `73-seat-late.rules` (`extraRules` lands in `99-local.rules`, too late),
  and `GROUP="plugdev"` only helps if you're in `plugdev`. Symptom of
  neither: `make prog` fails with "There is no current hw_target" even
  though the board shows up as `0403:6014` on USB. Check with
  `getfacl /dev/bus/usb/<bus>/<dev>` (expect a `user:<you>:rw-` line) or
  `id`. After fixing, replug the board and kill any running `hw_server`,
  which keeps its old view of the cable.
  **The Arty Z7-20 (side track G) is a different chip:** an FT2232H,
  `0403:6010` (manufacturer "Digilent", product "Digilent Adept USB
  Device"), so a rule for `6014` alone doesn't cover it; the symptom is
  `jtag-pick: 0 cable(s) connected` with the Arty on USB (it also shows
  up as two `ttyUSB` ports, which need no rule). Same rule with
  `ATTR{idProduct}=="6010"` (optionally `ATTR{manufacturer}=="Digilent"`
  to leave other FT2232 devices alone), in the same early rules file.
- `xsdb` (Vivado/bin) programs the PL and the PS for systems with a PS app;
  `tools/xsdb.sh` runs it through `vivado-env` like the `vivado` wrapper
  does. No Vitis workspace or ARM GCC is used: Zig cross-compiles the PS
  code (`arm-freestanding-eabihf`, Cortex-A9).
- `bootgen` (Vivado/bin; v2026.1, checked 2026-10-04) builds the SD
  card's `BOOT.BIN` (`make boot-image`, side track F) through
  `tools/bootgen.sh`, the same way. The PS app links Vivado's generated
  `ps7_init.c` for the ZedBoard (`data/embeddedsw/sample_sdt/zed/sdt/`,
  beside the `ps7_init.tcl` that `make prog` uses), compiled by `zig`;
  no Xilinx FSBL, BSP or Vitis.
- Part number: `xc7z020clg484-1`. Board files are optional, because the
  flow uses raw XDC and doesn't depend on the Digilent board part.

## Generated output locations

| Path | Contents |
|---|---|
| `build/<system>/verilog/` | Clash output (git-ignored) |
| `build/<system>/obj_dir/` | Verilator build, clock-wizard stand-in, `vga-sim` harness binary |
| `build/zig-cache/` | Zig build cache (shared by all systems; `zig build --cache-dir`) |
| `build/<system>/vivado/` | checkpoints, `build.log`, `.bit`, IP (`ip/`), reports (`reports/`) |
| `build/<system>/frames/` | PNG frames from simulation |
