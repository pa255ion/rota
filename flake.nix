{
  description = "emulation_clash: retro console emulators in Clash for the ZedBoard";

  # Pinned through flake.lock. Vivado is NOT provided here; see docs/TOOLCHAIN.md.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # nixos-26.05 default: GHC 9.10.3, Clash 1.8.5.
        hsPkgs = pkgs.haskellPackages;

        ghc = hsPkgs.ghcWithPackages (p: with p; [
          # Clash
          clash-ghc
          clash-prelude
          clash-lib
          ghc-typelits-natnormalise
          ghc-typelits-extra
          ghc-typelits-knownnat
          # Testing
          tasty
          tasty-hedgehog
          tasty-golden
          tasty-hunit
          hedgehog
          # Simulation / tools
          JuicyPixels
          vector
          bytestring
          containers
          # SingleStepTests JSON (M3 step 3.1; already in clash-lib's closure)
          aeson
          # the 68000's SingleStepTests, gzipped JSON (side track P)
          zlib
        ]);
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            ghc
            pkgs.cabal-install
            hsPkgs.haskell-language-server
            pkgs.verilator
            # SDL3 for make play (nixpkgs' SDL2 is only sdl2-compat over it)
            pkgs.sdl3
            pkgs.pkg-config
            pkgs.gnumake
            # tools/*.py; tkinter for tools/keys.py (make keys)
            (pkgs.python3.withPackages (ps: [ ps.tkinter ]))
            # pdftotext/pdftoppm, for the board manuals in boards/*/reference
            pkgs.poppler-utils
            # Software (CLAUDE.md rule 9): Verilator harness, host tools
            pkgs.zig
            pkgs.zls
            # Game Boy regression ROMs (M2 step 2.0; CLAUDE.md rule 5)
            pkgs.rgbds
            # NES regression ROMs (M3 step 3.0): ca65, ld65
            pkgs.cc65
            # 68000 programs (side track P: the model's and the benchmark
            # project's): GNU as and objcopy for m68k, no C library
            pkgs.pkgsCross.m68k.buildPackages.binutils-unwrapped
            # Holy Mapperel's release is a .7z (M3 step 3.10, tools/fetch-roms.sh)
            pkgs.p7zip
            # FAT test images for the PS app's SD card menu (M1b stage C,
            # tools/mkfat-test.sh): mkfs.fat, mtools, sfdisk
            pkgs.dosfstools
            pkgs.mtools
            pkgs.util-linux
            # make sd-sync (side track K): the card's files from the image
            # onto a card mounted by the desktop
            pkgs.rsync
            # Game Gear and Z80 regression ROMs (M5 step 5.0): wla-z80, wlalink
            pkgs.wla-dx
            # Side track Q (Q.7): profiling the desktop emulators' fast
            # versions (perf record/report, user space only)
            pkgs.perf
          ];

          # Side track Q (Q.6): LLVM for GHC's -fllvm in the desktop builds
          # (make desktop-lib, DESKTOP_LLVM=0 to leave it out), by path only:
          # not on PATH, so no build picks up another C compiler or assembler
          EMU_LLVM_BIN = "${pkgs.llvmPackages_18.llvm}/bin";
          EMU_LLVM_CLANG = "${pkgs.llvmPackages_18.clang}/bin/clang";

          shellHook = ''
            echo "emulation_clash dev shell: $(clash --version)"
          '';
        };
      });
}
