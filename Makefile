# Top-level entry points. Haskell targets run inside `nix develop`;
# Vivado targets run outside it, through the `vivado` FHS wrapper if one is
# on PATH, else by sourcing $(VIVADO_SETTINGS) (docs/TOOLCHAIN.md).
#
#   make hdl  SYSTEM=<name>    Clash -> Verilog in build/<name>/verilog
#   make test SYSTEM=<name>    layers 0-3: platform + that system's tests
#                              (no SYSTEM: every package in cabal.project),
#                              plus the board XDC check
#   make bit  SYSTEM=<name>    layer 5: Clash Verilog -> .bit (needs make hdl)
#   make prog SYSTEM=<name>    layer 6: program the board over JTAG
#
# Per-system board settings come from systems/<name>/zedboard.mk:
#   TOP_MODULE       Haskell module holding topEntity, e.g. Chip8.Top
#   BOARD_XDC_EXTRA  optional XDC files from boards/zedboard/, e.g. vga.xdc
#   HDL_SRC_EXTRA    optional extra Haskell source dirs for make hdl, e.g.
#                    systems/m0a/src when one system reuses another's code
#   HDL_PREREQ       optional make hdl prerequisites (e.g. a RAM image that
#                    blockRamFile reads, see systems/chip8)
#
# BOARD (side track G) picks the board: arty-z7-20 (the default since
# 2026-10-06: settings from systems/<name>/arty-z7-20.mk, files from
# boards/arty-z7-20/, outputs in build/arty-z7-20/<name>/) or zedboard
# (everything above, outputs in build/<name>/). Only systems with a
# <board>.mk build for it.

SYSTEM          ?=
VIVADO_SETTINGS ?= $(HOME)/Applications/AMD/Vivado/2026.1/Vivado/settings64.sh
CABAL           ?= cabal

# The Arty Z7-20 is the main board since 2026-10-06 (the user): the default.
# The ZedBoard: BOARD=zedboard (make ci's ZedBoard chains pin it).
BOARD           ?= arty-z7-20
BOARDS          = zedboard arty-z7-20
ifeq ($(filter $(BOARD),$(BOARDS)),)
$(error BOARD=$(BOARD): known boards are $(BOARDS))
endif

# Before the system's .mk: its per-board files (HDL_PREREQ) live below it.
BUILD           ?= $(if $(filter zedboard,$(BOARD)),build/$(SYSTEM),build/$(BOARD)/$(SYSTEM))

-include systems/$(SYSTEM)/$(BOARD).mk

# Side track G phase 2: the Arty's UART is the second interface (if01) of
# its FTDI chip (the first is JTAG), not the ZedBoard's Cypress bridge;
# make keys/load/ping/sd use it by default (KEYS_PORT=… overrides).
ifeq ($(BOARD),arty-z7-20)
KEYS_PORT ?= $(firstword $(wildcard /dev/serial/by-id/*Digilent*-if01-port0) /dev/ttyUSB1)
# make sd for the Arty's card: its bitstreams and (step 5) its boot image;
# and without Mooneye's emulator-only ROMs (26 of the image's 35 MiB),
# which only test MBCs: SD_GB_ROMS="… roms/gb/mooneye/emulator-only" puts
# them back
SD_BUILD   ?= build/arty-z7-20
SD_BOOT    ?= build/arty-z7-20/boot/BOOT.BIN
SD_GB_ROMS ?= roms/gb/games $(if $(SD_TESTS),roms/gb/dmg-acid2 roms/gb/blargg build/gb/asm)
endif

BOARD_DIR       = boards/$(BOARD)
CLASH_TOP_DIR   ?= $(BUILD)/verilog/$(TOP_MODULE).topEntity
CLASH_CONNECTOR ?= $(BUILD)/verilog/clashConnector.tcl
XDC             ?= $(BOARD_DIR)/$(BOARD).xdc $(addprefix $(BOARD_DIR)/,$(BOARD_XDC_EXTRA))
VIVADO_DIR      = $(BUILD)/vivado

# Clash compiles the system and platform sources directly, with the same
# extensions and GHC options as the cabal build: both are read from the
# `common clash-common` stanza of the system's .cabal file.
SYSTEM_CABAL    = systems/$(SYSTEM)/$(SYSTEM).cabal
CABAL_FIELD     = awk -v f=$(1) '/^common clash-common/{c=1;next} /^[^ ]/{c=0} c&&$$1==f":"{g=1;next} c&&g&&/^  [a-z-]+:/{g=0} c&&g{for(i=1;i<=NF;i++)if($$i!~/^--/)print $$i;else break}' $(SYSTEM_CABAL)
CLASH_FLAGS     = $(addprefix -X,$(shell $(call CABAL_FIELD,default-extensions))) \
                  $(shell $(call CABAL_FIELD,ghc-options))
CLASH_OBJ_DIR   = $(BUILD)/obj

TEST_TARGETS = $(if $(SYSTEM),platform $(SYSTEM),all)

# Run Vivado in batch mode from $(VIVADO_DIR), so its .Xil/ and logs land
# there. Arguments must be absolute paths. If `vivado` is already on PATH
# (on NixOS: the FHS wrapper, which sources settings64.sh itself) it is used
# as is; otherwise $(VIVADO_SETTINGS) is sourced first. Sourcing it in front
# of the wrapper would shadow the wrapper with Vivado's own launcher, which
# needs /bin/bash and so fails outside an FHS environment.
VIVADO_ENV = $(if $(shell command -v vivado),true,source "$(VIVADO_SETTINGS)")
#
# Vivado's exit status can't be trusted: if it fails to initialise (for
# example a missing shared library in the FHS environment) it still exits
# 0, or waits at an interactive Tcl prompt (hence stdin from /dev/null).
# So success is the script's own "<script>.tcl: OK" line in the log.
VIVADO = bash -c '$(VIVADO_ENV) && cd "$(abspath $(VIVADO_DIR))" && rm -f $(1).log && \
         { vivado -mode batch -nojournal -log $(1).log -source "$(abspath $(BOARD_DIR)/$(1).tcl)" -tclargs $(2) </dev/null; \
           grep -q "^$(1).tcl: OK" $(1).log 2>/dev/null || \
           { echo "make: $(1).tcl did not finish (see $(abspath $(VIVADO_DIR))/$(1).log, or Vivado output above)" >&2; exit 1; }; }'

# xsdb (Vivado's system debugger, for the PS), through tools/xsdb.sh. Like
# Vivado, success is the script's own "<script>.tcl: OK" line.
XSDB = VIVADO_SETTINGS="$(VIVADO_SETTINGS)" tools/xsdb.sh "$(abspath $(BOARD_DIR)/$(1).tcl)" $(2) \
         </dev/null 2>&1 | tee "$(abspath $(VIVADO_DIR))/$(1).log"; \
       grep -q "^$(1).tcl: OK" "$(abspath $(VIVADO_DIR))/$(1).log" || \
         { echo "make: $(1).tcl did not finish (see $(abspath $(VIVADO_DIR))/$(1).log)" >&2; exit 1; }

.PHONY: help ps-app link keys keymap show-keymap load ping sd saves-pull nix-shell need-system roms gb-asm test test-zig check-xdc hdl lint lint-run sim sim-run play play-run bit prog ci clean FORCE resim

help:
	@echo "make test [SYSTEM=<name>] [FULL=1]  Haskell tests (layers 0-3, quick tier unless FULL=1), XDC check, harness tests"
	@echo "make hdl SYSTEM=<name>      Clash -> Verilog (build/<name>/verilog)"
	@echo "make bit SYSTEM=<name>      Vivado build (layer 5), after make hdl"
	@echo "make queue                   who holds the make ci / make bit slots, who waits, the waiting list"
	@echo "make queue-join WHO=<session> join the waiting list (PENDING=1: not yet); queue-ready, queue-leave"
	@echo "make watch                   progress bars of every running make ci/hdl/sim/bit (make progress: once)"
	@echo "make prog SYSTEM=<name>     program the board over JTAG (layer 6)"
	@echo "  ... BOARD=zedboard        hdl/lint/sim/bit/prog/sd for the ZedBoard (the default is BOARD=arty-z7-20)"
	@echo "make boot-image             BOOT.BIN for the SD card (the PS app as first-stage program)"
	@echo "make lint SYSTEM=<name>     verilator --lint-only -Wall on the Clash output"
	@echo "make sim SYSTEM=<name> [FRAMES=n]   Verilator run -> PNG frames, golden check (layer 4)"
	@echo "make play SYSTEM=<name>     SDL3 window on the simulated VGA output (keyboard = buttons)"
	@echo "make gb-asm                  Game Boy regression ROMs (rgbds) -> build/gb/asm"
	@echo "make nes-asm                 NES regression ROMs (ca65) -> build/nes/asm"
	@echo "make a800-asm                800XL regression programs (ca65) -> build/a800xl/asm"
	@echo "make resim SYSTEM=<name> ROM=…   rerun the last make sim model with another ROM (no Clash run)"
	@echo "make ci [FULL=1]            layers 0-4 (systems in parallel)"
	@echo "make clean"

# The host may have its own ghc/cabal on PATH; only the flake's are pinned.
nix-shell:
ifndef IN_NIX_SHELL
	$(error Run inside the dev shell: nix develop -c make $(MAKECMDGOALS))
endif

need-system:
ifeq ($(SYSTEM),)
	$(error Set SYSTEM=<name>)
endif

# FULL=1 takes the "ci" slot (side track K, C1): the full tier needs the
# machine's memory as a make ci does
test: nix-shell check-xdc test-zig roms gb-asm
	$(if $(filter 1,$(FULL)),$(call QUEUE_RUN,ci) env) EMU_FULL_TESTS=$(FULL) CABAL=$(CABAL) tools/run-tests.sh $(TEST_TARGETS)

# Test ROMs (git-ignored, CLAUDE.md rule 7): fetches only what is missing and
# checks every file's SHA-256.
roms:
	tools/fetch-roms.sh

# Game Boy regression ROMs (CLAUDE.md rule 5): every systems/gb/test/asm/*.asm
# assembles into build/gb/asm/*.gb (rgbds; title = upper-cased file name).
# The gb tests read them from there; they are never hand-patched.
GB_ASM_DIR = systems/gb/test/asm
GB_ASM     = $(patsubst $(GB_ASM_DIR)/%.asm,build/gb/asm/%.gb,$(wildcard $(GB_ASM_DIR)/*.asm))

gb-asm: nix-shell $(GB_ASM)

# 68000 programs (side track P): every cpus/m68k/test/asm/*.s assembled by
# GNU as (-m68000) into build/m68k/asm/*.bin, a raw image from address 0 of
# the .text section (m68k-lib loads them at 0x1000)
M68K_ASM_DIR = cpus/m68k/test/asm
M68K_ASM     = $(patsubst $(M68K_ASM_DIR)/%.s,build/m68k/asm/%.bin,$(wildcard $(M68K_ASM_DIR)/*.s))
M68K_AS      = m68k-unknown-linux-gnu-as
M68K_OBJCOPY = m68k-unknown-linux-gnu-objcopy
.PHONY: m68k-asm
m68k-asm: nix-shell $(M68K_ASM)
test: m68k-asm
build/m68k/asm/%.bin: $(M68K_ASM_DIR)/%.s
	mkdir -p $(@D)
	$(M68K_AS) -m68000 --fatal-warnings -o $(@:.bin=.o) $<
	$(M68K_OBJCOPY) -O binary $(@:.bin=.o) $@

build/gb/asm/%.gb: $(GB_ASM_DIR)/%.asm
	mkdir -p $(@D)
	rgbasm -Wall -Werror -I $(GB_ASM_DIR) -o $(@:.gb=.o) $<
	rgblink -o $@.part $(@:.gb=.o)
	rgbfix -v -p 0xFF -t "$$(echo $* | tr a-z A-Z | cut -c1-15)" $@.part
	mv $@.part $@

# Every derived pin must match the Digilent/Avnet master (CLAUDE.md), on
# both boards whatever BOARD is.
check-xdc: nix-shell
	$(if $(wildcard boards/zedboard/reference/zedboard_master.xdc),python3 tools/check-xdc.py boards/zedboard/reference/zedboard_master.xdc $(wildcard boards/zedboard/*.xdc),@echo "check-xdc: no boards/zedboard/reference/zedboard_master.xdc (Avnet's file is not redistributable: download it yourself); the ZedBoard's pins not checked")
	python3 tools/check-xdc.py boards/arty-z7-20/reference/arty_z7_20_master.xdc $(wildcard boards/arty-z7-20/*.xdc)

# Layer 4 input. Writes build/<system>/verilog/<TOP_MODULE>.topEntity/ and
# copies clash-lib's clashConnector.tcl next to it for build.tcl.
# -fclash-no-cache -fclash-clear: Clash would otherwise reuse its output
# while the Haskell design is unchanged, and the contents of copied data files
# (a RAM image for blockRamFile) aren't part of that check; regenerating is
# cheap because GHC's compilation stays cached in CLASH_OBJ_DIR.
#
# Memory images (HDL_PREREQ) are made per board, in $(BUILD)/images/, so
# that both boards' layer 4 runs of one system can run at the same time
# (make ci). The design names one path for them, HDL_IMAGE_DIR (e.g.
# Chip8.Top.ramImage, build/chip8/ram.bin): Clash only copies that file
# next to the Verilog ($readmemb loads it by its base name), so the board's
# own images are copied there before Clash runs and over Clash's copies
# after it; the generated Verilog doesn't depend on their contents.
#
# Every Clash run takes its turn (tools/clash-slot.sh, locks in LOCK_DIR
# shared by every worktree and session): at most CLASH_MAX (2) at a time,
# none below CLASH_MEM_MIN (6) GB available, and one at a time of those
# whose .mk sets CLASH_HEAVY = 1 (the 800XL's: about 10 GB). Measured
# 2026-10-07/08: a real system's Clash run needs 4 to 10 GB; two 800XL
# runs at once made the machine swap, three Clash runs of make ci's
# layer 4 at once ran it out of memory.
LOCK_DIR ?= $(or $(XDG_CACHE_HOME),$(HOME)/.cache)/emulation_clash
# Clash-only flags of a top (its systems/<n>/<board>.mk; side track L: the
# save states' BitPack of the Game Boy's record needs a higher
# specialisation limit); not in the .cabal file, which GHC reads too
CLASH_EXTRA ?=
# Verilator options of a top (its .mk; side track L: +1364-2005ext+v reads
# the .v files as the Verilog-2005 Clash writes, where `packed`, a name in
# Clash's generic BitPack code, is no keyword)
VERILATOR_EXTRA ?=
#
# The result cache (side track K, D3; tools/ci-cache.py): a Clash run's
# Verilog is kept under a key of its source directories, its .mk, the Clash
# command and flake.lock, in ~/.cache/emulation_clash/ci-cache (shared by
# every worktree). make hdl (and so make sim and make bit) restores it
# instead of running Clash when the key matches, and leaves the files alone
# when they are already the same (no Verilator rebuild). CI_CACHE=off runs
# Clash and keeps nothing; CI_CACHE=write (a full make ci ALL=1) runs Clash
# and keeps the result. The memory images (HDL_PREREQ) aren't kept: they are
# copied in after either way.
# Side track O: the build's optional features, on unless turned off here
# (PLAN.md, "Side track O"): MOVIES=0 builds without input movies and frame
# advance, SCREENSHOTS=0 without the frame capture, in the PL
# (Platform.Features, through Clash's CPP: in CLASH_CMD, so in the Verilog
# cache's key) and in the PS app (zig build -Dmovies, -Dscreenshots; in its
# version mark).
MOVIES      ?= 1
SCREENSHOTS ?= 1
FEATURE_CLASH = $(if $(filter 0,$(MOVIES)),-DPLATFORM_NO_MOVIES) $(if $(filter 0,$(SCREENSHOTS)),-DPLATFORM_NO_SCREENSHOTS)
FEATURE_ZIG   = -Dmovies=$(if $(filter 0,$(MOVIES)),false,true) -Dscreenshots=$(if $(filter 0,$(SCREENSHOTS)),false,true)
# the PS app is rebuilt when the features change: this file holds them and
# is rewritten only when they differ (as the model's hdl.sha256)
PS_FEATURES = build/ps-features.txt
$(PS_FEATURES): FORCE
	@mkdir -p $(@D)
	@echo '$(FEATURE_ZIG)' > $@.new
	@cmp -s $@.new $@ && rm $@.new || mv $@.new $@
CLASH_CMD = clash --verilog $(FEATURE_CLASH) -isystems/$(SYSTEM)/src $(addprefix -i,$(HDL_SRC_EXTRA)) -iplatform/src \
  -outputdir $(CLASH_OBJ_DIR) -fclash-hdldir $(BUILD)/verilog -fclash-no-cache -fclash-clear \
  $(CLASH_FLAGS) $(CLASH_EXTRA) $(TOP_MODULE)
HDL_KEY = tools/ci-cache.py hdl-key --dirs "systems/$(SYSTEM)/src $(HDL_SRC_EXTRA) platform/src" \
  --files "systems/$(SYSTEM)/$(BOARD).mk flake.lock" --text "$(BOARD) $(CLASH_TOP_DIR) $(CLASH_CMD)"
hdl: nix-shell need-system $(HDL_PREREQ) $(PS_ELF)
	@test -n "$(TOP_MODULE)" || { echo "make hdl: no TOP_MODULE; add systems/$(SYSTEM)/$(BOARD).mk" >&2; exit 1; }
	mkdir -p $(BUILD)/verilog $(CLASH_OBJ_DIR)
	$(if $(HDL_IMAGE_DIR),@mkdir -p $(HDL_IMAGE_DIR) && for f in $(HDL_PREREQ); do \
	  cp -f $$f $(HDL_IMAGE_DIR)/.$$(basename $$f).$$$$ && mv -f $(HDL_IMAGE_DIR)/.$$(basename $$f).$$$$ $(HDL_IMAGE_DIR)/$$(basename $$f); done)
	k=$$($(HDL_KEY)) && { tools/ci-cache.py hdl-restore $$k $(CLASH_TOP_DIR) $(notdir $(HDL_PREREQ)) || { \
	  $(if $(CLASH_HEAVY),$(call QUEUE_RUN,ci) env) LOCK_DIR="$(LOCK_DIR)" tools/clash-slot.sh $(if $(CLASH_HEAVY),--heavy) -- \
	  $(PROGRESS) clash $(BOARD):$(TOP_MODULE) -- $(CLASH_CMD) && \
	  tools/ci-cache.py hdl-store $$k $(CLASH_TOP_DIR) $(notdir $(HDL_PREREQ)); }; }
	$(if $(HDL_IMAGE_DIR),cp -f $(HDL_PREREQ) $(CLASH_TOP_DIR)/)
	cp -f "$$(ghc-pkg field clash-lib data-dir --simple-output)/data-files/tcl/clashConnector.tcl" $(CLASH_CONNECTOR)
	chmod u+w $(CLASH_CONNECTOR)

# make hdl's cache key (tools/ci-cache.py reads it for a chain's tops)
.PHONY: ci-hdl-key
ci-hdl-key: need-system
	@$(HDL_KEY)

# --- Layer 4: Verilator (make lint, make sim) -------------------------------
# Both run `make hdl` first, then a sub-make, so that everything derived from
# the Clash output (file list, top module, clock-wizard IP name) is read after
# it has been regenerated.
#
# The model is the generated Verilog plus a stand-in for the clock-wizard IP
# (boards/sim/clk_wiz_sim.v.in, named after the IP in the Clash output),
# with the waivers in boards/sim/clash.vlt. The harness (platform/sim/) is
# Zig plus a C++ shim (vtop.cpp); all C++ is compiled with `zig c++` so it
# links against the same libc++ as the Zig side.
#
#   make lint SYSTEM=<name>               verilator --lint-only -Wall
#   make sim  SYSTEM=<name> [FRAMES=n] [SIM_ARGS="--sw 0x81"] [SIM_GOLDEN=f.png]
#       writes $(BUILD)/frames/frame_NNNN.png and compares them with
#       systems/<name>/test/golden/frame_NNNN.png (the layer 3 goldens) if
#       there are any; SIM_GOLDEN is compared with the last frame;
#       tops with HDMI: SIM_HDMI_GOLDEN with the last HDMI frame and
#       SIM_I2C_GOLDEN with the I2C register writes (platform/sim/main.zig);
#       tops with the PS link: SIM_PS plays a PS link script (platform/sim/ps.zig)

FRAMES        ?= 3
SIM_ARGS      ?=
SIM_DIR       = boards/sim
HARNESS_DIR   = platform/sim
OBJ_DIR       = $(BUILD)/obj_dir
FRAMES_DIR    = $(BUILD)/frames
GOLDEN_DIR    = systems/$(SYSTEM)/test/golden
SIM_BIN       = $(OBJ_DIR)/vga-sim
# Side track H: every Zig program and test is built by build.zig (zig
# build), called from here; each step installs below --prefix at the path
# this Makefile used before. ZIG_SRC: what those builds read.
ZIG_CACHE     = build/zig-cache
ZIG_BUILD     = zig build --cache-dir $(ZIG_CACHE)
ZIG_SRC       = build.zig $(wildcard platform/ps/*.zig platform/ps/systems/*.zig platform/host/*.zig $(HARNESS_DIR)/*.zig)
VLT           = $(SIM_DIR)/clash.vlt
CLASH_V       = $(wildcard $(CLASH_TOP_DIR)/*.v)
# Read from the Clash output, if there is one (these are expanded while the
# Makefile is parsed, so they must not run sed on an empty file list).
MANIFEST      = $(wildcard $(CLASH_TOP_DIR)/clash-manifest.json)
CLK_WIZ_TCL   = $(wildcard $(CLASH_TOP_DIR)/*.clash.tcl)
HDL_TOP       = $(if $(MANIFEST),$(shell sed -n '/"top_component"/,/"name"/s/.*"name": "\(.*\)".*/\1/p' $(MANIFEST)))
CLK_WIZ_IP    = $(if $(CLK_WIZ_TCL),$(shell sed -n 's/.*variable ipName {\(.*\)}.*/\1/p' $(CLK_WIZ_TCL)))
CLK_WIZ_V     = $(if $(CLK_WIZ_IP),$(OBJ_DIR)/$(CLK_WIZ_IP).v)
VERILATOR_SRC = $(VLT) $(CLASH_V) $(CLK_WIZ_V)
# Tops with the HDMI ports (M0d onwards) get the shim's HDMI/I2C side, tops
# with the PS link (PS_IN, M1b stage C) its PS side.
HAS_HDMI      = $(if $(CLASH_V),$(shell grep -lw HD_D $(CLASH_V)))
HAS_PS        = $(if $(CLASH_V),$(shell grep -lw PS_IN $(CLASH_V)))
# The harness plays the PS through the PS app's own link encoder
# (platform/ps/pl.zig, a module in build.zig).
HAS_SPDIF     = $(if $(CLASH_V),$(shell grep -lw HD_SPDIF $(CLASH_V)))
# Tops with the HP0 read channels (side track C, M0e.Top) get the shim's AXI
# side, played by platform/sim/axi.zig.
HAS_AXI       = $(if $(CLASH_V),$(shell grep -lw HP0_ARVALID $(CLASH_V)))
# (side track O, O.4) HP0's write channels: screenshots
HAS_AXI_W     = $(if $(CLASH_V),$(shell grep -lw HP0_AWVALID $(CLASH_V)))
# zig c++ enables UBSan on files built without -O (some of Verilator's are),
# and its runtime isn't linked; the model is generated code, so leave it off.
ZIG_CXX       = zig c++ -fno-sanitize=undefined
# Verilator's default list minus -Wno-subobject-linkage, which clang lacks.
VL_NO_UNUSED  = -faligned-new -Wno-bool-operation -Wno-int-in-bool-context -Wno-shadow \
                -Wno-sign-compare -Wno-tautological-compare -Wno-uninitialized \
                -Wno-unused-but-set-parameter -Wno-unused-but-set-variable \
                -Wno-unused-parameter -Wno-unused-variable

# The shim and its feature flags; the Arty's (side track G) are set below.
VTOP_CPP  = $(HARNESS_DIR)/vtop.cpp
# Tops with the PS's PL reset (PS_RESETN): the shim drives it (both boards)
HAS_PSRST = $(if $(CLASH_V),$(shell grep -lw PS_RESETN $(CLASH_V)))
VTOP_DEFS = $(if $(HAS_HDMI),-DVTOP_HDMI) $(if $(HAS_SPDIF),-DVTOP_SPDIF) $(if $(HAS_PS),-DVTOP_PS) $(if $(HAS_AXI),-DVTOP_AXI) $(if $(HAS_PSRST),-DVTOP_PSRST)
SIM_STEP  = sim
ifeq ($(BOARD),arty-z7-20)
# Arty tops: the shim vtop_arty.cpp (its ports are the Arty XDC's), the
# harness arty-sim (platform/sim/arty.zig: HDMI received as TMDS, frames
# compared with the system's goldens exactly).
HAS_TMDS  = $(if $(CLASH_V),$(shell grep -lw hdmi_tx_clk_p $(CLASH_V)))
HAS_LED4G = $(if $(CLASH_V),$(shell grep -lw led4_g $(CLASH_V)))
HAS_AUD   = $(if $(CLASH_V),$(shell grep -lw aud_pwm $(CLASH_V)))
VTOP_CPP  = $(HARNESS_DIR)/vtop_arty.cpp
VTOP_DEFS = $(if $(HAS_TMDS),-DARTY_TMDS) $(if $(HAS_LED4G),-DARTY_LED4G) $(if $(HAS_PS),-DARTY_PS) $(if $(HAS_AXI),-DARTY_AXI) $(if $(HAS_AXI_W),-DARTY_AXI_W) $(if $(HAS_AUD),-DARTY_AUDIO) $(if $(HAS_PSRST),-DARTY_PSRST)
SIM_STEP  = sim-arty
SIM_BIN   = $(OBJ_DIR)/arty-sim
# the stand-ins for the hand-written MMCM and serialisers (boards/sim/)
$(OBJ_DIR)/Vtop__ALL.a: $(SIM_DIR)/arty_mmcm.v $(SIM_DIR)/tmds_out.v
endif

lint: hdl
	@$(MAKE) --no-print-directory lint-run

sim: hdl
	@$(MAKE) --no-print-directory sim-run

lint-run: nix-shell need-system $(CLK_WIZ_V)
	verilator --lint-only -Wall --top-module $(HDL_TOP) -y $(SIM_DIR) $(VERILATOR_EXTRA) $(VERILATOR_SRC)
	@echo "make lint: OK ($(HDL_TOP), no warnings beyond $(VLT))"

# Runs in the Clash output directory: memory images (blockRamFile) are
# loaded by $readmemb relative to it.
sim-run: nix-shell need-system $(SIM_BIN)
	rm -rf $(FRAMES_DIR)
	cd $(CLASH_TOP_DIR) && $(PROGRESS) sim $(BOARD)/$(SYSTEM) "$(abspath $(FRAMES_DIR)) $(FRAMES)" -- \
	  $(abspath $(SIM_BIN)) --out $(abspath $(FRAMES_DIR)) --frames $(FRAMES) \
	  $(if $(wildcard $(GOLDEN_DIR)/frame_*.png),--expect $(abspath $(GOLDEN_DIR))) \
	  $(if $(SIM_GOLDEN),--expect-last $(abspath $(SIM_GOLDEN))) \
	  $(if $(SIM_HDMI_GOLDEN),--expect-hdmi-last $(abspath $(SIM_HDMI_GOLDEN))) \
	  $(if $(SIM_I2C_GOLDEN),--expect-i2c $(abspath $(SIM_I2C_GOLDEN))) \
	  $(if $(SIM_PS),--ps $(abspath $(SIM_PS))) $(SIM_SYS_ARGS) $(SIM_ARGS)

ifneq ($(CLK_WIZ_IP),)
$(CLK_WIZ_V): $(SIM_DIR)/clk_wiz_sim.v.in
	mkdir -p $(OBJ_DIR)
	sed 's/@MODULE@/$(CLK_WIZ_IP)/' $< > $@
endif

# Verilator's own makefile; tools/ci-cache.py sets VL_MAKE=: so that its
# dry runs (make -n) don't print what an earlier model's makefile would do.
VL_MAKE ?= $(MAKE)

# make hdl rewrites the Verilog every time; only rebuild the model when its
# contents changed.
$(OBJ_DIR)/hdl.sha256: FORCE
	mkdir -p $(OBJ_DIR)
	@cat $(CLASH_V) | sha256sum > $@.new
	@cmp -s $@.new $@ && rm $@.new || mv $@.new $@

$(OBJ_DIR)/Vtop__ALL.a: $(OBJ_DIR)/hdl.sha256 $(VLT) $(CLK_WIZ_V)
	verilator --cc --prefix Vtop --top-module $(HDL_TOP) -y $(SIM_DIR) --pins-inout-enables -Mdir $(OBJ_DIR) \
	  -O3 --x-assign fast --x-initial fast --no-timing -Wno-fatal $(VERILATOR_EXTRA) $(VERILATOR_SRC)
	+$(VL_MAKE) -s -C $(OBJ_DIR) -f Vtop.mk CXX="$(ZIG_CXX)" LINK="$(ZIG_CXX)" \
	  CFG_CXXFLAGS_NO_UNUSED="$(VL_NO_UNUSED)" OPT_FAST=-O2 Vtop__ALL.a libverilated.a

$(OBJ_DIR)/vtop.o: $(VTOP_CPP) $(OBJ_DIR)/Vtop__ALL.a
	$(ZIG_CXX) -std=c++20 -O2 -c $< -I$(OBJ_DIR) $(VTOP_DEFS) \
	  -I$$(verilator --getenv VERILATOR_ROOT)/include \
	  -I$$(verilator --getenv VERILATOR_ROOT)/include/vltstd -o $@

$(SIM_BIN): $(OBJ_DIR)/vtop.o $(OBJ_DIR)/Vtop__ALL.a $(ZIG_SRC)
	$(ZIG_BUILD) $(SIM_STEP) -Dverilator-obj=$(OBJ_DIR) --prefix $(OBJ_DIR)

# --- Board link (desktop input and ROM loading over J14) --------------------
# build/host/link (platform/host/, Zig): keyboards and gamepads read with
# evdev, ROM loading, 921600 baud negotiated with the PS relay. Runs inside
# nix develop (it's built there); needs the input and dialout groups.
#   make keys [KEYS_PORT=/dev/ttyACM0] [LINK_ARGS=--verbose]
#   make load ROM=roms/chip8/timendus/5-quirks.ch8 [RAM_PRESET="0x1FF=1"]
#   make ping                  round-trip time to the PS
# tools/keys.py is the older tkinter version (no input group needed, but
# needs window focus and adds 40 ms to every release).
LINK_BIN  = build/host/link
# Every link on the board's port asks first whether another process holds
# it (two links on one port steal each other's replies; side track K,
# side J's request: tools/port-holder.sh)
PORT_FREE = tools/port-holder.sh "$(KEYS_PORT)" &&
KEYS_PORT ?= $(firstword $(wildcard /dev/serial/by-id/*Cypress*) /dev/ttyACM0)
LINK_ARGS ?=

link: nix-shell $(LINK_BIN)

# The wire format (platform/ps/protocol.zig) is one module shared with the
# relay, and so are the key maps (platform/ps/keymap.zig, input_codes.zig;
# side track E reads them on the PS too): modules in build.zig.
$(LINK_BIN): $(ZIG_SRC)
	$(ZIG_BUILD) link --prefix build

# SYSTEM=gb or nes picks link's key map and cartridge path (--system); the
# NES's .nes file goes into DDR, then its header and CHR over the link, as
# the SD menu does it (side track D): PRG up to 512 KiB, CHR up to 256 KiB. Key maps (side track B, platform/ps/keymap.zig): the
# defaults, overridden by $XDG_CONFIG_HOME/emulation_clash/keymap.conf or
# KEYMAP=FILE; a bad file stops link with its line number.
#   make keymap SYSTEM=gb [KEYMAP=FILE]   remap a system's buttons, write the file
#   make show-keymap SYSTEM=gb            the map in force
# Neither needs the board.
KEYMAP ?=
LINK_SYSTEM = $(if $(filter gb nes,$(SYSTEM)),--system $(SYSTEM)) $(if $(KEYMAP),--keymap "$(KEYMAP)")

keys: nix-shell $(LINK_BIN)
	$(PORT_FREE) $(LINK_BIN) --port "$(KEYS_PORT)" $(LINK_SYSTEM) $(LINK_ARGS)

load: nix-shell $(LINK_BIN)
	@test -f "$(ROM)" || { echo "make load: set ROM=<file.ch8> (SYSTEM=gb: <file.gb>, SYSTEM=nes: <file.nes>)" >&2; exit 1; }
	$(PORT_FREE) $(BOARD_QUEUE) $(LINK_BIN) --port "$(KEYS_PORT)" $(LINK_SYSTEM) --load "$(ROM)" $(foreach p,$(RAM_PRESET),--preset $(p)) $(LINK_ARGS)

keymap show-keymap: nix-shell $(LINK_BIN)
	@case "$(SYSTEM)" in chip8|gb|nes|gbc|a800xl|gg|sms|snes) ;; *) echo "make $@: set SYSTEM=chip8, gb, nes, gbc, a800xl, gg, sms or snes" >&2; exit 1;; esac
	$(LINK_BIN) --$(if $(filter keymap,$@),remap,show-keymap) --system $(SYSTEM) $(if $(KEYMAP),--keymap "$(KEYMAP)") $(LINK_ARGS)

# The settings screen (platform/host/settings.zig; the user's request,
# 2026-10-22): every system's buttons and the [global] settings in one
# terminal UI; starts on SYSTEM's page (default: the first). No board.
.PHONY: settings
settings: nix-shell $(LINK_BIN)
	$(LINK_BIN) --settings $(if $(filter chip8 gb nes gbc a800xl gg sms snes,$(SYSTEM)),--system $(SYSTEM)) $(if $(KEYMAP),--keymap "$(KEYMAP)") $(LINK_ARGS)

ping: nix-shell $(LINK_BIN)
	$(PORT_FREE) $(LINK_BIN) --port "$(KEYS_PORT)" --ping 200

# Write the board's SD card through the board (no card reader needed):
# tools/mksd.sh makes a 64 MiB FAT32 image with SD_ROMS in /chip8 and each
# folder of SD_GB_ROMS in /gb (side track A), link sends it to the PS
# relay, which writes and reads back every block, then reopens the ROM
# menu. ERASES THE CARD. Needs the board running
# (make prog SYSTEM=chip8) and J14 on USB.
#   make sd [SD_ROMS=roms/chip8] [SD_GB_ROMS="roms/gb/games ..."] [SD_REWRITE=1]
# Side track G phase 2: chunks the card already holds (by hash) are
# skipped, the rest go as multi-block writes; SD_REWRITE=1 writes all.
SD_ROMS ?= roms/chip8
# Test ROMs and our regression programs go on the card only with
# SD_TESTS=1 (each system's SD_*_ROMS below; the user, 2026-10-16: the
# 64 MiB image was full); games, the user's local folders and the demos
# always do.
SD_TESTS ?=
SD_GB_ROMS ?= roms/gb/games $(if $(SD_TESTS),roms/gb/dmg-acid2 roms/gb/blargg build/gb/asm roms/gb/mooneye/emulator-only)
# NES (M3 step 3.8): each folder into /nes as a folder of its own name
SD_NES_ROMS ?= roms/nes/games roms/nes/homebrew $(if $(SD_TESTS),roms/nes/blargg roms/nes/accuracycoin-d103cbd roms/nes/nestest roms/nes/holy-mapperel build/nes/asm)
# Side track F: the boot image too (make boot-image first; skipped with a
# note if there is none, and a warning if it is older than the app).
SD_BOOT ?= build/boot/BOOT.BIN
# where the /systems bitstreams come from (side track G: the Arty's in
# build/arty-z7-20/; its relay refuses a ZedBoard bitstream anyway)
SD_BUILD ?= build
# and each system's bitstream (make bit SYSTEM=...) as /systems/<name>.bit;
# the first is the one loaded at power-on
SD_SYSTEMS ?= chip8 gb nes
# and the key map (KEYMAP, else link's default file) as /keymap.conf, so
# the board uses the desktop's maps; checked with link's parser first
SD_KEYMAP ?= $(or $(KEYMAP),$(or $(XDG_CONFIG_HOME),$(HOME)/.config)/emulation_clash/keymap.conf)
# The Game Boy Color (side track I, phase 3): its folders into /gbc
# (tools/mksd.sh's GBC_DIRS) and gbc.bit into /systems
SD_GBC_ROMS ?= roms/gbc/games $(if $(SD_TESTS),roms/gbc/cgb-acid2 roms/gbc/blargg build/gb/asm)
# and the Game Boy's folders (the board's SD_GB_ROMS) into /gbc/gb: a
# second copy of /gb, with SD_TESTS=1 only (SD_GBC_GB_ROMS=… sets it)
SD_GBC_GB_ROMS ?= $(if $(SD_TESTS),$(SD_GB_ROMS))
SD_SYSTEMS += gbc
sd: export GBC_DIRS = $(SD_GBC_ROMS)
sd: export GBC_GB_DIRS = $(SD_GBC_GB_ROMS)
# make keys/load SYSTEM=gbc: link --system gbc (the Game Boy's cartridge
# load and key map: the GBC's ROM is in DDR at the same address)
LINK_SYSTEM += $(if $(filter gbc,$(SYSTEM)),--system gbc)
#
# Side track I: battery saves. The card's /saves are pulled into SAVES_DIR
# first (make saves-pull; outside the repo, shared by every worktree), and
# make sd stops without erasing anything if that fails (SAVES=skip
# erases anyway: a new card, or one without a file system). The new image
# gets a save file for every battery cartridge, from SAVES_DIR where one is
# stored (tools/mksd.sh).
SAVES_DIR ?= $(or $(XDG_DATA_HOME),$(HOME)/.local/share)/emulation_clash/saves
# side track L: the card's /states (save-state slots) beside them, pulled
# and restored the same way
STATES_DIR ?= $(or $(XDG_DATA_HOME),$(HOME)/.local/share)/emulation_clash/states
# side track O: the card's /movies (input movies' slots) too; each slot
# holds MOVIE_MINUTES of frames (tools/mksd.sh)
MOVIES_DIR ?= $(or $(XDG_DATA_HOME),$(HOME)/.local/share)/emulation_clash/movies
# side track O, O.4: screenshots (the card's /screens), kept the same way
SCREENS_DIR ?= $(or $(XDG_DATA_HOME),$(HOME)/.local/share)/emulation_clash/screens
MOVIE_MINUTES ?= 30
SAVES ?=
# make sd takes the board's slot (side track K, C1: tools/queue.sh); the
# work is sd-run's.
# It first refuses a card that wouldn't boot or would lack a system: no
# boot image (SD_BOOT), or a system in SD_SYSTEMS without a bitstream in
# SD_BUILD. Nothing is pulled or erased then, and it says how to build
# them; SD_PARTIAL=1 writes a card with what there is (side track K, M6's
# request: a card from a worktree with only sms.bit and no BOOT.BIN left
# the Arty without a boot, 2026-10-12, as one did side J's on 2026-10-07).
SD_NO_BITS = $(foreach s,$(SD_SYSTEMS),$(if $(wildcard $(SD_BUILD)/$(s)/vivado/*.bit),,$(s)))
SD_NO_BOOT = $(if $(wildcard $(SD_BOOT)),,$(SD_BOOT))
# The refusal, for make sd and make sd-sync: $(1) the target, $(2) what
# nothing was ("pulled or erased")
SD_GUARD = $(if $(filter 1,$(SD_PARTIAL)),,$(if $(strip $(SD_NO_BITS) $(SD_NO_BOOT)),{ \
	  echo "$(1): refused (nothing $(2)): this card would not boot or would lack systems:" >&2; \
	  $(if $(SD_NO_BOOT),echo "$(1):   no boot image $(SD_NO_BOOT): nix develop -c make ps-app SYSTEM=chip8 BOARD=$(BOARD) and then make boot-image BOARD=$(BOARD)" >&2;) \
	  $(foreach s,$(SD_NO_BITS),echo "$(1):   no $(s) bitstream in $(SD_BUILD)/$(s)/vivado: nix develop -c make hdl SYSTEM=$(s) BOARD=$(BOARD) and then make bit SYSTEM=$(s) BOARD=$(BOARD)" >&2;) \
	  echo "$(1): (or SD_SYSTEMS=\"...\" for fewer systems; SD_PARTIAL=1 writes the card with what there is)" >&2; \
	  exit 1; }))
# The card's image: tools/mksd.sh into $(1), $(2) MiB, with every system's
# folders, saves, states and movies (make sd, make sd-sync)
SD_MB ?= 64
MKSD = LINK=$(LINK_BIN) SAVES_DIR="$(SAVES_DIR)" STATES_DIR="$(STATES_DIR)" MOVIES_DIR="$(MOVIES_DIR)" SCREENS_DIR="$(SCREENS_DIR)" MOVIES="$(MOVIES)" MOVIE_MINUTES="$(MOVIE_MINUTES)" \
	KEYMAP_CONF="$(SD_KEYMAP)" BOOT_BIN="$(SD_BOOT)" NES_DIRS="$(SD_NES_ROMS)" CHIP8_DIRS="$(SD_CHIP8_DEMOS)" BOOT_FILES="$(SD_BOOT_FILES)" \
	  SYSTEM_BITS="$(foreach s,$(SD_SYSTEMS),$(s)=$(firstword $(wildcard $(SD_BUILD)/$(s)/vivado/*.bit) $(SD_BUILD)/$(s)/vivado/$(s)_zynq.bit))" \
	  SD_MB="$(2)" tools/mksd.sh $(1) "$(SD_ROMS)" $(SD_GB_ROMS)
sd: nix-shell $(LINK_BIN)
	@$(call SD_GUARD,make sd,pulled or erased)
	@$(BOARD_QUEUE) $(MAKE) --no-print-directory sd-run
.PHONY: sd-run
sd-run: nix-shell $(LINK_BIN)
	@if [ -f "$(SD_BOOT)" ] && [ "$(BOOT_ELF)" -nt "$(SD_BOOT)" ]; then \
	  echo "make sd: warning: $(SD_BOOT) is older than $(BOOT_ELF) (make boot-image)" >&2; fi
	@if [ -f "$(SD_KEYMAP)" ] && ! $(LINK_BIN) --show-keymap --keymap "$(SD_KEYMAP)" >/dev/null 2>&1; then \
	  $(LINK_BIN) --show-keymap --keymap "$(SD_KEYMAP)" >/dev/null; exit 1; fi
	@if [ "$(SAVES)" != skip ]; then \
	  $(PORT_FREE) $(LINK_BIN) --port "$(KEYS_PORT)" --saves-pull "$(SAVES_DIR)" --states-dir "$(STATES_DIR)" --movies-dir "$(MOVIES_DIR)" --screens-dir "$(SCREENS_DIR)" \
	    --keymap-pull $(if $(KEYMAP),--keymap "$(KEYMAP)") $(LINK_ARGS) || { \
	    echo "make sd: the saves could not be pulled off the card, so it was not erased." >&2; \
	    echo "make sd: SAVES=skip erases it anyway (its saves are lost)." >&2; exit 1; }; fi
	$(call MKSD,build/sd/card.img,$(SD_MB))
	$(PORT_FREE) $(LINK_BIN) --port "$(KEYS_PORT)" --sd-image build/sd/card.img $(if $(SD_REWRITE),--sd-rewrite) $(LINK_ARGS)

# The card through the desktop's SD card reader (side track K, the user's
# request 2026-10-16; tools/sd-sync.sh): with the card mounted by the
# desktop (FAT32, one MBR partition: any size), its /saves, /states and
# /movies are copied into SAVES_DIR, STATES_DIR and MOVIES_DIR (clock saves
# stamped, as make sd's pull through the board does), the image built as
# for make sd (SD_SYNC_MB, default 4096: a sparse file, only room for the
# files), and the card's files made the image's: only what changed is
# written, what is gone removed (the desktop's own folders kept). No board,
# no root; the same refusal as make sd. SD_MOUNT=<folder> if the card
# isn't the one FAT mount under /run/media or /media. Unmount (eject) the
# card before taking it out.
SD_SYNC_MB ?= 4096
# each system's folders, as the tracks' `sd: export` lines give make sd's
# recipe (tools/test_sd_sync.sh checks that every one has its line here)
sd-sync: export GBC_DIRS = $(SD_GBC_ROMS)
sd-sync: export GBC_GB_DIRS = $(SD_GBC_GB_ROMS)
sd-sync: export A800_DIRS = $(SD_A800_ROMS)
sd-sync: export GG_DIRS = $(SD_GG_ROMS)
sd-sync: export SMS_DIRS = $(SD_SMS_ROMS)
sd-sync: export SMS_BIOS := $(SMS_BIOS)
sd-sync: nix-shell $(LINK_BIN)
	@$(call SD_GUARD,make sd-sync,pulled or written)
	@m=$$(tools/sd-sync.sh mount) && echo "make sd-sync: the card at $$m" && \
	  if [ "$(SAVES)" != skip ]; then \
	    tools/sd-sync.sh pull "$$m" "$(SAVES_DIR)" "$(STATES_DIR)" "$(MOVIES_DIR)" "$(SCREENS_DIR)" && \
	    $(LINK_BIN) --stamp-clocks "$(SAVES_DIR)" && \
	    { k=$$(find "$$m" -maxdepth 1 -iname keymap.conf -type f | head -1); \
	      [ -z "$$k" ] || $(LINK_BIN) --keymap-take "$$k" $(if $(KEYMAP),--keymap "$(KEYMAP)"); } || { \
	      echo "make sd-sync: the saves could not be copied off the card, so nothing was written." >&2; \
	      echo "make sd-sync: SAVES=skip writes it anyway (its saves are replaced)." >&2; exit 1; }; fi && \
	  $(call MKSD,build/sd/card-sync.img,$(SD_SYNC_MB)) && \
	  tools/sd-sync.sh push "$$m" build/sd/card-sync.img
.PHONY: sd-sync

# The card's /saves (battery saves, side track I) into SAVES_DIR, through
# the board (the relay's R command); what make sd does first.
saves-pull: nix-shell $(LINK_BIN)
	$(PORT_FREE) $(BOARD_QUEUE) $(LINK_BIN) --port "$(KEYS_PORT)" --saves-pull "$(SAVES_DIR)" --states-dir "$(STATES_DIR)" --movies-dir "$(MOVIES_DIR)" --screens-dir "$(SCREENS_DIR)" $(LINK_ARGS)

# Input movies (side track O, O.2c): the card's /movies into MOVIES_DIR
# (make sd does it too, before it erases the card), and a movie file into
# a game's slot on the card, then Play movie in the board's Options:
#   make movie-play MOVIE=$(MOVIES_DIR)/gb/games/tetris.mv1   (the slot from
#     its place below MOVIES_DIR; else SLOT=/movies/gb/games/tetris.mv1)
movie-pull: nix-shell $(LINK_BIN)
	$(PORT_FREE) $(BOARD_QUEUE) $(LINK_BIN) --port "$(KEYS_PORT)" --movies-pull "$(MOVIES_DIR)" $(LINK_ARGS)
SLOT ?= $(if $(filter $(MOVIES_DIR)/%,$(abspath $(MOVIE))),/movies/$(patsubst $(MOVIES_DIR)/%,%,$(abspath $(MOVIE))))
movie-play: nix-shell $(LINK_BIN)
	@test -n "$(MOVIE)" || { echo "make movie-play: MOVIE=<file> (and SLOT=/movies/... unless it is below $(MOVIES_DIR))" >&2; exit 1; }
	@test -n "$(SLOT)" || { echo "make movie-play: SLOT=/movies/<system>/<folder>/<name>.mv1 ($(MOVIE) is not below $(MOVIES_DIR))" >&2; exit 1; }
	$(PORT_FREE) $(BOARD_QUEUE) $(LINK_BIN) --port "$(KEYS_PORT)" --movie-put "$(MOVIE)" --movie-slot "$(SLOT)" $(LINK_ARGS)
.PHONY: movie-pull movie-play

# Screenshots (side track O, O.4): one taken on the board (the running
# game's next frame, into the card's /screens) and copied into SCREENS_DIR;
# F12 on the board's keyboard or in make keys takes one onto the card only
screenshot: nix-shell $(LINK_BIN)
	$(PORT_FREE) $(BOARD_QUEUE) $(LINK_BIN) --port "$(KEYS_PORT)" --screenshot "$(SCREENS_DIR)" $(LINK_ARGS)
.PHONY: screenshot

# Every Zig unit test (the harness, the PS app, link: build.zig's test
# step); part of make test. The FAT and SD images the tests read are made
# here first (shell tools), and link is checked against a fake relay after.
test-zig: nix-shell
	tools/mkfat-test.sh build/ps-test
	rm -rf build/ps-test/sdroms && mkdir -p build/ps-test/sdroms/games
	seq 1 400 > "build/ps-test/sdroms/Space Invaders [test].ch8"
	seq 7 2 900 > build/ps-test/sdroms/games/pong.ch8
	head -c 3000 /dev/zero > build/ps-test/sdroms/zeros.ch8
	# battery saves (side track I): three Game Boy cartridge headers
	# (MBC1+RAM+BATTERY 8 KiB, MBC5+RAM+BATTERY 32 KiB, MBC1 without RAM)
	# and a stored save for the first (byte i = (i * 5 + 1) & 0xFF), so the
	# image gets /saves/gb/games/{count,blank}.sav (platform/ps/save.zig,
	# platform/host/saves.zig test them)
	rm -rf build/ps-test/gbroms build/ps-test/saves && mkdir -p build/ps-test/gbroms/games build/ps-test/saves/gb/games
	for c in count:03:02 blank:1B:03 plain:01:00 clock:10:03; do python3 -c 'import sys; r = bytearray(0x8000); \
	  r[0x147] = int(sys.argv[2], 16); r[0x149] = int(sys.argv[3], 16); open(sys.argv[1], "wb").write(r)' \
	  build/ps-test/gbroms/games/$${c%%:*}.gb $$(echo $$c | cut -d: -f2) $$(echo $$c | cut -d: -f3) || exit 1; done
	python3 -c 'open("build/ps-test/saves/gb/games/count.sav", "wb").write(bytes((i * 5 + 1) & 255 for i in range(0x2000)))'
	$(MAKE) --no-print-directory $(LINK_BIN)
	# a stored state and movie (side track K, F1: the board makes new ones
	# itself; what the card had goes back as it is, no empty slots)
	rm -rf build/ps-test/states build/ps-test/movies && mkdir -p build/ps-test/states/gb/games build/ps-test/movies/gb/games
	head -c 54272 /dev/urandom > build/ps-test/states/gb/games/count.st2
	head -c 3584 /dev/urandom > build/ps-test/movies/gb/games/count.mv1
	LINK=$(LINK_BIN) SAVES_DIR=build/ps-test/saves STATES_DIR=build/ps-test/states MOVIES_DIR=build/ps-test/movies \
	  tools/mksd.sh build/ps-test/sd.img build/ps-test/sdroms build/ps-test/gbroms/games
	for f in states/gb/games/count.st2 movies/gb/games/count.mv1; do \
	  MTOOLS_SKIP_CHECK=1 mtype -i build/ps-test/sd.img@@1M ::/$$f | cmp -s - build/ps-test/$$f || \
	    { echo "test-zig: /$$f not put back as it was" >&2; exit 1; }; done
	! MTOOLS_SKIP_CHECK=1 mdir -b -i build/ps-test/sd.img@@1M ::/states/gb/games/count.st1 >/dev/null 2>&1 || \
	  { echo "test-zig: an empty state slot was made (the board makes them now)" >&2; exit 1; }
	demos/test-mksd.sh build/ps-test/mksd
	$(ZIG_BUILD) test --summary all
	# the card after a save was written back in place (save.zig's test):
	# the file system still clean
	dd if=build/ps-test/sd-saved.img of=build/ps-test/sd-saved.part bs=512 skip=2048 status=none
	fsck.fat -n build/ps-test/sd-saved.part
	# FAT32 writes (side track K, F1a; fat.zig's test made folders and files
	# in it): the file system clean (its free count left unknown, on purpose)
	dd if=build/ps-test/fat32-written.img of=build/ps-test/fat32-written.part bs=512 skip=2048 status=none
	fsck.fat -n build/ps-test/fat32-written.part
	# a file made with its folders on make sd's card (save.zig's makeFile test)
	dd if=build/ps-test/sd-made.img of=build/ps-test/sd-made.part bs=512 skip=2048 status=none
	fsck.fat -n build/ps-test/sd-made.part
	# ps7_init.c's revision 1.0 and 2.0 tables left out (tools/ps7-trim.sh),
	# on each board's file this machine has
	tools/test_ps7_trim.sh $(wildcard $(ZED_PS7_INIT_C) $(ARTY_PS7_DIR)/ps7_init.c)
	# a screenshot's PNG (platform/ps/png.zig's test made it), inflated by
	# Python's zlib: its size and first pixels (side track O, O.4c)
	python3 -c 'import sys, zlib, struct; d = open("build/ps-test/shot.png", "rb").read(); \
	  assert d[:8] == b"\x89PNG\r\n\x1a\n"; w, h = struct.unpack(">II", d[16:24]); i = d.index(b"IDAT"); n = struct.unpack(">I", d[i-4:i])[0]; \
	  raw = zlib.decompress(d[i+4:i+4+n]); assert (w, h) == (160, 144) and len(raw) == 144 * (1 + 480), (w, h, len(raw)); \
	  assert raw[0] == 0 and raw[1:4] == bytes([0, 1, 0x5A]), raw[:4]; print("shot.png: 160 x 144, inflated by zlib")'
	tools/gen-input-codes.sh | cmp -s - platform/ps/input_codes.zig || \
	  { echo "platform/ps/input_codes.zig is stale: tools/gen-input-codes.sh > platform/ps/input_codes.zig" >&2; exit 1; }
	python3 tools/keys.py --self-test
	$(MAKE) --no-print-directory $(LINK_BIN)
	python3 tools/test_link.py $(LINK_BIN)

# Interactive: an SDL3 window on the VGA output (platform/sim/play.zig).
# SDL3, called directly from Zig (nixpkgs' SDL2 is only sdl2-compat over it).
# Runs with LD_LIBRARY_PATH unset: the rpaths find SDL3's libraries, and a
# host LD_LIBRARY_PATH (here: an alsa-lib built for a newer glibc) would
# override them.
#   make play SYSTEM=<name> [PLAY_ARGS="--sw 0x20"]
PLAY_ARGS ?=
PLAY_BIN  = $(OBJ_DIR)/vga-play

play: hdl
	@$(MAKE) --no-print-directory play-run

play-run: nix-shell need-system $(PLAY_BIN)
	cd $(CLASH_TOP_DIR) && env -u LD_LIBRARY_PATH $(abspath $(PLAY_BIN)) $(PLAY_ARGS)

$(PLAY_BIN): $(OBJ_DIR)/vtop.o $(OBJ_DIR)/Vtop__ALL.a $(ZIG_SRC)
	$(ZIG_BUILD) play -Dverilator-obj=$(OBJ_DIR) -Dsdl3-libdir=$$(pkg-config --variable=libdir sdl3) --prefix $(OBJ_DIR)

FORCE:

# The machine-wide queue (CLAUDE.md, working rule 10; tools/queue.sh):
# make bit takes the "bit" slot, make ci both ("ci" and "bit": one heavy
# job at a time on the machine, the user, 2026-10-08, after a make ci and
# a make bit side by side ran it out of memory), each waiting its turn,
# first come first served, across every worktree and session. make queue shows who holds a slot and who
# waits. NO_QUEUE=1 skips it: only when the user says so (e.g. the
# queue's state is broken).
comma := ,
# (not under make -n: a dry run doesn't wait in the queue)
QUEUE_RUN = $(if $(filter 1,$(NO_QUEUE))$(findstring n,$(firstword -$(MAKEFLAGS))),,tools/queue.sh run $(1) --)
# Progress (make watch): make ci's steps, suites and chains, Clash runs,
# simulations and Vivado runs log their start and end through
# tools/progress.sh; make watch shows every running one on the machine
# with progress bars (tools/progress-watch.py), make progress prints it
# once (for agents).
PROGRESS = $(abspath tools/progress.sh) run
# make ci and make bit warn (never stop) when heavy work runs that no
# job tracks (a branch from before the tracking, or run by hand): the
# queue can't see it. Not under make -n, nor inside a job.
UNTRACKED_WARN = $(if $(findstring n,$(firstword -$(MAKEFLAGS)))$(PROGRESS_JOB),true,tools/progress-watch.py --untracked)
# make version: the PS app's version mark per board, as the SD menu shows
# it at the title bar's right end ("PS 3fa9c1"; tools/ps-version.sh): the
# board runs this build when the two agree.
# PS7_DIR (make ci on the worker, tools/ci-remote.sh; side K, the user
# 2026-10-18): both boards' ps7_init from a directory of copies,
# <dir>/zedboard/ and <dir>/arty-z7-20/, where there is no Vivado (the
# ZedBoard's is Vivado's sample, the Arty's generated and git-ignored).
PS7_DIR ?=
ZED_PS7_DIR = $(if $(PS7_DIR),$(abspath $(PS7_DIR))/zedboard,$(abspath $(dir $(VIVADO_SETTINGS))..)/data/embeddedsw/sample_sdt/zed/sdt)
ZED_PS7_INIT_C = $(ZED_PS7_DIR)/ps7_init.c
.PHONY: version
version:
	@echo "zedboard:   PS $$(tools/ps-version.sh --mark zedboard "$(ZED_PS7_INIT_C)" $(FEATURE_ZIG))"
	@echo "arty-z7-20: PS $$(tools/ps-version.sh --mark arty-z7-20 "$(abspath $(ARTY_PS7_DIR))/ps7_init.c" $(FEATURE_ZIG))"
.PHONY: queue test-queue bit-run watch progress
queue:
	@tools/queue.sh show
# The waiting list (CLAUDE.md, working rule 10): a place in line without a
# waiting process. WHO: your session's name (as ListAgents shows it, for
# SendMessage); PENDING=1: your user hasn't said go yet; SLOTS: default
# ci,bit (make ci), bit for make bit. NOTE is passed quoted (any text:
# parentheses once broke it, side I's finding).
#   make queue-join WHO="<name>" [PENDING=1] [SLOTS=bit] [NOTE="..."]
#   make queue-ready WHO="<name>"     make queue-leave WHO="<name>"
.PHONY: queue-join queue-ready queue-leave
queue-join queue-ready queue-leave: QUEUE_WHO_CHECK = @test -n "$(WHO)" || { echo 'make $@: set WHO="<your session name>"' >&2; exit 1; }
queue-join:
	$(QUEUE_WHO_CHECK)
	@tools/queue.sh list add "$(WHO)" $(or $(SLOTS),ci$(comma)bit) $(if $(filter 1,$(PENDING)),--pending) $(if $(NOTE),'$(subst ','\'',$(NOTE))')
queue-ready:
	$(QUEUE_WHO_CHECK)
	@tools/queue.sh list ready "$(WHO)"
queue-leave:
	$(QUEUE_WHO_CHECK)
	@tools/queue.sh list remove "$(WHO)"
# Holds (side track K, C1 and C4): a board or the merge slot kept by a
# session across several commands and the user's time (a board check: make
# prog, the user looks, make load...; a merge: merge main, the gates, the
# fast-forward, make ci ALL=1). SLOTS: board-arty (the default for the
# Arty), board-zedboard, merge.
#   make queue-hold WHO="<name>" [SLOTS=merge] [NOTE="..."]
#   make queue-release WHO="<name>" [SLOTS=merge]
.PHONY: queue-hold queue-release
queue-hold queue-release: QUEUE_WHO_CHECK = @test -n "$(WHO)" || { echo 'make $@: set WHO="<your session name>"' >&2; exit 1; }
queue-hold:
	$(QUEUE_WHO_CHECK)
	@tools/queue.sh hold $(or $(SLOTS),board-$(BOARD_SLOT)) "$(WHO)" $(if $(NOTE),'$(subst ','\'',$(NOTE))')
queue-release:
	$(QUEUE_WHO_CHECK)
	@tools/queue.sh release $(or $(SLOTS),board-$(BOARD_SLOT)) "$(WHO)"
# main fast-forwarded to this branch under the merge slot, in one command
# (side Q's idea, the user 2026-10-28): the hold first, the push only with
# it, only a fast-forward; the slot stays held for the post-merge
# make ci ALL=1 (tools/merge-push.sh).
#   make merge-push WHO="<name>" [REMOTE=origin]
.PHONY: merge-push
merge-push: QUEUE_WHO_CHECK = @test -n "$(WHO)" || { echo 'make $@: set WHO="<your session name>"' >&2; exit 1; }
merge-push:
	$(QUEUE_WHO_CHECK)
	@tools/merge-push.sh "$(WHO)" $(or $(REMOTE),origin)
# Submitted jobs (the user, 2026-10-14: the queue pings the session when
# its work ends): CMD runs detached when it's its turn (its own make ci or
# make bit wait in the queue, however long), and queue-await, run as the
# session's background command, ends when the job does: that is the ping.
#   make queue-submit WHO="<name>" CMD="make ci ALL=1"     (prints JOB)
#   make queue-await JOB=<id> [MAX=<seconds>]               (exit = CMD's)
.PHONY: queue-submit queue-await
queue-submit: QUEUE_WHO_CHECK = @test -n "$(WHO)" || { echo 'make $@: set WHO="<your session name>"' >&2; exit 1; }
queue-submit:
	$(QUEUE_WHO_CHECK)
	@test -n "$(CMD)" || { echo 'make queue-submit: set CMD="<command>", e.g. CMD="make ci"' >&2; exit 1; }
	@tools/queue.sh submit "$(WHO)" -- bash -c '$(subst ','\'',$(CMD))'
queue-await:
	@test -n "$(JOB)" || { echo 'make queue-await: set JOB=<id> (make queue lists them)' >&2; exit 1; }
	@tools/queue.sh await $(JOB) $(MAX)
# every submitted job of the last 3 days (make queue shows the running ones
# and those ended in the last 30 minutes, QUEUE_SHOW_DONE seconds)
.PHONY: queue-jobs
queue-jobs:
	@tools/queue.sh jobs
# a submitted job stopped, with everything it started (queue-await on it
# ends with 130)
.PHONY: queue-cancel
queue-cancel:
	@test -n "$(JOB)" || { echo 'make queue-cancel: set JOB=<id> (make queue lists them)' >&2; exit 1; }
	@tools/queue.sh cancel $(JOB)
# each board's slot: board-arty, board-zedboard
BOARD_SLOT = $(if $(filter arty-z7-20,$(BOARD)),arty,zedboard)
BOARD_QUEUE = $(call QUEUE_RUN,board-$(BOARD_SLOT))
watch:
	@tools/progress-watch.py
progress:
	@tools/progress-watch.py --once
# run as make ci runs them (inside a queued, tracked job: these variables
# set), which broke them once (2026-10-08)
test-queue:
	QUEUE_HELD=ci PROGRESS_JOB=/nonexistent tools/test_queue.sh
test: test-queue
.PHONY: test-merge-push
test-merge-push:
	tools/test_merge_push.sh
test: test-merge-push
.PHONY: test-progress
test-progress:
	QUEUE_HELD=ci PROGRESS_JOB=/nonexistent tools/test_progress.sh
test: test-progress
.PHONY: test-clash-slot test-port-holder
test-clash-slot:
	tools/test_clash_slot.sh
test: test-clash-slot
test-port-holder:
	tools/test_port_holder.sh
test: test-port-holder
# the result cache's six checks (side track K, D3), on a scratch copy and
# a cache of their own; run with CI_CACHE and its log unset (they set them)
.PHONY: test-ci-cache
test-ci-cache: nix-shell
	env -u CI_CACHE -u CI_CACHE_LOG -u CI_CACHE_RUN tools/test_ci_cache.sh
test: test-ci-cache
# the bitstream cache (a fake vivado; no Vivado needed)
.PHONY: test-bit-cache
test-bit-cache:
	env -u CI_CACHE -u CI_CACHE_LOG -u CI_CACHE_RUN tools/test_bit_cache.sh
test: test-bit-cache
# make sd's refusal of a card that wouldn't boot (no board needed)
.PHONY: test-sd-guard
test-sd-guard: nix-shell
	tools/test_sd_guard.sh
test: test-sd-guard
# make sd-sync's tools/sd-sync.sh, on a folder standing in for the card
.PHONY: test-sd-sync
test-sd-sync: nix-shell
	tools/test_sd_sync.sh
test: test-sd-sync

# Layer 5. Fails on critical warnings, negative slack, untimed paths or DRC
# errors (boards/zedboard/build.tcl). Reports: $(VIVADO_DIR)/reports/.
#
# The bitstream cache (side track K; tools/ci-cache.py, docs/TESTING.md,
# "The result cache"): a build that passed its gates is kept under a key
# of everything Vivado reads (the Clash output with its memory images, the
# connector, the boards' Tcl, the wrappers and XDC, the top, the Vivado
# installation). make bit restores it, before taking the queue's slot, and
# prints its timing; CI_CACHE=off builds anyway, CI_CACHE=write (a full make ci
# ALL=1) builds and keeps it. (Vivado's incremental implementation, tried
# 2026-10-12, needs more than the BASIC licence: "doesn't cover the
# feature", Vivado 12-29181.)
BIT_KEY = tools/ci-cache.py bit-key --dirs "$(CLASH_TOP_DIR)" \
  --files "$(CLASH_CONNECTOR) $(sort $(wildcard boards/zedboard/*.tcl $(BOARD_DIR)/*.tcl)) $(addprefix $(BOARD_DIR)/,$(BOARD_WRAPPER)) $(XDC)" \
  --text "$(BOARD) $(SYSTEM) $(BUILD_TOP) $(BOARD_WRAPPER) $(BOARD_IP_TCL) $(XDC) $(VIVADO_SETTINGS)"
bit: need-system
	@$(UNTRACKED_WARN)
	@if [ -f "$(CLASH_TOP_DIR)/clash-manifest.json" ] && tools/ci-cache.py bit-restore "$$($(BIT_KEY))" $(VIVADO_DIR); then :; \
	  else $(call QUEUE_RUN,bit) $(MAKE) --no-print-directory bit-run; fi
bit-run: need-system
	@test -f "$(CLASH_TOP_DIR)/clash-manifest.json" || \
	  { echo "make bit: no Clash output in $(CLASH_TOP_DIR); run make hdl SYSTEM=$(SYSTEM) first" >&2; exit 1; }
	@test -f "$(CLASH_CONNECTOR)" || \
	  { echo "make bit: missing $(CLASH_CONNECTOR); run make hdl SYSTEM=$(SYSTEM) first" >&2; exit 1; }
	@command -v vivado >/dev/null || test -f "$(VIVADO_SETTINGS)" || \
	  { echo "make bit: no vivado on PATH and VIVADO_SETTINGS=$(VIVADO_SETTINGS) not found" >&2; exit 1; }
	mkdir -p $(VIVADO_DIR)
	rm -rf $(VIVADO_DIR)/*.bit $(VIVADO_DIR)/ip
	k=$$($(BIT_KEY)) && \
	$(PROGRESS) bit $(BOARD):$(SYSTEM) $(abspath $(VIVADO_DIR))/build.log -- $(call VIVADO,build,$(if $(BUILD_TOP),-top $(BUILD_TOP)) \
	  $(foreach w,$(BOARD_WRAPPER),-wrapper "$(abspath $(BOARD_DIR)/$(w))") \
	  $(foreach i,$(BOARD_IP_TCL),-ip "$(abspath $(BOARD_DIR)/$(i))") \
	  "$(abspath $(CLASH_TOP_DIR))" "$(abspath $(VIVADO_DIR))" \
	  "$(abspath $(CLASH_CONNECTOR))" $(foreach x,$(XDC),"$(abspath $(x))")) && \
	tools/ci-cache.py bit-store $$k $(VIVADO_DIR)

# make bit's cache key (tools/test_bit_cache.sh)
.PHONY: ci-bit-key
ci-bit-key: need-system
	@$(BIT_KEY)

# Layer 6. Programs the PL (volatile; power-cycle clears it). For systems
# with a PS app (PS_APP in zedboard.mk) it uses xsdb instead: program the
# PL, run the ZedBoard ps7_init, load the app into OCM and start it
# (boards/zedboard/prog_ps.tcl). PS_INIT=app (side track F) leaves
# ps7_init to the app, as when it boots from the SD card.
PS_INIT ?= xsdb
#
# Side track G: the JTAG cable is picked first (tools/jtag-pick.sh, by
# its serial from local/boards.mk; refuses rather than guess with two
# boards plugged in), and only that cable is programmed. make jtag-cables
# shows the cables and the choice without programming anything.
# (the board's slot first: side track K, C1)
prog: need-system
	@$(BOARD_QUEUE) $(MAKE) --no-print-directory prog-run
.PHONY: prog-run
prog-run: need-system
	@set -- $(VIVADO_DIR)/*.bit; test $$# -eq 1 -a -f "$$1" || \
	  { echo "make prog: expected exactly one .bit in $(VIVADO_DIR); run make bit SYSTEM=$(SYSTEM)" >&2; exit 1; }
	$(JTAG_PICK) > $(VIVADO_DIR)/jtag-serial
ifeq ($(PS_APP),)
	$(call VIVADO,program,"$(abspath $(wildcard $(VIVADO_DIR)/*.bit))" "$$(cat $(abspath $(VIVADO_DIR))/jtag-serial)")
else
	@test -f "$(PS_ELF)" || \
	  { echo "make prog: no $(PS_ELF); run nix develop -c make ps-app SYSTEM=$(SYSTEM)" >&2; exit 1; }
	@test -f "$(PS7_INIT)" || { echo "make prog: no ps7_init at $(PS7_INIT)" >&2; exit 1; }
	$(call XSDB,prog_ps,"$(abspath $(wildcard $(VIVADO_DIR)/*.bit))" "$(if $(filter app,$(PS_INIT)),-,$(PS7_INIT))" "$(abspath $(PS_ELF))" "$$(cat $(VIVADO_DIR)/jtag-serial)")
endif

# --- Side track G: JTAG cable selection ---------------------------------------
# local/boards.mk (git-ignored) names each board's cable:
#   JTAG_SERIAL_zedboard   = 210248XXXXXX
#   JTAG_SERIAL_arty-z7-20 = XXXXXXXXXXXX
# (make jtag-cables lists the connected ones). Rules: tools/jtag-pick.sh.
-include local/boards.mk
JTAG_SERIALS = $(foreach b,$(BOARDS),$(b)=$(JTAG_SERIAL_$(b)))
JTAG_PICK    = VIVADO_SETTINGS="$(VIVADO_SETTINGS)" JTAG_SERIALS="$(JTAG_SERIALS)" tools/jtag-pick.sh $(BOARD)
.PHONY: jtag-cables test-jtag
jtag-cables:
	@$(JTAG_PICK) >/dev/null && echo "make jtag-cables: make prog BOARD=$(BOARD) would program that cable; nothing was programmed"
# the picker's rules with made-up cable lists (no hardware); part of make test
test-jtag:
	tools/test-jtag-pick.sh
test: test-jtag

# --- PS software (Zig, bare metal, Cortex-A9; platform/ps/) ----------------
# PS_APP (zedboard.mk) names platform/ps/<app>.zig. make hdl builds it too,
# since Zig is only in the dev shell and make prog runs outside it.
PS_ELF   = $(if $(PS_APP),build/ps/$(PS_APP).elf)
PS7_INIT ?= $(ZED_PS7_DIR)/ps7_init.tcl
ifeq ($(BOARD),arty-z7-20)
# Side track G phase 2: the Arty's app (board_arty_z7_20.zig) with its own
# generated ps7_init (make ps7-init BOARD=arty-z7-20), not the zed sample.
PS_ELF     = $(if $(PS_APP),build/arty-z7-20/ps/$(PS_APP).elf)
PS7_INIT   = $(abspath $(ARTY_PS7_DIR))/ps7_init.tcl
PS7_INIT_C = $(abspath $(ARTY_PS7_DIR))/ps7_init.c
endif

ps-app: nix-shell need-system $(PS_ELF)

# Side track F: the SD card's boot image, build/boot/BOOT.BIN, with the PS
# app as the boot ROM's first-stage program (it runs ps7_init itself).
# bootgen comes with Vivado (tools/bootgen.sh, like xsdb), so this runs
# outside the dev shell, like make prog; the app is built inside it first
# (nix develop -c make ps-app SYSTEM=chip8; every PS system uses the same
# app). The header is checked against the boot ROM's rules
# (tools/check-boot-image.sh).
BOOT_ELF = build/ps/relay.elf
BOOT_DIR = build/boot
# Side track G phase 2, step 5: the Arty's boot image, its relay (built
# with -Dboard=arty-z7-20 and its generated ps7_init.c) in
# build/arty-z7-20/boot/, where make sd BOARD=arty-z7-20 takes it from.
ifeq ($(BOARD),arty-z7-20)
BOOT_ELF = build/arty-z7-20/ps/relay.elf
BOOT_DIR = build/arty-z7-20/boot
endif
.PHONY: boot-image
boot-image:
	@test -f "$(BOOT_ELF)" || \
	  { echo "make boot-image: no $(BOOT_ELF); run nix develop -c make ps-app SYSTEM=chip8$(if $(filter arty-z7-20,$(BOARD)), BOARD=arty-z7-20)" >&2; exit 1; }
	mkdir -p $(BOOT_DIR)
	printf 'the_ROM_image:\n{\n  [bootloader] %s\n}\n' "$(abspath $(BOOT_ELF))" > $(BOOT_DIR)/boot.bif
	VIVADO_SETTINGS="$(VIVADO_SETTINGS)" tools/bootgen.sh -arch zynq -image $(BOOT_DIR)/boot.bif \
	  -o $(BOOT_DIR)/BOOT.BIN -w on
	tools/check-boot-image.sh $(BOOT_DIR)/BOOT.BIN

# +strict_align: the app runs with the MMU off, where unaligned accesses
# fault (they did, on the first board run of the SD menu); the assembly is
# checked for that (tools/check-ps-align.sh).
#
# Side track F: the app runs ps7_init itself when nobody else has (booted
# from the SD card), so Vivado's generated C version for the ZedBoard,
# PS7_INIT_C, is linked in (compiled by zig; platform/ps/c/ stands in for
# Xilinx's xil_io.h; its warnings are the generator's, so -w). Its
# assembly is checked for alignment too.
#
# Side track H: built by build.zig (zig build ps-app: the target, linker
# script and entry point; the ps7_init.s compile), which installs the ELF
# only if both checks pass; the old ELF is removed first, so a failed
# check leaves none: nothing may load or package it. Side track J: the
# relay's stack along its deepest known paths (tools/check-ps-stack.py,
# after a system switch overflowed it on the board) is checked the same way.
PS7_INIT_C ?= $(dir $(PS7_INIT))ps7_init.c
build/ps/%.elf: platform/ps/%.zig $(ZIG_SRC) $(PS_FEATURES) platform/ps/link.ld platform/ps/c/xil_io.h tools/check-ps-align.sh
	@test -f "$(PS7_INIT_C)" || { echo "no ps7_init.c at $(PS7_INIT_C) (Vivado's ZedBoard sample; PS7_INIT_C=...)" >&2; exit 1; }
	rm -f $@
	$(ZIG_BUILD) ps-app -Dps-app=$* -Dps7-init-c="$(PS7_INIT_C)" $(FEATURE_ZIG) -Dps-version="$$(tools/ps-version.sh --mark zedboard "$(PS7_INIT_C)" $(FEATURE_ZIG))" --prefix build
	$(if $(filter relay,$*),python3 tools/check-ps-stack.py build/ps/relay.s || { rm -f $@; exit 1; })

# Layers 0-4: every Haskell test, then lint and simulate the generated Verilog.
# CI_SIM systems need the VGA ports of platform/sim/vtop.cpp.
# Two lists of chains (side track K's D2; the planning review, the user,
# 2026-10-08): CI_LINT, the Arty's, run by every make ci; CI_LINT_ALL,
# the ZedBoard's, only by make ci ALL=1 (after each fast-forward of
# main, before a milestone tag, on a branch that changes a ZedBoard-only
# file). A new system registers CI_LINT += arty-<name> and
# CI_LINT_ALL += <name>; each chain still runs alone (make ci-<name>).
CI_LINT     =
CI_LINT_ALL = m0a m0b m0c m0d chip8 gb nes m0e
CI_SIM  = m0b m0d
# CHIP-8 test ROMs through the generated Verilog: <rom>:<frames>, each
# compared with systems/chip8/test/golden/vga/<rom>.png.
CI_CHIP8 = 1-chip8-logo:70 2-ibm-logo:30 3-corax+:200 4-flags:200
CHIP8_ROMS = roms/chip8/timendus
# make ci: the test suites (make test, quick tier unless FULL=1), then
# (or at the same time, CI_OVERLAP=1) per system and board lint and the
# layer 4 runs. Each
# chain (ci-<name>: the Arty's, with ALL=1 the ZedBoard's too) runs in parallel with the
# others, CI_JOBS at a time, the longest (CI_LONG) first; within a chain,
# `make hdl` runs once and further ROMs reuse the simulation model (make
# resim). Before that, ci-prep builds what both halves read, so they don't
# build it at the same time: test ROMs and programs, the Haskell packages
# (the image generators), both boards' PS app (every system's relay).
# CI_OVERLAP=1 runs the suites and layer 4 at the same time: faster on an
# otherwise idle machine, but off by default: measured 2026-10-07, the
# suites (gb-lockstep alone 4 to 13 GB) with layer 4's Clash runs (an
# 800XL's about 10 GB) need more than the machine's 30 GB, and it swapped.
CI_JOBS    ?= 3
CI_OVERLAP ?= 0
# measured 2026-10-07: the 800XL's chains about 30 to 37 minutes, the
# NES's and Game Boy's 8 to 14, every other one less. The two 800XL
# chains' Clash runs take turns (CLASH_HEAVY), so the second starts a
# few chains later, about when the first one's Clash run ends.
CI_LONG    = a800xl arty-nes nes arty-a800xl arty-gg gg arty-gb gb
# the chains this run includes, the longest first
CI_RUN     = $(CI_LINT) $(if $(filter 1,$(ALL)),$(CI_LINT_ALL))
CI_CHAINS  = $(filter $(CI_RUN),$(CI_LONG)) $(filter-out $(CI_LONG),$(CI_RUN))
# CI_AFTER chains (below) run in every make ci, CI_AFTER_ALL's with ALL=1
CI_AFTER_RUN = $(strip $(CI_AFTER) $(if $(filter 1,$(ALL)),$(CI_AFTER_ALL)))
.PHONY: ci-run ci-prep ci-tests ci-layer4 ci-list ci-remote ci-remote-start ci-remote-wait
# make ci-remote [ALL=1] [FULL=1]: make ci on the remote worker, detached
# there, waited for, its passes pulled into this machine's cache
# (tools/ci-remote.sh; docs/TESTING.md, "make ci on the worker");
# ci-remote-start only starts it, ci-remote-wait [MAX=<s>] waits (exit 124
# after MAX seconds, the run going on)
CI_REMOTE_ARGS = $(if $(ALL),ALL=$(ALL)) $(if $(FULL),FULL=$(FULL)) $(if $(filter-out on,$(CI_CACHE)),CI_CACHE=$(CI_CACHE))
ci-remote:
	tools/ci-remote.sh $(CI_REMOTE_ARGS)
ci-remote-start:
	tools/ci-remote.sh --start $(CI_REMOTE_ARGS)
ci-remote-wait:
	CI_REMOTE_WAIT_MAX=$(or $(MAX),0) tools/ci-remote.sh --wait
# make ci-list [ALL=1]: the chains make ci would run, in order (one per
# line; the ones after the pool marked "after")
ci-list:
	@for c in $(CI_CHAINS); do echo "ci-$$c"; done
	@for c in $(CI_AFTER_RUN); do echo "ci-$$c (after)"; done
# The result cache (side track K, D3; tools/ci-cache.py, docs/TESTING.md,
# "The result cache"): a suite or chain whose inputs have a cached pass is
# skipped and reported "cached"; make ci ends with "N run, M cached".
# ALL=1 reads it too (the user, 2026-10-15: so the post-merge run reruns
# only what the merge changed); CI_CACHE=write runs everything and records
# the passes (`make ci ALL=1 CI_CACHE=write`: before a milestone tag and
# weekly); CI_CACHE=off turns it off for a run.
CI_CACHE ?= on
ci:
	@$(UNTRACKED_WARN)
	@$(call QUEUE_RUN,ci$(comma)bit) $(PROGRESS) ci ci -- \
	  env CI_CACHE=$(CI_CACHE) CI_CACHE_LOG=$(abspath build/ci-cache.log) $(MAKE) --no-print-directory ci-run
ci-run:
	@tools/progress.sh plan step prep tests layer4
	@tools/progress.sh plan chain $(CI_CHAINS) $(CI_AFTER_RUN)
	@tools/progress.sh serial chain $(CI_AFTER_RUN)
	@mkdir -p build && rm -f build/ci-cache.log
	$(MAKE) --no-print-directory ci-step-prep
	$(MAKE) --no-print-directory $(if $(filter 1,$(CI_OVERLAP)),-j2) ci-step-tests ci-step-layer4
	@tools/ci-cache.py summary
	@echo "make ci: OK"
# make ci's steps and chains, each logged for make watch
ci-step-%:
	@$(PROGRESS) step $* -- $(MAKE) --no-print-directory ci-$*
ci-chain-%:
	@$(PROGRESS) chain $* -- tools/ci-cache.py chain $* -- $(MAKE) --no-print-directory ci-$*
ci-prep: nix-shell check-xdc roms gb-asm nes-asm a800-asm m68k-asm
	$(CABAL) build all
	# both boards' PS app in every make ci, each with its stack check (the
	# user, 2026-10-13: the ZedBoard's, with ALL=1 only before, once stood
	# 72 bytes under the limit unseen; a few seconds, Zig caches it)
	$(MAKE) --no-print-directory BOARD=zedboard ps-app SYSTEM=chip8
	$(MAKE) --no-print-directory BOARD=arty-z7-20 ps-app SYSTEM=chip8
	$(ZIG_BUILD) ps-encode --prefix build
ci-tests:
	CI_CACHE_RUN=1 $(MAKE) --no-print-directory -j1 test
# CI_AFTER: chains that run after the pool, one at a time, without
# BOARD pinned (e.g. one that re-simulates the models the pool's chains
# built and writes their frames: it can't run beside them). Register with
# CI_AFTER += <name>, as CI_LINT (CI_AFTER_ALL += <name> for one that
# only runs with ALL=1).
CI_AFTER ?=
CI_AFTER_ALL ?=
ci-layer4:
	$(MAKE) --no-print-directory BOARD=zedboard -j$(CI_JOBS) $(addprefix ci-chain-,$(CI_CHAINS))
	$(if $(CI_AFTER_RUN),$(MAKE) --no-print-directory -j1 $(addprefix ci-chain-,$(CI_AFTER_RUN)))

.PHONY: $(addprefix ci-,$(CI_LINT_ALL))
ci-m0a ci-m0c:
	$(MAKE) --no-print-directory BOARD=zedboard lint SYSTEM=$(@:ci-%=%)
ci-m0b ci-m0d:
	$(MAKE) --no-print-directory BOARD=zedboard sim SYSTEM=$(@:ci-%=%)
	$(MAKE) --no-print-directory BOARD=zedboard lint-run SYSTEM=$(@:ci-%=%)
ci-chip8:
	$(MAKE) --no-print-directory BOARD=zedboard sim SYSTEM=chip8 ROM=$(CHIP8_ROMS)/$(firstword $(subst :, ,$(firstword $(CI_CHIP8)))).ch8 \
	  FRAMES=$(lastword $(subst :, ,$(firstword $(CI_CHIP8))))
	$(MAKE) --no-print-directory BOARD=zedboard lint-run SYSTEM=chip8
	@for s in $(wordlist 2,99,$(CI_CHIP8)); do $(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=chip8 \
	  ROM=$(CHIP8_ROMS)/$${s%%:*}.ch8 FRAMES=$${s##*:} || exit 1; done
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=chip8 ROM=$(CHIP8_ROMS)/5-quirks.ch8 \
	  RAM_PRESET=0x1FF=1 FRAMES=400 SIM_GOLDEN=systems/chip8/test/golden/vga/5-quirks-chip8.png
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=chip8 ROM=$(CHIP8_ROMS)/5-quirks.ch8 \
	  RAM_PRESET=0x1FF=1 FRAMES=400 SIM_ARGS="--sw 0x3F" \
	  SIM_GOLDEN=systems/chip8/test/golden/vga/5-quirks-chip8-sw1f.png
	# The PS link (M1b stage C): the menu's text overlay written as the PS
	# app does (platform/sim/ps.zig over platform/ps/pl.zig), and PS_IN
	# with BTNC held, SW5 on and the CHIP-8 system ID.
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=chip8 FRAMES=30 SIM_PS=systems/chip8/test/ps/menu.ps \
	  SIM_GOLDEN=systems/chip8/test/golden/vga/menu-overlay.png \
	  SIM_ARGS="--sw 0x20 --btn 0x01 --expect-ps-in 0x00012001"
ci-gb:
	# (the PS's PL reset held for the first 100000 pixel clocks: no HP0 read
	# meanwhile, as ci-arty-gb; the reset race, side track I 2026-10-10)
	$(MAKE) --no-print-directory BOARD=zedboard sim SYSTEM=gb ROM=roms/gb/dmg-acid2/dmg-acid2.gb FRAMES=25 \
	  SIM_ARGS="--ps-reset 100000"
	$(MAKE) --no-print-directory BOARD=zedboard lint-run SYSTEM=gb
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gb ROM=roms/gb/blargg/instr_timing.gb FRAMES=45
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gb ROM=build/gb/asm/buttons.gb FRAMES=18 SIM_ARGS="--btn 0x09"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gb ROM=build/gb/asm/tone.gb FRAMES=20 SIM_ARGS="--expect-tone 439.8"
	# The Game Boy on the PS link (side track A): its menu screen through the
	# PS app's encoder, and PS_IN with BTNC, SW0 and the Game Boy's ID (2).
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gb FRAMES=25 SIM_PS=systems/gb/test/ps/menu.ps \
	  SIM_GOLDEN=systems/gb/test/golden/vga/menu-overlay.png \
	  SIM_ARGS="--btn 0x01 --sw 0x01 --expect-ps-in 0x00020101"
	# ... and the game's serial output on PS_IN's byte channel (for the desktop)
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gb ROM=build/gb/asm/serial_pass.gb FRAMES=20 \
	  SIM_ARGS="--expect-ps-text Passed"
	# Battery saves (side track I): a .sav into cartridge RAM while held,
	# save.gb counts the boot (its screen: 00042), cartridge RAM read back
	# on the byte channel while the game runs; PS_IN with the dirty flag
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gb ROM=build/gb/asm/save.gb FRAMES=20 \
	  SIM_PS=systems/gb/test/ps/save.ps \
	  SIM_ARGS="--expect-ps-hex $(abspath systems/gb/test/ps/save.hex) --expect-ps-in 0x0F020040"

# The Game Boy Color (side track I, phase 3): gbc.bit's top (Gb.TopCgb) with
# cgb-acid2 against its VGA and HDMI goldens, and sound in double speed
# (the harness's S/PDIF and AXI checks).
CI_LINT_ALL += gbc
ci-gbc:
	# (the PS's PL reset held at first, as ci-gb)
	$(MAKE) --no-print-directory BOARD=zedboard sim SYSTEM=gbc FRAMES=25 SIM_ARGS="--ps-reset 100000"
	$(MAKE) --no-print-directory BOARD=zedboard lint-run SYSTEM=gbc
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gbc ROM=build/gb/asm/sound_ds.gb FRAMES=20
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gbc ROM=roms/gb/dmg-acid2/dmg-acid2.gb FRAMES=25 \
	  SIM_PS=systems/gb/test/ps/palette.ps SIM_GOLDEN=systems/gb/test/golden/vga/dmg-acid2-gbc.png \
	  SIM_HDMI_GOLDEN=systems/gb/test/golden/hdmi/dmg-acid2-gbc.png
	# battery saves on the GBC, as ci-gb's: the .sav in while held, save.gb
	# counts its boot, the read-back on the byte channel; PS_IN with the
	# dirty flag and system 6
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gbc ROM=build/gb/asm/save.gb FRAMES=20 \
	  SIM_PS=systems/gb/test/ps/save.ps SIM_GOLDEN= SIM_HDMI_GOLDEN= \
	  SIM_ARGS="--expect-ps-hex $(abspath systems/gb/test/ps/save.hex) --expect-ps-in 0x0F060040"

# The NES (M3 step 3.7): our scene ROM, nestest's menu, and controller 1
# through the board buttons (BTNC = A, BTNU = Up).
ci-nes:
	$(MAKE) --no-print-directory BOARD=zedboard sim SYSTEM=nes FRAMES=20
	$(MAKE) --no-print-directory BOARD=zedboard lint-run SYSTEM=nes
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=nes ROM=roms/nes/nestest/nestest.nes FRAMES=35
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=nes ROM=build/nes/asm/buttons.nes FRAMES=20 SIM_ARGS="--btn 0x11"
	# sound (step 3.9): pulse 1 at 440.4 Hz through the mixer, the filters and S/PDIF
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=nes ROM=build/nes/asm/tone.nes FRAMES=20 SIM_ARGS="--expect-tone 440.4"
	# ... and no whine from a triangle at period 0 (aliased from 56 kHz; heard on the board)
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=nes ROM=build/nes/asm/tri_ultra.nes FRAMES=20 SIM_ARGS="--expect-quiet 4"
	# The NES on the PS link (step 3.8): its menu screen through the PS app's
	# encoder, and PS_IN with BTNC, SW0 and the NES's ID (3) ...
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=nes FRAMES=25 SIM_PS=systems/nes/test/ps/menu.ps \
	  SIM_GOLDEN=systems/nes/test/golden/vga/menu-overlay.png \
	  SIM_ARGS="--btn 0x01 --sw 0x01 --expect-ps-in 0x00030101"
	# ... and nestest loaded over the link (as the menu does) into the
	# running build of ppu_scene: the loader, the header decoder, restart
	tools/ps-load-script.sh roms/nes/nestest/nestest.nes build/nes/load-nestest.ps
	# (PRG ROM is read from DDR since side track D: the harness's DDR holds
	# nestest from the start, standing for the PS's DDR write while held)
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=nes FRAMES=45 SIM_PS=build/nes/load-nestest.ps \
	  SIM_GOLDEN=systems/nes/test/golden/vga/nestest.png SIM_ARGS="--ddr-file 0x02000000=$(abspath roms/nes/nestest/nestest.nes)"

# Run the simulation model of the last `make sim` / `make hdl` again with
# another ROM (or RAM preset): only the memory image is regenerated and
# copied next to the Verilog ($readmemb loads it at start-up); no Clash
# run, and Verilator only rebuilds if the Verilog changed. After changing
# the design, run make sim (or make hdl) first.
resim: nix-shell need-system $(HDL_PREREQ)
	@test -n "$(CLASH_V)" || { echo "make resim: no Clash output for $(SYSTEM); run make sim SYSTEM=$(SYSTEM) first" >&2; exit 1; }
	$(if $(HDL_PREREQ),cp -f $(HDL_PREREQ) $(CLASH_TOP_DIR)/)
	@$(MAKE) --no-print-directory sim-run

clean:
	rm -rf dist-newstyle build

# --- NES (milestone 3) -------------------------------------------------------
# NES regression ROMs (CLAUDE.md rule 5): every systems/nes/test/asm/*.s
# assembles into build/nes/asm/*.nes (ca65/ld65, layout nrom.cfg). The nes
# tests read them from there; they are never hand-patched.
NES_ASM_DIR = systems/nes/test/asm
NES_ASM     = $(patsubst $(NES_ASM_DIR)/%.s,build/nes/asm/%.nes,$(wildcard $(NES_ASM_DIR)/*.s))

.PHONY: nes-asm
nes-asm: nix-shell $(NES_ASM)

build/nes/asm/%.nes: $(NES_ASM_DIR)/%.s $(NES_ASM_DIR)/nrom.cfg $(wildcard $(NES_ASM_DIR)/*.inc)
	mkdir -p $(@D)
	ca65 -I $(NES_ASM_DIR) -o $(@:.nes=.o) $<
	ld65 -C $(NES_ASM_DIR)/nrom.cfg -o $@.part $(@:.nes=.o)
	mv $@.part $@
# (side track D) the MMC5 test has its own layout: eight PRG banks, 16 KiB CHR
build/nes/asm/mmc5.nes: $(NES_ASM_DIR)/mmc5.s $(NES_ASM_DIR)/mmc5.cfg
	mkdir -p $(@D)
	ca65 -I $(NES_ASM_DIR) -o $(@:.nes=.o) $<
	ld65 -C $(NES_ASM_DIR)/mmc5.cfg -o $@.part $(@:.nes=.o)
	mv $@.part $@

test: nes-asm

# The shared 6502 package's tests run with the NES (it has no system of
# its own); with no SYSTEM, TEST_TARGETS = all covers it already.
TEST_TARGETS += $(if $(filter nes,$(SYSTEM)),mos6502)

# The shared Z80 package's tests run with the Game Gear (M5), as the 6502's
# with the NES.
TEST_TARGETS += $(if $(filter gg,$(SYSTEM)),z80)

# --- Side track C: the DDR path ----------------------------------------------
# M0e (systems/m0e): the PS link script sets two DDR test runs up and reads
# their result bytes back (test/ps/ddr.ps); the harness's AXI slave
# (platform/sim/axi.zig) plays the HP port with random timing and checks
# the master; the bytes must equal test/golden/ps_bytes.hex (m0e-unit
# checks that golden against its own reference). Then the same with a slower
# slave and another seed.
.PHONY: ci-m0e
ci-m0e:
	$(MAKE) --no-print-directory BOARD=zedboard sim SYSTEM=m0e
	$(MAKE) --no-print-directory BOARD=zedboard lint-run SYSTEM=m0e
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=m0e \
	  SIM_ARGS="--expect-ps-hex $(abspath systems/m0e/test/golden/ps_bytes.hex) --axi-latency 60 --axi-seed 7"

# The DDR test on the board (needs make prog SYSTEM=m0e and J14): the
# suite of platform/ps/ddrtest.zig, or one run (DDR_RUN = 17 hex digits:
# base, mode, words, flags; ddrtest.zig, protocol.zig's T command).
# make ddr-load FILE=… writes a file into DDR through the relay (W) and
# has the PL read it back over the HP port.
DDR_RUN ?= suite
.PHONY: ddr-test ddr-load
ddr-test: nix-shell $(LINK_BIN)
	$(PORT_FREE) $(LINK_BIN) --port "$(KEYS_PORT)" --ddr-test $(DDR_RUN) $(LINK_ARGS)
ddr-load: nix-shell $(LINK_BIN)
	@test -f "$(FILE)" || { echo "make ddr-load: set FILE=<file> (up to 16 MiB)" >&2; exit 1; }
	$(PORT_FREE) $(LINK_BIN) --port "$(KEYS_PORT)" --ddr-load "$(FILE)" $(LINK_ARGS)

# --- Atari 800XL (milestone 4) -----------------------------------------------
# Regression programs (CLAUDE.md rule 5): systems/a800xl/test/asm/cart_*.s
# assemble into 8 KiB cartridges build/a800xl/asm/cart_*.rom (cart8.cfg),
# every other *.s into an XEX build/a800xl/asm/*.xex (xex.cfg, xex.inc);
# also the XEX loader's page, build/a800xl/xexload.bin.
# The a800xl tests read them from there; they are never hand-patched.
A800_ASM_DIR  = systems/a800xl/test/asm
A800_ASM_SRC  = $(wildcard $(A800_ASM_DIR)/*.s)
A800_ASM      = $(patsubst $(A800_ASM_DIR)/%.s,build/a800xl/asm/%.xex,$(filter-out $(A800_ASM_DIR)/cart_% $(A800_ASM_DIR)/xegs_% $(A800_ASM_DIR)/cas_%,$(A800_ASM_SRC))) \
                $(patsubst $(A800_ASM_DIR)/%.s,build/a800xl/asm/%.cas,$(filter $(A800_ASM_DIR)/cas_%,$(A800_ASM_SRC))) \
                $(patsubst $(A800_ASM_DIR)/%.s,build/a800xl/asm/%.rom,$(filter $(A800_ASM_DIR)/cart_%,$(A800_ASM_SRC))) \
                $(patsubst $(A800_ASM_DIR)/%.s,build/a800xl/asm/%.car,$(filter $(A800_ASM_DIR)/xegs_%,$(A800_ASM_SRC)))
A800_ASM_INC  = $(wildcard $(A800_ASM_DIR)/*.inc)

.PHONY: a800-asm
a800-asm: nix-shell $(A800_ASM) build/a800xl/xexload.bin

# The XEX loader's ROM page ($D500-$D5FF; systems/a800xl/loader/): read by
# the reference model and, from step 4.8, baked into the bitstream.
build/a800xl/xexload.bin: systems/a800xl/loader/xexload.s systems/a800xl/loader/loader.cfg
	mkdir -p $(@D)
	ca65 -o $(@:.bin=.o) $<
	ld65 -C systems/a800xl/loader/loader.cfg -o $@.part $(@:.bin=.o)
	mv $@.part $@

build/a800xl/asm/%.xex: $(A800_ASM_DIR)/%.s $(A800_ASM_DIR)/xex.cfg $(A800_ASM_INC)
	mkdir -p $(@D)
	ca65 -I $(A800_ASM_DIR) -o $(@:.xex=.o) $<
	ld65 -C $(A800_ASM_DIR)/xex.cfg -o $@.part $(@:.xex=.o)
	mv $@.part $@

build/a800xl/asm/cart_%.rom: $(A800_ASM_DIR)/cart_%.s $(A800_ASM_DIR)/cart8.cfg $(A800_ASM_INC)
	mkdir -p $(@D)
	ca65 -I $(A800_ASM_DIR) -o $(@:.rom=.o) $<
	ld65 -C $(A800_ASM_DIR)/cart8.cfg -o $@.part $(@:.rom=.o)
	mv $@.part $@

# Side track Q: cas_*.s, cassette boot programs (cas.cfg: one raw block at
# $3000, the boot header first) as .cas tape images (a800-cas).
build/a800xl/asm/cas_%.cas: $(A800_ASM_DIR)/cas_%.s $(A800_ASM_DIR)/cas.cfg $(A800_ASM_INC)
	mkdir -p $(@D)
	ca65 -I $(A800_ASM_DIR) -o $(@:.cas=.o) $<
	ld65 -C $(A800_ASM_DIR)/cas.cfg -o $(@:.cas=.bin) $(@:.cas=.o)
	$(CABAL) run -v0 a800-cas -- $(@:.cas=.bin) $@.part
	mv $@.part $@

# Side track Q (Q.7): xegs_*.s, 32 KiB XEGS cartridges (xegs32.cfg) with
# the .car header in front: "CART", type 12, the bytes' sum (big-endian),
# four zero bytes.
build/a800xl/asm/xegs_%.car: $(A800_ASM_DIR)/xegs_%.s $(A800_ASM_DIR)/xegs32.cfg $(A800_ASM_INC)
	mkdir -p $(@D)
	ca65 -I $(A800_ASM_DIR) -o $(@:.car=.o) $<
	ld65 -C $(A800_ASM_DIR)/xegs32.cfg -o $(@:.car=.bin) $(@:.car=.o)
	sum=$$(od -An -v -tu1 $(@:.car=.bin) | awk '{for (i = 1; i <= NF; i++) s += $$i} END {print s}'); \
	{ printf 'CART\000\000\000\014'; printf "$$(printf '\\%03o\\%03o\\%03o\\%03o' $$((sum >> 24 & 255)) $$((sum >> 16 & 255)) $$((sum >> 8 & 255)) $$((sum & 255)))"; printf '\000\000\000\000'; cat $(@:.car=.bin); } > $@.part
	mv $@.part $@

test: a800-asm

# The shared 6502's suites run with the 800XL too (it is the second
# machine on cpus/mos6502).
TEST_TARGETS += $(if $(filter a800xl,$(SYSTEM)),mos6502)

# The 800XL (M4 step 4.7): scene.s, rasterbars.s and tone.s as executables
# through the loader (their VGA and HDMI goldens from the spec's;
# scene.s is also the regression for the core starting from the
# configuration word read from block RAM), the tone decoded from S/PDIF;
# with FULL=1 also BASIC's Ready screen without an executable (the OS's
# boot without a disk drive takes about 200 frames, 12 minutes).
CI_LINT_ALL += a800xl
.PHONY: ci-a800xl
ci-a800xl: a800-asm
	$(MAKE) --no-print-directory BOARD=zedboard sim SYSTEM=a800xl XEX=build/a800xl/asm/scene.xex FRAMES=100
	$(MAKE) --no-print-directory BOARD=zedboard lint-run SYSTEM=a800xl
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=a800xl XEX=build/a800xl/asm/rasterbars.xex FRAMES=100
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=a800xl XEX=build/a800xl/asm/tone.xex FRAMES=100 SIM_ARGS="--expect-tone 437.8"
	# step 4.8: scene.xex loaded over the PS link as the SD menu does it
	# (A800.Loader), and the keyboard: keys.xex, then A held over the link
	tools/ps-load-script.sh build/a800xl/asm/scene.xex build/a800xl/load-scene.ps
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=a800xl FRAMES=100 SIM_PS=build/a800xl/load-scene.ps \
	  SIM_GOLDEN=systems/a800xl/test/golden/vga/scene.png
	tools/ps-load-script.sh build/a800xl/asm/keys.xex build/a800xl/load-keys.ps
	cat systems/a800xl/test/ps/keys-tail.ps >> build/a800xl/load-keys.ps
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=a800xl FRAMES=100 SIM_PS=build/a800xl/load-keys.ps \
	  SIM_GOLDEN=systems/a800xl/test/golden/vga/keys.png
	$(if $(filter 1,$(FULL)),$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=a800xl FRAMES=200)

# The 800XL on the SD card (M4 step 4.8): each folder into /a800xl as a
# folder of its own name (our regression programs, Acid800's tests,
# Scorch), and a800xl.bit into /systems.
SD_A800_ROMS ?= roms/a800xl/games $(if $(SD_TESTS),build/a800xl/asm roms/a800xl/acid800)
# only make sd's card (exported for every target, it also put /a800xl
# into make test-zig's test card; M5 found it with its GG_DIRS)
sd: export A800_DIRS = $(SD_A800_ROMS)
SD_SYSTEMS += a800xl

# make load SYSTEM=a800xl ROM=<file.xex|.car|.rom> (M4 step 4.8): the file
# from link address 0 as the SD menu sends it; make keys SYSTEM=a800xl
# sends the [a800xl] joystick (the keyboard itself only from the board's
# USB keyboard so far).
LINK_SYSTEM += $(if $(filter a800xl,$(SYSTEM)),--system a800xl)

# --- Side track G: the Arty Z7-20 tops in make ci ------------------------------
# M0a: lint; M0b: its layer 3 golden through TMDS; CHIP-8: the IBM logo's
# VGA golden through TMDS.
# The Game Boy Color on the Arty (side track I, phase 3): cgb-acid2 against
# its golden through TMDS, the menu overlay (PS_IN: SW0, SW1 = A, system
# 6), and tone.gb (a DMG cartridge, compatibility mode) on aud_pwm and in
# the HDMI audio (side track G phase 3).
.PHONY: ci-arty-gbc
ci-arty: ci-arty-gbc
ci-arty-gbc:
	$(MAKE) --no-print-directory sim SYSTEM=gbc BOARD=arty-z7-20 FRAMES=25 SIM_ARGS="--ps-reset 100000"
	$(MAKE) --no-print-directory lint-run SYSTEM=gbc BOARD=arty-z7-20
	$(MAKE) --no-print-directory resim SYSTEM=gbc BOARD=arty-z7-20 FRAMES=25 SIM_PS=systems/gb/test/ps/menu.ps \
	  SIM_GOLDEN=systems/gb/test/golden/vga/menu-overlay.png SIM_ARGS="--sw 0x3 --expect-ps-in 0x00060301"
	$(MAKE) --no-print-directory resim SYSTEM=gbc BOARD=arty-z7-20 ROM=build/gb/asm/tone.gb FRAMES=20 \
	  SIM_GOLDEN= SIM_ARGS="--expect-tone 439.8 --expect-hdmi-tone 439.8"
	# a DMG cartridge's palettes written by the PS while held (Gb.Loader's
	# palette window): dmg-acid2 in them, as the spec draws it
	$(MAKE) --no-print-directory resim SYSTEM=gbc BOARD=arty-z7-20 ROM=roms/gb/dmg-acid2/dmg-acid2.gb FRAMES=25 \
	  SIM_PS=systems/gb/test/ps/palette.ps SIM_GOLDEN=systems/gb/test/golden/rgb/dmg-acid2-gbc.png
	# battery saves, as ci-gbc's (side K, 2026-10-11: the Arty's harness
	# has --expect-ps-hex now): every read-back byte against save.hex
	$(MAKE) --no-print-directory resim SYSTEM=gbc BOARD=arty-z7-20 ROM=build/gb/asm/save.gb FRAMES=20 \
	  SIM_PS=systems/gb/test/ps/save.ps SIM_GOLDEN= \
	  SIM_ARGS="--expect-ps-hex $(abspath systems/gb/test/ps/save.hex) --expect-ps-in 0x0F060040"

# One chain per system, run in make ci's pool beside the ZedBoard's
# (their memory images and scripts are per board); make ci-arty runs them
# all.
CI_LINT += arty-m0 arty-chip8 arty-gb arty-nes arty-gbc
.PHONY: ci-arty ci-arty-m0 ci-arty-chip8 ci-arty-gb ci-arty-nes
ci-arty: ci-arty-m0 ci-arty-chip8 ci-arty-gb ci-arty-nes ci-arty-a800xl
ci-arty-m0:
	$(MAKE) --no-print-directory lint SYSTEM=m0a BOARD=arty-z7-20
	$(MAKE) --no-print-directory sim SYSTEM=m0b BOARD=arty-z7-20 FRAMES=3
	$(MAKE) --no-print-directory lint-run SYSTEM=m0b BOARD=arty-z7-20
ci-arty-chip8:
	$(MAKE) --no-print-directory sim SYSTEM=chip8 BOARD=arty-z7-20 ROM=$(CHIP8_ROMS)/2-ibm-logo.ch8 FRAMES=30
	$(MAKE) --no-print-directory lint-run SYSTEM=chip8 BOARD=arty-z7-20
	# phase 2: the PS link on the Arty, the menu's overlay as ci-chip8 plays
	# it (the ZedBoard's golden), PS_IN with SW0, SW1 (= C) and system 1
	$(MAKE) --no-print-directory resim SYSTEM=chip8 BOARD=arty-z7-20 FRAMES=30 SIM_PS=systems/chip8/test/ps/menu.ps \
	  SIM_GOLDEN=systems/chip8/test/golden/vga/menu-overlay.png SIM_ARGS="--sw 0x3 --expect-ps-in 0x00010301"
	# sound on aud_pwm (phase 2 step 6) and in the HDMI audio (phase 3):
	# Timendus's beep, key B held (SW1 + BTN3)
	$(MAKE) --no-print-directory resim SYSTEM=chip8 BOARD=arty-z7-20 ROM=$(CHIP8_ROMS)/7-beep.ch8 FRAMES=40 \
	  SIM_GOLDEN= SIM_ARGS="--sw 0x3 --btn 0x8 --expect-tone 440.0 --expect-hdmi-tone 440.0"
ci-arty-gb:
	# the Game Boy on the Arty (phase 2): dmg-acid2 from simulated DDR, its
	# menu overlay over the link (PS_IN: SW0, SW1 = A, system 2), and the
	# serial port's bytes on the byte channel, as ci-gb on the ZedBoard
	# (the PS's PL reset held for the first 100000 pixel clocks: no HP0 read
	# meanwhile; the board's white screen after a power-on, phase 3)
	$(MAKE) --no-print-directory sim SYSTEM=gb BOARD=arty-z7-20 ROM=roms/gb/dmg-acid2/dmg-acid2.gb FRAMES=25 \
	  SIM_ARGS="--ps-reset 100000"
	$(MAKE) --no-print-directory lint-run SYSTEM=gb BOARD=arty-z7-20
	# side track O, O.4: a screenshot of dmg-acid2 into DDR through HP0's
	# write channels equals the picture on screen (shown x3 from (80, 24)),
	# in both palettes (SW0)
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=roms/gb/dmg-acid2/dmg-acid2.gb FRAMES=25 SIM_GOLDEN= \
	  SIM_PS=systems/gb/test/ps/capture.ps SIM_ARGS="--expect-capture 160,144,80,24,3"
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=roms/gb/dmg-acid2/dmg-acid2.gb FRAMES=25 SIM_GOLDEN= \
	  SIM_PS=systems/gb/test/ps/capture.ps SIM_ARGS="--sw 0x1 --expect-capture 160,144,80,24,3"
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 FRAMES=25 SIM_PS=systems/gb/test/ps/menu.ps \
	  SIM_GOLDEN=systems/gb/test/golden/vga/menu-overlay.png SIM_ARGS="--sw 0x3 --expect-ps-in 0x00020301"
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/serial_pass.gb FRAMES=20 \
	  SIM_GOLDEN= SIM_ARGS="--expect-ps-text Passed"
	# ... and the APU's tone on aud_pwm, as ci-gb hears it on S/PDIF
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/tone.gb FRAMES=20 \
	  SIM_GOLDEN= SIM_ARGS="--expect-tone 439.8 --expect-hdmi-tone 439.8"
	# ... and battery saves, as ci-gb's (side K, 2026-10-11): the .sav in
	# while held, save.gb counts its boot, every read-back byte against
	# save.hex; PS_IN with the dirty flag
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/save.gb FRAMES=20 \
	  SIM_PS=systems/gb/test/ps/save.ps SIM_GOLDEN= \
	  SIM_ARGS="--expect-ps-hex $(abspath systems/gb/test/ps/save.hex) --expect-ps-in 0x0F020040"
	# side track O, O.1: frame-synchronous input. joyecho.gb sends each new
	# direction nibble and the line it was seen on: with the movie
	# register's sync bit, Right pressed mid-frame reaches the core at its
	# frame start (LY 144, $90); without it at the line it was pressed on
	# ($78); built with MOVIES=0 SCREENSHOTS=0 (its own build directory)
	# there is no latch, so the sync script gives the live result
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/joyecho.gb FRAMES=6 SIM_GOLDEN= \
	  SIM_PS=systems/gb/test/ps/movie_sync.ps SIM_ARGS="--expect-ps-hex $(abspath systems/gb/test/ps/movie_sync.hex)"
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/joyecho.gb FRAMES=6 SIM_GOLDEN= \
	  SIM_PS=systems/gb/test/ps/movie_live.ps SIM_ARGS="--expect-ps-hex $(abspath systems/gb/test/ps/movie_live.hex)"
	# side track O, O.2: the determinism gate. joybank.gb mixes its pads and
	# 64 reads a frame over 112 KiB of banked ROM (most miss the DDR cache)
	# into a digest it sends each frame. Recorded with the PS app's frame
	# rule (arty-sim --movie-record) under one random DDR latency, played
	# back (--movie-play) under another: the same bytes and frames. The
	# control: the same keys sent live under the second latency diverge
	# (the gate would see a lost frame): that run must differ
	# (--expect-ps-hex-differ: a `! $(MAKE)` line fails make -n, which the
	# result cache keys chains with)
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/joybank.gb FRAMES=16 SIM_GOLDEN= \
	  SIM_PS=systems/gb/test/ps/movie_rec.ps SIM_ARGS="--axi-latency 60 --axi-seed 1 \
	  --movie-record $(abspath build/arty-z7-20/gb/joybank.movie) --expect-ps-hex $(abspath systems/gb/test/ps/movie_bank.hex)"
	rm -rf build/arty-z7-20/gb/movie-frames && cp -r build/arty-z7-20/gb/frames build/arty-z7-20/gb/movie-frames
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/joybank.gb FRAMES=16 SIM_GOLDEN= \
	  SIM_PS=systems/gb/test/ps/movie_play.ps SIM_ARGS="--axi-latency 60 --axi-seed 99 \
	  --movie-play $(abspath build/arty-z7-20/gb/joybank.movie) --expect-ps-hex $(abspath systems/gb/test/ps/movie_bank.hex) \
	  --expect $(abspath build/arty-z7-20/gb/movie-frames)"
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/joybank.gb FRAMES=16 SIM_GOLDEN= \
	  SIM_PS=systems/gb/test/ps/movie_rec.ps SIM_ARGS="--axi-latency 60 --axi-seed 99 --expect-ps-hex-differ $(abspath systems/gb/test/ps/movie_bank.hex)"
	$(MAKE) --no-print-directory sim SYSTEM=gb BOARD=arty-z7-20 BUILD=build/arty-z7-20/gb-nofeatures MOVIES=0 SCREENSHOTS=0 \
	  ROM=build/gb/asm/joyecho.gb FRAMES=6 SIM_GOLDEN= \
	  SIM_PS=systems/gb/test/ps/movie_sync.ps SIM_ARGS="--expect-ps-hex $(abspath systems/gb/test/ps/movie_live.hex)"
	# side track O, O.3: frame advance. Paused at once, three steps: three
	# frames (PS_IN bit 7's flips); built with MOVIES=0 there is no frame
	# advance and no toggle
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/joyecho.gb FRAMES=14 SIM_GOLDEN= \
	  SIM_PS=systems/gb/test/ps/frame_step.ps SIM_ARGS="--expect-frame-toggles 3"
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 BUILD=build/arty-z7-20/gb-nofeatures MOVIES=0 SCREENSHOTS=0 \
	  ROM=build/gb/asm/joyecho.gb FRAMES=14 SIM_GOLDEN= \
	  SIM_PS=systems/gb/test/ps/frame_step.ps SIM_ARGS="--expect-frame-toggles 0"
ci-arty-nes:
	# the NES on the Arty (phase 2): our scene ROM, the menu overlay over the
	# link (system 3), nestest loaded over the link into the running core,
	# compared on the VGA goldens' 4 bits a channel (systems/nes/arty-z7-20.mk)
	$(MAKE) --no-print-directory sim SYSTEM=nes BOARD=arty-z7-20 FRAMES=20
	$(MAKE) --no-print-directory lint-run SYSTEM=nes BOARD=arty-z7-20
	$(MAKE) --no-print-directory resim SYSTEM=nes BOARD=arty-z7-20 FRAMES=25 SIM_PS=systems/nes/test/ps/menu.ps \
	  SIM_GOLDEN=systems/nes/test/golden/vga/menu-overlay.png SIM_ARGS="--sw 0x3 --expect-ps-in 0x00030301"
	tools/ps-load-script.sh roms/nes/nestest/nestest.nes build/arty-z7-20/nes/load-nestest.ps
	$(MAKE) --no-print-directory resim SYSTEM=nes BOARD=arty-z7-20 FRAMES=45 SIM_PS=build/arty-z7-20/nes/load-nestest.ps \
	  SIM_GOLDEN=systems/nes/test/golden/vga/nestest.png SIM_ARGS="--ddr-file 0x02000000=$(abspath roms/nes/nestest/nestest.nes)"
	# ... and pulse 1 on aud_pwm, as ci-nes hears it on S/PDIF
	$(MAKE) --no-print-directory resim SYSTEM=nes BOARD=arty-z7-20 ROM=build/nes/asm/tone.nes FRAMES=20 \
	  SIM_GOLDEN= SIM_ARGS="--expect-tone 440.4 --expect-hdmi-tone 440.4"

# --- Side track G phase 2: the Arty's PS ---------------------------------------
# make ps7-init BOARD=arty-z7-20: Vivado writes the Arty's ps7_init.c/.h/.tcl
# (and the PS7 IP's port list, the achieved clocks) into ARTY_PS7_DIR, from
# boards/arty-z7-20/ps7.tcl (Digilent's preset plus ours). Outside the dev
# shell, like make bit; an IP-only Vivado run (no synthesis).
ARTY_PS7_DIR = $(if $(PS7_DIR),$(abspath $(PS7_DIR))/arty-z7-20,build/arty-z7-20/ps7)
# where this checkout's ps7_init files are (tools/ci-remote.sh copies them)
.PHONY: ps7-dirs
ps7-dirs:
	@echo "$(ZED_PS7_DIR) $(abspath $(ARTY_PS7_DIR))"
.PHONY: ps7-init
ps7-init: VIVADO_DIR = $(ARTY_PS7_DIR)
ps7-init:
	@test "$(BOARD)" = arty-z7-20 || { echo "make ps7-init: only for BOARD=arty-z7-20 (the ZedBoard uses Vivado's zed sample)" >&2; exit 1; }
	mkdir -p $(ARTY_PS7_DIR)
	$(call VIVADO,ps7_init_gen,"$(abspath $(ARTY_PS7_DIR))")
# make ps7-check BOARD=arty-z7-20 (layer 6: ask first): xsdb runs that
# ps7_init on the Arty and tests DDR over JTAG (boards/arty-z7-20/
# ps7_check.tcl); the cable is picked as for make prog. No bitstream.
.PHONY: ps7-check
ps7-check: VIVADO_DIR = $(ARTY_PS7_DIR)
ps7-check:
	@test "$(BOARD)" = arty-z7-20 || { echo "make ps7-check: only for BOARD=arty-z7-20" >&2; exit 1; }
	@test -f $(ARTY_PS7_DIR)/ps7_init.tcl || { echo "make ps7-check: no $(ARTY_PS7_DIR)/ps7_init.tcl; run make ps7-init BOARD=arty-z7-20" >&2; exit 1; }
	$(JTAG_PICK) > $(ARTY_PS7_DIR)/jtag-serial
	$(call XSDB,ps7_check,"$(abspath $(ARTY_PS7_DIR))/ps7_init.tcl" "$$(cat $(ARTY_PS7_DIR)/jtag-serial)")

# The PS app for the Arty, whatever BOARD says (make ps-app BOARD=arty-z7-20
# SYSTEM=…, or this file directly): as build/ps/%.elf, with -Dboard and the
# Arty's ps7_init.c; the same alignment checks.
ARTY_PS7_C = $(abspath $(ARTY_PS7_DIR))/ps7_init.c
build/arty-z7-20/ps/%.elf: platform/ps/%.zig $(ZIG_SRC) $(PS_FEATURES) platform/ps/link.ld platform/ps/c/xil_io.h tools/check-ps-align.sh
	@test -f "$(ARTY_PS7_C)" || { echo "no $(ARTY_PS7_C): run make ps7-init BOARD=arty-z7-20 (outside the dev shell)" >&2; exit 1; }
	rm -f $@
	$(ZIG_BUILD) ps-app -Dps-app=$* -Dboard=arty-z7-20 -Dps7-init-c="$(ARTY_PS7_C)" $(FEATURE_ZIG) -Dps-version="$$(tools/ps-version.sh --mark arty-z7-20 "$(ARTY_PS7_C)" $(FEATURE_ZIG))" --prefix build/arty-z7-20
	$(if $(filter relay,$*),python3 tools/check-ps-stack.py build/arty-z7-20/ps/relay.s || { rm -f $@; exit 1; })

# ---------------------------------------------------------------------------
# Side track J: the boot splash's demo ROMs (PLAN.md; demos/, docs/brand.md).
# The generator (demos/rota-brand) writes build/demos/gen/; the ROMs only
# copy its streams. make demos -> build/demos/rota.gb, rota.nes,
# rota-split.nes and the CHIP-8 program, and the card's folders.
DEMO_GEN = build/demos/gen
# The NES demo on the card: one background pattern table (the default),
# or NES_SPLIT=1 for the mid-screen split (the user's toggle, 2026-10-07)
NES_SPLIT ?= 0
DEMO_NES = build/demos/$(if $(filter 1,$(NES_SPLIT)),rota-split,rota).nes

.PHONY: demos demos-gen
demos: nix-shell demos-gen build/demos/rota.gb build/demos/rota.nes build/demos/rota-split.nes build/demos/rota.xex build/demos/rota.gg
	rm -rf build/demos/card/nes/rota
	mkdir -p build/demos/card/rota build/demos/card/chip8/rota build/demos/card/nes/rota build/demos/card/a800xl/rota build/demos/card/gg/rota
	cp build/demos/rota.xex build/demos/card/a800xl/rota/
	cp build/demos/rota.gg build/demos/card/gg/rota/
	cp build/demos/rota.gb build/demos/card/rota/
	cp $(DEMO_GEN)/chip8/rota.ch8 build/demos/card/chip8/rota/
	cp $(DEMO_NES) build/demos/card/nes/rota/rota.nes
	mkdir -p build/demos/card/boot
	cp $(DEMO_GEN)/chip8/rota.ch8 build/demos/card/boot/chip8.ch8
	cp build/demos/rota.gb build/demos/card/boot/gb.gb
	cp build/demos/rota.gb build/demos/card/boot/gbc.gbc
	cp $(DEMO_NES) build/demos/card/boot/nes.nes
	cp build/demos/rota.xex build/demos/card/boot/a800xl.xex
	cp build/demos/rota.gg build/demos/card/boot/gg.gg

# make test (and so make ci) runs the generator's tests and the demos' layer
# 3 tests (rota-brand, rota-demos), which read the built ROMs
test: demos

# make sd puts the built demos on the card as ROMs of their own (/gb/rota,
# /chip8/rota, /nes/rota, /a800xl/rota)
SD_GB_ROMS += $(wildcard build/demos/card/rota)
SD_CHIP8_DEMOS = $(wildcard build/demos/card/chip8/rota)
SD_NES_ROMS += $(wildcard build/demos/card/nes/rota)
SD_A800_ROMS += $(wildcard build/demos/card/a800xl/rota)
# and the user's own programs, if any (side track N: git-ignored roms/a800xl/local)
SD_A800_ROMS += $(wildcard roms/a800xl/local)
# ... and as the boot splash (step 5): /systems/boot/<name><ext>, which the
# PS app plays after a system starts (platform/ps/splash.zig)
SD_BOOT_FILES = $(wildcard build/demos/card/boot/*)

# Step 5 at layer 4 (make ci, after the pool's Arty chains built the models):
# each demo as the boot splash, then the menu opened over it as the PS app
# does (demos/ps/splash-*.ps): frame 115 (the splash; checked against the
# spec when made, 2026-10-07, the Game Boy and NES again for rendition B
# with rota-compare; CHIP-8 at the fast setting, SW0 on) and 125
# (the menu) as in demos/golden/splash-*/ (8 bits a channel: the NES
# without its VGA goldens' --golden-4bit)
# make ci runs it after the pool's chains (it re-simulates their Arty models)
CI_AFTER += demos
.PHONY: ci-demos
ci-demos: demos
	$(MAKE) --no-print-directory resim SYSTEM=chip8 BOARD=arty-z7-20 ROM=$(DEMO_GEN)/chip8/rota.ch8 FRAMES=126 \
	  SIM_PS=demos/ps/splash-chip8.ps SIM_GOLDEN= SIM_ARGS="--sw 0x1 --expect $(abspath demos/golden/splash-chip8)"
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/demos/rota.gb FRAMES=126 \
	  SIM_PS=demos/ps/splash-gb.ps SIM_GOLDEN= SIM_ARGS="--expect $(abspath demos/golden/splash-gb)"
	$(MAKE) --no-print-directory resim SYSTEM=nes BOARD=arty-z7-20 ROM=build/demos/rota.nes FRAMES=126 \
	  SIM_PS=demos/ps/splash-nes.ps SIM_GOLDEN= SIM_SYS_ARGS= \
	  SIM_ARGS="--ddr-file 0x02000000=$(abspath build/demos/rota.nes) --expect $(abspath demos/golden/splash-nes)"
	$(MAKE) --no-print-directory resim SYSTEM=gg BOARD=arty-z7-20 ROM=build/demos/rota.gg FRAMES=126 \
	  SIM_PS=demos/ps/splash-gg.ps SIM_GOLDEN= SIM_ARGS="--expect $(abspath demos/golden/splash-gg)"
	$(MAKE) --no-print-directory resim SYSTEM=sms BOARD=arty-z7-20 ROM=build/demos/rota.sms FRAMES=126 \
	  SIM_PS=demos/ps/splash-sms.ps SIM_GOLDEN= SIM_ARGS="--expect $(abspath demos/golden/splash-sms)"
	$(MAKE) --no-print-directory resim SYSTEM=snes BOARD=arty-z7-20 ROM=build/demos/rota.sfc FRAMES=126 \
	  SIM_PS=demos/ps/splash-snes.ps SIM_GOLDEN= SIM_ARGS="--expect $(abspath demos/golden/splash-snes)"

build/demos/rota.nes: demos/nes/rota.s demos/nes/rota.cfg $(DEMO_GEN)/nes/rota_sound.inc $(DEMO_GEN)/nes/rota_pic.inc
	mkdir -p $(@D)
	ca65 -g -I $(DEMO_GEN)/nes -o build/demos/rota-nes.o $<
	ld65 -C demos/nes/rota.cfg -Ln build/demos/rota-nes.lbl -o $@.part build/demos/rota-nes.o
	mv $@.part $@

# The Game Gear demo (step 5): WLA-DX, as M5's test ROMs
build/demos/rota.gg: demos/gg/rota.s $(DEMO_GEN)/gg/rota_data.inc
	mkdir -p $(@D)
	cd $(DEMO_GEN)/gg && wla-z80 -o $(abspath build/demos/rota-gg.o) $(abspath demos/gg/rota.s)
	printf '[objects]\n%s\n' $(abspath build/demos/rota-gg.o) > build/demos/rota-gg.link
	wlalink -r -S build/demos/rota-gg.link $@.part
	mv $@.part $@

# The Master System's boot splash demo (M6; Rota.Sms.Rom, demos/sms/rota.s):
# make demos also makes build/demos/rota.sms, the card's /sms/rota and
# /systems/boot/sms.sms
demos: demos-sms
.PHONY: demos-sms
demos-sms: build/demos/rota.sms
	mkdir -p build/demos/card/sms/rota build/demos/card/boot
	cp build/demos/rota.sms build/demos/card/sms/rota/
	cp build/demos/rota.sms build/demos/card/boot/sms.sms
$(DEMO_GEN)/sms/rota_data.inc: demos-gen ;
build/demos/rota.sms: demos/sms/rota.s $(DEMO_GEN)/sms/rota_data.inc
	mkdir -p $(@D)
	cd $(DEMO_GEN)/sms && wla-z80 -o $(abspath build/demos/rota-sms.o) $(abspath demos/sms/rota.s)
	printf '[objects]\n%s\n' $(abspath build/demos/rota-sms.o) > build/demos/rota-sms.link
	wlalink -r -S build/demos/rota-sms.link $@.part
	mv $@.part $@

# The Super Nintendo's boot splash demo (M7 step 7.13; Rota.Snes.Rom,
# Rota.Sound.Snes, demos/snes/rota.s and rota-spc.s): make demos also makes
# build/demos/rota.sfc, the card's /snes/rota and /systems/boot/snes.sfc
demos: demos-snes
.PHONY: demos-snes
# (rota-demos runs the ROM on the spec with our boot ROM)
demos-snes: build/demos/rota.sfc build/snes/ipl.bin
	mkdir -p build/demos/card/snes/rota build/demos/card/boot
	cp build/demos/rota.sfc build/demos/card/snes/rota/
	cp build/demos/rota.sfc build/demos/card/boot/snes.sfc
$(DEMO_GEN)/snes/rota_data.inc $(DEMO_GEN)/snes/rota_spc.inc &: demos-gen ;
# the SPC700's driver and samples, then the ROM (both assembled in the
# generator's folder, where their includes are)
$(DEMO_GEN)/snes/rota-spc.bin: demos/snes/rota-spc.s $(DEMO_GEN)/snes/rota_spc.inc
	cd $(DEMO_GEN)/snes && wla-spc700 -o $(abspath build/demos/rota-snes-spc.o) $(abspath demos/snes/rota-spc.s)
	printf '[objects]\n%s\n' $(abspath build/demos/rota-snes-spc.o) > build/demos/rota-snes-spc.link
	wlalink -r build/demos/rota-snes-spc.link $@.part
	mv $@.part $@
build/demos/rota.sfc: demos/snes/rota.s $(DEMO_GEN)/snes/rota_data.inc $(DEMO_GEN)/snes/rota-spc.bin
	mkdir -p $(@D)
	cd $(DEMO_GEN)/snes && wla-65816 -o $(abspath build/demos/rota-snes.o) $(abspath demos/snes/rota.s)
	printf '[objects]\n%s\n' $(abspath build/demos/rota-snes.o) > build/demos/rota-snes.link
	wlalink -r -S build/demos/rota-snes.link $@.part
	mv $@.part $@

build/demos/rota.xex: demos/a800xl/rota.s demos/a800xl/rota.cfg $(DEMO_GEN)/a800xl/rota_sound.inc $(DEMO_GEN)/a800xl/rota_pic.inc
	mkdir -p $(@D)
	ca65 -g -I $(DEMO_GEN)/a800xl -o build/demos/rota-a800.o $<
	ld65 -C demos/a800xl/rota.cfg -Ln build/demos/rota-a800.lbl -o $@.part build/demos/rota-a800.o
	mv $@.part $@

build/demos/rota-split.nes: demos/nes/rota.s demos/nes/rota.cfg $(DEMO_GEN)/nes/rota_sound.inc $(DEMO_GEN)/nes/rota_pic_split.inc
	mkdir -p $(@D)
	ca65 -g -D SPLIT -I $(DEMO_GEN)/nes -o build/demos/rota-nes-split.o $<
	ld65 -C demos/nes/rota.cfg -Ln build/demos/rota-nes-split.lbl -o $@.part build/demos/rota-nes-split.o
	mv $@.part $@

demos-gen: nix-shell
	$(CABAL) run -v0 rota-brand:exe:rota-brand -- --out $(DEMO_GEN)

# The generator's files (Rota.Emit's outputs), all written by demos-gen: a
# rule of their own, so make -n and the ROMs' rules see where they come
# from before the generator has run (side track K's note, 2026-10-09)
DEMO_GEN_FILES = $(addprefix $(DEMO_GEN)/,gb/rota_sound.inc gb/rota_pic.inc nes/rota_sound.inc \
  nes/rota_pic.inc nes/rota_pic_split.inc a800xl/rota_pic.inc a800xl/rota_sound.inc gg/rota_data.inc chip8/rota.ch8)
$(DEMO_GEN_FILES) &: demos-gen ;

build/demos/rota.gb: demos/gb/rota.asm $(DEMO_GEN)/gb/rota_sound.inc $(DEMO_GEN)/gb/rota_pic.inc
	mkdir -p $(@D)
	rgbasm -Wall -Werror -I $(DEMO_GEN)/gb -o build/demos/rota-gb.o $<
	rgblink -t -w -n build/demos/rota.sym -o $@.part build/demos/rota-gb.o
	rgbfix -v -p 0xFF -t ROTA $@.part
	mv $@.part $@
# --- M5: the Game Gear --------------------------------------------------------
# Game Gear / Z80 regression ROMs (CLAUDE.md rule 5): every
# systems/gg/test/asm/*.s assembles into build/gg/asm/*.gg (WLA-DX; the
# Sega header and checksum from gg.inc). The gg tests read them from there.
GG_ASM_DIR = systems/gg/test/asm
GG_ASM     = $(patsubst $(GG_ASM_DIR)/%.s,build/gg/asm/%.gg,$(wildcard $(GG_ASM_DIR)/*.s))

.PHONY: gg-asm
gg-asm: nix-shell $(GG_ASM)

build/gg/asm/%.gg: $(GG_ASM_DIR)/%.s $(GG_ASM_DIR)/gg.inc
	mkdir -p $(@D)
	cd $(GG_ASM_DIR) && wla-z80 -o $(abspath $(@:.gg=.o)) $*.s
	printf '[objects]\n%s\n' $(abspath $(@:.gg=.o)) > $(@:.gg=.link)
	wlalink -r $(@:.gg=.link) $@.part
	mv $@.part $@

test: gg-asm

# --- M6: the Master System ---------------------------------------------------
# Master System regression ROMs (CLAUDE.md rule 5): every
# systems/sms/test/asm/*.s assembles into build/sms/asm/*.sms (WLA-DX; the
# header, boot and verdict from sms.inc, which includes the Game Gear's
# gg.inc with SMS defined). The sms tests read them from there.
SMS_ASM_DIR = systems/sms/test/asm
SMS_ASM     = $(patsubst $(SMS_ASM_DIR)/%.s,build/sms/asm/%.sms,$(wildcard $(SMS_ASM_DIR)/*.s))

.PHONY: sms-asm
sms-asm: nix-shell $(SMS_ASM)

build/sms/asm/%.sms: $(SMS_ASM_DIR)/%.s $(SMS_ASM_DIR)/sms.inc $(GG_ASM_DIR)/gg.inc
	mkdir -p $(@D)
	cd $(SMS_ASM_DIR) && wla-z80 -o $(abspath $(@:.sms=.o)) $*.s
	printf '[objects]\n%s\n' $(abspath $(@:.sms=.o)) > $(@:.sms=.link)
	wlalink -r $(@:.sms=.link) $@.part
	mv $@.part $@

test: sms-asm

# The YM2413 oracle (step 6.3): Nuked-OPLL, fetched (GPL, never committed),
# behind a small Zig driver; sms-spec runs it as a black box.
OPLL_ORACLE = build/sms/opll-oracle
.PHONY: opll-oracle
opll-oracle: nix-shell roms $(OPLL_ORACLE)
$(OPLL_ORACLE): systems/sms/tools/opll_oracle.zig build.zig roms/sms/nuked-opll/opll.c
	$(ZIG_BUILD) opll-oracle -Dnuked-opll=$(abspath roms/sms/nuked-opll) --prefix build
test: opll-oracle

# Layer 4 of the Master System (M6 step 6.6): its picture against the
# spec's golden (systems/sms/test/golden/vga; sms-run --vga-at), the check
# ROMs' verdicts on the screen, our stand-in BIOS handing over, the FM
# unit's tone in the audio, the PS link (a load as the menu does it; PS_IN
# says system 8) and the menu's overlay.
SMS_PASS = smoke ports padloop
.PHONY: ci-arty-sms ci-sms
ci-arty-sms: sms-asm gg-asm
	$(MAKE) --no-print-directory BOARD=arty-z7-20 lint SYSTEM=sms
	$(MAKE) --no-print-directory BOARD=arty-z7-20 sim SYSTEM=sms ROM=build/sms/asm/picture.sms FRAMES=8
	$(foreach r,$(SMS_PASS),$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=sms \
	  ROM=build/sms/asm/$(r).sms FRAMES=12 SIM_GOLDEN=systems/sms/test/golden/vga/pass.png &&) true
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=sms ROM=build/sms/asm/biosgame.sms BIOS=build/sms/asm/bios.sms \
	  FRAMES=12 SIM_GOLDEN=systems/sms/test/golden/vga/pass.png
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=sms ROM=build/sms/asm/fm.sms FRAMES=14 \
	  SIM_GOLDEN= SIM_ARGS="--expect-tone 437.0 --expect-hdmi-tone 437.0"
	# the menu's speed (side track L's settings register): at 50 % the FM
	# tone at half its pitch (the volume: ci-sms, the ZedBoard's harness
	# has --expect-quiet)
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=sms ROM=build/sms/asm/fm.sms FRAMES=26 \
	  SIM_GOLDEN= SIM_PS=platform/test/ps/speed-2.ps SIM_ARGS="--expect-tone 218.5 --expect-hdmi-tone 218.5"
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=sms ROM=build/sms/asm/picture.sms FRAMES=12 \
	  SIM_PS=systems/gg/test/ps/reload.ps SIM_ARGS="--expect-ps-in 0x00080000"
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=sms FRAMES=10 SIM_PS=systems/gb/test/ps/menu.ps \
	  SIM_GOLDEN=systems/gb/test/golden/vga/menu-overlay.png
	# battery saves (step 6.7): the Game Gear's save.gg (it counts boots in
	# cartridge RAM through the Sega mapper, as a Master System cartridge)
	# and its script: every read-back byte against save.hex (the Arty's
	# harness has --expect-ps-hex since side K's item 1); the dirty flag
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=sms ROM=build/gg/asm/save.gg FRAMES=4 SIM_GOLDEN= \
	  SIM_PS=systems/gg/test/ps/save.ps SIM_ARGS="--expect-ps-hex $(abspath systems/gg/test/ps/save.hex) --expect-ps-in 0x0F080040"
	@echo "make ci-arty-sms: OK"

ci-sms: sms-asm gg-asm
	$(MAKE) --no-print-directory BOARD=zedboard lint SYSTEM=sms
	$(MAKE) --no-print-directory BOARD=zedboard sim SYSTEM=sms ROM=build/sms/asm/picture.sms FRAMES=25
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=sms ROM=build/sms/asm/ports.sms FRAMES=25 \
	  SIM_GOLDEN=systems/sms/test/golden/vga/pass.png
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=sms ROM=build/sms/asm/fm.sms FRAMES=25 \
	  SIM_GOLDEN= SIM_ARGS="--expect-tone 437.0"
	# the menu's volume at 0 (side track L's settings register): no sound left
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=sms ROM=build/sms/asm/fm.sms FRAMES=25 \
	  SIM_GOLDEN= SIM_PS=platform/test/ps/volume-0.ps SIM_ARGS="--expect-quiet 0"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=sms ROM=build/sms/asm/picture.sms FRAMES=25 \
	  SIM_PS=systems/gg/test/ps/reload.ps SIM_ARGS="--expect-ps-in 0x00080000"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=sms FRAMES=25 SIM_PS=systems/gb/test/ps/menu.ps \
	  SIM_GOLDEN=systems/gb/test/golden/vga/menu-overlay.png SIM_ARGS="--btn 0x01 --sw 0x01 --expect-ps-in 0x00080101"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=sms ROM=build/gg/asm/save.gg FRAMES=25 SIM_GOLDEN= \
	  SIM_PS=systems/gg/test/ps/save.ps SIM_ARGS="--expect-ps-hex $(abspath systems/gg/test/ps/save.hex) --expect-ps-in 0x0F080040"
	@echo "make ci-sms: OK"
CI_LINT += arty-sms
CI_LINT_ALL += sms

# The Clash YM2413's synthesis check (M6 step 6.5): Sms.Fm stepped per
# internal cycle (systems/sms/hdl/FmTop.hs) into Verilog, Verilator lint.
SMS_CABAL = systems/sms/sms.cabal
.PHONY: sms-fm-hdl
sms-fm-hdl: nix-shell
	mkdir -p build/sms/fm-verilog build/sms/fm-obj
	clash --verilog -isystems/sms/src -isystems/sms/hdl -isystems/gg/src -icpus/z80/src -outputdir build/sms/fm-obj \
	  -fclash-hdldir build/sms/fm-verilog -fclash-no-cache -fclash-clear \
	  $(addprefix -X,$(shell $(subst $(SYSTEM_CABAL),$(SMS_CABAL),$(call CABAL_FIELD,default-extensions)))) \
	  $(shell $(subst $(SYSTEM_CABAL),$(SMS_CABAL),$(call CABAL_FIELD,ghc-options))) \
	  FmTop
	verilator --lint-only -Wall --top-module sms_fm_top boards/sim/clash.vlt build/sms/fm-verilog/FmTop.topEntity/*.v
	@echo "make sms-fm-hdl: OK (sms_fm_top: Clash Verilog, Verilator lint clean)"

# The Master System is built on the Game Gear's packages (M6): its tests run
# theirs too, so a change made for the SMS shows at once if it moves the GG.
TEST_TARGETS += $(if $(filter sms,$(SYSTEM)),gg z80)

# The Clash Z80's synthesis check (M5 step 5.5): Clash turns the core's
# T-state wrapper (cpus/z80/hdl/Z80Top.hs) into Verilog, Verilator lints it
# with the usual waivers. No system needed.
Z80_CABAL     = cpus/z80/z80.cabal
Z80_HDL       = build/z80/verilog

.PHONY: z80-hdl
z80-hdl: nix-shell
	mkdir -p $(Z80_HDL) build/z80/obj
	clash --verilog -icpus/z80/src -icpus/z80/hdl -outputdir build/z80/obj \
	  -fclash-hdldir $(Z80_HDL) -fclash-no-cache -fclash-clear \
	  $(addprefix -X,$(shell $(subst $(SYSTEM_CABAL),$(Z80_CABAL),$(call CABAL_FIELD,default-extensions)))) \
	  $(shell $(subst $(SYSTEM_CABAL),$(Z80_CABAL),$(call CABAL_FIELD,ghc-options))) \
	  Z80Top
	verilator --lint-only -Wall --top-module z80_top boards/sim/clash.vlt $(Z80_HDL)/Z80Top.topEntity/*.v
	@echo "make z80-hdl: OK (z80_top: Clash Verilog, Verilator lint clean)"

# The Clash 68000's synthesis check (side track P, P.4): Clash turns the
# core's clock wrapper (cpus/m68k/hdl/M68kTop.hs) into Verilog, Verilator
# lints it. The package's common stanza has no Clash plugins (only
# the core's modules are Clash): they're given here. No system needed.
M68K_CABAL    = cpus/m68k/m68k.cabal
M68K_HDL      = build/m68k/verilog
M68K_TOP     ?= M68kTop
M68K_TOP_NAME = m68k_top

.PHONY: m68k-hdl
m68k-hdl: nix-shell
	mkdir -p $(M68K_HDL) build/m68k/obj
	clash --verilog -icpus/m68k/src -icpus/m68k/hdl -outputdir build/m68k/obj \
	  -fclash-hdldir $(M68K_HDL) -fclash-no-cache -fclash-clear \
	  $(addprefix -X,$(shell $(subst $(SYSTEM_CABAL),$(M68K_CABAL),$(call CABAL_FIELD,default-extensions)))) \
	  $(shell $(subst $(SYSTEM_CABAL),$(M68K_CABAL),$(call CABAL_FIELD,ghc-options))) \
	  -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver -fplugin GHC.TypeLits.Extra.Solver \
	  -fexpose-all-unfoldings -fno-worker-wrapper -fconstraint-solver-iterations=100 $(M68K_CLASH_EXTRA) \
	  $(M68K_TOP)
	verilator --lint-only -Wall --top-module $(M68K_TOP_NAME) boards/sim/clash.vlt $(M68K_HDL)/$(M68K_TOP).topEntity/*.v
	@echo "make m68k-hdl: OK ($(M68K_TOP_NAME): Clash Verilog, Verilator lint clean)"

# The Clash 68000's size and Fmax (side track P, P.4): out-of-context
# Vivado on make m68k-hdl's Verilog (cpus/m68k/ooc.tcl), in the queue's bit
# slot; Clash isn't rerun, so the Verilog may come from the worker
# (WRUN_PULL="build/m68k/verilog" wrun nix develop -c make m68k-hdl).
M68K_OOC_DIR = build/m68k/ooc
.PHONY: m68k-ooc m68k-ooc-run
m68k-ooc:
	@test -d $(M68K_HDL)/$(M68K_TOP).topEntity || { echo "make m68k-ooc: no Verilog in $(M68K_HDL): make m68k-hdl first" >&2; exit 1; }
	$(call QUEUE_RUN,bit) $(MAKE) --no-print-directory m68k-ooc-run
m68k-ooc-run:
	@command -v vivado >/dev/null || test -f "$(VIVADO_SETTINGS)" || \
	  { echo "make m68k-ooc: no vivado on PATH and VIVADO_SETTINGS=$(VIVADO_SETTINGS) not found" >&2; exit 1; }
	mkdir -p $(M68K_OOC_DIR)
	bash -c '$(VIVADO_ENV) && cd "$(abspath $(M68K_OOC_DIR))" && rm -f ooc.log && \
	  { vivado -mode batch -nojournal -log ooc.log -source "$(abspath cpus/m68k/ooc.tcl)" \
	      -tclargs "$(abspath $(M68K_HDL))/$(M68K_TOP).topEntity" "$(abspath $(M68K_OOC_DIR))/reports" $(M68K_TOP_NAME) $(M68K_OOC_MODE) </dev/null; \
	    grep -q "^ooc.tcl: OK" ooc.log || { echo "make m68k-ooc: see $(abspath $(M68K_OOC_DIR))/ooc.log" >&2; exit 1; }; }'
	@grep -A3 "WNS(ns)" $(M68K_OOC_DIR)/reports/timing.txt | head -4
	@grep -E "^\| (Slice LUTs|Slice Registers|DSPs|Block RAM Tile) " $(M68K_OOC_DIR)/reports/utilization.txt

# The Clash 68000's Verilator model (side track P, P.5; cpus/m68k/sim/):
# libm68k.so with m68k.h (the C library, the user's benchmark project's),
# m68k-sim (the harness, m68k-run's options and output), m68k-ctest (a C
# program on the library), in build/m68k/sim. make m68k-sim-test is P.5's
# gate: m68k-sim and m68k-run (the model) print the same on the sample
# program, and m68k-ctest gets the model's counts. The Verilog is make
# m68k-hdl's (made if missing).
M68K_SIM_OBJ  = build/m68k/obj_dir
M68K_SIM_DIR  = build/m68k/sim
M68K_TOP_V    = $(M68K_HDL)/M68kTop.topEntity/m68k_top.v

$(M68K_TOP_V): cpus/m68k/src/M68k/Core.hs cpus/m68k/src/M68k/Micro.hs cpus/m68k/src/M68k/Datapath.hs cpus/m68k/src/M68k/Ops.hs cpus/m68k/src/M68k/Bus.hs cpus/m68k/hdl/M68kTop.hs
	$(MAKE) --no-print-directory m68k-hdl

$(M68K_SIM_OBJ)/Vm68k__ALL.a: $(M68K_TOP_V)
	mkdir -p $(M68K_SIM_OBJ)
	verilator --cc --prefix Vm68k --top-module m68k_top -Mdir $(M68K_SIM_OBJ) -O3 --x-assign fast \
	  --x-initial fast --no-timing -Wno-fatal $(VLT) $(M68K_HDL)/M68kTop.topEntity/*.v
	+$(VL_MAKE) -s -C $(M68K_SIM_OBJ) -f Vm68k.mk CXX="$(ZIG_CXX)" LINK="$(ZIG_CXX)" \
	  CFG_CXXFLAGS_NO_UNUSED="$(VL_NO_UNUSED)" OPT_FAST=-O2 Vm68k__ALL.a libverilated.a

$(M68K_SIM_OBJ)/vm68k.o: cpus/m68k/sim/vm68k.cpp $(M68K_SIM_OBJ)/Vm68k__ALL.a
	$(ZIG_CXX) -std=c++20 -O2 -c $< -I$(M68K_SIM_OBJ) -I$$(verilator --getenv VERILATOR_ROOT)/include \
	  -I$$(verilator --getenv VERILATOR_ROOT)/include/vltstd -o $@

.PHONY: m68k-sim m68k-sim-test
m68k-sim: nix-shell $(M68K_SIM_OBJ)/vm68k.o
	$(ZIG_BUILD) m68k-lib m68k-sim m68k-ctest -Dm68k-obj=$(abspath $(M68K_SIM_OBJ)) --prefix $(M68K_SIM_DIR)
	@echo "make m68k-sim: $(M68K_SIM_DIR)/lib/libm68k.so, $(M68K_SIM_DIR)/include/m68k.h, $(M68K_SIM_DIR)/bin/m68k-sim"

m68k-sim-test: m68k-sim m68k-asm
	cabal run -v0 m68k-run -- build/m68k/asm/sum.bin --at 0x1000 --pc 0x1000 --sp 0x8000 > $(M68K_SIM_DIR)/sum-model.txt
	$(M68K_SIM_DIR)/bin/m68k-sim build/m68k/asm/sum.bin --at 0x1000 --pc 0x1000 --sp 0x8000 > $(M68K_SIM_DIR)/sum-core.txt
	diff $(M68K_SIM_DIR)/sum-model.txt $(M68K_SIM_DIR)/sum-core.txt
	$(M68K_SIM_DIR)/bin/m68k-ctest
	@echo "make m68k-sim-test: OK (m68k-sim prints m68k-run's counts and registers; m68k-ctest on libm68k.so)"

# The Clash VDP's synthesis check (M5 step 5.6): Gg.Vdp with four block
# RAM banks (systems/gg/hdl/VdpTop.hs) into Verilog, Verilator lint.
GG_CABAL = systems/gg/gg.cabal
.PHONY: gg-vdp-hdl
gg-vdp-hdl: nix-shell
	mkdir -p build/gg/vdp-verilog build/gg/vdp-obj
	clash --verilog -isystems/gg/src -isystems/gg/hdl -icpus/z80/src -outputdir build/gg/vdp-obj \
	  -fclash-hdldir build/gg/vdp-verilog -fclash-no-cache -fclash-clear \
	  $(addprefix -X,$(shell $(subst $(SYSTEM_CABAL),$(GG_CABAL),$(call CABAL_FIELD,default-extensions)))) \
	  $(shell $(subst $(SYSTEM_CABAL),$(GG_CABAL),$(call CABAL_FIELD,ghc-options))) \
	  VdpTop
	verilator --lint-only -Wall --top-module gg_vdp_top boards/sim/clash.vlt build/gg/vdp-verilog/VdpTop.topEntity/*.v
	@echo "make gg-vdp-hdl: OK (gg_vdp_top: Clash Verilog, Verilator lint clean)"

# The whole Clash Game Gear's synthesis check (M5 step 5.7): Gg.Core with
# its RAMs in block RAM, the ROM as a port (systems/gg/hdl/CoreTop.hs).
.PHONY: gg-core-hdl
gg-core-hdl: nix-shell
	mkdir -p build/gg/core-verilog build/gg/core-obj
	clash --verilog -isystems/gg/src -isystems/gg/hdl -icpus/z80/src -outputdir build/gg/core-obj \
	  -fclash-hdldir build/gg/core-verilog -fclash-no-cache -fclash-clear \
	  $(addprefix -X,$(shell $(subst $(SYSTEM_CABAL),$(GG_CABAL),$(call CABAL_FIELD,default-extensions)))) \
	  $(shell $(subst $(SYSTEM_CABAL),$(GG_CABAL),$(call CABAL_FIELD,ghc-options))) \
	  CoreTop
	verilator --lint-only -Wall --top-module gg_core_top boards/sim/clash.vlt build/gg/core-verilog/CoreTop.topEntity/*.v
	@echo "make gg-core-hdl: OK (gg_core_top: Clash Verilog, Verilator lint clean)"

# Layer 4 for the Game Gear on the Arty (M5 step 5.8): lint, the test
# picture against the spec's golden (systems/gg/test/golden/vga), the tone
# on the PWM pin and in HDMI's audio (tone.s: 440.4 Hz).
.PHONY: ci-arty-gg
ci-arty-gg: gg-asm
	$(MAKE) --no-print-directory BOARD=arty-z7-20 lint SYSTEM=gg
	$(MAKE) --no-print-directory BOARD=arty-z7-20 sim SYSTEM=gg ROM=build/gg/asm/pattern.gg FRAMES=8
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=gg ROM=build/gg/asm/tone.gg FRAMES=12 \
	  SIM_ARGS="--expect-tone 440.4 --expect-hdmi-tone 440.4"
	# the ROM in DDR without changing the timing (Gg.RomPort, step 5.9):
	# raster.s's picture as the spec's, at the harness's usual DDR latency
	# and at up to 60 clocks (beyond the board's measured worst, 35)
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=gg ROM=build/gg/asm/raster.gg FRAMES=10
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=gg ROM=build/gg/asm/raster.gg FRAMES=10 \
	  SIM_ARGS="--axi-latency 60 --axi-seed 7"
	# the PS link (step 5.9): a load as the SD menu does it (hold, the
	# cartridge word, release: the machine starts afresh, raster.s's picture
	# again; PS_IN says system 7), and the menu's overlay with the machine
	# paused (the Game Boy's menu script and golden: the overlay covers the
	# whole picture)
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=gg ROM=build/gg/asm/raster.gg FRAMES=12 \
	  SIM_PS=systems/gg/test/ps/reload.ps SIM_ARGS="--expect-ps-in 0x00070000"
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=gg FRAMES=10 SIM_PS=systems/gb/test/ps/menu.ps \
	  SIM_GOLDEN=systems/gb/test/golden/vga/menu-overlay.png
	# the check ROMs' verdicts on the screen (step 5.10: hardware = model):
	# each passes, so each last frame is the spec's green picture
	# (test/golden/vga/pass.png; a fail is red with n squares)
	$(foreach r,mapper ports vdpint sprites console codemasters,$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=gg \
	  ROM=build/gg/asm/$(r).gg FRAMES=12 SIM_GOLDEN=systems/gg/test/golden/vga/pass.png &&) true
	# battery saves (step 5.10): the save loaded while held, the game's
	# count up, the read back on the byte channel (test/ps/save.ps), every
	# byte against save.hex as ci-gg; PS_IN with the dirty flag
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=gg ROM=build/gg/asm/save.gg FRAMES=4 \
	  SIM_PS=systems/gg/test/ps/save.ps SIM_ARGS="--expect-ps-hex $(abspath systems/gg/test/ps/save.hex) --expect-ps-in 0x0F070040"
	@echo "make ci-arty-gg: OK"

# Layer 4 for the Game Gear on the ZedBoard (M5 step 5.8): lint, the test
# picture against the spec's golden, the ADV7511's init, the tone on
# S/PDIF (tone.s: 440.4 Hz). Part of make ci (CI_LINT).
CI_LINT += arty-gg
CI_LINT_ALL += gg
# make ci-arty: the Arty's chains, the Game Gear's too
ci-arty: ci-arty-gg
.PHONY: ci-gg
ci-gg: gg-asm
	$(MAKE) --no-print-directory BOARD=zedboard lint SYSTEM=gg
	$(MAKE) --no-print-directory BOARD=zedboard sim SYSTEM=gg ROM=build/gg/asm/pattern.gg FRAMES=25
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gg ROM=build/gg/asm/tone.gg FRAMES=25 \
	  SIM_ARGS="--expect-tone 440.4"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gg ROM=build/gg/asm/raster.gg FRAMES=25
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gg ROM=build/gg/asm/raster.gg FRAMES=25 \
	  SIM_PS=systems/gg/test/ps/reload.ps SIM_ARGS="--expect-ps-in 0x00070000"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gg ROM=build/gg/asm/save.gg FRAMES=25 \
	  SIM_PS=systems/gg/test/ps/save.ps SIM_ARGS="--expect-ps-hex $(abspath systems/gg/test/ps/save.hex) --expect-ps-in 0x0F070040"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gg FRAMES=25 SIM_PS=systems/gb/test/ps/menu.ps \
	  SIM_GOLDEN=systems/gb/test/golden/vga/menu-overlay.png SIM_ARGS="--btn 0x01 --sw 0x01 --expect-ps-in 0x00070101"
	@echo "make ci-gg: OK"

# M5 step 5.10's board check while the PS app doesn't know the Game Gear
# yet (step 5.9's PS part waits for side track I): make gg-prog SYSTEM=gg
# [BOARD=…] ROM=<file.gg> is make prog with the ROM written into DDR over
# JTAG, between ps7_init and the PL reset's release
# (systems/gg/ps7_init_rom.tcl). The bitstream's cartridge word (mapper,
# banks) is built in: the ROM's size must give the same banks (any 64 KiB
# Sega-mapper ROM fits the default build), checked here; the mapper isn't.
# Layer 6: ask before running it.
.PHONY: gg-prog
gg-prog: need-system
	@test "$(SYSTEM)" = gg || { echo "make gg-prog: only for SYSTEM=gg" >&2; exit 1; }
	@test -f "$(ROM)" || { echo "make gg-prog: ROM=$(ROM) not found" >&2; exit 1; }
	@test -f $(VIVADO_DIR)/cart.bin || { echo "make gg-prog: no $(VIVADO_DIR)/cart.bin; run make bit SYSTEM=gg" >&2; exit 1; }
	@w=$$(cat $(VIVADO_DIR)/cart.bin); \
	  banks=$$(echo "$$w" | cut -c1-8 | awk '{v=0; for(i=1;i<=8;i++) v=v*2+substr($$0,i,1); print v+1}'); \
	  size=$$(wc -c < "$(ROM)"); \
	  echo "make gg-prog: the bitstream's cartridge: $$banks banks, $$(case $$w in *1) echo Codemasters;; *) echo Sega;; esac) mapper; $(ROM): $$size bytes"; \
	  test $$size -le $$((banks * 16384)) -a \( $$size -gt $$((banks * 8192)) -o $$banks -eq 1 \) || \
	  { echo "make gg-prog: $(ROM) doesn't fit the bitstream's $$banks banks; make bit SYSTEM=gg ROM=$(ROM) first" >&2; exit 1; }
	printf 'set gg_ps7_init {%s}\nset gg_rom {%s}\nsource {%s}\n' \
	  "$(PS7_INIT)" "$(abspath $(ROM))" "$(abspath systems/gg/ps7_init_rom.tcl)" > $(VIVADO_DIR)/ps7_init_rom.tcl
	$(MAKE) --no-print-directory prog SYSTEM=gg BOARD=$(BOARD) ROM=$(ROM) PS7_INIT=$(abspath $(VIVADO_DIR))/ps7_init_rom.tcl

# The 800XL on the Arty (M4, the Arty port, 2026-10-07): scene.s built in
# (compared on the VGA golden's 4 bits a channel, systems/a800xl/
# arty-z7-20.mk), tone.s on aud_pwm and in the HDMI audio, and keys.xex loaded over the PS link
# with A held over it (the loader and the keyboard), and our XEGS
# cartridge from the harness's DDR (side track Q); the rest of
# ci-a800xl's runs are the same machine (A800.System) on the ZedBoard.
CI_LINT += arty-a800xl
.PHONY: ci-arty-a800xl
ci-arty-a800xl: a800-asm
	$(MAKE) --no-print-directory sim SYSTEM=a800xl BOARD=arty-z7-20 XEX=build/a800xl/asm/scene.xex FRAMES=100
	$(MAKE) --no-print-directory lint-run SYSTEM=a800xl BOARD=arty-z7-20
	$(MAKE) --no-print-directory resim SYSTEM=a800xl BOARD=arty-z7-20 XEX=build/a800xl/asm/tone.xex FRAMES=100 \
	  SIM_GOLDEN= SIM_ARGS="--expect-tone 437.8 --expect-hdmi-tone 437.8"
	tools/ps-load-script.sh build/a800xl/asm/keys.xex build/arty-z7-20/a800xl/load-keys.ps
	cat systems/a800xl/test/ps/keys-tail.ps >> build/arty-z7-20/a800xl/load-keys.ps
	$(MAKE) --no-print-directory resim SYSTEM=a800xl BOARD=arty-z7-20 FRAMES=100 SIM_PS=build/arty-z7-20/a800xl/load-keys.ps \
	  SIM_GOLDEN=systems/a800xl/test/golden/vga/keys.png
	# a cartridge from the harness's DDR (side track Q: every cartridge
	# from DDR on the Arty, A800.CartDdr): our XEGS cartridge's "Pass"
	$(MAKE) --no-print-directory resim SYSTEM=a800xl BOARD=arty-z7-20 CART=build/a800xl/asm/xegs_banks.car FRAMES=100
	# a tape from the harness's DDR (side track Q, cassettes on the board):
	# cassio.xex reads cas_data.cas through SIOV (the OS's own tape boot
	# waits over 800 frames for the leader: too long here)
	$(MAKE) --no-print-directory resim SYSTEM=a800xl BOARD=arty-z7-20 XEX=build/a800xl/asm/cassio.xex CART=build/a800xl/asm/cas_data.cas \
	  FRAMES=150 SIM_GOLDEN=systems/a800xl/test/golden/vga/cassio.png

# The Game Gear's keys (M5 step 5.9): make keys SYSTEM=gg sends the [gg]
# map (platform/ps/keymap.zig, the Game Boy's layout). make load can't load
# it yet (make gg-prog).
LINK_SYSTEM += $(if $(filter gg,$(SYSTEM)),--system gg)

# The Game Gear on the SD card (M5 step 5.9): each folder into /gg as a
# folder of its own name (tools/mksd.sh's GG_DIRS), and gg.bit into
# /systems.
SD_GG_ROMS ?= roms/gg/games $(if $(SD_TESTS),build/gg/asm)
# (for make sd only: the test image of make test-zig stays small)
sd: export GG_DIRS = $(SD_GG_ROMS)
# (side track J: the Game Gear demo in /gg/rota, after the default above)
SD_GG_ROMS += $(wildcard build/demos/card/gg/rota)
# and the user's own games, if any (side track N: git-ignored roms/gg/local)
SD_GG_ROMS += $(wildcard roms/gg/local)
SD_SYSTEMS += gg

# The Master System on the SD card (M6 step 6.7): each folder into /sms
# (tools/mksd.sh's SMS_DIRS), sms.bit into /systems; SMS_BIOS=FILE (the
# user's own dump, wherever they keep it) as /sms/bios.bin, only when given.
SD_SMS_ROMS ?= roms/sms/games $(if $(SD_TESTS),build/sms/asm)
sd: export SMS_DIRS = $(SD_SMS_ROMS)
sd: export SMS_BIOS := $(SMS_BIOS)
# (the Master System's boot splash demo in /sms/rota, after the default)
SD_SMS_ROMS += $(wildcard build/demos/card/sms/rota)
# and the user's own games, if any (side track N: git-ignored roms/sms/local)
SD_SMS_ROMS += $(wildcard roms/sms/local)
SD_SYSTEMS += sms
# make keys/load SYSTEM=sms: link's Master System (pad 1, pad 2, Pause);
# make load SYSTEM=sms BIOS=FILE puts a BIOS into DDR before the ROM
LINK_SYSTEM += $(if $(filter sms,$(SYSTEM)),--system sms $(if $(BIOS),--bios $(BIOS)))

# The Super Nintendo on the SD card (M7 step 7.11): each folder into /snes
# (systems/snes/sd.sh's SNES_DIRS), snes.bit into /systems. make keys/load
# SYSTEM=snes: link's Super Nintendo (pad 1).
# (roms/snes/local: the user's own cartridges, never fetched; skipped
# where there is none. The menu refuses its chip games by name.)
SD_SNES_ROMS ?= roms/snes/games roms/snes/opensnes roms/snes/local build/snes/asm
sd: export SNES_DIRS = $(SD_SNES_ROMS)
sd-sync: export SNES_DIRS = $(SD_SNES_ROMS)
SD_SYSTEMS += snes
LINK_SYSTEM += $(if $(filter snes,$(SYSTEM)),--system snes)

# --- Side track L: the in-game menu's volume and speed (PLAN.md L.2, L.4) ---
# After the pool (CI_AFTER), on the models the Arty's chains built: each
# system's tone ROM at 50 % and at its fastest step (platform/ps/menu.zig's
# speed_top, measured), the pitch following the speed within 1 %; the Game
# Boy Color's double speed (tone_ds.gb) at 100 % and 50 %. With ALL=1 the
# ZedBoard's NES and Game Boy with the volume at 0: no sound left.
L_TONE = SIM_GOLDEN= SIM_PS=platform/test/ps/speed-$(1).ps SIM_ARGS="--expect-tone $(2) --expect-hdmi-tone $(2)"
CI_AFTER += speed
.PHONY: ci-speed
ci-speed: gb-asm nes-asm gg-asm a800-asm
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/tone.gb FRAMES=20 $(call L_TONE,2,219.9)
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/tone.gb FRAMES=20 $(call L_TONE,5,549.8)
	$(MAKE) --no-print-directory resim SYSTEM=gbc BOARD=arty-z7-20 ROM=build/gb/asm/tone_ds.gb FRAMES=20 \
	  SIM_GOLDEN= SIM_ARGS="--expect-tone 439.8 --expect-hdmi-tone 439.8"
	$(MAKE) --no-print-directory resim SYSTEM=gbc BOARD=arty-z7-20 ROM=build/gb/asm/tone_ds.gb FRAMES=20 $(call L_TONE,2,219.9)
	$(MAKE) --no-print-directory resim SYSTEM=nes BOARD=arty-z7-20 ROM=build/nes/asm/tone.nes FRAMES=20 $(call L_TONE,2,220.2)
	$(MAKE) --no-print-directory resim SYSTEM=nes BOARD=arty-z7-20 ROM=build/nes/asm/tone.nes FRAMES=20 $(call L_TONE,6,660.6)
	$(MAKE) --no-print-directory resim SYSTEM=gg BOARD=arty-z7-20 ROM=build/gg/asm/tone.gg FRAMES=12 $(call L_TONE,2,220.2)
	$(MAKE) --no-print-directory resim SYSTEM=a800xl BOARD=arty-z7-20 XEX=build/a800xl/asm/tone.xex FRAMES=100 $(call L_TONE,5,547.2)
CI_AFTER_ALL += volume
.PHONY: ci-volume
ci-volume: gb-asm nes-asm
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=nes ROM=build/nes/asm/tone.nes FRAMES=20 \
	  SIM_GOLDEN= SIM_PS=platform/test/ps/volume-0.ps SIM_ARGS="--expect-quiet 0"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=gb ROM=build/gb/asm/tone.gb FRAMES=20 \
	  SIM_GOLDEN= SIM_PS=platform/test/ps/volume-0.ps SIM_ARGS="--expect-quiet 0"

# --- Side track L: save states (PLAN.md L.6) -------------------------------------
# After the pool (CI_AFTER), on the Arty Game Boy model ci-arty-gb built
# (WithStateHooks): state_fill.gb filled, the core frozen, its status,
# VRAM, HRAM and WRAM bytes read back on the byte channel, a VRAM byte
# written and read back (systems/gb/test/ps/state.ps, state.hex).
CI_AFTER += savestate
.PHONY: ci-savestate
ci-savestate: gb-asm
	$(MAKE) --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/gb/asm/state_fill.gb FRAMES=4 \
	  SIM_GOLDEN= SIM_PS=systems/gb/test/ps/state.ps SIM_ARGS="--expect-ps-hex $(abspath systems/gb/test/ps/state.hex)"
	# ... and on the Game Boy Color (L.8), a DMG cartridge in compatibility mode
	$(MAKE) --no-print-directory resim SYSTEM=gbc BOARD=arty-z7-20 ROM=build/gb/asm/state_fill.gb FRAMES=4 \
	  SIM_GOLDEN= SIM_PS=systems/gb/test/ps/state.ps SIM_ARGS="--expect-ps-hex $(abspath systems/gb/test/ps/state.hex)"

# The Game Boy's restore routine for a state table (side track L, L.6c):
# gb-spec and gb-savestate run it (build/host/gb-restore)
.PHONY: gb-restore
gb-restore: nix-shell
	$(ZIG_BUILD) gb-restore --prefix build
test: gb-restore

# The Game Gear's and Master System's restore routine for a state table
# (side track N, N.1): gg-spec runs it (build/host/gg-restore)
.PHONY: gg-restore
gg-restore: nix-shell
	$(ZIG_BUILD) gg-restore --prefix build
test: gg-restore

# The 800XL's restore routine for a state table (side track N, N.4):
# a800xl-spec runs it (build/host/a800-restore)
.PHONY: a800-restore
a800-restore: nix-shell
	$(ZIG_BUILD) a800-restore --prefix build
test: a800-restore

# The NES's restore routine for a state table (side track N, N.7);
# nes-spec runs it (build/host/nes-restore)
.PHONY: nes-restore
nes-restore: nix-shell
	$(ZIG_BUILD) nes-restore --prefix build
test: nes-restore

# Save states on the Game Gear and the Master System at layer 4 (side
# track N, N.2): the Arty's tops (with the hooks) on a PS link script each,
# the bytes read back compared (--expect-ps-hex): the freeze, the status,
# RAM's window, a load through the CPU (systems/gg/test/ps/state.ps: the
# game boots again from its ROM after the routine)
CI_AFTER += savestate-gg
.PHONY: ci-savestate-gg
ci-savestate-gg: gg-asm sms-asm
	$(MAKE) --no-print-directory resim SYSTEM=gg BOARD=arty-z7-20 ROM=build/gg/asm/save.gg FRAMES=8 \
	  SIM_GOLDEN= SIM_PS=systems/gg/test/ps/state.ps SIM_ARGS="--expect-ps-hex $(abspath systems/gg/test/ps/state.hex)"
	$(MAKE) --no-print-directory resim SYSTEM=sms BOARD=arty-z7-20 ROM=build/sms/asm/picture.sms FRAMES=8 \
	  SIM_GOLDEN= SIM_PS=systems/sms/test/ps/state.ps SIM_ARGS="--expect-ps-hex $(abspath systems/sms/test/ps/state.hex)"

# Save states on the 800XL at layer 4 (side track N, N.5): the Arty's top
# (A800.Hooks) frozen, its windows, a load through the ROM overlay
# (systems/a800xl/test/ps/state.ps; the bytes it reads back, state.hex)
CI_AFTER += savestate-a800xl
.PHONY: ci-savestate-a800xl
ci-savestate-a800xl: a800-asm
	$(MAKE) --no-print-directory resim SYSTEM=a800xl BOARD=arty-z7-20 FRAMES=30 \
	  SIM_GOLDEN= SIM_PS=systems/a800xl/test/ps/state.ps SIM_ARGS="--expect-ps-hex $(abspath systems/a800xl/test/ps/state.hex)"

# Side track N, N.8: the NES's save states at layer 4 on the Arty (its top
# has the hooks): freeze, the status, RAM and PRG RAM through the windows,
# a load through the CPU (systems/nes/test/ps/state.ps)
CI_AFTER += savestate-nes
.PHONY: ci-savestate-nes
ci-savestate-nes: nes-asm
	$(MAKE) --no-print-directory resim SYSTEM=nes BOARD=arty-z7-20 FRAMES=30 \
	  SIM_GOLDEN= SIM_PS=systems/nes/test/ps/state.ps SIM_ARGS="--expect-ps-hex $(abspath systems/nes/test/ps/state.hex)"

# Side track N, N.10: CHIP-8's save states at layer 4 on the Arty (its top
# has the hooks): freeze, the status, the record, RAM and the framebuffer
# through the windows, a state written in and run (systems/chip8/test/ps/state.ps)
CI_AFTER += savestate-chip8
.PHONY: ci-savestate-chip8
ci-savestate-chip8:
	$(MAKE) --no-print-directory resim SYSTEM=chip8 BOARD=arty-z7-20 FRAMES=10 \
	  SIM_GOLDEN= SIM_PS=systems/chip8/test/ps/state.ps SIM_ARGS="--expect-ps-hex $(abspath systems/chip8/test/ps/state.hex)"

# --- M7: the Super Nintendo ----------------------------------------------------
# Super Nintendo regression ROMs (CLAUDE.md rule 5): every
# systems/snes/test/asm/*.s assembles into build/snes/asm/*.sfc (WLA-DX:
# wla-65816, wlalink; the header, vectors, start-up and verdict from
# snes.inc). The snes tests read them from there.
SNES_ASM_DIR = systems/snes/test/asm
SNES_ASM     = $(patsubst $(SNES_ASM_DIR)/%.s,build/snes/asm/%.sfc,$(wildcard $(SNES_ASM_DIR)/*.s))

.PHONY: snes-asm
snes-asm: nix-shell $(SNES_ASM)

build/snes/asm/%.sfc: $(SNES_ASM_DIR)/%.s $(SNES_ASM_DIR)/snes.inc
	mkdir -p $(@D)
	cd $(SNES_ASM_DIR) && wla-65816 -o $(abspath $(@:.sfc=.o)) $*.s
	printf '[objects]\n%s\n' $(abspath $(@:.sfc=.o)) > $(@:.sfc=.link)
	wlalink -r $(@:.sfc=.link) $@.part
	mv $@.part $@

test: snes-asm

# The Super Nintendo's CPUs are packages of their own (cpus/w65c816,
# cpus/spc700): its tests run theirs too.
TEST_TARGETS += $(if $(filter snes,$(SYSTEM)),w65c816 spc700)

# A debugging build (the freeze hunt, 2026-10-19): `make hdl SYSTEM=snes
# SNES_TRACE=1` (then make bit) prints a line of the core's state a second
# on the PS byte channel (Snes.Machine, "The trace"); the PS app shows it as
# "serial: c ..." lines. It writes the same build folder as the normal
# build: rebuild without it before a release.
CLASH_FLAGS += $(if $(and $(filter snes,$(SYSTEM)),$(filter 1,$(SNES_TRACE))),-DSNES_TRACE)

# Our own SPC700 boot ROM (M7 step 7.4; docs/systems/snes.md): 64 bytes,
# build/snes/ipl.bin, read by the model and (later) the core. The user's
# own dump replaces it on the card; Nintendo's is never fetched.
SNES_IPL = build/snes/ipl.bin
snes-asm: $(SNES_IPL)
$(SNES_IPL): systems/snes/ipl/ipl.s
	mkdir -p $(@D)
	cd systems/snes/ipl && wla-spc700 -o $(abspath $(@:.bin=.o)) ipl.s
	printf '[objects]\n%s\n' $(abspath $(@:.bin=.o)) > $(@:.bin=.link)
	wlalink -r $(@:.bin=.link) $@.part
	test "$$(stat -c %s $@.part)" = 64
	mv $@.part $@

# SPC700 programs for the test ROMs (step 7.4): systems/snes/test/spc/*.s
# into build/snes/spc/*.bin (wla-spc700), included by the 65816 ROMs.
build/snes/spc/%.bin: systems/snes/test/spc/%.s
	mkdir -p $(@D)
	cd systems/snes/test/spc && wla-spc700 -o $(abspath $(@:.bin=.o)) $*.s
	printf '[objects]\n%s\n' $(abspath $(@:.bin=.o)) > $(@:.bin=.link)
	wlalink -r $(@:.bin=.link) $@.part
	mv $@.part $@
build/snes/asm/tone.sfc: build/snes/spc/tone.bin
build/snes/asm/spcfuzz.sfc: build/snes/spc/spcfuzz.bin
build/snes/asm/dsprace.sfc: build/snes/spc/dsprace.bin

# Out-of-context implementation of a part (step 7.6: the PPU; snes.md
# records its numbers): `make snes-ooc [SNES_OOC=Snes.PpuOoc]` runs Clash
# on that top, then Vivado's synthesis, placement and routing of it alone
# on the Arty's part at 25.175 MHz (systems/snes/ooc.tcl), in the queue's
# bit slot (it is a Vivado run like make bit's); reports in
# build/snes/ooc/<top>/reports/.
SNES_OOC      ?= Snes.PpuOoc
SNES_OOC_TOP  ?= snes_ppu_ooc
SNES_OOC_DIR   = build/snes/ooc/$(SNES_OOC)$(if $(filter hier,$(SNES_OOC_MODE)),-hier)
# SNES_OOC_MODE=hier keeps the hierarchy (a row per Clash component)
SNES_OOC_MODE ?= flat
# make snes-ooc-hdl is its Clash half alone (for the remote worker:
# WRUN_PULL=build/snes/ooc/<top>/verilog), make snes-ooc-run the Vivado half.
.PHONY: snes-ooc snes-ooc-hdl snes-ooc-run
snes-ooc: snes-ooc-hdl
	$(call QUEUE_RUN,bit) $(MAKE) --no-print-directory snes-ooc-run
snes-ooc-hdl: SYSTEM_CABAL = systems/snes/snes.cabal
snes-ooc-hdl: nix-shell
	mkdir -p $(SNES_OOC_DIR)/obj $(SNES_OOC_DIR)/verilog
	LOCK_DIR="$(LOCK_DIR)" tools/clash-slot.sh -- $(PROGRESS) clash ooc:$(SNES_OOC) -- \
	  clash --verilog -isystems/snes/src -iplatform/src -icpus/w65c816/src -icpus/spc700/src -outputdir $(SNES_OOC_DIR)/obj \
	  -fclash-hdldir $(SNES_OOC_DIR)/verilog -fclash-no-cache -fclash-clear $(CLASH_FLAGS) $(if $(filter hier,$(SNES_OOC_MODE)),-DSNES_HIER) $(SNES_OOC)
snes-ooc-run:
	@command -v vivado >/dev/null || test -f "$(VIVADO_SETTINGS)" || \
	  { echo "make snes-ooc: no vivado on PATH and VIVADO_SETTINGS=$(VIVADO_SETTINGS) not found" >&2; exit 1; }
	bash -c '$(VIVADO_ENV) && cd "$(abspath $(SNES_OOC_DIR))" && rm -f ooc.log && \
	  { vivado -mode batch -nojournal -log ooc.log -source "$(abspath systems/snes/ooc.tcl)" \
	      -tclargs "$(abspath $(SNES_OOC_DIR))/verilog/$(SNES_OOC).topEntity" $(SNES_OOC_TOP) "$(abspath $(SNES_OOC_DIR))/reports" $(SNES_OOC_MODE) </dev/null; \
	    grep -q "^ooc.tcl: OK" ooc.log || { echo "make snes-ooc: see $(abspath $(SNES_OOC_DIR))/ooc.log" >&2; exit 1; }; }'

# Layer 4 for the Super Nintendo on the Arty (M7 step 7.10): lint; the
# krom picture against the spec's golden (systems/snes/test/golden/vga;
# snes-run --vga: the harness's last of 8 frames is the spec's ninth), and
# again with a DDR of long latency (the machine held, nothing changed); the
# exit game, shmup_1942, in play at frame 31 with that latency (its
# golden; LED1, rows late, stays off: --expect-led); the
# tone on HDMI and the jack (1000 Hz, 0.26 % low: the line-locked pace),
# and none in HDMI's audio at the menu's volume 0;
# a load through the PS link (PS_IN says system 9); the menu's overlay
# over black. Part of make ci (CI_LINT).
SNES_PIC = roms/snes/krom/8x8BG1Map2BPP32x328PAL.sfc
.PHONY: ci-arty-snes
ci-arty-snes: snes-asm
	$(MAKE) --no-print-directory BOARD=arty-z7-20 lint SYSTEM=snes
	$(MAKE) --no-print-directory BOARD=arty-z7-20 sim SYSTEM=snes ROM=$(SNES_PIC) FRAMES=8
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=snes ROM=$(SNES_PIC) FRAMES=8 \
	  SIM_ARGS="--axi-latency 60 --axi-seed 7"
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=snes ROM=roms/snes/opensnes/shmup_1942.sfc FRAMES=30 \
	  SIM_ARGS="--axi-latency 60 --axi-seed 7 --expect-led 0x2=0"
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=snes ROM=build/snes/asm/tone.sfc FRAMES=20 \
	  SIM_GOLDEN= SIM_ARGS="--expect-tone 997.4 --expect-hdmi-tone 997.4"
	# the menu's volume at 0 (side track L's settings register): no sound left
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=snes ROM=build/snes/asm/tone.sfc FRAMES=20 \
	  SIM_GOLDEN= SIM_PS=platform/test/ps/volume-0.ps SIM_ARGS="--expect-quiet 0"
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=snes ROM=$(SNES_PIC) FRAMES=24 \
	  SIM_PS=systems/snes/test/ps/reload.ps SIM_GOLDEN=systems/snes/test/golden/vga/8x8BG1Map2BPP32x328PAL-faded-in.png \
	  SIM_ARGS="--expect-ps-in 0x00090000"
	# (LD1 off: the rows the menu holds back aren't late)
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=snes ROM=$(SNES_PIC) FRAMES=10 \
	  SIM_PS=systems/gb/test/ps/menu.ps SIM_GOLDEN=systems/gb/test/golden/vga/menu-overlay.png SIM_ARGS="--expect-led 0x2=0"
	# pad 2 (step 7.12): its link bytes, as the relay writes them
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=snes ROM=build/snes/asm/joy2.sfc FRAMES=10 \
	  SIM_PS=systems/snes/test/ps/pad2.ps
	# battery saves (step 7.11): SRAM loaded while held, the game's write,
	# the read back on the byte channel, the dirty flag
	$(MAKE) --no-print-directory BOARD=arty-z7-20 resim SYSTEM=snes ROM=build/snes/asm/save.sfc FRAMES=25 SIM_GOLDEN= \
	  SIM_PS=systems/snes/test/ps/save.ps SIM_ARGS="--expect-ps-hex $(abspath systems/snes/test/ps/save.hex) --expect-ps-in 0x0F090040"
	@echo "make ci-arty-snes: OK"
CI_LINT += arty-snes
ci-arty: ci-arty-snes

# Save states at layer 4 (M7 step 7.14b): the Arty's top (Snes.StateHooks)
# on tone.sfc: the freeze, the status, a byte of each memory through the
# windows, the chain read round twice, then a thaw: the tone plays on
# (systems/snes/test/ps/state.ps; the bytes it reads back, state.hex)
CI_AFTER += savestate-snes
.PHONY: ci-savestate-snes
ci-savestate-snes: snes-asm
	$(MAKE) --no-print-directory resim SYSTEM=snes BOARD=arty-z7-20 ROM=build/snes/asm/tone.sfc FRAMES=24 \
	  SIM_GOLDEN= SIM_PS=systems/snes/test/ps/state.ps SIM_ARGS="--expect-ps-hex $(abspath systems/snes/test/ps/state.hex) --expect-tone 997.4 --expect-hdmi-tone 997.4"

# The same on the ZedBoard (the second board; make ci ALL=1): VGA (the
# goldens on its 4-bit pins, golden/vga4) and HDMI through the ADV7511
# (its I2C init against the Game Boy's golden; 25 frames, past its boot
# wait; the reload's picture is the faded-in one, as frame 26), the
# tone on S/PDIF and none at the menu's volume 0 (side track L's settings
# register), the buttons and switches in PS_IN with the menu.
.PHONY: ci-snes
ci-snes: snes-asm
	$(MAKE) --no-print-directory BOARD=zedboard lint SYSTEM=snes
	$(MAKE) --no-print-directory BOARD=zedboard sim SYSTEM=snes ROM=$(SNES_PIC) FRAMES=25
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=snes ROM=roms/snes/opensnes/shmup_1942.sfc FRAMES=30 \
	  SIM_ARGS="--axi-latency 60 --axi-seed 7"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=snes ROM=build/snes/asm/tone.sfc FRAMES=25 \
	  SIM_GOLDEN= SIM_ARGS="--expect-tone 997.4"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=snes ROM=build/snes/asm/tone.sfc FRAMES=25 \
	  SIM_GOLDEN= SIM_PS=platform/test/ps/volume-0.ps SIM_ARGS="--expect-quiet 0"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=snes ROM=$(SNES_PIC) FRAMES=25 \
	  SIM_PS=systems/snes/test/ps/reload.ps SIM_ARGS="--expect-ps-in 0x00090000"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=snes ROM=$(SNES_PIC) FRAMES=25 SIM_PS=systems/gb/test/ps/menu.ps \
	  SIM_GOLDEN=systems/gb/test/golden/vga/menu-overlay.png SIM_ARGS="--btn 0x01 --sw 0x01 --expect-ps-in 0x00090101"
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=snes ROM=build/snes/asm/joy2.sfc FRAMES=25 \
	  SIM_PS=systems/snes/test/ps/pad2.ps
	$(MAKE) --no-print-directory BOARD=zedboard resim SYSTEM=snes ROM=build/snes/asm/save.sfc FRAMES=25 SIM_GOLDEN= \
	  SIM_PS=systems/snes/test/ps/save.ps SIM_ARGS="--expect-ps-hex $(abspath systems/snes/test/ps/save.hex) --expect-ps-in 0x0F090040"
	@echo "make ci-snes: OK"
CI_LINT_ALL += snes

# The card's ROMs from another folder (side track K, the user's request
# 2026-10-17): SD_ROMS_DIR=<folder>, laid out as roms/ (<system>/games,
# <system>/blargg, ...; the main checkout's roms/ holds the user's games,
# a worktree's only the fetched test ROMs), and every SD_*_ROMS folder
# under roms/ is taken from there instead, for make sd and make sd-sync.
# Here at the end, after every track's SD_*_ROMS lines and their +=.
ifneq ($(SD_ROMS_DIR),)
$(foreach v,$(filter SD_%ROMS,$(.VARIABLES)),$(eval $(v) := $$(patsubst roms/%,$(SD_ROMS_DIR)/%,$$($(v)))))
endif

# --- Side track Q: the desktop emulator from the specs ------------------
# A system's reference model as cabal's foreign library (PLAN.md Q.1;
# systems/gb/desktop/Gb/Desktop.hs), loaded by the Zig front end
# (platform/desktop/; dlopen, gb_spec.zig says why). make test-desktop: the library against
# the spec's goldens (part of make test); make desktop ROM=…: the window.
# a foreign library's .so in a build directory (cabal names its place)
desktop_so_in = $(abspath $(shell find $(2) -path '*/f/$(1)-desktop/build/$(1)-desktop/lib$(1)-desktop.so' 2>/dev/null | head -1))
desktop_libs_in = -Dgb-lib="$(call desktop_so_in,gb,$(1))" -Dnes-lib="$(call desktop_so_in,nes,$(1))" -Dchip8-lib="$(call desktop_so_in,chip8,$(1))" -Dsms-lib="$(call desktop_so_in,sms,$(1))" -Da800-lib="$(call desktop_so_in,a800xl,$(1))" -Dsnes-lib="$(call desktop_so_in,snes,$(1))"
DESKTOP_LIBS    = $(call desktop_libs_in,dist-newstyle)
DESKTOP_FLIBS   = gb:flib:gb-desktop nes:flib:nes-desktop chip8:flib:chip8-desktop sms:flib:sms-desktop a800xl:flib:a800xl-desktop snes:flib:snes-desktop
DESKTOP_ARGS   ?=
# Q.6: make desktop builds the libraries with GHC's LLVM backend (measured:
# the 800XL 19% faster, the SNES 11%), in a build directory of its own so
# the normal builds stay as they are; LLVM 18 from flake.nix by path
# (EMU_LLVM_BIN, EMU_LLVM_CLANG). DESKTOP_LLVM=0: the normal build. The
# first LLVM build of a package takes long (its Clash modules too).
# make test-desktop always tests the normal build.
DESKTOP_LLVM   ?= 1
DESKTOP_LLVM_DIR = build/desktop-llvm
DESKTOP_RUN_DIR  = $(if $(filter 1,$(DESKTOP_LLVM)),$(DESKTOP_LLVM_DIR),dist-newstyle)
.PHONY: desktop-lib desktop-lib-llvm test-desktop desktop

# The desktop installed outside the repo (side track Q, the user
# 2026-11-01): `rota ROM` from anywhere; tools/install-desktop.sh
# (NixOS: a GC root keeps its libraries). Rerun to update.
INSTALL_PREFIX ?= $(or $(XDG_DATA_HOME),$(HOME)/.local/share)/emulation_clash/desktop
INSTALL_BIN    ?= $(HOME)/.local/bin
.PHONY: install install-desktop
install: install-desktop
install-desktop: $(if $(filter 1,$(DESKTOP_LLVM)),desktop-lib-llvm,desktop-lib) a800-asm snes-asm gb-asm
	tools/install-desktop.sh "$(INSTALL_PREFIX)" "$(INSTALL_BIN)" "$$(pkg-config --variable=libdir sdl3)" \
	  $(foreach s,gb nes chip8 sms a800xl snes,"$(call desktop_so_in,$(s),$(DESKTOP_RUN_DIR))")
desktop-lib: nix-shell
	$(CABAL) build -v0 $(DESKTOP_FLIBS)
desktop-lib-llvm: nix-shell
	@test -n "$$EMU_LLVM_BIN" || { echo 'make desktop: no EMU_LLVM_BIN (the nix shell has it; DESKTOP_LLVM=0 builds without LLVM)' >&2; exit 1; }
	$(CABAL) build -v0 --builddir=$(DESKTOP_LLVM_DIR) --ghc-options="-fllvm -pgmlc $$EMU_LLVM_BIN/llc -pgmlo $$EMU_LLVM_BIN/opt -pgmlas $$EMU_LLVM_CLANG" $(DESKTOP_FLIBS)
test-desktop: desktop-lib gb-asm nes-asm a800-asm snes-asm sms-asm
	$(ZIG_BUILD) desktop-test $(DESKTOP_LIBS)
	$(ZIG_BUILD) desktop $(DESKTOP_LIBS) -Dsdl3-libdir=$$(pkg-config --variable=libdir sdl3) --prefix build/desktop
	# the front end headless (no window, no sound device): its frames as the spec's
	# (--scale 1: the dummy video driver scales in software)
	SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy env -u LD_LIBRARY_PATH build/desktop/desktop --scale 1 --keymap /dev/null --unpaced --frames 30 \
	  --expect-png systems/gb/test/golden/spec/dmg-acid2.png roms/gb/dmg-acid2/dmg-acid2.gb
	SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy env -u LD_LIBRARY_PATH build/desktop/desktop --scale 1 --keymap /dev/null --unpaced --frames 30 \
	  --expect-png systems/nes/test/golden/spec/nestest.png roms/nes/nestest/nestest.nes
	SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy env -u LD_LIBRARY_PATH build/desktop/desktop --scale 1 --keymap /dev/null --unpaced --frames 60 \
	  --fast --expect-png systems/chip8/test/golden/spec/2-ibm-logo.png roms/chip8/timendus/2-ibm-logo.ch8
	# a battery save through the front end: save.gb counts its boots in
	# cartridge RAM, so two runs leave "SAVE" and 2 in the board's layout
	rm -rf build/desktop/saves
	for run in 1 2; do SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy env -u LD_LIBRARY_PATH build/desktop/desktop --keymap /dev/null \
	  --unpaced --frames 5 --saves build/desktop/saves build/gb/asm/save.gb || exit 1; done
	test "$$(od -An -tx1 -N6 build/desktop/saves/gb/asm/save.sav | tr -d ' ')" = 534156450200 || \
	  { echo "make test-desktop: FAIL build/desktop/saves/gb/asm/save.sav:"; od -An -c -N8 build/desktop/saves/gb/asm/save.sav; exit 1; }
	@echo 'make test-desktop: the battery save round trip OK ("SAVE", 2)'
	# an input movie (Q.8; sms_spec's test wrote it from Waternet's script):
	# gg-run plays it as the script (the same frame hashes), and the
	# desktop (the fast version) ends where gg-run (the reference model) does
	$(CABAL) run -v0 gg-run -- roms/gg/games/waternet.gg --input systems/gg/test/input/waternet-start.txt --frames 600 --hashes build/desktop/waternet-script.hashes
	$(CABAL) run -v0 gg-run -- roms/gg/games/waternet.gg --movie build/desktop/waternet.mv1 --frames 600 --hashes build/desktop/waternet-movie.hashes \
	  --png-at 599 build/desktop/waternet-movie.png
	cmp build/desktop/waternet-script.hashes build/desktop/waternet-movie.hashes
	SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy env -u LD_LIBRARY_PATH build/desktop/desktop --scale 1 --keymap /dev/null --unpaced --no-save \
	  --movie build/desktop/waternet.mv1 --frames 600 --expect-png build/desktop/waternet-movie.png roms/gg/games/waternet.gg
	@echo 'make test-desktop: the movie plays the same in gg-run and on the desktop'
	# gg-run's command mode (Q.8): a save state loaded replays the same frames
	$(CABAL) run -v0 gg-run -- roms/gg/games/waternet.gg --movie build/desktop/waternet.mv1 \
	  --commands systems/gg/test/input/commands-state.txt > build/desktop/gg-cmd.out
	test "$$(grep -c '^ok [0-9a-f]\{16\}$$' build/desktop/gg-cmd.out)" = 2 && \
	  test "$$(grep '^ok [0-9a-f]\{16\}$$' build/desktop/gg-cmd.out | uniq | wc -l)" = 1 || \
	  { echo "make test-desktop: FAIL gg-run's state round trip:"; cat build/desktop/gg-cmd.out; exit 1; }
	@echo 'make test-desktop: gg-run --commands, a state loaded replays the same frames'
	# a movie from a state (sms_spec's test: the desktop's state at Waternet's
	# frame 300, then 300 frames): gg-run (the reference model) and the
	# desktop end where the power-on movie does at 599; gg-run's command
	# mode records one (record/stop) that both play the same
	$(CABAL) run -v0 gg-run -- roms/gg/games/waternet.gg --movie build/desktop/waternet-state.mv1 --frames 300 \
	  --png-at 299 build/desktop/waternet-state.png
	cmp build/desktop/waternet-movie.png build/desktop/waternet-state.png
	SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy env -u LD_LIBRARY_PATH build/desktop/desktop --scale 1 --keymap /dev/null --unpaced --no-save \
	  --movie build/desktop/waternet-state.mv1 --frames 300 --expect-png build/desktop/waternet-movie.png roms/gg/games/waternet.gg
	printf 'frames 300\nrecord build/desktop/gg-rec.mv1\nframes 300\nstop\nquit\n' | \
	  $(CABAL) run -v0 gg-run -- roms/gg/games/waternet.gg --movie build/desktop/waternet.mv1 --commands - > build/desktop/gg-rec.out
	grep -q '^ok build/desktop/gg-rec.mv1 300 frames$$' build/desktop/gg-rec.out || \
	  { echo "make test-desktop: FAIL gg-run's record/stop:"; cat build/desktop/gg-rec.out; exit 1; }
	$(CABAL) run -v0 gg-run -- roms/gg/games/waternet.gg --movie build/desktop/gg-rec.mv1 --frames 300 \
	  --png-at 299 build/desktop/gg-rec.png
	cmp build/desktop/waternet-movie.png build/desktop/gg-rec.png
	SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy env -u LD_LIBRARY_PATH build/desktop/desktop --scale 1 --keymap /dev/null --unpaced --no-save \
	  --movie build/desktop/gg-rec.mv1 --frames 300 --expect-png build/desktop/waternet-movie.png roms/gg/games/waternet.gg
	@echo 'make test-desktop: movies from a state play the same in gg-run and on the desktop, both ways'
	# wider movies (the header's frame_bytes): the 800XL's 11 key bytes, the
	# SNES's two pads from a state (the spec tests wrote them)
	SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy env -u LD_LIBRARY_PATH build/desktop/desktop --scale 1 --keymap /dev/null --unpaced --no-save \
	  --movie build/desktop/keys.mv1 --frames 100 --expect-png systems/a800xl/test/golden/keys.png build/a800xl/asm/keys.xex
	SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy env -u LD_LIBRARY_PATH build/desktop/desktop --scale 1 --keymap /dev/null --unpaced --no-save \
	  --movie build/desktop/snes-state.mv1 --frames 120 --expect-png build/desktop/snes-state.png roms/snes/games/unnamed-tech-demo-v3.sfc
	@echo 'make test-desktop: 800XL and SNES movies (11 and 4 bytes a frame) play on the desktop'
	# the debugger's socket (side track R, R.1): a client drives the desktop
	python3 platform/desktop/test_debug.py build/desktop/desktop build/desktop/dbg.sock roms/gg/games/waternet.gg
	# rota-dbg, the terminal debugger (R.2), drawn in a pseudo-terminal
	python3 platform/desktop/test_dbg_tui.py build/desktop/desktop build/desktop/rota-dbg build/desktop/dbg.sock roms/gg/games/waternet.gg
	# with FULL=1 also cpu_instrs to its end (about 45 s on the worker: make ci's
	# result cache doesn't skip a Makefile step, side K 2026-10-19)
	$(if $(filter 1,$(FULL)),SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy env -u LD_LIBRARY_PATH build/desktop/desktop --keymap /dev/null --unpaced --frames 3300 \
	  --expect-png systems/gb/test/golden/spec/blargg-cpu_instrs.png roms/gb/blargg/cpu_instrs/cpu_instrs.gb)
test: test-desktop
desktop: $(if $(filter 1,$(DESKTOP_LLVM)),desktop-lib-llvm,desktop-lib)
	@test -n "$(ROM)" || { echo 'make desktop: set ROM=<file> (any system, by its extension; DESKTOP_ARGS=… for the options, e.g. --gbc, --reference)' >&2; exit 1; }
	$(ZIG_BUILD) desktop $(call desktop_libs_in,$(DESKTOP_RUN_DIR)) -Dsdl3-libdir=$$(pkg-config --variable=libdir sdl3) --prefix build/desktop-run
	env -u LD_LIBRARY_PATH build/desktop-run/desktop --saves "$(SAVES_DIR)" $(DESKTOP_ARGS) "$(ROM)"
