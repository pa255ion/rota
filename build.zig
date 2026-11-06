//! `zig build` for the repo's Zig code (side track H): the PS app, the
//! desktop `link`, the Verilator harness's Zig side and every Zig unit test.
//! Make stays the front door and calls these steps; Clash, Verilator's C++
//! model, Vivado, xsdb, bootgen and the shell/Python tools stay in the
//! Makefile. Make passes `--cache-dir build/zig-cache` and a `--prefix`, and
//! every step installs at the path Make used before, so nothing downstream
//! changes:
//!
//!   zig build ps-app -Dps7-init-c=… --prefix build   ps/relay.elf, .s, ps7_init.s
//!   zig build link --prefix build                    host/link
//!   zig build sim  -Dverilator-obj=DIR --prefix DIR  vga-sim (Make's OBJ_DIR)
//!   zig build play -Dverilator-obj=DIR -Dsdl3-libdir=… --prefix DIR   vga-play
//!   zig build test                                   every Zig unit test
//!   zig build desktop-test -Dgb-lib=… -Dnes-lib=… -Dchip8-lib=… -Dsms-lib=… -Da800-lib=… -Dsnes-lib=…   the spec libraries' tests (side track Q)
//!   zig build desktop -Dgb-lib=… -Dnes-lib=… -Dchip8-lib=… -Dsms-lib=… -Da800-lib=… -Dsnes-lib=… -Dsdl3-libdir=… --prefix DIR   desktop
//!
//! Each compilation has the same modules as the `zig build-exe` / `zig test`
//! command line it replaces (a file may belong to only one module of a
//! compilation: the relay imports keymap.zig as a file, `link` as the
//! module `keymap`).

const std = @import("std");

pub fn build(b: *std.Build) void {
    const host = b.graph.host;

    // Shared modules, declared once (the desktop builds and the harness;
    // the PS app imports its siblings by file name).
    const protocol = b.createModule(.{ .root_source_file = b.path("platform/ps/protocol.zig") });
    const keymap = b.createModule(.{ .root_source_file = b.path("platform/ps/keymap.zig") });
    const fat = b.createModule(.{ .root_source_file = b.path("platform/ps/fat.zig") });
    const pl = b.createModule(.{ .root_source_file = b.path("platform/ps/pl.zig") });
    // the PS app's system table (battery save sizes, side track I), for link
    const menu = b.createModule(.{ .root_source_file = b.path("platform/ps/menu.zig") });

    psApp(b);

    // SDL3 by its library directory (vga-play, desktop)
    const sdl3_libdir = b.option([]const u8, "sdl3-libdir", "SDL3's library directory (pkg-config --variable=libdir sdl3)");

    // --- link (platform/host/): keyboards, ROM loading, SD writing -------
    const link = b.addExecutable(.{
        .name = "link",
        .root_module = b.createModule(.{
            .root_source_file = b.path("platform/host/link.zig"),
            .target = host,
            .optimize = .ReleaseFast,
            .imports = &.{
                .{ .name = "protocol", .module = protocol },
                .{ .name = "keymap", .module = keymap },
                .{ .name = "menu", .module = menu },
                .{ .name = "fat", .module = fat },
            },
        }),
    });
    const link_step = b.step("link", "the desktop link (prefix/host/link)");
    link_step.dependOn(&b.addInstallFileWithDir(link.getEmittedBin(), .prefix, "host/link").step);

    // ps-encode (side track K, D3): PS link scripts hashed as the harness
    // plays them, for make ci's result cache (tools/ci-cache.py)
    const ps_encode = b.addExecutable(.{
        .name = "ps-encode",
        .root_module = b.createModule(.{
            .root_source_file = b.path("platform/sim/ps_encode.zig"),
            .target = host,
            .optimize = .ReleaseFast,
            .imports = &.{.{ .name = "pl", .module = pl }},
        }),
    });
    const ps_encode_step = b.step("ps-encode", "PS link scripts hashed as played (prefix/host/ps-encode)");
    ps_encode_step.dependOn(&b.addInstallFileWithDir(ps_encode.getEmittedBin(), .prefix, "host/ps-encode").step);

    // gb-restore (side track L, L.6c): the Game Boy's restore routine for a
    // state table, for gb-spec and gb-savestate (platform/ps/systems/gb_restore.zig)
    const gb_restore = b.addExecutable(.{
        .name = "gb-restore",
        .root_module = b.createModule(.{
            .root_source_file = b.path("platform/host/gb_restore_tool.zig"),
            .target = host,
            .optimize = .ReleaseSafe,
            .imports = &.{.{ .name = "gb_restore", .module = b.createModule(.{
                .root_source_file = b.path("platform/ps/systems/gb_restore.zig"),
                .target = host,
            }) }},
        }),
    });
    const gb_restore_step = b.step("gb-restore", "the Game Boy's restore routine for a state table (prefix/host/gb-restore)");
    gb_restore_step.dependOn(&b.addInstallFileWithDir(gb_restore.getEmittedBin(), .prefix, "host/gb-restore").step);

    // gg-restore (side track N, N.1): the Game Gear's and Master System's
    // restore routine for a state table, for gg-spec (platform/ps/systems/gg_restore.zig)
    const gg_restore = b.addExecutable(.{
        .name = "gg-restore",
        .root_module = b.createModule(.{
            .root_source_file = b.path("platform/host/gg_restore_tool.zig"),
            .target = host,
            .optimize = .ReleaseSafe,
            .imports = &.{.{ .name = "gg_restore", .module = b.createModule(.{
                .root_source_file = b.path("platform/ps/systems/gg_restore.zig"),
                .target = host,
            }) }},
        }),
    });
    const gg_restore_step = b.step("gg-restore", "the Game Gear's restore routine for a state table (prefix/host/gg-restore)");
    gg_restore_step.dependOn(&b.addInstallFileWithDir(gg_restore.getEmittedBin(), .prefix, "host/gg-restore").step);

    // a800-restore (side track N, N.4): the Atari 800XL's restore routine
    // for a state table, for a800xl-spec (platform/ps/systems/a800_restore.zig)
    const a800_restore = b.addExecutable(.{
        .name = "a800-restore",
        .root_module = b.createModule(.{
            .root_source_file = b.path("platform/host/a800_restore_tool.zig"),
            .target = host,
            .optimize = .ReleaseSafe,
            .imports = &.{.{ .name = "a800_restore", .module = b.createModule(.{
                .root_source_file = b.path("platform/ps/systems/a800_restore.zig"),
                .target = host,
            }) }},
        }),
    });
    const a800_restore_step = b.step("a800-restore", "the 800XL's restore routine for a state table (prefix/host/a800-restore)");
    a800_restore_step.dependOn(&b.addInstallFileWithDir(a800_restore.getEmittedBin(), .prefix, "host/a800-restore").step);

    // nes-restore (side track N, N.7): the NES's restore routine for a
    // state table, for nes-spec (platform/ps/systems/nes_restore.zig)
    const nes_restore = b.addExecutable(.{
        .name = "nes-restore",
        .root_module = b.createModule(.{
            .root_source_file = b.path("platform/host/nes_restore_tool.zig"),
            .target = host,
            .optimize = .ReleaseSafe,
            .imports = &.{.{ .name = "nes_restore", .module = b.createModule(.{
                .root_source_file = b.path("platform/ps/systems/nes_restore.zig"),
                .target = host,
            }) }},
        }),
    });
    const nes_restore_step = b.step("nes-restore", "the NES's restore routine for a state table (prefix/host/nes-restore)");
    nes_restore_step.dependOn(&b.addInstallFileWithDir(nes_restore.getEmittedBin(), .prefix, "host/nes-restore").step);

    // --- the Clash 68000's Verilator model (side track P, P.5) -----------
    // Make builds the model (Vm68k__ALL.a, libverilated.a, the vm68k.cpp
    // shim's vm68k.o) in its M68K_SIM_OBJ and passes that directory:
    // libm68k.so with m68k.h (the C library), m68k-sim (the harness, on the
    // same library code), m68k-ctest (a C program linked against it).
    const m68k_lib_step = b.step("m68k-lib", "the 68000 library libm68k.so and m68k.h (needs -Dm68k-obj)");
    const m68k_sim_step = b.step("m68k-sim", "the 68000 harness m68k-sim (needs -Dm68k-obj)");
    const m68k_ctest_step = b.step("m68k-ctest", "the C program on libm68k.so (needs -Dm68k-obj)");
    if (b.option([]const u8, "m68k-obj", "Make's 68000 Verilator OBJ_DIR (vm68k.o, Vm68k__ALL.a, libverilated.a)")) |obj| {
        const m68k_mod = b.createModule(.{
            .root_source_file = b.path("cpus/m68k/sim/lib.zig"),
            .target = b.graph.host,
            .optimize = .ReleaseFast,
            .link_libc = true,
            .link_libcpp = true,
        });
        for ([_][]const u8{ "vm68k.o", "Vm68k__ALL.a", "libverilated.a" }) |f|
            m68k_mod.addObjectFile(.{ .cwd_relative = b.pathJoin(&.{ obj, f }) });
        const lib = b.addLibrary(.{ .name = "m68k", .linkage = .dynamic, .root_module = m68k_mod });
        lib.installHeader(b.path("cpus/m68k/sim/m68k.h"), "m68k.h");
        m68k_lib_step.dependOn(&b.addInstallArtifact(lib, .{}).step);
        const sim_mod = b.createModule(.{
            .root_source_file = b.path("cpus/m68k/sim/main.zig"),
            .target = b.graph.host,
            .optimize = .ReleaseFast,
        });
        sim_mod.addImport("m68k", m68k_mod);
        const m68k_sim = b.addExecutable(.{ .name = "m68k-sim", .root_module = sim_mod });
        m68k_sim_step.dependOn(&b.addInstallArtifact(m68k_sim, .{}).step);
        const ctest_mod = b.createModule(.{ .target = b.graph.host, .optimize = .ReleaseSafe, .link_libc = true });
        ctest_mod.addCSourceFile(.{ .file = b.path("cpus/m68k/sim/test.c"), .flags = &.{ "-std=c11", "-Wall", "-Werror" } });
        ctest_mod.addIncludePath(b.path("cpus/m68k/sim"));
        ctest_mod.linkLibrary(lib);
        const ctest = b.addExecutable(.{ .name = "m68k-ctest", .root_module = ctest_mod });
        m68k_ctest_step.dependOn(&b.addInstallArtifact(ctest, .{}).step);
    } else {
        m68k_lib_step.dependOn(&b.addFail("zig build m68k-lib: set -Dm68k-obj (make m68k-sim does)").step);
        m68k_sim_step.dependOn(&b.addFail("zig build m68k-sim: set -Dm68k-obj (make m68k-sim does)").step);
        m68k_ctest_step.dependOn(&b.addFail("zig build m68k-ctest: set -Dm68k-obj (make m68k-sim does)").step);
    }

    // --- the Verilator harness (platform/sim/), around Make's model ------
    // Make builds the model (Vtop__ALL.a, libverilated.a, the vtop.cpp
    // shim's vtop.o) in OBJ_DIR and passes that directory.
    const sim_step = b.step("sim", "the layer 4 harness vga-sim (needs -Dverilator-obj)");
    const play_step = b.step("play", "the SDL3 window vga-play (needs -Dverilator-obj, -Dsdl3-libdir)");
    // Side track G: Arty Z7-20 tops (Make builds their vtop.o from
    // vtop_arty.cpp instead of vtop.cpp).
    const sim_arty_step = b.step("sim-arty", "the Arty's layer 4 harness arty-sim (needs -Dverilator-obj)");
    if (b.option([]const u8, "verilator-obj", "Make's Verilator OBJ_DIR (vtop.o, Vtop__ALL.a, libverilated.a)")) |obj| {
        const sim = b.addExecutable(.{
            .name = "vga-sim",
            .root_module = harnessModule(b, "platform/sim/main.zig", obj),
        });
        sim.root_module.addImport("pl", pl);
        sim_step.dependOn(&b.addInstallFileWithDir(sim.getEmittedBin(), .prefix, "vga-sim").step);
        const sim_arty = b.addExecutable(.{
            .name = "arty-sim",
            .root_module = harnessModule(b, "platform/sim/arty.zig", obj),
        });
        sim_arty.root_module.addImport("pl", pl);
        sim_arty_step.dependOn(&b.addInstallFileWithDir(sim_arty.getEmittedBin(), .prefix, "arty-sim").step);

        // SDL3 by its library directory (pkg-config, in Make), with an
        // rpath, so the binary runs without LD_LIBRARY_PATH.
        if (sdl3_libdir) |sdl| {
            const play = b.addExecutable(.{
                .name = "vga-play",
                .root_module = harnessModule(b, "platform/sim/play.zig", obj),
            });
            play.root_module.addLibraryPath(.{ .cwd_relative = sdl });
            play.root_module.addRPath(.{ .cwd_relative = sdl });
            play.root_module.linkSystemLibrary("SDL3", .{ .use_pkg_config = .no });
            play_step.dependOn(&b.addInstallFileWithDir(play.getEmittedBin(), .prefix, "vga-play").step);
        } else play_step.dependOn(&b.addFail("zig build play: set -Dsdl3-libdir (make play does)").step);
    } else {
        sim_step.dependOn(&b.addFail("zig build sim: set -Dverilator-obj (make sim does)").step);
        sim_arty_step.dependOn(&b.addFail("zig build sim-arty: set -Dverilator-obj (make sim BOARD=arty-z7-20 does)").step);
        play_step.dependOn(&b.addFail("zig build play: set -Dverilator-obj (make play does)").step);
    }

    // --- unit tests: every Zig test (Make's test-zig runs this) ----------
    // The tests read files relative to the repo root (FAT images that Make
    // generates in build/ps-test first, the build's bitstreams), which the
    // build graph doesn't see: so they always run, from the repo root.
    // --- M6: the YM2413 oracle for sms-spec (systems/sms/tools/) ----------
    // Nuked-OPLL's C (GPL, fetched by tools/fetch-roms.sh, never committed)
    // as a black box; Make passes its directory.
    const opll_step = b.step("opll-oracle", "the YM2413 oracle opll-oracle (needs -Dnuked-opll)");
    if (b.option([]const u8, "nuked-opll", "the directory with Nuked-OPLL's opll.c and opll.h")) |dir| {
        const mod = b.createModule(.{
            .root_source_file = b.path("systems/sms/tools/opll_oracle.zig"),
            .target = host,
            .optimize = .ReleaseFast,
            .link_libc = true,
        });
        mod.addIncludePath(.{ .cwd_relative = dir });
        mod.addCSourceFile(.{ .file = .{ .cwd_relative = b.fmt("{s}/opll.c", .{dir}) }, .flags = &.{"-O2"} });
        const exe = b.addExecutable(.{ .name = "opll-oracle", .root_module = mod });
        opll_step.dependOn(&b.addInstallFileWithDir(exe.getEmittedBin(), .prefix, "sms/opll-oracle").step);
    }

    // --- side track Q: the desktop emulator (platform/desktop/) ----------
    // A system's reference model as cabal's foreign library (Make builds
    // it and passes its path), and SDL3 for the window.
    desktop(b, sdl3_libdir);

    const test_step = b.step("test", "every Zig unit test (make test-zig prepares the images first)");
    const tests = [_]struct { []const u8, []const std.Build.Module.Import }{
        // the harness
        .{ "platform/sim/vga.zig", &.{} },
        .{ "platform/sim/png.zig", &.{} },
        .{ "platform/sim/i2c.zig", &.{} },
        .{ "platform/sim/hdmi.zig", &.{} },
        .{ "platform/sim/spdif.zig", &.{} },
        .{ "platform/sim/axi.zig", &.{} },
        .{ "platform/sim/ps.zig", &.{.{ .name = "pl", .module = pl }} },
        .{ "platform/sim/ps_encode.zig", &.{.{ .name = "pl", .module = pl }} },
        .{ "platform/sim/tmds.zig", &.{} },
        .{ "platform/sim/arty_tone.zig", &.{} },
        // the PS app (on the desktop)
        .{ "platform/ps/protocol.zig", &.{} },
        .{ "platform/ps/pl.zig", &.{} },
        .{ "platform/ps/savestate.zig", &.{} },
        .{ "platform/ps/movie.zig", &.{} },
        .{ "platform/ps/png.zig", &.{} },
        .{ "platform/ps/systems/gb_restore.zig", &.{} },
        .{ "platform/ps/systems/gg_restore.zig", &.{} },
        .{ "platform/ps/systems/a800_restore.zig", &.{} },
        .{ "platform/ps/systems/nes_restore.zig", &.{} },
        .{ "platform/ps/systems/a800xl_cas.zig", &.{} },
        .{ "platform/ps/pcap.zig", &.{} },
        .{ "platform/ps/mmu.zig", &.{} },
        .{ "platform/ps/menu.zig", &.{} },
        .{ "platform/ps/sd.zig", &.{} },
        .{ "platform/ps/fat.zig", &.{} },
        .{ "platform/ps/load.zig", &.{} },
        .{ "platform/ps/save.zig", &.{} },
        .{ "platform/ps/ddrtest.zig", &.{} },
        .{ "platform/ps/usb_fake.zig", &.{} },
        .{ "platform/ps/usb.zig", &.{} },
        .{ "platform/ps/usb_keys.zig", &.{} },
        .{ "platform/ps/a800_keys.zig", &.{} },
        .{ "platform/ps/keymap.zig", &.{} },
        .{ "platform/ps/splash.zig", &.{} },
        // the desktop side
        .{ "platform/host/sdimage.zig", &.{.{ .name = "protocol", .module = protocol }} },
        .{ "platform/host/sdimage_test.zig", &.{ .{ .name = "protocol", .module = protocol }, .{ .name = "fat", .module = fat } } },
        .{ "platform/host/link.zig", &.{ .{ .name = "protocol", .module = protocol }, .{ .name = "keymap", .module = keymap }, .{ .name = "menu", .module = menu }, .{ .name = "fat", .module = fat } } },
    };
    for (tests) |t| {
        const file, const imports = t;
        const unit = b.addTest(.{
            .name = std.fs.path.stem(file),
            .root_module = b.createModule(.{
                .root_source_file = b.path(file),
                .target = host,
                .imports = imports,
            }),
        });
        const run = b.addRunArtifact(unit);
        run.setName(b.fmt("test {s}", .{file}));
        run.setCwd(b.path("."));
        run.has_side_effects = true;
        test_step.dependOn(&run.step);
    }
    // The harness's executables need the Verilator model to link: here
    // they are only parsed.
    for ([_][]const u8{ "platform/sim/play.zig", "platform/sim/main.zig", "platform/sim/arty.zig" }) |file| {
        const check = b.addSystemCommand(&.{ b.graph.zig_exe, "ast-check" });
        check.setName(b.fmt("ast-check {s}", .{file}));
        check.addFileArg(b.path(file));
        test_step.dependOn(&check.step);
    }
}

/// A harness executable's root module: native, linked against Make's
/// Verilator model and libc++ (all C++ is built by `zig c++`, Makefile).
fn harnessModule(b: *std.Build, root: []const u8, obj: []const u8) *std.Build.Module {
    const m = b.createModule(.{
        .root_source_file = b.path(root),
        .target = b.graph.host,
        .optimize = .ReleaseFast,
        .link_libc = true,
        .link_libcpp = true,
    });
    for ([_][]const u8{ "vtop.o", "Vtop__ALL.a", "libverilated.a" }) |f|
        m.addObjectFile(.{ .cwd_relative = b.pathJoin(&.{ obj, f }) });
    // input movies' file format (side track O, O.2: arty-sim's --movie-*)
    m.addImport("movie", b.createModule(.{ .root_source_file = b.path("platform/ps/movie.zig") }));
    return m;
}

/// The PS app (platform/ps/<app>.zig; every PS system runs `relay`): bare
/// metal on the Cortex-A9, linked with Vivado's ps7_init.c for the SD boot
/// (side track F), installed as ps/<app>.elf with its assembly beside it.
fn psApp(b: *std.Build) void {
    const step = b.step("ps-app", "the PS app (prefix/ps/<app>.elf; needs -Dps7-init-c)");
    const app = b.option([]const u8, "ps-app", "the PS app, platform/ps/<name>.zig (default relay)") orelse "relay";
    // Side track G phase 2: the board module (platform/ps/board_<board>.zig,
    // '-' as '_'): the UART, the timer's rate, the USB PHY's reset.
    const board = b.option([]const u8, "board", "zedboard (default) or arty-z7-20") orelse "zedboard";
    const ps7_init_c = b.option([]const u8, "ps7-init-c", "Vivado's ZedBoard ps7_init.c (Make's PS7_INIT_C)") orelse {
        step.dependOn(&b.addFail("zig build ps-app: set -Dps7-init-c (Vivado's ZedBoard ps7_init.c; make ps-app does)").step);
        return;
    };

    // +strict_align: the app runs with the MMU off, where unaligned
    // accesses fault (tools/check-ps-align.sh, Makefile).
    var query: std.Target.Query = .{
        .cpu_arch = .arm,
        .os_tag = .freestanding,
        .abi = .eabihf,
        .cpu_model = .{ .explicit = &std.Target.arm.cpu.cortex_a9 },
    };
    query.cpu_features_add.addFeature(@intFromEnum(std.Target.arm.Feature.strict_align));
    const target = b.resolveTargetQuery(query);

    const mod = b.createModule(.{
        .root_source_file = b.path(b.fmt("platform/ps/{s}.zig", .{app})),
        .target = target,
        .optimize = .ReleaseSmall,
    });
    // ps7_init.c's warnings are the generator's, so -w; platform/ps/c/
    // stands in for Xilinx's xil_io.h.
    mod.addImport("board", b.createModule(.{
        .root_source_file = b.path(b.fmt("platform/ps/board_{s}.zig", .{std.mem.replaceOwned(u8, b.allocator, board, "-", "_") catch @panic("OOM")})),
        .target = target,
        .optimize = .ReleaseSmall,
    }));
    mod.addIncludePath(b.path("platform/ps/c"));
    // ps7_init.c without silicon revisions 1.0 and 2.0's tables (both
    // boards are 3.x; about 7 KiB of the OCM: tools/ps7-trim.sh), compiled
    // with its own directory on the include path (its ps7_init.h)
    const trim = b.addSystemCommand(&.{"tools/ps7-trim.sh"});
    trim.addFileArg(.{ .cwd_relative = ps7_init_c });
    trim.addFileInput(b.path("tools/ps7-trim.sh"));
    const ps7_trimmed = trim.addOutputFileArg("ps7_init.c");
    const ps7_dir: std.Build.LazyPath = .{ .cwd_relative = std.fs.path.dirname(ps7_init_c) orelse "." };
    mod.addIncludePath(ps7_dir);
    mod.addCSourceFile(.{ .file = ps7_trimmed, .flags = &.{"-w"} });
    // The version mark the menu shows (side track K, the user 2026-10-10):
    // a hash of the app's sources (tools/ps-version.sh; Make passes it), so
    // the same sources make the same ELF.
    const info = b.addOptions();
    info.addOption([]const u8, "ps_version", b.option([]const u8, "ps-version", "the menu's version mark (Make: tools/ps-version.sh)") orelse "dev");
    // side track O: the build's optional features (Make: MOVIES, SCREENSHOTS)
    info.addOption(bool, "movies", b.option(bool, "movies", "input movies and frame advance (side track O; default on)") orelse true);
    info.addOption(bool, "screenshots", b.option(bool, "screenshots", "screenshots (side track O; default on)") orelse true);
    mod.addOptions("build_info", info);
    const elf = b.addExecutable(.{ .name = app, .root_module = mod });
    elf.setLinkerScript(b.path("platform/ps/link.ld"));
    elf.entry = .{ .symbol_name = "_vectors" };

    // ps7_init.c's own assembly, for the alignment check.
    const cc = b.addSystemCommand(&.{ b.graph.zig_exe, "cc", "-target", "arm-freestanding-eabihf", "-mcpu=cortex_a9+strict_align", "-Os", "-w" });
    cc.addPrefixedDirectoryArg("-I", b.path("platform/ps/c"));
    cc.addPrefixedDirectoryArg("-I", ps7_dir);
    cc.addArg("-S");
    cc.addFileArg(ps7_trimmed);
    cc.addArg("-o");
    const init_s = cc.addOutputFileArg("ps7_init.s");

    // The assembly is installed first and checked where it lands; the ELF
    // is installed only if both checks pass (Make removes the old one
    // before it builds, so a failed check leaves no ELF to load or package).
    const install_elf = b.addInstallFileWithDir(elf.getEmittedBin(), .prefix, b.fmt("ps/{s}.elf", .{app}));
    for ([_]struct { std.Build.LazyPath, []const u8 }{
        .{ elf.getEmittedAsm(), b.fmt("ps/{s}.s", .{app}) },
        .{ init_s, "ps/ps7_init.s" },
    }) |s| {
        const asm_file, const dest = s;
        const install = b.addInstallFileWithDir(asm_file, .prefix, dest);
        const check = b.addSystemCommand(&.{ "tools/check-ps-align.sh", b.getInstallPath(.prefix, dest) });
        check.setCwd(b.path("."));
        check.step.dependOn(&install.step);
        install_elf.step.dependOn(&check.step);
    }
    step.dependOn(&install_elf.step);
}

/// Side track Q: the spec libraries' tests and the SDL3 front end. The
/// libraries are loaded at run time (platform/desktop/spec.zig says why),
/// from the paths Make passes; SDL3 is linked as for vga-play.
fn desktop(b: *std.Build, sdl3_libdir: ?[]const u8) void {
    const host = b.graph.host;
    const test_step = b.step("desktop-test", "the spec libraries' tests (needs -Dgb-lib, -Dnes-lib)");
    const exe_step = b.step("desktop", "the desktop emulator (needs -Dgb-lib, -Dnes-lib, -Dsdl3-libdir)");
    const gb_lib = b.option([]const u8, "gb-lib", "the Game Boy spec's foreign library, libgb-desktop.so (make desktop)");
    const nes_lib = b.option([]const u8, "nes-lib", "the NES spec's foreign library, libnes-desktop.so (make desktop)");
    const chip8_lib = b.option([]const u8, "chip8-lib", "CHIP-8's spec's foreign library, libchip8-desktop.so (make desktop)");
    const sms_lib = b.option([]const u8, "sms-lib", "the Game Gear's and Master System's spec's foreign library, libsms-desktop.so (make desktop)");
    const a800_lib = b.option([]const u8, "a800-lib", "the 800XL's foreign library, liba800xl-desktop.so (make desktop)");
    const snes_lib = b.option([]const u8, "snes-lib", "the SNES's foreign library, libsnes-desktop.so (make desktop)");
    if (gb_lib == null or nes_lib == null or chip8_lib == null or sms_lib == null or a800_lib == null or snes_lib == null) {
        test_step.dependOn(&b.addFail("zig build desktop-test: set -Dgb-lib, -Dnes-lib, -Dchip8-lib, -Dsms-lib, -Da800-lib and -Dsnes-lib (make test-desktop does)").step);
        exe_step.dependOn(&b.addFail("zig build desktop: set -Dgb-lib, -Dnes-lib, -Dchip8-lib, -Dsms-lib, -Da800-lib and -Dsnes-lib (make desktop does)").step);
        return;
    }
    const opts = b.addOptions();
    opts.addOption([:0]const u8, "gb_lib", b.allocator.dupeZ(u8, gb_lib.?) catch @panic("OOM"));
    opts.addOption([:0]const u8, "nes_lib", b.allocator.dupeZ(u8, nes_lib.?) catch @panic("OOM"));
    opts.addOption([:0]const u8, "chip8_lib", b.allocator.dupeZ(u8, chip8_lib.?) catch @panic("OOM"));
    opts.addOption([:0]const u8, "sms_lib", b.allocator.dupeZ(u8, sms_lib.?) catch @panic("OOM"));
    opts.addOption([:0]const u8, "a800_lib", b.allocator.dupeZ(u8, a800_lib.?) catch @panic("OOM"));
    opts.addOption([:0]const u8, "snes_lib", b.allocator.dupeZ(u8, snes_lib.?) catch @panic("OOM"));
    // the repo, for the 800XL's OS, BASIC and loader by default (an
    // installed copy, make install-desktop: its own copies of them)
    const desktop_data = b.option([]const u8, "desktop-data", "where the desktop finds roms/a800xl/os/, build/a800xl/xexload.bin, build/snes/ipl.bin (default: the repo)");
    opts.addOption([]const u8, "repo", desktop_data orelse b.build_root.path orelse ".");
    const png = b.createModule(.{ .root_source_file = b.path("platform/sim/png.zig") });
    for ([_][]const u8{ "platform/desktop/gb_spec.zig", "platform/desktop/nes_spec.zig", "platform/desktop/chip8_spec.zig", "platform/desktop/sms_spec.zig", "platform/desktop/a800_spec.zig", "platform/desktop/snes_spec.zig", "platform/desktop/debug.zig", "platform/desktop/z80dis.zig", "platform/desktop/dbg.zig" }) |file| {
        const unit = b.addTest(.{
            .name = std.fs.path.stem(file),
            .root_module = b.createModule(.{
                .root_source_file = b.path(file),
                .target = host,
                .link_libc = true,
                .imports = &.{ .{ .name = "png", .module = png }, .{ .name = "movie", .module = b.createModule(.{ .root_source_file = b.path("platform/ps/movie.zig") }) } },
            }),
        });
        unit.root_module.addOptions("build_options", opts);
        const run = b.addRunArtifact(unit);
        run.setName(b.fmt("test {s}", .{file}));
        run.setCwd(b.path("."));
        run.has_side_effects = true;
        test_step.dependOn(&run.step);
    }
    const save_unit = b.addTest(.{ .name = "desktop_save", .root_module = b.createModule(.{ .root_source_file = b.path("platform/desktop/save.zig"), .target = host }) });
    test_step.dependOn(&b.addRunArtifact(save_unit).step);
    const ui_unit = b.addTest(.{ .name = "desktop_ui", .root_module = b.createModule(.{ .root_source_file = b.path("platform/desktop/ui.zig"), .target = host }) });
    test_step.dependOn(&b.addRunArtifact(ui_unit).step);

    if (sdl3_libdir) |sdl| {
        const exe = b.addExecutable(.{
            .name = "desktop",
            .root_module = b.createModule(.{
                .root_source_file = b.path("platform/desktop/main.zig"),
                .target = host,
                .optimize = .ReleaseSafe,
                .link_libc = true,
                .imports = &.{
                    // the 800XL's keyboard, and through it usb_keys.zig, the key
                    // maps and the system table (the PS files import each other
                    // by file: one module)
                    .{ .name = "ps_keys", .module = b.createModule(.{ .root_source_file = b.path("platform/ps/a800_keys.zig") }) },
                    .{ .name = "png", .module = png },
                },
            }),
        });
        exe.root_module.addOptions("build_options", opts);
        exe.root_module.addLibraryPath(.{ .cwd_relative = sdl });
        exe.root_module.addRPath(.{ .cwd_relative = sdl });
        exe.root_module.linkSystemLibrary("SDL3", .{ .use_pkg_config = .no });
        exe_step.dependOn(&b.addInstallFileWithDir(exe.getEmittedBin(), .prefix, "desktop").step);
        // the debugger in a terminal (side track R): rota-dbg, beside it
        const dbg_exe = b.addExecutable(.{
            .name = "rota-dbg",
            .root_module = b.createModule(.{
                .root_source_file = b.path("platform/desktop/dbg.zig"),
                .target = host,
                .optimize = .ReleaseSafe,
            }),
        });
        exe_step.dependOn(&b.addInstallFileWithDir(dbg_exe.getEmittedBin(), .prefix, "rota-dbg").step);
    } else exe_step.dependOn(&b.addFail("zig build desktop: set -Dsdl3-libdir (make desktop does)").step);
}
