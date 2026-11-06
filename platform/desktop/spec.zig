//! A system's reference model as a library (side track Q): cabal's
//! foreign libraries (`<sys>-desktop`, lib<sys>-desktop.so) are loaded at
//! run time (dlopen), not linked: GHC's shared objects mark their symbols
//! with version index 0, which Zig's linker and lld both refuse ("version
//! definition index 0 ... out of bounds"), while glibc's loader takes them,
//! as for any GHC program. GHC's runtime is started once per process
//! (`hs_init`, from whichever library is loaded first: they share
//! libHSrts) and never stopped. Everything is called from one thread.

const std = @import("std");

pub const LoadError = error{ SpecLibMissing, SpecLibSymbol };

var hs_started = false;

/// The library at `$<env>` or `path`, with every function `Api` names
/// looked up (a field per function, by its name).
pub fn load(comptime Api: type, path: [:0]const u8, env: [*:0]const u8) LoadError!Api {
    const p: [:0]const u8 = if (std.c.getenv(env)) |e| std.mem.span(e) else path;
    var lib = std.DynLib.openZ(p.ptr) catch {
        std.debug.print("desktop: can't load {s} (make desktop-lib builds it)\n", .{p});
        return error.SpecLibMissing;
    };
    var a: Api = undefined;
    inline for (@typeInfo(Api).@"struct".fields) |f| {
        @field(a, f.name) = lib.lookup(f.type, f.name) orelse {
            std.debug.print("desktop: {s} has no {s}\n", .{ p, f.name });
            return error.SpecLibSymbol;
        };
    }
    if (!hs_started) {
        // hs_init_with_rtsopts, not hs_init: GHCRTS (the runtime's options,
        // e.g. its garbage collector's) is read only with every option
        // enabled
        const hs_init = lib.lookup(*const fn (argc: *c_int, argv: *[*:null]?[*:0]const u8) callconv(.c) void, "hs_init_with_rtsopts") orelse {
            std.debug.print("desktop: {s} has no hs_init_with_rtsopts\n", .{p});
            return error.SpecLibSymbol;
        };
        const S = struct {
            var argv = [_:null]?[*:0]const u8{"desktop"};
            var argc: c_int = 1;
            var ptr: [*:null]?[*:0]const u8 = &argv;
        };
        hs_init(&S.argc, &S.ptr);
        hs_started = true;
    }
    return a;
}

/// Save states (Q.8): the three calls every library has
/// (`<sys>_state_save` takes one and says its size, `_get` copies it out,
/// `_load` puts one back: 0, or -1 when it doesn't fit), as a wrapper's
/// `stateSave` / `stateLoad`.
pub fn stateSave(gpa: std.mem.Allocator, h: ?*anyopaque, save: *const fn (?*anyopaque) callconv(.c) usize, get: *const fn (?*anyopaque, [*]u8, usize) callconv(.c) void) ![]u8 {
    const n = save(h);
    if (n == 0) return error.NoStates;
    const buf = try gpa.alloc(u8, n);
    get(h, buf.ptr, buf.len);
    return buf;
}

/// For the wrappers' tests: takes a state of `m`, runs it on `n` frames
/// through `step` (a frame with frame k's input; its picture and sound's
/// hash), then loads the state into `fresh` (the same game, just made:
/// power-on state everywhere) and runs that: the same hashes (a part of
/// the machine the state leaves out would show).
pub fn expectStateReplays(m: anytype, fresh: @TypeOf(m), n: usize, step: fn (@TypeOf(m), usize) u64) !void {
    const testing = std.testing;
    const blob = try m.stateSave(testing.allocator);
    defer testing.allocator.free(blob);
    var first: [240]u64 = undefined;
    std.debug.assert(n <= first.len);
    for (0..n) |k| first[k] = step(m, k);
    try testing.expect(fresh.stateLoad(blob));
    for (0..n) |k| try testing.expectEqual(first[k], step(fresh, k));
}
