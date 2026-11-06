#!/usr/bin/env bash
# The result cache's gate (side track K, D3; PLAN.md, "D3"): on a scratch
# copy of the tree, with a cache of its own, checks that
#   1. a change in systems/nes/ runs only the NES suites and NES chains;
#   2. a change in platform/src runs every chain whose Verilog changed,
#      and no other;
#   3. a change to pl.zig's encoding runs every chain with a PS script; a
#      comment in pl.zig runs none;
#   4. with the encoded PS scripts left out of the key, check 3 fails (the
#      test catches its own mutation);
#   5. a failing run is not cached;
#   6. a pass in one worktree is seen by another.
# No Clash or Verilator runs: a Clash run's result is seeded (hdl-seed) as
# the Verilog hash it would give, so "the Verilog changed" is a choice of
# the test. Chains: arty-m0 (no PS script), arty-chip8, arty-gb, arty-nes,
# and demos (re-simulates the Arty chip8, gb, nes and gg models it doesn't
# build: keyed on their Verilog all the same).
# Run by make test (inside nix develop).
set -uo pipefail
unset CI_CACHE CI_CACHE_LOG CI_CACHE_RUN CI_CACHE_MUTATION MAKEFLAGS MAKELEVEL
root=$(cd "$(dirname "$0")/.." && pwd)
tmp=$(mktemp -d "${TMPDIR:-/tmp}/test_ci_cache.XXXXXX")
trap 'rm -rf "$tmp"' EXIT
export CI_CACHE_DIR=$tmp/cache
export CI_CACHE_LIST_BIN_FILE=$tmp/generator.bin
echo "a generator" > "$CI_CACHE_LIST_BIN_FILE"
fail=0
ok() { echo "test_ci_cache: ok: $*"; }
bad() { echo "test_ci_cache: FAIL: $*"; fail=1; }

# the scratch copy: tracked and untracked files, no build/ or roms/
S=$tmp/a/emulation_clash
mkdir -p "$S"
( cd "$root" && git ls-files -co --exclude-standard -z | xargs -0 tar -cf - ) | tar -xf - -C "$S"
cc() { (cd "${W:-$S}" && python3 tools/ci-cache.py "$@"); }
encoder() { (cd "${W:-$S}" && zig build --cache-dir "$root/build/zig-cache" ps-encode --prefix build) || bad "zig build ps-encode"; }
encoder

chains=(arty-m0 arty-chip8 arty-gb arty-nes demos)
# every system with an Arty top is "built" at the baseline: the demos chain
# re-simulates several of them, and a new system's splash adds one (M6's
# Master System failed this test while the list was fixed, 2026-10-12)
tops=() systems=()
for f in "$S"/systems/*/arty-z7-20.mk; do s=${f%/arty-z7-20.mk}; tops+=("${s##*/} arty-z7-20"); systems+=("${s##*/}"); done
hdlkey() { (cd "${W:-$S}" && make -s --no-print-directory ci-hdl-key SYSTEM="$1" BOARD="$2"); }
seed() { # system board verilog-hash: the Clash run's result for today's key
    cc hdl-seed "$(hdlkey "$1" "$2")" "$3"
}
seed_extra() { # the Clash runs a chain builds in a build dir of its own
    # (ci-arty-gb's gb-nofeatures, side track O): "v1-<dir's name>"
    local c d k
    for c in "${chains[@]}"; do
        cc chain-hdl-keys "$c" | while read -r d k; do
            [[ -n $k && " ${systems[*]} " != *" ${d##*/} "* ]] && cc hdl-seed "$k" "v1-${d##*/}"
        done
    done
}
plan() { # chain -> cached | run
    local k
    if k=$(cc chain-key "$1" --plan) && cc check "chain:$1" "$k" 2>/dev/null; then echo cached; else echo run; fi
}
expect() { # check-name chain=state...
    local name=$1 e c want got all=1; shift
    for e in "$@"; do
        c=${e%=*}; want=${e#*=}; got=$(plan "$c")
        [[ $got == "$want" ]] || { bad "$name: $c: $got, expected $want"; all=0; }
    done
    [[ $all == 1 ]] && ok "$name"
    return $(( 1 - all ))
}
record_all() { local c k; for c in "${chains[@]}"; do k=$(cc chain-key "$c" --plan) && cc pass "chain:$c" "$k" 1; done; }
suites() { # "suite package key" for every suite
    local f d s
    for f in "$S"/platform/*.cabal "$S"/cpus/*/*.cabal "$S"/systems/*/*.cabal "$S"/demos/*/*.cabal; do
        [[ -f $f ]] || continue
        d=$(dirname "${f#"$S"/}")
        for s in $(awk '/^test-suite /{print $2}' "$f"); do echo "$s $(basename "$f" .cabal) $(cc suite-key "$d" "$s")"; done
    done
}
edit() { cp "$S/$1" "$tmp/saved"; printf '%s\n' "$2" >> "$S/$1"; }
undo() { cp "$tmp/saved" "$S/$1"; }

# the baseline: every top's Verilog "built" (v1), every chain passed
for t in "${tops[@]}"; do set -- $t; seed "$1" "$2" "v1-$1"; done
seed_extra
record_all
expect "baseline: every chain cached" arty-m0=cached arty-chip8=cached arty-gb=cached arty-nes=cached demos=cached
suites > "$tmp/suites.0"

# 1. a change in systems/nes/
nesfile=$(cd "$S" && ls systems/nes/src/Nes/*.hs | head -1)
edit "$nesfile" "-- test_ci_cache: a change in the NES"
expect "1. systems/nes/: only the NES chains run (and the demos', which re-simulate the NES)" arty-m0=cached arty-chip8=cached arty-gb=cached arty-nes=run demos=run
suites > "$tmp/suites.1"
changed=$(diff "$tmp/suites.0" "$tmp/suites.1" | awk '/^>/{print $2}' | sort | tr '\n' ' ')
# the suites of the packages that depend on the NES's: its own and the demos'
want=$(awk '$2 == "nes" || $2 == "rota-brand" {print $1}' "$tmp/suites.0" | sort | tr '\n' ' ')
if [[ $changed == "$want" ]]; then ok "1. systems/nes/: only the NES suites (and the demos', which use the NES) run: $changed"
else bad "1. suites run: '$changed', expected '$want'"; fi
undo "$nesfile"

# 2. a change in platform/src: every Clash key changes; the chains whose
# Verilog came out the same stay cached
pfile=$(cd "$S" && ls platform/src/Platform/Ps/*.hs | head -1)
edit "$pfile" "-- test_ci_cache: a change in the platform"
expect "2a. platform/src, Clash not run yet: every chain runs" arty-m0=run arty-chip8=run arty-gb=run arty-nes=run demos=run
seed m0a arty-z7-20 v1-m0a; seed m0b arty-z7-20 v1-m0b; seed gb arty-z7-20 v1-gb; seed gg arty-z7-20 v1-gg
seed chip8 arty-z7-20 v2-chip8; seed nes arty-z7-20 v1-nes; seed_extra
expect "2b. platform/src: only the chains whose Verilog changed run" arty-m0=cached arty-gb=cached arty-nes=cached arty-chip8=run demos=run
undo "$pfile"

# 3. pl.zig: its encoding, then a comment
check3() { # -> 0 if check 3 holds
    local r=0
    cp "$S/platform/ps/pl.zig" "$tmp/pl.zig"
    sed -i '0,/delayUs(2)/s//delayUs(3)/' "$S/platform/ps/pl.zig"
    encoder
    expect "3a. pl.zig's encoding: every chain with a PS script runs" arty-m0=cached arty-chip8=run arty-gb=run arty-nes=run demos=run || r=1
    cp "$tmp/pl.zig" "$S/platform/ps/pl.zig"
    printf '%s\n' "// test_ci_cache: a comment" >> "$S/platform/ps/pl.zig"
    encoder
    expect "3b. a comment in pl.zig: nothing runs" arty-m0=cached arty-chip8=cached arty-gb=cached arty-nes=cached demos=cached || r=1
    cp "$tmp/pl.zig" "$S/platform/ps/pl.zig"
    encoder
    return $r
}
check3

# 3c. the PS app (relay.zig, link.ld): not an input of layer 4 or of a suite
suites > "$tmp/suites.3"
edit platform/ps/relay.zig "// test_ci_cache: a change in the PS app"
expect "3c. the PS app: no chain runs" arty-m0=cached arty-chip8=cached arty-gb=cached arty-nes=cached demos=cached
suites > "$tmp/suites.3c"
cmp -s "$tmp/suites.3" "$tmp/suites.3c" && ok "3c. the PS app: no suite runs" || bad "3c. a PS app change reruns suites"
undo platform/ps/relay.zig

# 4. the mutation: without the encoded scripts in the key, check 3 must fail
(
    export CI_CACHE_MUTATION=no-ps-encoding
    record_all
    f0=$fail; fail=0
    check3 > "$tmp/mutation.log" 2>&1
    rc=$?
    if [[ $rc -ne 0 ]]; then echo "test_ci_cache: ok: 4. check 3 fails with the PS scripts left out of the key"
    else echo "test_ci_cache: FAIL: 4. check 3 still passes with the PS scripts left out of the key"; cat "$tmp/mutation.log"; exit 1; fi
) || fail=1

# 5. a failing run is not cached; a passing one is
cc run test:fail k5 -- false 2>/dev/null
if cc check test:fail k5 2>/dev/null; then bad "5. a failing run was cached"; else ok "5. a failing run is not cached"; fi
cc run test:pass k6 -- true 2>/dev/null
cc check test:pass k6 2>/dev/null && ok "5. a passing run is" || bad "5. a passing run wasn't cached"

# 6. another worktree (another path) sees the passes
S2=$tmp/b/elsewhere
mkdir -p "$S2"
cp -a "$S/." "$S2/"
cc pass "suite:gb-spec:quick" "$(cc suite-key systems/gb gb-spec)" 1
(
    W=$S2
    k=$(cc suite-key systems/gb gb-spec)
    cc check "suite:gb-spec:quick" "$k" 2>/dev/null || { echo "test_ci_cache: FAIL: 6. a suite's pass not seen from $S2"; exit 1; }
    for c in "${chains[@]}"; do
        k=$(cc chain-key "$c" --plan) && cc check "chain:$c" "$k" 2>/dev/null ||
            { echo "test_ci_cache: FAIL: 6. ci-$c's pass not seen from $S2"; exit 1; }
    done
    echo "test_ci_cache: ok: 6. another worktree sees the passes (a suite, every chain)"
) || fail=1

if [[ $fail -ne 0 ]]; then echo "test_ci_cache: FAIL"; exit 1; fi
echo "test_ci_cache: OK"
