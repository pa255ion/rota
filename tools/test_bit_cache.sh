#!/usr/bin/env bash
# The bitstream cache's tests (side track K; tools/ci-cache.py's bit-*,
# make bit): on a scratch copy of the tree, a fake Clash output and a fake
# `vivado` on PATH (it writes a .bit and build.tcl's OK line, counts its
# runs, and fails when asked), a private cache, queue and progress, checks
#   1. a passing build is cached: the next make bit restores it, says so
#      with its timing, and runs no Vivado;
#   2. what Vivado reads changes the key: the system's XDC, its wrapper,
#      the board's Tcl, the Verilog, a memory image; what it doesn't read
#      doesn't: another system's wrapper, a Haskell comment (the Verilog
#      decides);
#   3. a failing build is not cached;
#   4. CI_CACHE=off builds, and CI_CACHE=write builds and keeps;
#   5. another worktree (another path) sees the cached build.
# Run by make test.
set -uo pipefail
unset CI_CACHE CI_CACHE_LOG CI_CACHE_RUN MAKEFLAGS MAKELEVEL QUEUE_HELD CLAUDECODE QUEUE_WAIT
root=$(cd "$(dirname "$0")/.." && pwd)
tmp=$(mktemp -d "${TMPDIR:-/tmp}/test_bit_cache.XXXXXX")
trap 'rm -rf "$tmp"' EXIT
export CI_CACHE_DIR=$tmp/cache QUEUE_DIR=$tmp/queue PROGRESS_JOB=/nonexistent
fails=0
ok() { echo "test_bit_cache: ok    $*"; }
bad() { echo "test_bit_cache: FAIL  $*"; fails=$((fails + 1)); }
check() { local n=$1; shift; if "$@"; then ok "$n"; else bad "$n"; fi; }

# the fake vivado: build.tcl's last arguments are <topDir> <outDir> ...;
# it runs in outDir (the Makefile's VIVADO cd's there)
mkdir -p "$tmp/bin"
cat > "$tmp/bin/vivado" <<'EOF'
#!/usr/bin/env bash
echo run >> "$FAKE_VIVADO_RUNS"
log=build.log
while [[ $# -gt 0 && $1 != -tclargs ]]; do [[ $1 == -log ]] && log=$2; shift; done
[[ ${FAKE_VIVADO_FAIL:-} == 1 ]] && { echo "build.tcl: FAIL: setup timing not met" > "$log"; exit 1; }
mkdir -p reports && echo "timing" > reports/timing_summary.rpt
echo "bitstream $RANDOM" > top.bit
printf 'build.tcl: WNS = 1.234 ns, WHS = 0.056 ns\nbuild.tcl: OK, wrote top.bit\n' > "$log"
EOF
chmod +x "$tmp/bin/vivado"
export PATH=$tmp/bin:$PATH FAKE_VIVADO_RUNS=$tmp/runs

copy() { # dest: a scratch copy with a fake Clash output for the Arty Game Boy
    mkdir -p "$1"
    ( cd "$root" && git ls-files -co --exclude-standard -z | xargs -0 tar -cf - ) | tar -xf - -C "$1"
    local v=$1/build/arty-z7-20/gb/verilog
    mkdir -p "$v/Gb.ArtyTop.topEntity"
    echo '{"top_component": {"name": "gb_arty_top"}}' > "$v/Gb.ArtyTop.topEntity/clash-manifest.json"
    echo "module gb_arty_top; endmodule" > "$v/Gb.ArtyTop.topEntity/gb_arty_top.v"
    echo "0101" > "$v/Gb.ArtyTop.topEntity/ram.bin"
    echo "# connector" > "$v/clashConnector.tcl"
}
S=$tmp/a/emulation_clash
copy "$S"
mk() { (cd "${W:-$S}" && make -s --no-print-directory "$@" SYSTEM=gb BOARD=arty-z7-20 2>&1); }
key() { mk ci-bit-key | tail -1; }
runs() { [[ -f $FAKE_VIVADO_RUNS ]] && wc -l < "$FAKE_VIVADO_RUNS" || echo 0; }
bitfile=$S/build/arty-z7-20/gb/vivado/top.bit

# 1. pass, cached, restored without Vivado
mk bit > /dev/null
check "1. a passing build ran Vivado once" test "$(runs)" -eq 1
first=$(cat "$bitfile")
rm -f "$bitfile"
out=$(mk bit)
check "1. the next make bit runs no Vivado" test "$(runs)" -eq 1
check "1. it restores the same .bit" test "$(cat "$bitfile" 2>/dev/null)" = "$first"
check "1. and says so, with its timing" grep -q "make bit: cached (.*WNS = 1.234 ns, WHS = 0.056 ns" <<< "$out"

# 2. the key: what Vivado reads
k0=$(key)
changes() { # what file line: the key changes (want=changes) or not (want=same)
    local what=$1 f=$2 line=$3 want=$4 got
    cp "$S/$f" "$tmp/saved"; printf '%s\n' "$line" >> "$S/$f"
    if [[ $(key) == "$k0" ]]; then got=same; else got=changes; fi
    cp "$tmp/saved" "$S/$f"
    check "2. $what: the key $want" test "$got" = "$want"
}
changes "the system's XDC" boards/arty-z7-20/hdmi_tx.xdc "# test" changes
changes "its wrapper" boards/arty-z7-20/gb_zynq.v "// test" changes
changes "the build Tcl" boards/zedboard/build.tcl "# test" changes
changes "the Verilog" build/arty-z7-20/gb/verilog/Gb.ArtyTop.topEntity/gb_arty_top.v "// test" changes
changes "a memory image" build/arty-z7-20/gb/verilog/Gb.ArtyTop.topEntity/ram.bin "1111" changes
changes "another system's wrapper" boards/arty-z7-20/nes_zynq.v "// test" same
changes "a Haskell comment" systems/gb/src/Gb/Apu.hs "-- test" same
check "2. back to the start: the same key" test "$(key)" = "$k0"

# 3. a failing build isn't cached
printf '%s\n' "// failing" >> "$S/build/arty-z7-20/gb/verilog/Gb.ArtyTop.topEntity/gb_arty_top.v"
FAKE_VIVADO_FAIL=1 mk bit > /dev/null
n=$(runs)
mk bit > /dev/null
check "3. a failing build isn't cached (the next one runs Vivado)" test "$(runs)" -eq $((n + 1))

# 4. CI_CACHE=off builds; CI_CACHE=write builds and keeps
n=$(runs)
CI_CACHE=off mk bit > /dev/null
check "4. CI_CACHE=off runs Vivado" test "$(runs)" -eq $((n + 1))
CI_CACHE=write mk bit > /dev/null
check "4. CI_CACHE=write runs Vivado" test "$(runs)" -eq $((n + 2))
mk bit > /dev/null
check "4. ... and keeps it (the next is cached)" test "$(runs)" -eq $((n + 2))

# 5. another worktree sees it
S2=$tmp/b/elsewhere
mkdir -p "$S2" && cp -a "$S/." "$S2/"
rm -rf "$S2/build/arty-z7-20/gb/vivado"
n=$(runs)
W=$S2 mk bit > /dev/null
check "5. another worktree: cached, no Vivado run" test "$(runs)" -eq "$n"
check "5. ... and its .bit is there" test -f "$S2/build/arty-z7-20/gb/vivado/top.bit"

if [[ $fails -gt 0 ]]; then echo "test_bit_cache: $fails failed" >&2; exit 1; fi
echo "test_bit_cache: all passed"
