#!/usr/bin/env bash
# Run the Haskell test suites in parallel (make test).
#
#   tools/run-tests.sh [TARGET...]     TARGET: a package name, or "all"
#
# `cabal test` runs suites one after another, and most of them don't keep
# 12 cores busy. This builds everything once, then runs the suite binaries
# as separate processes, TEST_JOBS at a time (default: cores / 4), each
# with an equal share of the cores (+RTS -N), in its package directory
# (where cabal would run it), longest first. Each suite's output goes to
# build/test-logs/<suite>.log; a failing suite's log is printed at the end.
# Environment variables (EMU_FULL_TESTS, NES_CORE_ROMS) pass through.
#
# "Longest first" is measured: every passing suite's time is kept per tier
# in $TEST_TIMES_DIR (default ~/.cache/emulation_clash, shared by all
# worktrees, so a new worktree starts in a good order). A suite with no
# time yet (a new one) runs first, since it may be long: started last, a
# long suite runs alone at the end (a800xl-lockstep did, 8 minutes).
set -uo pipefail

cd "$(dirname "$0")/.."
root=$PWD

# Progress for make watch: run as one job (unless make ci's job holds us).
if [[ -z ${PROGRESS_JOB:-} && -z ${RUN_TESTS_NO_PROGRESS:-} ]]; then
    RUN_TESTS_NO_PROGRESS=1 exec "$root/tools/progress.sh" run test suites -- "$root/tools/run-tests.sh" "$@"
fi

CABAL=${CABAL:-cabal}
targets=("$@")
[[ ${#targets[@]} -eq 0 ]] && targets=(all)

cores=$(nproc)
jobs=${TEST_JOBS:-$(( cores / 4 > 0 ? cores / 4 : 1 ))}
per=$(( cores / jobs > 0 ? cores / jobs : 1 ))
logs=build/test-logs
mkdir -p "$logs"

# Suites of a package: "dir suite" pairs from the .cabal files.
suites_of() { # cabal file
    awk -v d="$(dirname "$1")" '/^test-suite /{print d, $2}' "$1"
}
cabals=(platform/platform.cabal cpus/*/*.cabal systems/*/*.cabal demos/*/*.cabal)
list=()
for c in "${cabals[@]}"; do
    pkg=$(basename "$c" .cabal)
    want=0
    for t in "${targets[@]}"; do [[ $t == all || $t == "$pkg" ]] && want=1; done
    [[ $want == 1 ]] || continue
    while read -r dir suite; do list+=("$dir $suite"); done < <(suites_of "$c")
done
if [[ ${#list[@]} -eq 0 ]]; then
    echo "run-tests: no test suites for: ${targets[*]}" >&2
    exit 1
fi

# Longest first: unmeasured suites, then by the last measured time; ties
# (and a missing times file) keep this order (quick tier, 2026-10-03;
# a800xl-lockstep, about 9 minutes, added 2026-10-07).
order=(gb-lockstep a800xl-lockstep chip8-golden nes-lockstep gb-golden chip8-lockstep nes-spec
       a800xl-spec platform-unit m0b-unit gb-spec a800xl-golden m0a-unit m0d-unit gb-unit
       mos6502-lockstep m0c-unit mos6502-spec)
tier=$([[ ${EMU_FULL_TESTS:-} == 1 ]] && echo full || echo quick)
[[ -n ${NES_CORE_ROMS:-} ]] && tier=$tier-nes-${NES_CORE_ROMS}
times_dir=${TEST_TIMES_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/emulation_clash}
times=$times_dir/test-times-$tier
declare -A secs
if [[ -r $times ]]; then
    while read -r s t; do [[ $t =~ ^[0-9]+$ ]] && secs[$s]=$t; done < "$times"
fi
rank() { # suite -> its index in the old order list (99 if absent)
    local i
    for i in "${!order[@]}"; do [[ ${order[$i]} == "$1" ]] && { echo "$i"; return; }; done
    echo 99
}
sorted=()
while read -r _ _ e; do sorted+=("${e/:/ }"); done < <(
    for e in "${list[@]}"; do
        s=${e#* }
        printf '%d %02d %s\n' "${secs[$s]:-999999}" "$(rank "$s")" "${e% *}:$s"
    done | sort -k1,1nr -k2,2n)

echo "run-tests: building ${targets[*]}"
$CABAL build "${targets[@]}" || exit 1

declare -A bins
for e in "${sorted[@]}"; do
    s=${e#* }
    bins[$s]=$($CABAL list-bin -v0 "$s" | tail -1) || { echo "run-tests: no binary for $s" >&2; exit 1; }
done

echo "run-tests: ${#sorted[@]} suites, $jobs at a time, -N$per each"
"$root/tools/progress.sh" plan suite "${sorted[@]##* }"
declare -A pids
failed=()
start=$SECONDS
# make ci (CI_CACHE_RUN=1): a suite whose inputs have a cached pass is
# skipped (tools/ci-cache.py; side track K, D3); make test alone runs all.
cache=$([[ ${CI_CACHE_RUN:-} == 1 && ${CI_CACHE:-on} != off ]] && echo 1)
run_one() { # dir suite
    local dir=$1 s=$2 t0=$SECONDS key=
    if [[ -n $cache ]]; then
        key=$("$root/tools/ci-cache.py" suite-key "$dir" "$s") || key=
        if [[ -n $key ]] && "$root/tools/ci-cache.py" check "suite:$s:$tier" "$key" 2> "$logs/$s.log"; then
            printf 'run-tests: %-18s cached  %s\n' "$s" "$(sed 's/^ci-cache: [^:]*:[^:]*:[^:]*: //' "$logs/$s.log")"
            "$root/tools/progress.sh" cached suite "$s"   # make watch: done, no run
            return 0
        fi
    fi
    ( cd "$dir" && "$root/tools/progress.sh" run suite "$s" -- "${bins[$s]}" +RTS -N"$per" -RTS ) > "$logs/$s.log" 2>&1
    local rc=$?
    if [[ -n $cache && -n $key ]]; then
        [[ $rc -eq 0 ]] && "$root/tools/ci-cache.py" pass "suite:$s:$tier" "$key" $(( SECONDS - t0 ))
        [[ -n ${CI_CACHE_LOG:-} ]] && echo "run suite:$s" >> "$CI_CACHE_LOG"
    fi
    local summary
    summary=$(grep -E "tests passed|out of [0-9]+ tests failed" "$logs/$s.log" | tail -1)
    printf 'run-tests: %-18s %4ds  %s\n' "$s" $(( SECONDS - t0 )) "${summary:-exit $rc}"
    [[ $rc -eq 0 ]] && echo "$s $(( SECONDS - t0 ))" > "$logs/$s.time"
    return $rc
}
running=0
for e in "${sorted[@]}"; do
    dir=${e% *}; s=${e#* }
    if (( running >= jobs )); then
        wait -n -p done_pid; rc=$?
        [[ $rc -ne 0 ]] && failed+=("${pids[$done_pid]}")
        running=$(( running - 1 ))
    fi
    run_one "$dir" "$s" &
    pids[$!]=$s
    running=$(( running + 1 ))
done
while (( running > 0 )); do
    wait -n -p done_pid; rc=$?
    [[ $rc -ne 0 ]] && failed+=("${pids[$done_pid]}")
    running=$(( running - 1 ))
done

# Record this run's times (passing suites only) for the next run's order.
if mkdir -p "$times_dir" 2>/dev/null; then
    for e in "${sorted[@]}"; do
        f=$logs/${e#* }.time
        [[ -f $f ]] && read -r s t < "$f" && secs[$s]=$t && rm -f "$f"
    done
    tmp=$(mktemp "$times.XXXXXX") && {
        for s in "${!secs[@]}"; do echo "$s ${secs[$s]}"; done | sort > "$tmp"
        mv "$tmp" "$times"
    }
fi

if [[ ${#failed[@]} -gt 0 ]]; then
    for s in "${failed[@]}"; do
        echo "=== $s: $logs/$s.log (last 60 lines)"
        tail -60 "$logs/$s.log"
    done
    echo "run-tests: FAILED: ${failed[*]} ($(( SECONDS - start ))s)" >&2
    exit 1
fi
echo "run-tests: all ${#sorted[@]} suites passed ($(( SECONDS - start ))s)"
