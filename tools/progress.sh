#!/usr/bin/env bash
# Progress events for make ci, make hdl, make sim and make bit, read by
# make watch (tools/progress-watch.py).
#
#   tools/progress.sh run TYPE KEY [DETAIL] -- COMMAND...
#   tools/progress.sh plan TYPE NAME...
#   tools/progress.sh serial TYPE NAME...
#   tools/progress.sh cached TYPE KEY
#
# `run` runs COMMAND and logs its start and end (and duration) as events
# of the current job; `plan` lists the items of a TYPE the job will run
# (suites, chains), so the watcher knows the whole; `serial` names the
# planned ones that run one at a time after the others (make ci's
# CI_AFTER chains, after the CI_JOBS pool); `cached` says an item
# was served from make ci's result cache (tools/ci-cache.py: a suite that
# then doesn't run, a chain whose `run` then takes seconds): the watcher
# counts it done, and its time never goes to the history. TYPE: ci, suite,
# chain, clash, sim, bit. DETAIL: for sim "FRAMES_DIR FRAMES" (the
# watcher counts the frames written), for bit Vivado's log (its phase).
#
# A job is a directory in $PROGRESS_DIR/jobs/ (its pid, worktree, branch,
# command, and an append-only `events` file). The outermost `run` creates
# it, exports PROGRESS_JOB to its command and removes it when done; inner
# runs (a suite of make ci, a Clash run of make sim) add to it. Each
# successful run's duration goes to $PROGRESS_DIR/history (TYPE:KEY SECS...:
# the last $HISTORY_RUNS runs' times, oldest first, so a reader that takes
# the last word gets the latest; the watcher takes their median, since
# 2026-11-01), the estimates for the next run, per tier: the quick tier's as TYPE:KEY,
# others as TYPE:KEY@TIER (TIER: full when FULL=1 or EMU_FULL_TESTS=1,
# plus -nes-<roms> with NES_CORE_ROMS, as tools/run-tests.sh names them,
# plus -all with ALL=1, make ci's both-boards run),
# since a full run's suites take several times as long. Without a writable PROGRESS_DIR the
# command just runs.
set -uo pipefail

PROGRESS_DIR=${PROGRESS_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/emulation_clash/progress}
HISTORY_RUNS=5   # times kept per key (tools/progress-watch.py: their median)

ev() { # words...
    [[ -n ${PROGRESS_JOB:-} && -d $PROGRESS_JOB ]] || return 0
    echo "$(date +%s) $*" >> "$PROGRESS_JOB/events" 2>/dev/null || true
}

# The test tier of this run, as tools/run-tests.sh names it ("" = quick).
tier() {
    local t=""
    [[ ${EMU_FULL_TESTS:-${FULL:-}} == 1 ]] && t=full
    [[ -n ${NES_CORE_ROMS:-} ]] && t=${t:-quick}-nes-$NES_CORE_ROMS
    # make ci ALL=1 (both boards' chains): its own estimates too
    [[ ${ALL:-} == 1 ]] && t=${t:+$t-}all
    echo "$t"
}

history_set() { # type:key secs (tier added here): appended to its last runs
    local h=$PROGRESS_DIR/history tmp
    tmp=$(mktemp "$h.XXXXXX" 2>/dev/null) || return 0
    local k=$1 t
    t=$(tier); [[ -n $t ]] && k=$k@$t
    { if [[ -f $h ]]; then
          awk -v k="$k" -v s="$2" -v n="$HISTORY_RUNS" '
              $1 == k { if (!found) { v = ""; i = NF - n + 2; if (i < 2) i = 2
                                      for (; i <= NF; i++) v = v " " $i
                                      print k v " " s }
                        found = 1; next }
              { print }
              END { if (!found) print k " " s }' "$h"
      else
          echo "$k $2"
      fi; } | sort > "$tmp" && mv -f "$tmp" "$h"
}

started() { # pid: start time, as tools/queue.sh
    local s
    s=$(cat "/proc/$1/stat" 2>/dev/null) || return 1
    s=${s##*) }
    set -- $s
    echo "${20}"
}

run() {
    local type=$1 key=$2 detail=""
    shift 2
    if [[ ${1:-} != -- ]]; then detail=$1; shift; fi
    [[ ${1:-} == -- ]] && shift
    [[ $# -gt 0 ]] || { echo "progress: run: no command" >&2; exit 2; }
    # Under make -n (a dry run still runs the lines that call $(MAKE), so
    # these wrappers run): no job, no events, no history (a dry run's
    # 0-second "chains" once replaced every measured time).
    local f=${MAKEFLAGS:-}
    f=${f%% *}
    if [[ -n $f && $f != -* && $f == *n* ]]; then "$@"; exit $?; fi

    local own=""
    if [[ -z ${PROGRESS_JOB:-} ]] && mkdir -p "$PROGRESS_DIR/jobs" 2>/dev/null; then
        own=$PROGRESS_DIR/jobs/$(date +%s%N)-$$
        mkdir -p "$own"
        {
            echo "pid=$$"
            echo "start=$(started $$)"
            echo "kind=$type"
            echo "tier=$(tier)"
            echo "key=$key"
            echo "since=$(date +%s)"
            echo "worktree=$(basename "$(git rev-parse --show-toplevel 2>/dev/null || pwd)")"
            # PROGRESS_BRANCH: make ci-remote's (the worker's copy has no history)
            echo "branch=${PROGRESS_BRANCH:-$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "?")}"
            echo "cmd=$(printf '%s ' "$@" | tr '\n' ' ' | cut -c1-200)"
        } > "$own/meta"
        : > "$own/events"
        export PROGRESS_JOB=$own
        trap 'rm -rf "$own"' EXIT
        trap 'exit 130' INT TERM HUP
    fi

    local t0=$SECONDS rc
    ev start "$type" "$key" "$detail"
    "$@"; rc=$?
    local secs=$(( SECONDS - t0 ))
    if [[ $rc -eq 0 ]]; then
        ev done "$type" "$key" "$secs"
        # a cached pass's seconds are not a run's time
        if ! grep -q "^[0-9]* cached $type $key\$" "${PROGRESS_JOB:-/nonexistent}/events" 2>/dev/null; then
            history_set "$type:$key" "$secs"
        fi
    else
        ev fail "$type" "$key" "$secs"
    fi
    exit $rc
}

case ${1:-} in
    run)  shift; [[ $# -ge 2 ]] || { echo "usage: tools/progress.sh run TYPE KEY [DETAIL] -- COMMAND..." >&2; exit 2; }; run "$@" ;;
    plan) shift; t=${1:-}; shift; ev plan "$t" "$@" ;;
    serial) shift; t=${1:-}; shift; ev serial "$t" "$@" ;;
    cached) shift; [[ $# -eq 2 ]] || { echo "usage: tools/progress.sh cached TYPE KEY" >&2; exit 2; }; ev cached "$1" "$2" ;;
    *)    echo "usage: tools/progress.sh run TYPE KEY [DETAIL] -- COMMAND... | plan TYPE NAME... | serial TYPE NAME... | cached TYPE KEY" >&2; exit 2 ;;
esac
