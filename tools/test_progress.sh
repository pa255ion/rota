#!/usr/bin/env bash
# Tests for tools/progress.sh and tools/progress-watch.py (make test runs
# them): a make ci shaped job (steps, suites, chains, a simulation writing
# frames, a Clash run) seen mid-run by the watcher, exit codes, the
# history, and a finished or dead job's removal. A private PROGRESS_DIR
# and QUEUE_DIR, so the machine's real ones are never touched.
set -uo pipefail
cd "$(dirname "$0")/.."
# run inside make ci (or another queued, tracked job): not part of it
unset QUEUE_HELD PROGRESS_JOB
# and not in the tier of a make test FULL=1 / NES_CORE_ROMS=… that runs us
unset FULL EMU_FULL_TESTS NES_CORE_ROMS ALL
# this machine's section only (the worker's: its own checks below, faked)
export WATCH_REMOTE= WATCH_LABEL=desktop
# the pools' sizes the estimates assume (make ci's CI_JOBS default)
export CI_JOBS=3 TEST_JOBS=2

export PROGRESS_DIR QUEUE_DIR
PROGRESS_DIR=$(mktemp -d); QUEUE_DIR=$(mktemp -d)
trap 'rm -rf "$PROGRESS_DIR" "$QUEUE_DIR"' EXIT
p=$PWD/tools/progress.sh
w=tools/progress-watch.py
fails=0
check() { # name, condition...
    local name=$1; shift
    if "$@"; then echo "test_progress: ok    $name"; else echo "test_progress: FAIL  $name"; fails=$((fails + 1)); fi
}

# history from "an earlier run": known times for the steps and items
printf '%s\n' "step:prep 10" "step:tests 10" "step:layer4 20" "suite:a 4" "suite:b 4" \
    "chain:x 10" "chain:y 10" "clash:zedboard:Top 4" "sim:zedboard/top 4" > "$PROGRESS_DIR/history"

frames=$PROGRESS_DIR/frames
# a make ci shaped job: prep done, tests done, layer 4 half through
fake_ci() {
    $p plan step prep tests layer4
    $p plan chain x y
    $p run step prep -- true
    $p run step tests -- bash -c "$p plan suite a b; $p run suite a -- true; $p run suite b -- true"
    $p run step layer4 -- bash -c "
        $p run chain x -- true
        $p run chain y -- bash -c '
            $p run clash zedboard:Top -- sleep 0.5
            mkdir -p $frames
            $p run sim zedboard/top \"$frames 10\" -- bash -c \"for i in 1 2 3 4; do touch $frames/frame_000\\\$i.png; done; sleep 3\"'"
}
$p run ci ci -- bash -c "$(declare -f fake_ci); p=$p; frames=$frames; fake_ci" &
job=$!
sleep 2.2
out=$($w --once)
wait $job; rc=$?

check "job ran green" test $rc -eq 0
check "header: ci and the worktree" grep -q "^ci    $(basename "$PWD") (" <<< "$out"
check "counts: suites 2/2, chains 1/2" grep -q "suites 2/2 · chains 1/2" <<< "$out"
check "ETA shown" grep -q "ETA ~" <<< "$out"
check "the running chain" grep -q "chain ci-y" <<< "$out"
check "the simulation's frames 4/10" grep -q "sim   zedboard/top .* frame 4/10" <<< "$out"
check "finished job removed" grep -q "^no make ci" <<< "$($w --once)"
check "history: the chain's time" grep -q "^chain:y [0-9]" "$PROGRESS_DIR/history"
check "history keeps others" grep -q "^suite:a " "$PROGRESS_DIR/history"

# exit codes pass through; a failed item is listed while the job runs
$p run bit zedboard:s /nonexistent/build.log -- sh -c 'exit 4'
check "exit 4" test $? -eq 4
$p run ci ci -- bash -c "$p run chain z -- false; sleep 1.5" &
sleep 0.8
check "failed item shown" grep -q "FAILED: chain z" <<< "$($w --once)"
wait

# the estimate, from fixed events (this shell plays the job's process):
# prep and tests done, layer 4 running with chain x done and chain y 5 of
# its 10 s in: 5 s to go (the ETA), 100 s elapsed, so 100 / 105 = 95 %
# (a delay of d s: (100 + d) / 105).
# The events are stamped before the watcher starts, so a slow start
# (under make ci's load: 0:03 once failed the post-merge run, 2026-10-12)
# shortens the ETA by the delay; 0:02 to 0:05 allows three seconds of it
fake_job() { # dir kind since-ago [tier]: a job directory this shell plays
    mkdir -p "$1"
    printf 'pid=%s\nstart=%s\nkind=%s\nkey=%s\ntier=%s\nsince=%s\nworktree=w\nbranch=b\n' \
        $$ "$(cut -d' ' -f22 /proc/$$/stat)" "$2" "$2" "${4:-}" "$(( $(date +%s) - $3 ))" > "$1/meta"
}
printf '%s\n' "step:prep 10" "step:tests 10" "step:layer4 20" "chain:x 10" "chain:y 10" > "$PROGRESS_DIR/history"
j=$PROGRESS_DIR/jobs/2-$$; fake_job "$j" ci 100
n=$(date +%s)
printf '%s\n' "$((n-100)) plan step prep tests layer4" "$((n-100)) plan chain x y" "$((n-100)) start ci ci" \
    "$((n-100)) start step prep" "$((n-90)) done step prep 10" "$((n-90)) start step tests" \
    "$((n-80)) done step tests 10" "$((n-80)) start step layer4" "$((n-80)) start chain x" \
    "$((n-70)) done chain x 10" "$((n-5)) start chain y" > "$j/events"
out=$($w --once)
check "estimate: 95 to 98 %, ETA ~0:02 to 0:05 first" grep -Eq "^ +█+░+ +9[5-8]%  ETA ~0:0[2-5]  chains 1/2  1:4[0-3] elapsed$" <<< "$out"
rm -rf "$j"

# side Q's case (2026-11-01): a chain 37 minutes into its 4 s estimate,
# 8 of 17 chains not started, suites unstarted: never 99 %, the running
# chain still has time to go; 80 columns keep the ETA (and every line
# fits); the medians of the last runs, cached passes left out
# (each c: the median of 300 1000 400 500 450, the 2 s cached passes
# left out, so 450 s; late: 9 s, so 37:25 in it still has a tenth of
# 9 + 2245 s, 225 s, to go; 3 chains at a time: 1350 s, ETA ~22:30)
hist=("step:prep 100" "step:tests 600" "step:layer4 1200" "chain:late 9")
for i in 1 2 3 4 5 6 7 8; do hist+=("chain:c$i 300 1000 400 2 2 500 450"); done
printf '%s\n' "${hist[@]}" > "$PROGRESS_DIR/history"
j=$PROGRESS_DIR/jobs/4-$$; fake_job "$j" ci 11000
n=$(date +%s)
printf '%s\n' "$((n-11000)) plan step prep tests layer4" "$((n-11000)) plan chain late c1 c2 c3 c4 c5 c6 c7 c8" \
    "$((n-11000)) start ci ci" "$((n-11000)) start step prep" "$((n-10900)) done step prep 100" \
    "$((n-10900)) start step tests" "$((n-3000)) done step tests 7900" "$((n-3000)) start step layer4" \
    "$((n-2245)) start chain late" > "$j/events"
out=$($w --once --width 80)
check "overrun: below 99 %" grep -Eq "^ +█*░+ +([0-8]?[0-9]|9[0-7])%  ETA" <<< "$out"
check "overrun: medians, the pool: ETA ~22:30" grep -Eq "  ETA ~22:[23][0-9]  chains 0/9" <<< "$out"
check "overrun: its estimate from its runs" grep -Eq "chain ci-late .* 37:2[5-9] of ~0:09$" <<< "$out"
check "width 80: every line fits" python3 -c 'import sys; sys.exit(max(len(l) for l in sys.stdin.read().splitlines()) > 80)' <<< "$out"
long=$(printf 'pid=%s\nstart=%s\nkind=ci\nkey=ci\nsince=%s\nworktree=a-very-long-worktree-name-here\nbranch=and-a-long-branch-name-too\n' \
    $$ "$(cut -d' ' -f22 /proc/$$/stat)" "$(( n - 11000 ))")
echo "$long" > "$j/meta"
out=$($w --once --width 60)
check "width 60: the ETA kept" grep -Eq "^ +█*░* *[0-9]+%  ETA ~22:[23][0-9]" <<< "$out"
check "width 60: every line fits" python3 -c 'import sys; sys.exit(max(len(l) for l in sys.stdin.read().splitlines()) > 60)' <<< "$out"
rm -rf "$j"

# cached items: done, out of the estimate, counted; ALL=1 falls back to
# the plain run's times; the CI_AFTER chains run after the pool, one at
# a time: pool (3 at a time) of a b c d at 100 s each = 200 s, then e and
# f, 100 s each = 400 s
printf '%s\n' "chain:a 100" "chain:b 100" "chain:c 100" "chain:d 100" "chain:e 100" "chain:f 100" \
    "chain:a@all 1" > "$PROGRESS_DIR/history"
j=$PROGRESS_DIR/jobs/5-$$; fake_job "$j" ci 10 all
n=$(date +%s)
printf '%s\n' "$((n-10)) plan step layer4" "$((n-10)) plan chain a b c d e f g" "$((n-10)) serial chain e f" \
    "$((n-10)) start step layer4" "$((n-10)) start chain g" "$((n-9)) cached chain g" "$((n-9)) done chain g 1" \
    "$((n-9)) start chain a" > "$j/events"
out=$($w --once --width 100)
check "cached: counted" grep -q "chains 1/7 (1 cached)" <<< "$out"
check "pool, then the serial ones: ETA ~6:3x" grep -Eq "ETA ~6:[23][0-9] " <<< "$out"
rm -rf "$j"

# thin history: mostly defaults gives a range, all defaults "ETA ?"
: > "$PROGRESS_DIR/history"
j=$PROGRESS_DIR/jobs/6-$$; fake_job "$j" ci 10
n=$(date +%s)
printf '%s\n' "$((n-10)) plan step layer4" "$((n-10)) plan chain p q" "$((n-10)) start step layer4" > "$j/events"
check "no history: ETA ?" grep -q "%  ETA ?  chains 0/2" <<< "$($w --once)"
printf '%s\n' "chain:p 200" > "$PROGRESS_DIR/history"
check "thin history: a range" grep -Eq "%  ETA ~[0-9]+-[0-9]+m  chains 0/2" <<< "$($w --once)"
rm -rf "$j"

# the history keeps the last 5 runs, oldest first (an old file's single
# time extended); a cached pass's seconds are not kept
printf '%s\n' "chain:old 7" > "$PROGRESS_DIR/history"
for i in 1 2 3 4 5 6; do $p run chain h -- true; done
$p run chain old -- true
check "history: last 5 runs" grep -q "^chain:h 0 0 0 0 0$" "$PROGRESS_DIR/history"
check "history: an old line extended" grep -q "^chain:old 7 0$" "$PROGRESS_DIR/history"
$p run ci ci -- bash -c "$p run chain cc -- $p cached chain cc"
check "history: a cached pass not kept" test -z "$(grep "^chain:cc " "$PROGRESS_DIR/history")"

# under make -n (MAKEFLAGS' first word has n): no job, no history (a
# dry run's 0-second chains once replaced every measured time)
MAKEFLAGS="n --no-print-directory" $p run chain dryrun -- true
check "dry run: no history" test -z "$(grep "^chain:dryrun " "$PROGRESS_DIR/history")"
MAKEFLAGS=" --no-print-directory" $p run chain wetrun -- true
check "not a dry run: history" grep -q "^chain:wetrun " "$PROGRESS_DIR/history"

# per-tier estimates: a full run's time has its own key, the quick
# tier's stays; the watcher uses the job's tier (D's FULL nes-lockstep
# once read "46:46 of ~9:51", the quick tier's time)
EMU_FULL_TESTS=1 $p run suite tiered -- true
check "full tier: its own key" grep -q "^suite:tiered@full " "$PROGRESS_DIR/history"
check "full tier: no quick key" test -z "$(grep "^suite:tiered " "$PROGRESS_DIR/history")"
NES_CORE_ROMS=all $p run suite tiered -- true
check "NES_CORE_ROMS: its own key" grep -q "^suite:tiered@quick-nes-all " "$PROGRESS_DIR/history"
ALL=1 $p run chain tiered -- true
check "ALL=1: its own key" grep -q "^chain:tiered@all " "$PROGRESS_DIR/history"
FULL=1 ALL=1 $p run chain tiered -- true
check "FULL=1 ALL=1: its own key" grep -q "^chain:tiered@full-all " "$PROGRESS_DIR/history"
printf '%s\n' "suite:s 10" "suite:s@full 100" > "$PROGRESS_DIR/history"
j=$PROGRESS_DIR/jobs/3-$$; mkdir -p "$j"
printf 'pid=%s\nstart=%s\nkind=test\nkey=suites\ntier=full\nsince=%s\nworktree=w\nbranch=b\n' \
    $$ "$(cut -d' ' -f22 /proc/$$/stat)" "$(( $(date +%s) - 50 ))" > "$j/meta"
n=$(date +%s)
printf '%s\n' "$((n-50)) plan suite s" "$((n-50)) start test suites" "$((n-50)) start suite s" > "$j/events"
out=$($w --once)
check "watcher: the job's tier shown" grep -q "^test  w (b)  \[full tier\]" <<< "$out"
check "watcher: the full tier's estimate" grep -Eq "suite s .* 0:5[01] of ~1:40" <<< "$out"
rm -rf "$j"

# a dead job's directory is dropped
mkdir -p "$PROGRESS_DIR/jobs/1-999999999"
printf 'pid=999999999\nstart=1\nkind=ci\n' > "$PROGRESS_DIR/jobs/1-999999999/meta"
$w --once > /dev/null
check "dead job removed" test ! -e "$PROGRESS_DIR/jobs/1-999999999"

# untracked heavy work: a "simulator" outside every job is listed and
# warned about (found by its pid), the same inside a job is not; its
# time running from its start (it read 0:00 on the worker, whose
# /proc/uptime is the container's: side Q, 2026-11-01)
bash -c "exec -a vga-sim bash -c 'sleep 3.5; true'" & u=$!
$p run sim zedboard/x -- bash -c "exec -a vga-sim bash -c 'sleep 3.5; true'" & t=$!
sleep 1.3
tc=$(pgrep -P "$t" | head -1)
out=$($w --once); warn=$($w --untracked 2>&1)
check "untracked: listed" grep -Eq "^  sim .* pid $u$" <<< "$out"
check "untracked: its worktree" grep -Eq "^  sim +$(basename "$PWD") .* pid $u$" <<< "$out"
check "untracked: its time running" grep -Eq "^  sim .* 0:0[1-3] running, .*pid $u$" <<< "$out"
check "untracked: a job's own not listed" test -n "$tc" -a -z "$(grep -E "pid $tc$" <<< "$out")"
check "untracked: warning" grep -q "warning: heavy work.*" <<< "$warn"
check "untracked: warning names it" grep -Eq "pid $u$" <<< "$warn"
wait

# Vivado's phase from its log
log=$PROGRESS_DIR/build.log
printf 'Command: synth_design -top t\nCommand: opt_design\nCommand: place_design\n' > "$log"
$p run bit zedboard:s "$log" -- sleep 1.5 &
sleep 0.5
check "bit phase: place (4/8)" grep -q "bit .*place (4/8)" <<< "$($w --once)"
wait

# memory (the user, 2026-10-18): the machine's, a job's tree and its
# biggest process by name (a script's, not its interpreter's)
printf 'import time\nb = b"x" * (200 << 20)\ntime.sleep(3)\n' > "$PROGRESS_DIR/hog.py"
$p run sim zedboard/m -- python3 "$PROGRESS_DIR/hog.py" &
sleep 1.5
out=$($w --once)
check "memory: the machine's" grep -Eq "^== desktop \(.*\): memory [0-9.]+ GB of [0-9.]+ GB in use" <<< "$out"
check "memory: the job's total" grep -Eq "^sim .*RAM 0\.[2-9] GB" <<< "$out"
check "memory: its biggest process" grep -Eq "^        RAM: hog\.py 0\.[2-9] GB" <<< "$out"
wait

# the worker's section: this script run there over ssh (a fake ssh runs
# the remote command here), its heading named "worker"; an unreachable
# worker says so
fake=$PROGRESS_DIR/fake-ssh
printf '#!/usr/bin/env bash\nwhile [[ $1 == -o ]]; do shift 2; done\nshift\nexec bash -c "$1"\n' > "$fake"
chmod +x "$fake"
out=$(WATCH_REMOTE=w WATCH_SSH=$fake WATCH_REMOTE_QUEUE=$PWD/tools/queue.sh $w --once)
check "remote: its section" grep -Eq "^== worker \(.*\): memory" <<< "$out"
check "remote: its queue" grep -q "^queue (worker):" <<< "$out"
check "remote: unreachable" grep -q "^== worker (w): unreachable" <<< "$(WATCH_REMOTE=w WATCH_SSH=false $w --once)"

if [[ $fails -gt 0 ]]; then echo "test_progress: $fails failed" >&2; exit 1; fi
echo "test_progress: all passed"
