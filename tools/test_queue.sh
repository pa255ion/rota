#!/usr/bin/env bash
# Tests for tools/queue.sh (make test runs them): exit codes, one job per
# slot, first come first served, the two slots independent, ci,bit
# waiting for both, nested runs, dead entries, show. A private QUEUE_DIR,
# so the machine's real queue is never touched.
set -uo pipefail
cd "$(dirname "$0")/.."
# run inside make ci (or another queued, tracked job): not part of it
unset QUEUE_HELD PROGRESS_JOB
# nor as an agent (whose runs don't wait), unless a test says so
unset CLAUDECODE QUEUE_WAIT

export QUEUE_DIR
QUEUE_DIR=$(mktemp -d)
export QUEUE_POLL=0.1
trap 'rm -rf "$QUEUE_DIR"' EXIT
q=tools/queue.sh
log=$QUEUE_DIR/log
fails=0
check() { # name, condition...
    local name=$1; shift
    if "$@"; then echo "test_queue: ok    $name"; else echo "test_queue: FAIL  $name"; fails=$((fails + 1)); fi
}
# job NAME SLOTS SECONDS: logs NAME+ at its start and NAME- at its end
job() { $q run "$2" -- sh -c "echo $1+ >> $log; sleep $3; echo $1- >> $log" 2>/dev/null; }
order() { tr '\n' ' ' < "$log"; }

# exit codes pass through
$q run ci -- true; check "exit 0" test $? -eq 0
$q run ci -- sh -c 'exit 3' 2>/dev/null; check "exit 3" test $? -eq 3

# one job per slot, first come first served; bit beside ci
: > "$log"
job A ci 1.2 & sleep 0.3
job D bit 0.2 & sleep 0.1
job B ci 0.2 & sleep 0.3
job C ci 0.2 &
wait
check "ci jobs in order, one at a time" test "$(grep -v D "$log" | tr '\n' ' ')" = "A+ A- B+ B- C+ C- "
check "bit runs beside ci" test "$(head -4 "$log" | tr '\n' ' ')" = "A+ D+ D- A- "

# ci,bit waits for both slots; a later bit job waits for it
: > "$log"
job A bit 0.8 & sleep 0.2
job F ci,bit 0.3 & sleep 0.2
job G bit 0.1 &
wait
check "ci,bit after bit, then bit" test "$(order)" = "A+ A- F+ F- G+ G- "

# nested: the outer job's slots are held, the inner run starts at once
: > "$log"
$q run ci -- sh -c "$q run ci -- sh -c 'echo inner >> $log'" 2>/dev/null
check "nested run" test "$(order)" = "inner "

# a dead job's entry doesn't block anyone (and show drops it)
mkdir -p "$QUEUE_DIR/entries"
printf 'pid=999999999\nstart=1\nslots=ci\nstate=running\nsince=1\n' > "$QUEUE_DIR/entries/1-999999999"
timeout 5 $q run ci -- true
check "dead entry ignored" test $? -eq 0
check "dead entry removed" test ! -e "$QUEUE_DIR/entries/1-999999999"

# show: the running job, the waiting one, the free slot
: > "$log"
job A ci 1 & sleep 0.3
QUEUE_NOTE="timed run" job B ci 0.1 & sleep 0.3
out=$($q show)
wait
check "show: running" grep -q "^running  ci .*sh -c echo A+" <<< "$out"
check "show: waiting, note" grep -q "^waiting  ci .*timed run" <<< "$out"
check "show: free bit" grep -q "^free slots: bit" <<< "$out"
check "show: empty after" grep -q "queue: empty" <<< "$($q show)"

# the waiting list (sessions without a process): pending, ready, order,
# one entry per session, the warning out of turn, the entry used up by
# the session's own run, the reminder of who's next, stale entries
QUEUE_WORKTREE=wt-a $q list add "session a" ci,bit --pending "after its user" > /dev/null
QUEUE_WORKTREE=wt-b $q list add "session b" ci,bit "ready now" > /dev/null
check "list next: the first ready one" test "$($q list next)" = "session b"
QUEUE_WORKTREE=wt-a $q list ready "session a" > /dev/null
check "list next: a, earlier in line, once ready" test "$($q list next)" = "session a"
QUEUE_WORKTREE=wt-a $q list add "session a" ci,bit "again" > /dev/null
check "re-adding keeps the place" test "$($q list next)" = "session a"
check "one entry per session" test "$(ls "$QUEUE_DIR/list" | wc -l)" -eq 2
check "show: the waiting list" grep -q "^  ready .* session a, wt-a" <<< "$($q show)"
w=$(QUEUE_WORKTREE=wt-b $q run ci,bit -- true 2>&1)
check "out of turn: warning" grep -q "warning: 'session a' (wt-a) is first" <<< "$w"
check "own entry used up" test -z "$($q show | grep 'session b')"
check "done: names who's next" grep -q "next on the waiting list: 'session a'" <<< "$w"
w=$(QUEUE_WORKTREE=wt-a $q run ci,bit -- true 2>&1)
check "in turn: no warning" test -z "$(grep warning <<< "$w")"
check "list empty after a's run" test -z "$($q list next)"
# someone behind us in line is no reason to warn (the gate's first live
# run warned about the session behind it)
QUEUE_WORKTREE=wt-d $q list add "session d" ci,bit > /dev/null
QUEUE_WORKTREE=wt-e $q list add "session e" ci,bit > /dev/null
w=$(QUEUE_WORKTREE=wt-d $q run ci,bit -- true 2>&1)
check "in turn, someone behind: no warning" test -z "$(grep warning <<< "$w")"
check "in turn, someone behind: names who's next" grep -q "next on the waiting list: 'session e'" <<< "$w"
$q list remove "session e" > /dev/null
QUEUE_WORKTREE=wt-c $q list add "session c" bit > /dev/null
sed -i 's/^since=.*/since=1000/' "$QUEUE_DIR"/list/*
check "stale: skipped" test -z "$($q list next)"
check "stale: marked" grep -q "\[stale" <<< "$($q show)"
$q list remove "session c" > /dev/null
check "remove" test -z "$(ls "$QUEUE_DIR/list")"

# an agent's run doesn't wait: busy slot -> exit 3 at once with the
# waiting-list hint; free slot -> runs; QUEUE_WAIT=1 -> waits
: > "$log"
job A ci 1 & sleep 0.3
t0=$SECONDS
w=$(CLAUDECODE=1 $q run ci -- sh -c "echo X+ >> $log" 2>&1); rc=$?
check "agent, busy: exit 3" test $rc -eq 3
check "agent, busy: at once" test $(( SECONDS - t0 )) -le 1
check "agent, busy: says who holds it" grep -q "slot ci is busy: running" <<< "$w"
check "agent, busy: the waiting-list hint" grep -q "make queue-join WHO=" <<< "$w"
check "agent, busy: didn't run" test -z "$(grep X+ "$log")"
check "agent, busy: no entry left" test -z "$($q show | grep "sh -c echo X+")"
CLAUDECODE=1 QUEUE_WAIT=1 $q run ci -- sh -c "echo Y+ >> $log" 2>/dev/null
check "agent, QUEUE_WAIT=1: waits, runs after" test "$(tr '\n' ' ' < "$log")" = "A+ A- Y+ "
wait
CLAUDECODE=1 $q run ci -- true; check "agent, free: runs" test $? -eq 0

# make queue-join passes NOTE to the shell quoted: parentheses and quotes
# survive (they once broke the recipe: "syntax error near unexpected
# token `('", side I's finding)
out=$(make -s queue-join WHO="session q" NOTE="gates (ZedBoard) it's \"fine\"" 2>&1)
check "queue-join: a note with ( ) ' \" accepted" grep -q "on the waiting list" <<< "$out"
check "queue-join: the note kept as given" grep -q "gates (ZedBoard) it's \"fine\"" <<< "$($q show)"
$q list remove "session q" > /dev/null

# C1 (side track K): FULL=1 test runs take "ci" and wait for a make ci
# (ci,bit); a second job on a board waits for the first; the two boards
# don't block each other
: > "$log"
job M ci,bit 0.6 & sleep 0.2
job T ci 0.1 &
wait
check "C1: a FULL=1 run (ci) waits for make ci (ci,bit)" test "$(order)" = "M+ M- T+ T- "
: > "$log"
job S1 board-arty 0.6 & sleep 0.2
job Z board-zedboard 0.1 & sleep 0.1
job S2 board-arty 0.1 &
wait
check "C1: a second make sd on the same board waits" test "$(grep S "$log" | tr '\n' ' ')" = "S1+ S1- S2+ S2- "
check "C1: the other board doesn't wait" test "$(head -3 "$log" | tr '\n' ' ')" = "S1+ Z+ Z- "
: > "$log"
job B1 bit 0.4 & sleep 0.1
job A1 board-arty 0.1 &
wait
check "C1: a board job doesn't wait for make bit" test "$(head -2 "$log" | tr '\n' ' ')" = "B1+ A1+ "

# holds (C1's board checks, C4's merges): a slot kept by a session without
# a process; another worktree's run waits (an agent's exits 3) and its
# hold is refused; the holder's own runs go ahead; release names who's next
QUEUE_WORKTREE=wt-h $q hold board-arty "session h" "board check: gb" > /dev/null
check "hold: shown" grep -q "^held     board-arty session h, wt-h .*board check: gb" <<< "$($q show)"
check "hold: not free" test -z "$($q show | grep '^free slots:.*board-arty')"
w=$(QUEUE_WORKTREE=wt-i CLAUDECODE=1 $q run board-arty -- true 2>&1); rc=$?
check "hold: another worktree's agent run exits 3" test $rc -eq 3
check "hold: and says who holds it" grep -q "busy: held .*session h" <<< "$w"
QUEUE_WORKTREE=wt-h CLAUDECODE=1 $q run board-arty -- true; check "hold: the holder's own run goes ahead" test $? -eq 0
QUEUE_WORKTREE=wt-i $q hold board-arty "session i" 2>/dev/null; check "hold: a second session's hold refused (3)" test $? -eq 3
: > "$log"
( QUEUE_WORKTREE=wt-i $q run board-arty -- sh -c "echo I+ >> $log" 2>/dev/null ) & sleep 0.4
check "hold: a terminal run waits for it" test -z "$(cat "$log")"
QUEUE_WORKTREE=wt-i $q list add "session i" board-arty > /dev/null
w=$($q release board-arty "session h")
wait
check "hold: released, the waiting run goes" test "$(order)" = "I+ "
check "release: names who's next" grep -q "next on the waiting list for board-arty: 'session i'" <<< "$w"
check "hold: the waiting run used up its place in line" test -z "$($q list next board-arty)"
$q release board-arty "session h" 2>/dev/null; check "release: not held refused" test $? -eq 2
QUEUE_WORKTREE=wt-h $q hold board-zedboard "session h" > /dev/null
$q release board-zedboard "session x" 2>/dev/null; check "release: by another session refused" test $? -eq 2
$q release board-zedboard "session h" > /dev/null
$q hold ci "session h" 2>/dev/null; check "hold: ci can't be held" test $? -eq 2
# C4: merges in turn: a held merge slot keeps a second merge in line, and
# doesn't stop an ordinary make ci
QUEUE_WORKTREE=wt-m $q hold merge "session m" "merging side-x" > /dev/null
QUEUE_WORKTREE=wt-n $q list add "session n" merge > /dev/null
QUEUE_WORKTREE=wt-n $q hold merge "session n" 2>/dev/null; check "C4: a second merge waits (hold refused, 3)" test $? -eq 3
check "C4: it's next for merge" test "$($q list next merge)" = "session n"
QUEUE_WORKTREE=wt-n CLAUDECODE=1 $q run ci,bit -- true; check "C4: a make ci isn't held up by a merge" test $? -eq 0
w=$($q release merge "session m")
check "C4: release names the next merge" grep -q "next on the waiting list for merge: 'session n'" <<< "$w"
QUEUE_WORKTREE=wt-n $q hold merge "session n" > /dev/null; check "C4: then it can hold it" test $? -eq 0
check "C4: its place in line used up" test -z "$($q list next merge)"
$q release merge "session n" > /dev/null
# a stale hold (over 12 h) is ignored and marked
QUEUE_WORKTREE=wt-s $q hold board-arty "session s" > /dev/null
sed -i 's/^since=.*/since=1000/' "$QUEUE_DIR/holds/board-arty"
QUEUE_WORKTREE=wt-t CLAUDECODE=1 $q run board-arty -- true; check "hold: stale, ignored" test $? -eq 0
check "hold: stale, marked" grep -q "^stale    board-arty" <<< "$($q show)"
rm -f "$QUEUE_DIR/holds/board-arty"

# make queue-hold / queue-release (the Makefile's quoting, SLOTS default)
out=$(make -s queue-hold WHO="session q" SLOTS=merge NOTE="side (x)'s merge" 2>&1)
check "queue-hold: held" grep -q "^held     merge   session q.*side (x)'s merge" <<< "$($q show)"
make -s queue-release WHO="session q" SLOTS=merge > /dev/null
check "queue-release: released" test -z "$($q show | grep '^held')"

# submitted jobs (the user, 2026-10-14: the queue pings the session when
# its work ends): a job submitted behind a busy slot starts by itself when
# the slot frees, and await ends when it does, with its exit status; a
# timeout says it still runs (124); a runner gone without a result is
# lost (125); show lists the jobs
: > "$log"
# A holds ci for 4 s: long enough that submit (it waits for its runner),
# two shows and await's 1 s all happen while A still runs (with 1 s, a
# busy machine let A and S finish first: M7 saw it fail twice, 2026-10-15)
job A ci 4 & sleep 0.3
id=$(CLAUDECODE=1 $q submit "session s" -- $q run ci -- sh -c "echo S+ >> $log" 2>/dev/null)
check "submit: an id" test -n "$id"
check "submit: listed" grep -q "^submitted jobs" <<< "$($q show)"
check "submit: shown as submitted" grep -q "$id  submitted .*session s" <<< "$($q show)"
w=$($q await "$id" 1); rc=$?
check "await: a timeout says it still runs (124)" test $rc -eq 124
check "await: and names the job" grep -q "job $id still not finished" <<< "$w"
w=$($q await "$id"); rc=$?
wait
check "await: ends when the job does, exit 0" test $rc -eq 0
check "await: the job waited its turn (behind A)" test "$(order)" = "A+ A- S+ "
check "await: says it finished" grep -q "job $id finished, exit 0" <<< "$w"
check "show: done, exit 0" grep -q "$id  done, exit 0" <<< "$($q show)"
id=$($q submit "session s" -- sh -c 'exit 5' 2>/dev/null)
$q await "$id" > /dev/null; check "await: the job's exit status passes through" test $? -eq 5
mkdir -p "$QUEUE_DIR/jobs/lost-1"
printf 'x\n' > "$QUEUE_DIR/jobs/lost-1/who"; printf 'wt\n' > "$QUEUE_DIR/jobs/lost-1/worktree"
printf 'true ' > "$QUEUE_DIR/jobs/lost-1/cmd"; date +%s > "$QUEUE_DIR/jobs/lost-1/since"
echo "999999999 1" > "$QUEUE_DIR/jobs/lost-1/runner"
timeout 5 $q await lost-1 > /dev/null; check "await: a lost job exits 125 at once" test $? -eq 125
# finished jobs (the user, 2026-10-18): show hides those that ended over
# QUEUE_SHOW_DONE seconds ago and counts them; jobs lists every one; a
# running one is always shown
old=$QUEUE_DIR/jobs/old-1
mkdir -p "$old"; printf 'x\n' > "$old/who"; printf 'wt\n' > "$old/worktree"; printf 'true ' > "$old/cmd"
echo $(( $(date +%s) - 7200 )) > "$old/since"; echo 0 > "$old/status"; echo $(( $(date +%s) - 3600 )) > "$old/ended"
out=$($q show)
check "show: an hour-old job hidden" test -z "$(grep "old-1 " <<< "$out")"
check "show: and counted" grep -q "finished earlier, not shown: make queue-jobs" <<< "$out"
check "show: a just-finished job shown" grep -q "$id  done, exit 5" <<< "$out"
check "show: QUEUE_SHOW_DONE=7200 shows it" grep -q "old-1  done, exit 0" <<< "$(QUEUE_SHOW_DONE=7200 $q show)"
check "jobs: every job" grep -q "old-1  done, exit 0" <<< "$($q jobs)"
# cancel (the user, 2026-10-18): a running job and what it started stopped
# at once; await then ends with 130; show says cancelled; a second cancel
# refused
# The job's sleep is unique to this run (60.<pid> s), so another run's
# beside it is never taken for this one's; it is polled for (a loaded
# machine took over 0.5 s to start it), and it has stopped when its /proc
# entry is gone or a zombie, polled for up to 10 s (a fixed "sleep 60"
# looked at once failed under load: M7, 2026-11-01)
nap=60.$$
cid=$($q submit "session s" -- sh -c "sleep $nap; echo late >> $log" 2>/dev/null)
child=
for _ in $(seq 50); do
    child=$(pgrep -xf "sleep 60[.]$$" | head -1); [ -n "$child" ] && break; sleep 0.1
done
stopped() { [ -n "$child" ] && { [ ! -e "/proc/$child" ] || [ "$(awk '{print $3}' "/proc/$child/stat" 2>/dev/null || echo Z)" = Z ]; }; }
t0=$SECONDS
$q cancel "$cid" > /dev/null; check "cancel: exit 0" test $? -eq 0
check "cancel: quick" test $((SECONDS - t0)) -lt 12
for _ in $(seq 100); do [ -z "$child" ] || stopped && break; sleep 0.1; done
check "cancel: what it started stopped too" stopped
timeout 5 $q await "$cid" > /dev/null; check "cancel: await ends with 130" test $? -eq 130
check "cancel: shown as cancelled" grep -q "$cid  cancelled" <<< "$($q show)"
$q cancel "$cid" 2> /dev/null; check "cancel: a finished job refused" test $? -ne 0
check "cancel: its command never finished" test -z "$(grep late "$log")"
id=$($q submit "session s" -- sleep 2 2>/dev/null)
echo $(( $(date +%s) - 7200 )) > "$QUEUE_DIR/jobs/$id/since"
check "show: a running job shown, however old" grep -q "$id  submitted" <<< "$($q show)"
$q await "$id" > /dev/null
check "show: lost" grep -q "lost-1  lost" <<< "$($q show)"
$q await no-such-job 2>/dev/null; check "await: an unknown job refused" test $? -eq 2

# make queue-submit / queue-await (the Makefile's quoting)
id=$(make -s queue-submit WHO="session m" CMD="echo 'it ran (quoted)' > $QUEUE_DIR/m.out" 2>/dev/null | tail -1)
make -s queue-await JOB="$id" > /dev/null 2>&1
check "queue-submit/await: the command ran, quotes kept" test "$(cat "$QUEUE_DIR/m.out" 2>/dev/null)" = "it ran (quoted)"

# the Makefile: queue.sh run execs its command, so an assignment after
# QUEUE_RUN needs env (make test FULL=1 ran "EMU_FULL_TESTS=1" as a
# command, M7's finding 2026-10-15)
check "Makefile: no assignment straight after QUEUE_RUN" \
    bash -c '! grep -nE "call QUEUE_RUN,[^)]*\)\)? +[A-Za-z_][A-Za-z0-9_]*=" Makefile'

# bad use
$q run gpu -- true 2>/dev/null; check "unknown slot refused" test $? -eq 2
$q list add x gpu 2>/dev/null; check "list: unknown slot refused" test $? -eq 2

if [[ $fails -gt 0 ]]; then echo "test_queue: $fails failed" >&2; exit 1; fi
echo "test_queue: all passed"
