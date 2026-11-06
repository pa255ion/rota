#!/usr/bin/env bash
# The machine-wide queue for heavy jobs (CLAUDE.md, working rule 10).
#
#   tools/queue.sh run SLOTS -- COMMAND...   run COMMAND when SLOTS are free
#   tools/queue.sh show                      who holds each slot, who waits,
#                                            and the waiting list
#   tools/queue.sh list add WHO SLOTS [--pending] [NOTE...]
#   tools/queue.sh list ready WHO | remove WHO | next [SLOTS]
#   tools/queue.sh hold SLOT WHO [NOTE...]   keep a board or merge slot
#   tools/queue.sh release SLOT WHO          (side track K, C1 and C4)
#   tools/queue.sh submit WHO -- COMMAND...  run COMMAND detached when it's
#                                            its turn; prints the job's id
#   tools/queue.sh await ID [MAXSECS]        wait for the job to end: its
#                                            exit status, its log's tail
#   tools/queue.sh cancel ID                 stop a submitted job and all it
#                                            started (await on it: 130)
#   tools/queue.sh jobs                      every submitted job of the last
#                                            3 days (show: the running ones,
#                                            those ended in QUEUE_SHOW_DONE s)
#
# Submitted jobs (the user, 2026-10-14: "the queue pings the session when
# work is finished"). A session can't be sent a message from a shell, but
# Claude Code tells a session when one of its background commands ends.
# So `submit` starts COMMAND detached (setsid, nohup; QUEUE_WAIT=1, so the
# heavy jobs inside it wait their turn however long), and `await ID`, run
# as the session's background command, ends when the job does, which is
# the ping. No "your turn" message is needed: the job starts by itself.
# MAXSECS (default 6600, under an agent's 2-hour limit): await then exits
# 124 saying the job still runs; await it again. A job whose runner died
# without a result (the machine restarted) is "lost": await exits 125.
# Each job is a directory in $QUEUE_DIR/jobs/ (who, worktree, command,
# runner pid, log, exit status); `show` lists the last day's.
#
# The waiting list (CLAUDE.md, working rule 10) is for sessions that want
# a heavy slot later without keeping a process waiting (an agent's
# background command lives at most 2 hours, waiting included): WHO is the
# session's name for SendMessage, --pending means "not yet: my user hasn't
# said go". When a job finishes, `run` prints the first ready session on
# the list, which the finishing session messages ("your turn"); that
# session then starts its job, which takes the free slot at once and
# removes its own entry (by worktree). `next` prints the first ready
# entry's WHO (for SLOTS, if given), so a session can also check for
# itself. Entries older than 12 hours are marked stale and skipped. `run`
# warns, never stops, when another worktree's ready entry is first.
# Adding an entry for another session (whose tools lack the list):
# QUEUE_WORKTREE=<its worktree> QUEUE_BRANCH=<its branch> list add ...
# (the worktree matters: a run from it takes the entry off the list).
#
# SLOTS: ci, bit, or ci,bit (make ci takes both: one heavy job at a time
# on the machine, the user, 2026-10-08); since C1 (side track K) also
# board-arty and board-zedboard (make prog, make sd, make load, make
# saves-pull on that board) and merge (C4: a merge into main, from merging
# main into the branch to make ci ALL=1 on main). make test FULL=1 and a
# heavy system's Clash run take "ci". Several
# sessions share this machine (16 cores, 30 GB) and their make ci / make
# bit runs don't fit in it together; this makes them take turns, in the
# order they asked (first come, first served), and lets every session and
# the user see the queue. make ci and make bit go through it by
# themselves (the Makefile); make queue runs `show`.
#
# Each job is a file in $QUEUE_DIR/entries/ (its pid, slots, worktree,
# branch, command, state), and each slot a flock'ed file, which the kernel
# releases when the job's process ends however it ends. A job waits until
# every live job that asked earlier for one of its slots has finished,
# then takes the slots' locks. Entries of dead processes are removed
# while waiting and by `show`.
#
# An agent's run doesn't wait (the user, 2026-10-08): from a Claude Code
# shell (CLAUDECODE=1) a job that would have to wait exits at once (3)
# and says how to join the waiting list instead, since an agent's
# background command lives at most 2 hours, waiting included. QUEUE_WAIT=1
# lets it wait anyway (a wait it knows is short). Terminal runs wait as
# long as it takes.
#
# Holds (C1, C4): a board check or a merge spans several commands and the
# user's time, so a session can keep a board or merge slot without a
# process: `hold SLOT WHO` takes it (exit 3 if another session holds it,
# or a job of another worktree runs on it: join the waiting list
# instead), `release SLOT WHO` gives it back and names who's next on the
# waiting list. While a hold stands, runs from other worktrees treat the
# slot as busy (an agent's exits 3, a terminal's waits); runs from the
# holder's own worktree go ahead. A hold older than 12 hours is stale:
# ignored, and marked so.
#
# QUEUE_HELD (set for COMMAND) lists the slots an outer job holds: a
# nested run for them starts at once. QUEUE_NOTE adds a note to the entry.
# QUEUE_DIR, QUEUE_POLL (seconds between checks) are for the tests.
set -uo pipefail

QUEUE_DIR=${QUEUE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/emulation_clash/queue}
POLL=${QUEUE_POLL:-5}
ALL_SLOTS=(ci bit board-arty board-zedboard merge)
HOLDABLE=(board-arty board-zedboard merge)
entries=$QUEUE_DIR/entries
list_dir=$QUEUE_DIR/list
holds_dir=$QUEUE_DIR/holds
jobs_dir=$QUEUE_DIR/jobs
STALE=${QUEUE_STALE:-43200}   # seconds: 12 hours

die() { echo "queue: $*" >&2; exit 2; }

known_slots() { # slots: dies on an unknown one
    local s k ok
    for s in ${1//,/ }; do
        ok=0; for k in "${ALL_SLOTS[@]}"; do [[ $s == "$k" ]] && ok=1; done
        [[ $ok == 1 ]] || die "unknown slot '$s' (known: ${ALL_SLOTS[*]})"
    done
}

# The process's start time (field 22 of /proc/<pid>/stat), so that a pid
# reused by another process doesn't keep a dead job's entry alive.
started() { # pid
    local s
    s=$(cat "/proc/$1/stat" 2>/dev/null) || return 1
    s=${s##*) }
    set -- $s
    echo "${20}"
}

# Read an entry into the associative array E; fails (and removes the
# file) if its process is gone.
declare -A E
load() { # file
    E=()
    local k v
    while IFS='=' read -r k v; do E[$k]=$v; done < "$1" 2>/dev/null || return 1
    if [[ -z ${E[pid]:-} || $(started "${E[pid]}") != "${E[start]:-}" ]]; then
        rm -f "$1"
        return 1
    fi
}

overlaps() { # slots slots
    local a b
    for a in ${1//,/ }; do for b in ${2//,/ }; do [[ $a == "$b" ]] && return 0; done; done
    return 1
}

describe() { # (from E) one line about a job
    local now since mins
    now=$(date +%s); since=${E[since]}; mins=$(( (now - since) / 60 ))
    printf '%-8s %-7s %s (%s): %s, since %s (%d min)%s, pid %s\n' \
        "${E[state]}" "${E[slots]}" "${E[worktree]}" "${E[branch]}" "${E[cmd]}" \
        "$(date -d "@$since" +%H:%M)" "$mins" "${E[note]:+, ${E[note]}}" "${E[pid]}"
}

write_entry() { # file state
    local tmp=$1.tmp
    {
        echo "pid=$$"
        echo "start=$(started $$)"
        echo "slots=$slots"
        echo "state=$2"
        echo "since=$(date +%s)"
        echo "worktree=$worktree"
        echo "branch=$branch"
        echo "cmd=$cmd"
        echo "note=${QUEUE_NOTE:-}"
    } > "$tmp" && mv -f "$tmp" "$1"
}

here_worktree() { echo "${QUEUE_WORKTREE:-$(basename "$(git rev-parse --show-toplevel 2>/dev/null || pwd)")}"; }

# Read a waiting-list entry into L (no liveness: a session is no process).
declare -A L
load_list() { # file
    L=()
    local k v
    while IFS='=' read -r k v; do L[$k]=$v; done < "$1" 2>/dev/null || return 1
    [[ -n ${L[who]:-} ]]
}

list_files() { ls "$list_dir" 2>/dev/null | grep -v '\.tmp$' | sort -n; }

list_find() { # who: its entry's file name, if any
    local f
    for f in $(list_files); do
        load_list "$list_dir/$f" && [[ ${L[who]} == "$1" ]] && { echo "$f"; return 0; }
    done
    return 1
}

list_write() { # file who slots state note
    local tmp=$1.tmp
    {
        echo "who=$2"
        echo "slots=$3"
        echo "state=$4"
        echo "since=${since:-$(date +%s)}"
        echo "worktree=$(here_worktree)"
        echo "branch=${QUEUE_BRANCH:-$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "?")}"
        echo "note=$5"
    } > "$tmp" && mv -f "$tmp" "$1"
}

# The first ready, fresh entry (for SLOTS if given): sets L, prints its file.
list_head() { # [slots] [skip-worktree]
    local f now
    now=$(date +%s)
    for f in $(list_files); do
        load_list "$list_dir/$f" || continue
        [[ ${L[state]} == ready ]] || continue
        (( now - ${L[since]:-0} > STALE )) && continue
        [[ -n ${1:-} ]] && ! overlaps "$1" "${L[slots]}" && continue
        [[ -n ${2:-} && ${L[worktree]} == "$2" ]] && continue
        HEAD_FILE=$f
        echo "$f"; return 0
    done
    return 1
}

list_cmd() {
    mkdir -p "$list_dir" || die "can't create $list_dir"
    local sub=${1:-}; shift || true
    case $sub in
        add)
            [[ $# -ge 2 ]] || die "usage: list add WHO SLOTS [--pending] [NOTE...]"
            local who=$1 slots=$2 state=ready f since=""; shift 2
            [[ ${1:-} == --pending ]] && { state=pending; shift; }
            known_slots "$slots"
            # one entry per session: re-adding keeps its place in line
            if f=$(list_find "$who"); then load_list "$list_dir/$f"; since=${L[since]}; else f=$(date +%s%N); fi
            list_write "$list_dir/$f" "$who" "$slots" "$state" "$*"
            echo "queue: '$who' on the waiting list ($state, slots $slots)" ;;
        ready)
            local f; f=$(list_find "${1:-}") || die "list ready: '${1:-}' is not on the waiting list"
            load_list "$list_dir/$f"
            since=${L[since]} list_write "$list_dir/$f" "${L[who]}" "${L[slots]}" ready "${L[note]}"
            echo "queue: '${L[who]}' is ready" ;;
        remove)
            local f; f=$(list_find "${1:-}") || die "list remove: '${1:-}' is not on the waiting list"
            rm -f "$list_dir/$f"; echo "queue: '$1' off the waiting list" ;;
        next)
            list_head "${1:-}" > /dev/null || exit 1
            echo "${L[who]}" ;;
        *)  die "usage: list add WHO SLOTS [--pending] [NOTE...] | ready WHO | remove WHO | next [SLOTS]" ;;
    esac
}

# A fresh hold on SLOT into H (who, worktree, branch, since, note).
declare -A H
load_hold() { # slot
    H=()
    local k v f=$holds_dir/$1
    [[ -f $f ]] || return 1
    while IFS='=' read -r k v; do H[$k]=$v; done < "$f" 2>/dev/null || return 1
    [[ -n ${H[who]:-} ]] && (( $(date +%s) - ${H[since]:-0} <= STALE ))
}

describe_hold() { # slot (from H)
    local now; now=$(date +%s)
    printf '%-8s %-7s %s, %s (%s), since %s (%d min)%s\n' held "$1" "${H[who]}" "${H[worktree]}" "${H[branch]}" \
        "$(date -d "@${H[since]}" +%H:%M)" $(( (now - H[since]) / 60 )) "${H[note]:+: ${H[note]}}"
}

# Another worktree's hold on one of SLOTS: prints it, succeeds.
held_by_other() { # slots worktree
    local s
    for s in ${1//,/ }; do
        load_hold "$s" && [[ ${H[worktree]} != "$2" ]] && { describe_hold "$s"; return 0; }
    done
    return 1
}

hold_cmd() { # slot who note...
    [[ $# -ge 2 ]] || die "usage: hold SLOT WHO [NOTE...]"
    local slot=$1 who=$2 k ok=0 f; shift 2
    for k in "${HOLDABLE[@]}"; do [[ $slot == "$k" ]] && ok=1; done
    [[ $ok == 1 ]] || die "hold: '$slot' can't be held (only ${HOLDABLE[*]}: ci and bit go with a job)"
    mkdir -p "$holds_dir" "$entries" || die "can't create $holds_dir"
    if load_hold "$slot" && [[ ${H[who]} != "$who" ]]; then
        echo "queue: $slot is held: $(describe_hold "$slot")" >&2
        echo "queue: join the waiting list: make queue-join WHO=\"$who\" SLOTS=$slot" >&2
        exit 3
    fi
    for f in $(ls "$entries" 2>/dev/null | sort -n); do
        [[ $f == *.tmp ]] && continue
        load "$entries/$f" || continue
        if overlaps "$slot" "${E[slots]}" && [[ ${E[worktree]} != "$(here_worktree)" ]]; then
            echo "queue: $slot is busy: $(describe)" >&2
            echo "queue: join the waiting list: make queue-join WHO=\"$who\" SLOTS=$slot" >&2
            exit 3
        fi
    done
    local tmp=$holds_dir/$slot.tmp
    {
        echo "who=$who"
        echo "since=$(date +%s)"
        echo "worktree=$(here_worktree)"
        echo "branch=${QUEUE_BRANCH:-$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "?")}"
        echo "note=$*"
    } > "$tmp" && mv -f "$tmp" "$holds_dir/$slot"
    # our place in line for it is used up
    if [[ -d $list_dir ]]; then
        for f in $(list_files); do
            load_list "$list_dir/$f" && [[ ${L[who]} == "$who" ]] && overlaps "$slot" "${L[slots]}" && rm -f "$list_dir/$f"
        done
    fi
    echo "queue: $slot held by '$who' (make queue-release SLOTS=$slot WHO=\"$who\" when done)"
}

release_cmd() { # slot who
    [[ $# -ge 2 ]] || die "usage: release SLOT WHO"
    local slot=$1 who=$2
    [[ -f $holds_dir/$slot ]] || die "release: $slot is not held"
    load_hold "$slot" || true
    [[ ${H[who]:-} == "$who" ]] || die "release: $slot is held by '${H[who]:-?}', not '$who'"
    rm -f "$holds_dir/$slot"
    echo "queue: $slot released"
    if list_head "$slot" > /dev/null; then
        echo "queue: next on the waiting list for $slot: '${L[who]}' (${L[worktree]}): message it that it's its turn (SendMessage)"
    fi
}

# A submitted job's state: done (exit N), lost, or submitted (the runner is
# alive: waiting for its turn or running; its queue entry says which).
job_state() { # job dir
    local d=$1 pid start
    if [[ -f $d/cancelled ]]; then echo "cancelled"; return; fi
    if [[ -f $d/status ]]; then echo "done, exit $(cat "$d/status")"; return; fi
    read -r pid start < "$d/runner" 2>/dev/null || { echo submitted; return; }
    if [[ $(started "$pid") == "$start" ]]; then echo submitted; else echo lost; fi
}

submit_cmd() { # who -- command...
    [[ $# -ge 3 && $2 == -- ]] || die "usage: submit WHO -- COMMAND..."
    local who=$1 id d; shift 2
    id=$(date +%Y%m%d-%H%M%S)-$$
    d=$jobs_dir/$id
    mkdir -p "$d" || die "can't create $d"
    printf '%s\n' "$who" > "$d/who"
    printf '%s\n' "$(here_worktree)" > "$d/worktree"
    printf '%s\n' "$PWD" > "$d/dir"
    printf '%s ' "$@" > "$d/cmd"
    date +%s > "$d/since"
    # the runner: records its pid (and start time, as the queue's entries
    # do), waits its turn inside COMMAND, then writes the exit status
    setsid nohup bash -c '
        d=$1; shift
        s=$(cat /proc/$$/stat); s=${s##*) }; set -- "$@"; read -r -a f <<< "$s"
        echo "$$ ${f[19]}" > "$d/runner"
        QUEUE_WAIT=1 "$@" > "$d/log" 2>&1
        echo $? > "$d/status.tmp" && mv -f "$d/status.tmp" "$d/status"
        date +%s > "$d/ended"' runner "$d" "$@" < /dev/null > /dev/null 2>&1 &
    # wait for the runner to write its pid, so a quick await sees it alive
    local i
    for i in $(seq 50); do [[ -f $d/runner || -f $d/status ]] && break; sleep 0.1; done
    echo "$id"
    echo "queue: job $id submitted for '$who' ($*); its log: $d/log" >&2
    echo "queue: be pinged when it ends: tools/queue.sh await $id (make queue-await JOB=$id), as a background command" >&2
}

# A submitted job stopped (the user, 2026-10-18): its runner leads a process
# group of its own (setsid), so the group is sent TERM (KILL if it is still
# there after ten seconds): the job and everything it started, its queue
# entries with them. Marked cancelled; queue-await on it ends with 130.
cancel_cmd() { # id
    local id=${1:-} d pid start i
    [[ -n $id ]] || die "usage: cancel ID"
    d=$jobs_dir/$id
    [[ -d $d ]] || die "cancel: no job $id (make queue lists them)"
    case $(job_state "$d") in
        submitted) ;;
        *) die "cancel: job $id is $(job_state "$d"), not running" ;;
    esac
    read -r pid start < "$d/runner" || die "cancel: job $id has no runner yet"
    touch "$d/cancelled"
    kill -TERM -- "-$pid" 2>/dev/null || true
    for i in $(seq 100); do
        [[ $(started "$pid") == "$start" ]] || break
        sleep 0.1
    done
    if [[ $(started "$pid") == "$start" ]]; then kill -KILL -- "-$pid" 2>/dev/null || true; fi
    echo "queue: job $id cancelled ($(cat "$d/cmd"))"
}

await_cmd() { # id [maxsecs]
    local id=${1:-} max=${2:-6600} d t0=$SECONDS st
    [[ -n $id ]] || die "usage: await ID [MAXSECS]"
    d=$jobs_dir/$id
    [[ -d $d ]] || die "await: no job $id (make queue lists them)"
    while :; do
        st=$(job_state "$d")
        case $st in
            done*) break ;;
            cancelled) echo "queue: job $id was cancelled ($(cat "$d/cmd"))"; exit 130 ;;
            lost) echo "queue: job $id is lost: its runner ended without a result ($(cat "$d/cmd"); log $d/log)"; exit 125 ;;
        esac
        if (( SECONDS - t0 >= max )); then
            echo "queue: job $id still not finished after ${max} s ($(cat "$d/cmd")); await it again"
            exit 124
        fi
        sleep "$POLL"
    done
    local rc; rc=$(cat "$d/status")
    echo "queue: job $id finished, exit $rc: $(cat "$d/cmd")(for '$(cat "$d/who")'; log $d/log)"
    tail -5 "$d/log" | sed 's/^/  | /'
    exit "$rc"
}

# Submitted jobs: make queue (and make watch) lists the ones still waiting
# or running and those that ended in the last QUEUE_SHOW_DONE seconds (30
# min; the user, 2026-10-18: a day's finished jobs buried the running
# ones), and counts the rest; `jobs` (make queue-jobs) lists every one.
# Their records stay three days, for queue-await.
SHOW_DONE=${QUEUE_SHOW_DONE:-1800}
show_jobs() { # [all]
    [[ -d $jobs_dir ]] || return 0
    local d now any=0 f st ended hidden=0
    now=$(date +%s)
    for f in $(ls "$jobs_dir" 2>/dev/null | sort); do
        d=$jobs_dir/$f
        [[ -f $d/since ]] || continue
        # pruned after three days
        if (( now - $(cat "$d/since") > 259200 )); then rm -rf "$d"; continue; fi
        st=$(job_state "$d")
        if [[ ${1:-} != all && $st != submitted ]]; then
            ended=$(cat "$d/ended" 2>/dev/null || cat "$d/since")
            if (( now - ended > SHOW_DONE )); then hidden=$((hidden + 1)); continue; fi
        fi
        [[ $any == 0 ]] && echo "submitted jobs (make queue-await JOB=<id> pings when one ends):"
        any=1
        printf '  %s  %-22s %s, %s: %s\n' "$f" "$st" "$(cat "$d/who")" "$(cat "$d/worktree")" "$(cat "$d/cmd")"
    done
    if (( hidden > 0 )); then
        echo "  ($hidden finished earlier, not shown: make queue-jobs lists every job of the last 3 days)"
    fi
}

show_list() {
    local f now any=0 age
    now=$(date +%s)
    for f in $(list_files); do
        load_list "$list_dir/$f" || continue
        [[ $any == 0 ]] && echo "waiting list (sessions without a process; the first ready one is next):"
        any=1
        age=$(( (now - ${L[since]:-$now}) / 60 ))
        printf '  %-8s %-7s %s, %s (%s), since %s (%d min)%s%s\n' \
            "${L[state]}" "${L[slots]}" "${L[who]}" "${L[worktree]}" "${L[branch]}" \
            "$(date -d "@${L[since]}" +%H:%M)" "$age" "${L[note]:+: ${L[note]}}" \
            "$( (( now - ${L[since]:-0} > STALE )) && echo "  [stale: over 12 h]")"
    done
}

show() {
    mkdir -p "$entries"
    local f any=0 busy=""
    for f in $(ls "$entries" 2>/dev/null | sort -n); do
        [[ $f == *.tmp ]] && continue
        load "$entries/$f" || continue
        any=1
        describe
        [[ ${E[state]} == running ]] && busy+=" ${E[slots]//,/ }"
    done
    local s free=""
    for s in "${HOLDABLE[@]}"; do
        if load_hold "$s"; then
            any=1; describe_hold "$s"; busy+=" $s"
        elif [[ -f $holds_dir/$s ]]; then
            any=1; echo "stale    $s (a hold over 12 h old, ignored: $(sed -n 's/^who=//p' "$holds_dir/$s"))"
        fi
    done
    [[ $any == 0 ]] && echo "queue: empty"
    for s in "${ALL_SLOTS[@]}"; do [[ " $busy " == *" $s "* ]] || free+=" $s"; done
    echo "free slots:${free:- none}"
    show_list
    show_jobs
}

# An agent's job that would wait: say why and how to join the list, exit 3
# (the EXIT trap removes our entry).
agent_refuse() { # what holds the slot
    {
        echo "queue: slot $slots is busy: $1"
        echo "queue: not waiting (an agent's background command lives at most 2 hours, waiting included):"
        echo "queue:   join the waiting list: make queue-join WHO=\"<your session name>\"$([[ $slots != ci,bit ]] && echo " SLOTS=$slots") [PENDING=1]"
        echo "queue:   and start when it's your turn (make queue; CLAUDE.md, working rule 10);"
        echo "queue:   QUEUE_WAIT=1 waits anyway (for a wait you know is short: make progress)"
    } >&2
    exit 3
}

run() {
    slots=$1; shift
    [[ ${1:-} == -- ]] && shift
    [[ $# -gt 0 ]] || die "run: no command"
    local s
    known_slots "$slots"

    # An outer job already holds these slots (e.g. make ci FULL=1 calling
    # a target that queues too): run at once.
    local need=""
    for s in ${slots//,/ }; do [[ ",${QUEUE_HELD:-}," == *",$s,"* ]] || need+="$s,"; done
    if [[ -z $need ]]; then
        "$@"; exit $?
    fi
    slots=${need%,}

    worktree=$(here_worktree)
    branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "?")
    cmd=$(printf '%s ' "$@" | tr '\n' ' '); cmd=${cmd% }
    mkdir -p "$entries" || die "can't create $entries"
    local seq me
    seq=$(date +%s%N)
    me=$entries/$seq-$$
    trap 'rm -f "$me" "$me.tmp"' EXIT
    trap 'exit 130' INT TERM HUP
    write_entry "$me" waiting

    # First come, first served: wait for every live job that asked
    # earlier for one of these slots.
    local f blocker said=""
    local agent=""
    [[ ${CLAUDECODE:-} == 1 && ${QUEUE_WAIT:-} != 1 ]] && agent=1
    while :; do
        blocker=""
        for f in $(ls "$entries" | sort -n); do
            [[ $f == *.tmp ]] && continue
            [[ $f == "$seq-$$" ]] && break
            load "$entries/$f" || continue
            if overlaps "$slots" "${E[slots]}"; then blocker=$f; break; fi
        done
        local why=""
        if [[ -n $blocker ]]; then why=$(describe)
        elif why=$(held_by_other "$slots" "$worktree"); then blocker="hold $why"
        else break; fi
        [[ -n $agent ]] && agent_refuse "$why"
        if [[ $blocker != "$said" ]]; then
            echo "queue: waiting for slot $slots behind: $why" >&2
            said=$blocker
        fi
        sleep "$POLL"
    done

    # The slots' locks, always in the same order (no deadlock). They are
    # free unless someone runs outside the queue; then wait for them.
    local fd
    for s in "${ALL_SLOTS[@]}"; do
        overlaps "$slots" "$s" || continue
        exec {fd}>"$QUEUE_DIR/slot-$s.lock"
        if ! flock -n "$fd"; then
            [[ -n $agent ]] && agent_refuse "slot $s, locked outside the queue order"
            echo "queue: slot $s is locked outside the queue order; waiting for it" >&2
            flock "$fd"
        fi
    done
    write_entry "$me" running
    [[ -n $said ]] && echo "queue: slot $slots is ours now" >&2

    # the waiting list: our own entry is used up; was someone ahead of it
    # (or, with no entry of ours, anyone)? Those behind us are no matter
    # (a warning about one once greeted a session starting in turn).
    mkdir -p "$list_dir"
    local mine_at=""
    for f in $(list_files); do
        if load_list "$list_dir/$f" && [[ ${L[worktree]} == "$worktree" ]] && overlaps "$slots" "${L[slots]}"; then
            [[ -z $mine_at ]] && mine_at=$f
            rm -f "$list_dir/$f"
        fi
    done
    if list_head "$slots" "$worktree" > /dev/null && { [[ -z $mine_at ]] || (( HEAD_FILE < mine_at )); }; then
        echo "queue: warning: '${L[who]}' (${L[worktree]}) is first on the waiting list for $slots; started out of turn? (make queue)" >&2
    fi

    local rc
    QUEUE_HELD="${QUEUE_HELD:+$QUEUE_HELD,}$slots" "$@"; rc=$?
    if list_head "$slots" > /dev/null; then
        echo "queue: done; next on the waiting list: '${L[who]}' (${L[worktree]}): message it that it's its turn (SendMessage)" >&2
    fi
    exit $rc
}

case ${1:-} in
    run)  shift; [[ $# -ge 1 ]] || die "usage: tools/queue.sh run SLOTS -- COMMAND..."; run "$@" ;;
    show) show ;;
    jobs) show_jobs all ;;
    list) shift; list_cmd "$@" ;;
    hold) shift; hold_cmd "$@" ;;
    release) shift; release_cmd "$@" ;;
    submit) shift; submit_cmd "$@" ;;
    await) shift; await_cmd "$@" ;;
    cancel) shift; cancel_cmd "$@" ;;
    *)    echo "usage: tools/queue.sh run SLOTS -- COMMAND... | show | list ... | hold|release SLOT WHO | submit WHO -- COMMAND... | await ID [MAXSECS]" >&2; exit 2 ;;
esac
