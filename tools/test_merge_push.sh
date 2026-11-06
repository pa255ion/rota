#!/usr/bin/env bash
# Tests for tools/merge-push.sh (make test runs them): the push happens only
# with the merge slot (another holder: exit 3, the remote untouched),
# only as a fast-forward, only from a clean tree; the slot stays held. A
# private QUEUE_DIR and throwaway repositories: the machine's queue and the
# project's remote are never touched.
set -uo pipefail
cd "$(dirname "$0")/.."
unset QUEUE_HELD PROGRESS_JOB CLAUDECODE QUEUE_WAIT
here=$(pwd)
mp=$here/tools/merge-push.sh
q=$here/tools/queue.sh
export QUEUE_DIR
tmp=$(mktemp -d)
QUEUE_DIR=$tmp/queue
trap 'rm -rf "$tmp"' EXIT
fails=0
check() { # name, condition...
    local name=$1; shift
    if "$@"; then echo "test_merge_push: ok    $name"; else echo "test_merge_push: FAIL  $name"; fails=$((fails + 1)); fi
}
g() { git -c user.name=t -c user.email=t@t -c init.defaultBranch=main "$@"; }

# a remote with main, a clone with a branch one commit ahead
g init -q --bare "$tmp/remote.git"
g clone -q "$tmp/remote.git" "$tmp/w" 2>/dev/null
cd "$tmp/w" || exit 1
echo a > f && g add f && g commit -q -m a && g push -q origin HEAD:main
g checkout -q -b side-x
echo b > f && g commit -q -am b
remote_main() { g --git-dir="$tmp/remote.git" rev-parse main; }

# another session holds the merge slot: refused, nothing pushed
$q hold merge other > /dev/null
before=$(remote_main)
$mp me > /dev/null 2>&1; rc=$?
check "held by another: exit 3" test $rc -eq 3
check "held by another: remote untouched" test "$(remote_main)" = "$before"
$q release merge other > /dev/null

# uncommitted changes: refused, the slot held by us
echo dirty >> f
$mp me > /dev/null 2>&1; rc=$?
check "dirty tree: exit 1" test $rc -eq 1
check "dirty tree: remote untouched" test "$(remote_main)" = "$before"
check "dirty tree: slot held by us" grep -q '^who=me$' "$QUEUE_DIR/holds/merge"
g checkout -q -- f

# a fast-forward: pushed, the slot still held
$mp me > /dev/null 2>&1; rc=$?
check "fast-forward: exit 0" test $rc -eq 0
check "fast-forward: remote main is the branch" test "$(remote_main)" = "$(g rev-parse HEAD)"
check "fast-forward: slot still held" grep -q '^who=me$' "$QUEUE_DIR/holds/merge"

# already there: nothing to do, exit 0
$mp me > /dev/null 2>&1; rc=$?
check "up to date: exit 0" test $rc -eq 0

# main moved on elsewhere: not a fast-forward, refused
g clone -q "$tmp/remote.git" "$tmp/w2" 2>/dev/null
(cd "$tmp/w2" && echo c > g && g add g && g commit -q -m c && g push -q origin HEAD:main)
moved=$(remote_main)
echo d >> f && g commit -q -am d
$mp me > /dev/null 2>&1; rc=$?
check "not a fast-forward: exit 1" test $rc -eq 1
check "not a fast-forward: remote untouched" test "$(remote_main)" = "$moved"

# on the target branch itself: refused
g checkout -q -b main-local "$moved" 2>/dev/null
MERGE_PUSH_TARGET=main-local $mp me > /dev/null 2>&1; rc=$?
check "on the target branch: exit 1" test $rc -eq 1

$q release merge me > /dev/null
[[ $fails -eq 0 ]] && echo "test_merge_push: all passed" || { echo "test_merge_push: $fails failed"; exit 1; }
