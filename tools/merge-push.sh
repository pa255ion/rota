#!/usr/bin/env bash
# make merge-push WHO=<session>: main fast-forwarded to this branch, under
# the queue's merge slot, in one command (side K; side Q's idea, the user
# 2026-10-28). Merging takes the merge slot from merging main into the
# branch to the post-merge make ci ALL=1 (CLAUDE.md, Parallel sessions,
# item 3); a push that went on after a failed hold (its output piped
# through tail, the push after a ';', 2026-10-27) moved main under another
# session's merge. Here the push can't run without the slot:
#   1. tools/queue.sh hold merge WHO (held by someone else: exit 3, nothing
#      pushed; held by WHO already: fine);
#   2. the work tree clean, the branch not main itself;
#   3. REMOTE's main an ancestor of HEAD (a fast-forward only: else merge
#      main in and re-run the gates first; exit 1);
#   4. git push REMOTE HEAD:main.
# The slot stays held in every case (the post-merge make ci ALL=1 runs
# under it; on a refusal the merge of main runs under it): release it with
# make queue-release WHO=<session> SLOTS=merge.
#   tools/merge-push.sh WHO [REMOTE]
# MERGE_PUSH_TARGET (default main) and QUEUE_SH are for the tests.
set -uo pipefail
die() { echo "merge-push: $*" >&2; exit 1; }
[[ $# -ge 1 && -n $1 ]] || die "usage: merge-push.sh WHO [REMOTE]"
who=$1
remote=${2:-origin}
target=${MERGE_PUSH_TARGET:-main}
q=${QUEUE_SH:-$(cd "$(dirname "$0")" && pwd)/queue.sh}

"$q" hold merge "$who"
rc=$?
if [[ $rc -ne 0 ]]; then
    echo "merge-push: no merge slot: nothing pushed" >&2
    exit "$rc"
fi
release_hint="(the merge slot stays held: make queue-release WHO=\"$who\" SLOTS=merge)"

git diff --quiet && git diff --cached --quiet || die "uncommitted changes: nothing pushed $release_hint"
branch=$(git rev-parse --abbrev-ref HEAD) || die "not a git work tree"
[[ $branch != "$target" ]] || die "on $target itself: run it from the branch to merge"
git fetch -q "$remote" "$target" || die "can't fetch $remote/$target $release_hint"
base=$(git rev-parse FETCH_HEAD)
head=$(git rev-parse HEAD)
if [[ $base == "$head" ]]; then
    echo "merge-push: $remote/$target is already at $(git rev-parse --short HEAD): nothing to push $release_hint"
    exit 0
fi
git merge-base --is-ancestor "$base" "$head" ||
    die "$remote/$target ($(git rev-parse --short "$base")) is not in $branch: merge it in, re-run the gates, then again; nothing pushed $release_hint"
git push -q "$remote" "HEAD:$target" || die "the push failed: nothing pushed $release_hint"
echo "merge-push: $remote/$target fast-forwarded to $(git rev-parse --short HEAD) ($branch)"
echo "merge-push: next: make ci ALL=1 on $target from the queue, then make queue-release WHO=\"$who\" SLOTS=merge"
