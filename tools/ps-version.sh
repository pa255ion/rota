#!/usr/bin/env bash
# The PS app's version mark (side track K, the user 2026-10-10): a short
# hash of everything its ELF is built from, which the SD menu shows at
# the title bar's right end ("PS 3fa9c1"), so a glance at the screen says
# whether the board runs the build you have.
#
#   tools/ps-version.sh BOARD PS7_INIT_C [FEATURE...]          -> 6 hex digits
#   tools/ps-version.sh --date BOARD PS7_INIT_C [FEATURE...]   -> MM-DD[+]
#
# FEATUREs: the build's options (side track O: -Dmovies=..., -Dscreenshots=...),
# hashed with the sources, so a build without a feature has its own mark.
#
# Hashed: platform/ps/ (its Zig, its C stub header, the linker script),
# tools/ps7-trim.sh (what of ps7_init.c is compiled),
# build.zig, the board's ps7_init.c, and the board's name, in a fixed
# order. Not the git commit: a docs-only commit doesn't change the mark,
# and the same sources give the same mark and the same ELF (the
# byte-identical checks still hold). `make version` prints both boards'.
#
# --date (M7's request for the user, 2026-11-03: how old a card's build
# is, at a glance): the newest commit date of the same files ("10-18"),
# with a "+" when one of them has uncommitted changes; nothing outside a
# git checkout (the worker's mirrors). From the sources, not the clock,
# so the same sources still give the same ELF. The menu shows it after
# the hash ("PS 3fa9c1 10-18").
#
# --mark: both, as the build passes them in (-Dps-version): "3fa9c1 10-18",
# or the hash alone where there is no date.
set -euo pipefail
cd "$(dirname "$0")/.."
mode=hash
case "${1:-}" in --date) mode=date; shift ;; --mark) mode=mark; shift ;; esac
board=${1:?usage: tools/ps-version.sh [--date|--mark] BOARD PS7_INIT_C}
ps7=${2:?usage: tools/ps-version.sh [--date|--mark] BOARD PS7_INIT_C}
shift 2
files="$(find platform/ps -type f \( -name '*.zig' -o -name '*.h' -o -name '*.ld' \) | LC_ALL=C sort) build.zig tools/ps7-trim.sh"
hash() {
    {
        echo "board $board"
        [ $# -gt 0 ] && echo "features $*"
        for f in $files; do
            echo "file $f"
            cat "$f"
        done
        echo "ps7_init.c"
        if [ -f "$ps7" ]; then cat "$ps7"; else echo "(missing)"; fi
    } | sha256sum | cut -c1-6
}
src_date() {
    git rev-parse --git-dir >/dev/null 2>&1 || return 0
    local d
    # shellcheck disable=SC2086
    d=$(git log -1 --format=%cd --date=format:%m-%d -- $files)
    # shellcheck disable=SC2086
    if [ -n "$(git status --porcelain -- $files)" ]; then d="$d+"; fi
    echo "$d"
}
case $mode in
    hash) hash "$@" ;;
    date) src_date ;;
    mark) h=$(hash "$@"); d=$(src_date); echo "$h${d:+ $d}" ;;
esac
