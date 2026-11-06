#!/usr/bin/env bash
# tools/ps7-trim.sh on each board's ps7_init.c (make test-zig): every 3.0
# table kept byte for byte, no 1.0 or 2.0 table left, ten #defines in
# their place, every other line the same. Arguments: the ps7_init.c files.
set -uo pipefail
cd "$(dirname "$0")/.."
fails=0
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
check() { # name, condition...
    local name=$1; shift
    if "$@"; then echo "test_ps7_trim: ok    $name"; else echo "test_ps7_trim: FAIL  $name"; fails=$((fails + 1)); fi
}
tables() { # file revision: the revision's table definitions, whole
    awk -v r="$2" '$0 ~ "^unsigned long ps7_[a-z]+_init_data_" r "\\[\\] = \\{" {on = 1} on {print} on && /^};/ {on = 0}' "$1"
}
for f in "$@"; do
    [[ -f $f ]] || { echo "test_ps7_trim: no $f" >&2; exit 1; }
    b=$(basename "$(dirname "$f")")
    tools/ps7-trim.sh "$f" "$tmp/out.c"
    check "$b: the 3.0 tables kept byte for byte" cmp -s <(tables "$f" 3_0) <(tables "$tmp/out.c" 3_0)
    check "$b: five 3.0 tables" test "$(grep -c '^unsigned long ps7_[a-z]*_init_data_3_0\[\] = {' "$tmp/out.c")" -eq 5
    check "$b: no 1.0 or 2.0 table left" test "$(grep -c '^unsigned long ps7_[a-z]*_init_data_[12]_0\[\] = {' "$tmp/out.c")" -eq 0
    check "$b: ten #defines naming their 3.0 twins" test "$(grep -cE '^#define ps7_[a-z]+_init_data_[12]_0 ps7_[a-z]+_init_data_3_0 ' "$tmp/out.c")" -eq 10
    # every other line as it was: the input without the 1.0/2.0 tables
    awk '/^unsigned long ps7_[a-z]+_init_data_[12]_0\[\] = \{/ {s = 1; next} s && /^};/ {s = 0; next} !s' "$f" > "$tmp/rest.c"
    check "$b: the rest unchanged" cmp -s "$tmp/rest.c" <(grep -v '^#define ps7_[a-z]*_init_data_[12]_0 ' "$tmp/out.c")
    check "$b: shorter by the two revisions" test "$(wc -c < "$tmp/out.c")" -lt "$(( $(wc -c < "$f") / 2 ))"
done
if [[ $fails -gt 0 ]]; then echo "test_ps7_trim: $fails failed" >&2; exit 1; fi
echo "test_ps7_trim: all passed"
