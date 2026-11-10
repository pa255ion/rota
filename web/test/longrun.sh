#!/bin/sh
# side track W: a long run of each case (web/test/longrun-cases.json) in the
# browser build and in the native desktop library (make install's), their
# pictures and sound compared every CHECK frames up to FRAMES: WebAssembly's
# Int is 32 bits, so a counter that only grows (a cycle count) wraps there
# after a while and the machines part, which the short goldens can't see.
#   web/test/longrun.sh [CASE ...]   (FRAMES=72000 CHECK=6000: 20 minutes)
set -u
cd "$(dirname "$0")/../.."
frames=${FRAMES:-72000}; check=${CHECK:-6000}
dir=${TMPDIR:-/tmp}/rota-longrun.$$; mkdir -p "$dir"
points=$(seq "$check" "$check" "$frames" | tr '\n' ' ')
fail=0
for c in ${*:-gg sms gb gbc chip8 nes a800 snes}; do
  python3 web/test/longrun-native.py "$c" "$dir/$c" $points &
  node --no-warnings web/test/longrun-wasm.mjs "$c" "$dir/$c" $points
  wait $! || { echo "$c: the native run failed"; fail=1; continue; }
  first=
  for n in $points; do
    if ! python3 web/test/longrun-diff.py "$dir/$c-$n.native" "$dir/$c-$n.wasm" >"$dir/$c-$n.diff"; then first=$n; break; fi
  done
  if [ -z "$first" ]; then echo "$c: OK, the same picture and sound at every $check frames up to $frames"
  else echo "$c: FAIL, they part by frame $first:"; cat "$dir/$c-$first.diff"; fail=1; fi
done
rm -rf "$dir"
exit $fail
