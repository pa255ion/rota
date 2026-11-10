#!/bin/sh
# side track W: the page's battery saves end to end, in headless Chromium
# (a regression test: the save lookup's timer fired every time and marked
# the storage absent, so no save was ever kept after a game's first two
# seconds). OpenSNES's SRAM test: A writes slot 1, the page's periodic save
# stores it, the page opened again loads it ("its battery save loaded"),
# B reads it back.
#   web/test/save-page.sh   (chromium from `nix shell nixpkgs#chromium`)
set -eu
dir=$(mktemp -d)
trap 'kill $srv $chrome 2>/dev/null; wait $chrome 2>/dev/null; rm -rf "$dir"' EXIT
cp web/www/* "$dir"/
cp build/snes/ipl.bin build/demos/rota.sfc roms/snes/opensnes/save_game.sfc "$dir"/
port=$((20000 + $$ % 20000)); dport=$((port + 1))
(cd "$dir" && exec python3 -m http.server "$port" --bind 127.0.0.1 >/dev/null 2>&1) &
srv=$!
env -u LD_LIBRARY_PATH chromium --headless=new --no-sandbox --disable-gpu --window-size=900,900 --remote-debugging-port="$dport" \
  --user-data-dir="$dir/profile" about:blank >/dev/null 2>&1 &
chrome=$!
sleep 2
out=$(node web/test/cdp-save.mjs "$dport" "http://127.0.0.1:$port/?rom=save_game.sfc&splash=0&debug" "$dir/shot.png")
echo "$out"
if echo "$out" | grep -q '^again: .*(its battery save loaded)'; then echo "save-page: OK, the save kept and loaded again"
else echo "save-page: FAIL, the save wasn't loaded again"; exit 1; fi
