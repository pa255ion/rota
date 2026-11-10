#!/bin/sh
# side track W's page gate (W.2, W.3): the page in headless Chromium (?test=N
# runs N frames at once after loading): each system's game, with the ROTA
# splash first (600 frames) and without it, ends on its golden picture.
# The browser runs in real time, driven over its debugging port
# (web/test/cdp.mjs): the emulator is in a Web Worker, which virtual time
# doesn't wait for.
#   web/test/page.sh      (from the repository's root; chromium from
#                          `nix shell nixpkgs#chromium`)
# The splash ROMs come from build/demos (make demos), the games from roms/
# and build/ (tools/fetch-roms.sh, the systems' asm targets).
set -eu
dir=$(mktemp -d)
trap 'kill $srv $chrome 2>/dev/null; wait $chrome 2>/dev/null; rm -rf "$dir"' EXIT
cp web/www/* "$dir"/
cp build/demos/rota.gg build/demos/rota.sms build/demos/rota.gb build/demos/rota.nes build/demos/rota.xex "$dir"/
# the 800XL's OS, BASIC and loader (as web/publish.sh puts them on the site)
cp roms/a800xl/os/altirraos_xl.rom roms/a800xl/os/altirra_basic.rom build/a800xl/xexload.bin "$dir"/
# the Super Nintendo's: our boot ROM, its splash
cp build/snes/ipl.bin build/demos/rota.sfc "$dir"/
port=$((20000 + $$ % 20000))
(cd "$dir" && exec python3 -m http.server "$port" --bind 127.0.0.1 >/dev/null 2>&1) &
srv=$!
dport=$((port + 1))
env -u LD_LIBRARY_PATH chromium --headless=new --no-sandbox --disable-gpu --remote-debugging-port="$dport" \
  --user-data-dir="$dir/profile" about:blank >/dev/null 2>&1 &
chrome=$!
sleep 2

# the hash the page reports for a golden PNG (the RGBA's FNV-1a, Core.hash)
pnghash() {
  node --input-type=module -e "
    import { readPng } from '$PWD/web/test/png.mjs';
    const img = readPng(process.argv[1]);
    let h = 0xcbf29ce484222325n;
    for (const p of img.rgb) for (const b of [(p >>> 16) & 255, (p >>> 8) & 255, p & 255, 255]) h = BigInt.asUintN(64, (h ^ BigInt(b)) * 0x100000001b3n);
    console.log(h.toString(16).padStart(16, '0'));" "$1"
}

fail=0
# NAME FILE FRAMES WANT: the game's FRAMES frames from power-on end on WANT
check() {
  cp "$2" "$dir/$1"
  for splash in 1 0; do
    n=$(( $3 + 600 * splash ))
    got=$(node web/test/cdp.mjs "$dport" "http://127.0.0.1:$port/?rom=$1&splash=$splash&test=$n" 60000 2>/dev/null || true)
    if [ "$got" = "$4" ]; then echo "page: $1, splash=$splash: frame $3 OK"
    else echo "page: $1, splash=$splash: FAIL ${got:-no hash}, want $4"; fail=1; fi
  done
}
# Waternet's frame 299 (the spec's golden; sms_hash is Gg.Screen's frameHash)
check waternet.gg roms/gg/games/waternet.gg 300 bc6ca62a2d5b8977
# Raposa do Sol's frame 419, before its first key (the golden is the spec's without the FM unit, the page's has it: the same picture)
check raposa.sms roms/sms/games/raposadosol_v20.sms "$(awk 'NR==1 {print $1 + 1}' systems/sms/test/golden/raposadosol.txt)" "$(awk 'NR==1 {print $2}' systems/sms/test/golden/raposadosol.txt)"
check dmg-acid2.gb roms/gb/dmg-acid2/dmg-acid2.gb 30 "$(pnghash systems/gb/test/golden/spec/dmg-acid2.png)"
check cgb-acid2.gbc roms/gbc/cgb-acid2/cgb-acid2.gbc 30 "$(pnghash systems/gb/test/golden/spec/cgb-acid2.png)"
check nestest.nes roms/nes/nestest/nestest.nes 30 "$(pnghash systems/nes/test/golden/spec/nestest.png)"
# the 800XL: a program through the loader (the desktop test's golden)
check rasterbars.xex build/a800xl/asm/rasterbars.xex 100 "$(pnghash systems/a800xl/test/golden/rasterbars.png)"
# the Super Nintendo: the homebrew demo's frame 120 (snes_hash, the desktop test's golden)
check snes-demo.sfc roms/snes/games/unnamed-tech-demo-v3.sfc 120 "$(cat systems/snes/test/golden/frames/unnamed-tech-demo-v3-120.txt)"
# (the page runs 12 instructions a frame, the golden 200: the IBM logo is drawn by then either way)
check ibm.ch8 roms/chip8/timendus/2-ibm-logo.ch8 60 "$(pnghash systems/chip8/test/golden/spec/2-ibm-logo.png)"
exit $fail
