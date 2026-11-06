#!/usr/bin/env bash
# Side track J: tools/mksd.sh with the boot splash (BOOT_FILES) and the
# systems' bitstreams (SYSTEM_BITS), on a terminal. mtools asks there when
# a folder exists (mmd), and make sd hung on that question with nothing on
# the screen (2026-10-08: /systems made twice). `script` gives mksd.sh a
# terminal whose input stays open (as a user's does), so a question times
# out here instead of passing unseen. Run by
# make test-zig, in nix develop: demos/test-mksd.sh DIR
set -euo pipefail
dir=$1
rm -rf "$dir" && mkdir -p "$dir/roms" "$dir/boot"
printf '\x00\xe0' > "$dir/roms/a.ch8"
printf 'bit' > "$dir/chip8.bit"
printf '\x12\x00' > "$dir/boot/chip8.ch8"
printf 'gb' > "$dir/boot/gb.gb"
cmd="SYSTEM_BITS='chip8=$dir/chip8.bit' BOOT_FILES='$dir/boot/chip8.ch8 $dir/boot/gb.gb' tools/mksd.sh '$dir/sd.img' '$dir/roms'"
if ! timeout 60 script -qec "$cmd" "$dir/mksd.log" < <(sleep 90) > /dev/null; then
    echo "test-mksd.sh: tools/mksd.sh failed or waited for an answer on the terminal:" >&2
    tail -n 5 "$dir/mksd.log" >&2
    exit 1
fi
for f in systems/chip8.bit systems/boot/chip8.ch8 systems/boot/gb.gb; do
    mdir -b -i "$dir/sd.img@@1M" "::/$(dirname "$f")" | grep -q "/$f\$" ||
        { echo "test-mksd.sh: no /$f on the card" >&2; exit 1; }
done
# side track L: the settings file at its fixed size (options.zig)
settings=$(mtype -i "$dir/sd.img@@1M" ::/settings.txt)
[ "$(mdir -b -i "$dir/sd.img@@1M" ::/ | grep -c '/settings.txt$')" = 1 ] &&
    [ "$(mtype -i "$dir/sd.img@@1M" ::/settings.txt | wc -c)" = 64 ] &&
    [ "${settings%% *}" = "volume=10" ] ||
    { echo "test-mksd.sh: no 64-byte /settings.txt with volume=10 on the card" >&2; exit 1; }
echo "test-mksd.sh: OK"
