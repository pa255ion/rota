#!/usr/bin/env bash
# tools/sd-sync.sh (make sd-sync) on a plain folder standing in for the
# mounted card (SD_SYNC_TEST=1): the saves pulled off it, the image's files
# put on it, what is gone removed, the desktop's own folders kept, a second
# push a no-op; the mount checks refusing a folder that isn't a FAT mount.
set -uo pipefail
cd "$(dirname "$0")/.."
export MTOOLS_SKIP_CHECK=1
t=$(mktemp -d)
trap 'rm -rf "$t"' EXIT
fails=0
check() { # name, condition...
    local name=$1; shift
    if "$@"; then echo "test_sd_sync: ok    $name"; else echo "test_sd_sync: FAIL  $name"; fails=$((fails + 1)); fi
}

# an image as make sd builds it (small: CHIP-8 ROMs and a boot file)
mkdir -p "$t/roms/games"
seq 1 300 > "$t/roms/games/pong.ch8"
seq 5 900 > "$t/roms/tetris.ch8"
head -c 4096 /dev/urandom > "$t/BOOT.BIN"
BOOT_BIN="$t/BOOT.BIN" SD_MB=64 tools/mksd.sh "$t/card.img" "$t/roms" >/dev/null 2>&1
check "mksd.sh: the image built" test -f "$t/card.img"

# the "card": what an older card held
m=$t/card
mkdir -p "$m/chip8" "$m/System Volume Information" "$m/saves/gb/games" "$m/STATES/gb/games"
echo old > "$m/chip8/old.ch8"
echo mine > "$m/System Volume Information/IndexerVolumeGuid"
head -c 8192 /dev/zero | tr '\0' '\7' > "$m/saves/gb/games/zelda.sav"
echo state > "$m/STATES/gb/games/zelda.st1"

# pull: the card's /saves, /states (any case), no /movies
SD_SYNC_TEST=1 tools/sd-sync.sh pull "$m" "$t/saves" "$t/states" "$t/movies" >/dev/null
check "pull: the card's saves" cmp -s "$m/saves/gb/games/zelda.sav" "$t/saves/gb/games/zelda.sav"
check "pull: the card's states (STATES, any case)" cmp -s "$m/STATES/gb/games/zelda.st1" "$t/states/gb/games/zelda.st1"
check "pull: no /movies on the card, none made" test ! -e "$t/movies"

# push: the card holds the image's files, and only those
out=$(SD_SYNC_TEST=1 tools/sd-sync.sh push "$m" "$t/card.img")
mkdir "$t/want"
mcopy -s -n -Q -m -i "$t/card.img@@1M" '::/*' "$t/want/"
check "push: every file of the image on the card, the same bytes" diff -r -x 'System Volume Information' "$t/want" "$m"
check "push: a ROM no longer in the image removed" test ! -e "$m/chip8/old.ch8"
check "push: the desktop's own folder kept" test -f "$m/System Volume Information/IndexerVolumeGuid"
check "push: BOOT.BIN on the card" cmp -s "$t/BOOT.BIN" "$m/BOOT.BIN"
check "push: says what it did" grep -q "change(s)" <<< "$out"

# a second push: nothing to change
out=$(SD_SYNC_TEST=1 tools/sd-sync.sh push "$m" "$t/card.img")
check "push again: no change" grep -q "; 0 change(s)" <<< "$out"

# the mount checks: a plain folder is not the card
SD_SYNC_TEST=0 tools/sd-sync.sh push "$m" "$t/card.img" 2>/dev/null
check "a folder that isn't a FAT mount point: refused" test $? -ne 0
SD_SYNC_TEST=0 tools/sd-sync.sh pull "$t/nowhere" a b c 2>/dev/null
check "no such folder: refused" test $? -ne 0
check "mount: SD_MOUNT as given" test "$(SD_MOUNT=/x/y tools/sd-sync.sh mount)" = /x/y

# make sd-sync gets every system's folders as make sd does: each
# `sd: export X` line in the Makefile has its `sd-sync: export X` (a card
# written by make sd-sync once lacked the GBC, Game Gear, Master System and
# 800XL folders)
sd_vars=$(grep -oE '^sd: export [A-Z0-9_]+' Makefile | awk '{print $3}' | sort)
sync_vars=$(grep -oE '^sd-sync: export [A-Z0-9_]+' Makefile | awk '{print $3}' | sort)
check "make sd-sync exports every system's folders as make sd does ($(echo $sd_vars))" test "$sd_vars" = "$sync_vars"

if [ "$fails" -gt 0 ]; then echo "test_sd_sync: $fails failed" >&2; exit 1; fi
echo "test_sd_sync: all passed"
