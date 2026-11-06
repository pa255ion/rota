#!/usr/bin/env bash
# An SD card image for the board's ROM menu, written to the card by the
# board itself (`make sd`: platform/host/link.zig --sd-image, through the
# PS relay), for users without a card reader:
#
#   [NES_DIRS="dir ..."] [CHIP8_DIRS="dir ..."] [GBC_DIRS="dir ..."] [GBC_GB_DIRS="dir ..."] [GG_DIRS="dir ..."] [BOOT_BIN=BOOT.BIN] [SYSTEM_BITS="name=f.bit ..."]
#   [BOOT_FILES="file ..."]
#   [KEYMAP_CONF=keymap.conf] [LINK=build/host/link [SAVES_DIR=dir]]
#   tools/mksd.sh OUT.img [CHIP8_DIR [GB_DIR...]]
#
# 64 MiB: an MBR with one FAT32 partition (type 0x0C) from 1 MiB, one
# sector per cluster (the smallest FAT32 that size allows), label ROMS.
# CHIP8_DIR's contents (default roms/chip8), subfolders included, go into
# /chip8; each GB_DIR goes into /gb as a folder of its own name (default
# none; `make sd` passes SD_GB_ROMS). Folders that don't exist are skipped
# with a note. Made with the real tools (sfdisk, mkfs.fat, mtools). Any PC
# reads the card afterwards; the rest of the card past 64 MiB is left
# unused.
set -euo pipefail

out=${1:?usage: tools/mksd.sh OUT.img [CHIP8_DIR [GB_DIR...]]}
src=${2:-roms/chip8}
shift $(( $# >= 2 ? 2 : $# ))
test -d "$src" || { echo "mksd.sh: no ROM folder $src (tools/fetch-roms.sh?)" >&2; exit 1; }
export MTOOLS_SKIP_CHECK=1

mkdir -p "$(dirname "$out")"
rm -f "$out"
# SD_MB: the image's size (default 64; make sd sends it through the board,
# make sd-sync copies its files onto a mounted card of any size). One
# sector a cluster up to 256 MiB (small files waste little); above,
# mkfs.fat's own choice (a FAT of one sector a cluster would be huge)
mb=${SD_MB:-64}
truncate -s "${mb}M" "$out"
echo 'start=2048, type=c' | sfdisk -q "$out"
if [ "$mb" -le 256 ]; then
    mkfs.fat -F 32 -s 1 -n ROMS --offset 2048 "$out" >/dev/null
else
    mkfs.fat -F 32 -n ROMS --offset 2048 "$out" >/dev/null
fi
img="$out@@1M"
# Each system's part of the card (side track K, D4): systems/<n>/sd.sh,
# its track's file, sourced in sorted order; each sees "$img", "$src", the
# arguments (GB_DIR...) and the environment above.
for part in $(LC_ALL=C ls "$(dirname "$0")"/../systems/*/sd.sh); do
    # shellcheck source=/dev/null
    . "$part"
done

# How old a build put on the card is (M7's request for the user,
# 2026-11-03: a card ran a two-week-old set unnoticed): a warning for a
# file built more than SD_OLD_DAYS days ago (default 7; 0: none), by its
# modification time. The board's menu shows the dates too (the PS app's
# mark, each system's on the Systems page).
old_build() {
    local days=${SD_OLD_DAYS:-7} age
    [ "$days" -gt 0 ] || return 0
    age=$(( ( $(date +%s) - $(stat -c %Y "$1") ) / 86400 ))
    if [ "$age" -gt "$days" ]; then
        echo "mksd.sh: warning: $1 was built $age days ago ($(date -r "$1" +%F)): $2" >&2
    fi
}

# Side track F: the boot image (BOOT_BIN, `make sd` passes SD_BOOT, made by
# `make boot-image`) into the root, where the boot ROM looks for it (UG585
# 6.3.7: the first partition, FAT16/32). Skipped with a note if missing.
if [ -n "${BOOT_BIN:-}" ]; then
    if [ -f "$BOOT_BIN" ]; then
        mcopy -Q -i "$img" "$BOOT_BIN" ::/BOOT.BIN
        echo "mksd.sh: $BOOT_BIN as /BOOT.BIN ($(stat -c %s "$BOOT_BIN") bytes)"
        old_build "$BOOT_BIN" "make boot-image for a new one"
    else
        echo "mksd.sh: no boot image $BOOT_BIN (make boot-image), skipped" >&2
    fi
fi

# Side track F: the systems' bitstreams (SYSTEM_BITS, space-separated
# NAME=FILE.bit; `make sd` passes SD_SYSTEMS' builds) into /systems as
# NAME.bit, in that order. Missing files are skipped. /systems/last.txt
# names the system the PS app starts at power-on (the first one here; the
# app rewrites it in place when another is chosen in its menu, so it is
# 32 bytes, the name padded with spaces: platform/ps/menu.zig, writeLast).
if [ -n "${SYSTEM_BITS:-}" ]; then
    mmd -i "$img" ::/systems
    first=
    for nb in $SYSTEM_BITS; do
        name=${nb%%=*} bit=${nb#*=}
        if [ ! -f "$bit" ]; then
            echo "mksd.sh: no bitstream $bit (make bit SYSTEM=$name), skipped" >&2
            continue
        fi
        mcopy -Q -i "$img" "$bit" "::/systems/$name.bit"
        echo "mksd.sh: $bit as /systems/$name.bit"
        old_build "$bit" "make bit SYSTEM=$name for a new one"
        first=${first:-$name}
    done
    if [ -n "$first" ]; then
        last=$(mktemp)
        printf '%-31s\n' "$first" > "$last"
        mcopy -Q -i "$img" "$last" ::/systems/last.txt
        rm -f "$last"
        echo "mksd.sh: /systems/last.txt: $first"
    fi
fi

# Side track J: the boot splash (BOOT_FILES, space-separated; `make sd`
# passes the demos as chip8.ch8, gb.gb, nes.nes) into /systems/boot, where
# the PS app looks for the started system's <name><ext> (platform/ps/
# splash.zig). Missing files are skipped.
if [ -n "${BOOT_FILES:-}" ]; then
    # (mmd asks on the terminal if the folder exists, as SYSTEM_BITS' does:
    # create /systems only if that didn't; demos/test-mksd.sh)
    mdir -i "$img" ::/systems >/dev/null 2>&1 || mmd -i "$img" ::/systems
    mmd -i "$img" ::/systems/boot
    for f in $BOOT_FILES; do
        if [ ! -f "$f" ]; then
            echo "mksd.sh: no boot file $f (make demos), skipped" >&2
            continue
        fi
        mcopy -Q -i "$img" "$f" "::/systems/boot/$(basename "$f")"
        echo "mksd.sh: $f as /systems/boot/$(basename "$f")"
    done
fi

# Save states and input movies (side tracks L and O): the board makes their
# files when it first needs them (side track K, F1: a state when one is
# saved, a movie when one records), so the card gets no empty slots. What
# the card had, pulled into STATES_DIR and MOVIES_DIR by make sd / make
# sd-sync first, goes back as it is (/states, /movies); none with MOVIES=0
# for the movies. The same for the screenshots (side track O, O.4:
# /screens, SCREENS_DIR).
for pair in "states:${STATES_DIR:-}" "movies:${MOVIES_DIR:-}" "screens:${SCREENS_DIR:-}"; do
    top=${pair%%:*} src=${pair#*:}
    [ "$top" = movies ] && [ "${MOVIES:-1}" = 0 ] && continue
    [ -n "$src" ] && [ -d "$src" ] && [ -n "$(ls -A "$src")" ] || continue
    stage=$(mktemp -d)
    cp -r "$src" "$stage/$top"
    mcopy -s -Q -i "$img" "$stage/$top" ::/
    echo "mksd.sh: $(find "$stage/$top" -type f | wc -l) file(s) from $src in /$top ($(du -sk --apparent-size "$stage/$top" | cut -f1) KiB)"
    rm -rf "$stage"
done

# Side track F: the key map (KEYMAP_CONF, `make sd` passes the desktop's
# keymap.conf, checked by link first) as /keymap.conf, which the PS app
# reads whenever the menu opens (side track E). Skipped if not set.
if [ -n "${KEYMAP_CONF:-}" ]; then
    if [ -f "$KEYMAP_CONF" ]; then
        mcopy -Q -i "$img" "$KEYMAP_CONF" ::/keymap.conf
        echo "mksd.sh: $KEYMAP_CONF as /keymap.conf"
    else
        echo "mksd.sh: no key map $KEYMAP_CONF, the board uses its defaults" >&2
    fi
fi

# Side track L: /settings.txt, the menu's volume kept on the card
# (platform/ps/options.zig): 64 bytes, "volume=10" padded with spaces and a
# newline at the end, so the PS app rewrites its one block in place.
settings=$(mktemp)
printf '%-63s\n' "volume=10" > "$settings"
mcopy -Q -i "$img" "$settings" ::/settings.txt
rm -f "$settings"
echo "mksd.sh: /settings.txt: volume=10"
