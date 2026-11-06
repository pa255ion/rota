# The Game Gear's part of the SD card image (side track K, D4:
# one fragment per system, its track's file). Sourced by tools/mksd.sh,
# in sorted order, after the image is made: it sees "$img", "$src"
# (CHIP8_DIR), the arguments (GB_DIR...) and the environment mksd.sh
# documents. Not a script of its own.

# The Game Gear (M5 step 5.9): each folder in GG_DIRS (space-separated,
# `make sd` exports SD_GG_ROMS) goes into /gg as a folder of its own name.
if [ -n "${GG_DIRS:-}" ]; then
    mmd -i "$img" ::/gg
    for d in $GG_DIRS; do
        if [ ! -d "$d" ]; then
            echo "mksd.sh: no folder $d, skipped" >&2
            continue
        fi
        name=$(basename "$d")
        mmd -i "$img" "::/gg/$name"
        mcopy -s -Q -i "$img" "$d"/* "::/gg/$name/"
        echo "mksd.sh: $(find -L "$d" -type f | wc -l) file(s) from $d in /gg/$name"
    done
fi

# The Game Gear's battery saves (M5 step 5.10), as the Game Boy's: each
# ROM in GG_DIRS gets a 32 KiB save file (its cartridge RAM; `link
# --system gg --save-slots`), /saves/gg/<folder>/<path>/<name>.sav, from
# SAVES_DIR when a stored one of that size is there, else blank.
if [ -n "${LINK:-}" ] && [ -n "${GG_DIRS:-}" ]; then
    stage=$(mktemp -d)
    slots=0 restored=0
    for d in $GG_DIRS; do
        [ -d "$d" ] || continue
        name=$(basename "$d")
        while IFS=$'\t' read -r rel size; do
            sav="gg/$name/${rel%.*}.sav"
            mkdir -p "$stage/saves/$(dirname "$sav")"
            stored="${SAVES_DIR:-}/$sav"
            if [ -n "${SAVES_DIR:-}" ] && [ -f "$stored" ] && [ "$(stat -L -c %s "$stored")" = "$size" ]; then
                cp "$stored" "$stage/saves/$sav"
                restored=$((restored + 1))
            else
                truncate -s "$size" "$stage/saves/$sav"
            fi
            slots=$((slots + 1))
        done < <("$LINK" --system gg --save-slots "$d")
    done
    if [ "$slots" -gt 0 ]; then
        mdir -i "$img" ::/saves >/dev/null 2>&1 || mmd -i "$img" ::/saves
        mcopy -s -Q -i "$img" "$stage/saves/gg" ::/saves/
        echo "mksd.sh: $slots Game Gear save file(s) in /saves/gg ($restored from ${SAVES_DIR:-nowhere}, the rest blank)"
    fi
    rm -rf "$stage"
fi
