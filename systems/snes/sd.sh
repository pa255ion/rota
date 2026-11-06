# The Super Nintendo's part of the SD card image (side track K, D4:
# one fragment per system, its track's file). Sourced by tools/mksd.sh,
# in sorted order, after the image is made: it sees "$img", "$src"
# (CHIP8_DIR), the arguments (GB_DIR...) and the environment mksd.sh
# documents. Not a script of its own.

# The Super Nintendo (M7 step 7.11), as the Master System: each folder in
# SNES_DIRS (`make sd` exports SD_SNES_ROMS) into /snes as a folder of its
# own name.
if [ -n "${SNES_DIRS:-}" ]; then
    mmd -i "$img" ::/snes
    for d in $SNES_DIRS; do
        if [ ! -d "$d" ]; then
            echo "mksd.sh: no folder $d, skipped" >&2
            continue
        fi
        name=$(basename "$d")
        mmd -i "$img" "::/snes/$name"
        mcopy -s -Q -i "$img" "$d"/* "::/snes/$name/"
        echo "mksd.sh: $(find -L "$d" -type f | wc -l) file(s) from $d in /snes/$name"
    done
fi

# The Super Nintendo's battery saves (SRAM), as the Master System's: a slot
# per ROM (link --save-slots), the stored save when its size matches.
if [ -n "${LINK:-}" ] && [ -n "${SNES_DIRS:-}" ]; then
    stage=$(mktemp -d)
    slots=0 restored=0
    for d in $SNES_DIRS; do
        [ -d "$d" ] || continue
        name=$(basename "$d")
        while IFS=$'\t' read -r rel size; do
            sav="snes/$name/${rel%.*}.sav"
            mkdir -p "$stage/saves/$(dirname "$sav")"
            stored="${SAVES_DIR:-}/$sav"
            if [ -n "${SAVES_DIR:-}" ] && [ -f "$stored" ] && [ "$(stat -L -c %s "$stored")" = "$size" ]; then
                cp "$stored" "$stage/saves/$sav"
                restored=$((restored + 1))
            else
                truncate -s "$size" "$stage/saves/$sav"
            fi
            slots=$((slots + 1))
        done < <("$LINK" --system snes --save-slots "$d")
    done
    if [ "$slots" -gt 0 ]; then
        mdir -i "$img" ::/saves >/dev/null 2>&1 || mmd -i "$img" ::/saves
        mcopy -s -Q -i "$img" "$stage/saves/snes" ::/saves/
        echo "mksd.sh: $slots Super Nintendo save file(s) in /saves/snes ($restored from ${SAVES_DIR:-nowhere}, the rest blank)"
    fi
    rm -rf "$stage"
fi
