# The Game Boy's part of the SD card image (side track K, D4:
# one fragment per system, its track's file). Sourced by tools/mksd.sh,
# in sorted order, after the image is made: it sees "$img", "$src"
# (CHIP8_DIR), the arguments (GB_DIR...) and the environment mksd.sh
# documents. Not a script of its own.

if [ $# -gt 0 ]; then
    mmd -i "$img" ::/gb
    for d in "$@"; do
        if [ ! -d "$d" ]; then
            echo "mksd.sh: no folder $d, skipped" >&2
            continue
        fi
        name=$(basename "$d")
        mmd -i "$img" "::/gb/$name"
        mcopy -s -Q -i "$img" "$d"/* "::/gb/$name/"
        echo "mksd.sh: $(find -L "$d" -type f | wc -l) file(s) from $d in /gb/$name"
    done
fi

# Side track I: battery saves. With LINK set (the desktop link; `make sd`
# passes it), every Game Boy cartridge with battery-backed RAM gets its
# save file at full size, /saves/gb/<folder>/<path>/<name>.sav (`link
# --save-slots` asks the PS app's own system table, platform/ps/menu.zig,
# which ROMs and how big). The board rewrites those blocks in place and
# never the FAT, so only these files can hold saves. Each one starts as
# the stored save in SAVES_DIR (`make saves-pull` keeps them there) if
# one of the right size is there, else as zeros (a blank save).
if [ -n "${LINK:-}" ] && [ $# -gt 0 ]; then
    stage=$(mktemp -d)
    slots=0 restored=0
    for d in "$@"; do
        [ -d "$d" ] || continue
        name=$(basename "$d")
        while IFS=$'\t' read -r rel size; do
            sav="gb/$name/${rel%.*}.sav"
            mkdir -p "$stage/saves/$(dirname "$sav")"
            stored="${SAVES_DIR:-}/$sav"
            if [ -n "${SAVES_DIR:-}" ] && [ -f "$stored" ]; then
                if [ "$(stat -L -c %s "$stored")" = "$size" ]; then
                    cp "$stored" "$stage/saves/$sav"
                    restored=$((restored + 1))
                else
                    echo "mksd.sh: $stored is not $size bytes: not used, /saves/$sav starts blank" >&2
                    truncate -s "$size" "$stage/saves/$sav"
                fi
            else
                truncate -s "$size" "$stage/saves/$sav"
            fi
            slots=$((slots + 1))
        done < <("$LINK" --system gb --save-slots "$d")
    done
    if [ "$slots" -gt 0 ]; then
        # (into /saves, made here or by another system's part: D4's
        # fragments run in any order)
        mdir -i "$img" ::/saves >/dev/null 2>&1 || mmd -i "$img" ::/saves
        mcopy -s -Q -i "$img" "$stage/saves/gb" ::/saves/
        echo "mksd.sh: $slots save file(s) in /saves ($restored from ${SAVES_DIR:-nowhere}, the rest blank)"
    fi
    rm -rf "$stage"
fi
