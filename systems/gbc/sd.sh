# The Game Boy Color's part of the SD card image (side track K, D4:
# one fragment per system, its track's file). Sourced by tools/mksd.sh,
# in sorted order, after the image is made: it sees "$img", "$src"
# (CHIP8_DIR), the arguments (GB_DIR...) and the environment mksd.sh
# documents. Not a script of its own.

# The Game Boy Color's battery saves (side track I, phase 3), as the Game
# Boy's above: /saves/gbc/<folder>/<path>/<name>.sav for each folder of
# GBC_DIRS. The copies of the Game Boy's folders in /gbc/gb share the Game
# Boy's saves (/saves/gb/<folder>/..., made above; the PS app maps the
# path, platform/ps/save.zig): a GBC_GB_DIRS folder that isn't also a
# GB_DIR gets no save slot (a note says so).
if [ -n "${LINK:-}" ]; then
    for d in ${GBC_GB_DIRS:-}; do
        [ -d "$d" ] || continue
        shared=0
        for g in "$@"; do [ "$(basename "$g")" = "$(basename "$d")" ] && shared=1; done
        [ "$shared" = 1 ] || echo "mksd.sh: $d is in /gbc/gb but not in /gb: its games get no save slot (add it to the Game Boy's folders)" >&2
    done
fi
if [ -n "${LINK:-}" ] && [ -n "${GBC_DIRS:-}" ]; then
    stage=$(mktemp -d)
    slots=0 restored=0
    for spec in $(for d in ${GBC_DIRS:-}; do echo "gbc:$d"; done); do
        top=${spec%%:*}
        d=${spec#*:}
        [ -d "$d" ] || continue
        name=$(basename "$d")
        while IFS=$'\t' read -r rel size; do
            sav="$top/$name/${rel%.*}.sav"
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
        done < <("$LINK" --system gbc --save-slots "$d")
    done
    if [ "$slots" -gt 0 ]; then
        # (into /saves, which the Game Boy's block may have made already)
        mdir -i "$img" ::/saves >/dev/null 2>&1 || mmd -i "$img" ::/saves
        mcopy -s -Q -i "$img" "$stage/saves/gbc" ::/saves/
        echo "mksd.sh: $slots Game Boy Color save file(s) in /saves/gbc ($restored from ${SAVES_DIR:-nowhere}, the rest blank)"
    fi
    rm -rf "$stage"
fi

# The Game Boy Color (side track I, phase 3): each folder in GBC_DIRS
# (space-separated, `make sd` passes SD_GBC_ROMS) goes into /gbc as a
# folder of its own name.
if [ -n "${GBC_DIRS:-}" ]; then
    mmd -i "$img" ::/gbc
    for d in $GBC_DIRS; do
        if [ ! -d "$d" ]; then
            echo "mksd.sh: no folder $d, skipped" >&2
            continue
        fi
        name=$(basename "$d")
        mmd -i "$img" "::/gbc/$name"
        mcopy -s -Q -i "$img" "$d"/* "::/gbc/$name/"
        echo "mksd.sh: $(find -L "$d" -type f | wc -l) file(s) from $d in /gbc/$name"
    done
fi
# ... and each folder in GBC_GB_DIRS (`make sd` passes the Game Boy's,
# SD_GB_ROMS) into /gbc/gb as a folder of its own name: DMG cartridges for
# the Game Boy Color (compatibility mode, or the colours of a cartridge made
# for both). Copies, as the menu lists one folder per system.
if [ -n "${GBC_GB_DIRS:-}" ]; then
    # (mmd asks on the terminal if the folder exists: create /gbc only if
    # GBC_DIRS didn't)
    mdir -i "$img" ::/gbc >/dev/null 2>&1 || mmd -i "$img" ::/gbc
    mmd -i "$img" ::/gbc/gb
    for d in $GBC_GB_DIRS; do
        if [ ! -d "$d" ]; then
            echo "mksd.sh: no folder $d, skipped" >&2
            continue
        fi
        name=$(basename "$d")
        mmd -i "$img" "::/gbc/gb/$name"
        mcopy -s -Q -i "$img" "$d"/* "::/gbc/gb/$name/"
        echo "mksd.sh: $(find -L "$d" -type f | wc -l) file(s) from $d in /gbc/gb/$name"
    done
fi
