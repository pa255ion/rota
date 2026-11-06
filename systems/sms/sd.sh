# The Master System's part of the SD card image (side track K, D4:
# one fragment per system, its track's file). Sourced by tools/mksd.sh,
# in sorted order, after the image is made: it sees "$img", "$src"
# (CHIP8_DIR), the arguments (GB_DIR...) and the environment mksd.sh
# documents. Not a script of its own.

# The Master System (M6 step 6.7), as the Game Gear: each folder in
# SMS_DIRS (`make sd` exports SD_SMS_ROMS) into /sms as a folder of its own
# name, and SMS_BIOS (the user's own BIOS dump, from wherever they keep it:
# never fetched or committed) as /sms/bios.bin when given.
if [ -n "${SMS_DIRS:-}" ] || [ -n "${SMS_BIOS:-}" ]; then
    mmd -i "$img" ::/sms
    for d in ${SMS_DIRS:-}; do
        if [ ! -d "$d" ]; then
            echo "mksd.sh: no folder $d, skipped" >&2
            continue
        fi
        name=$(basename "$d")
        mmd -i "$img" "::/sms/$name"
        mcopy -s -Q -i "$img" "$d"/* "::/sms/$name/"
        echo "mksd.sh: $(find -L "$d" -type f | wc -l) file(s) from $d in /sms/$name"
    done
    if [ -n "${SMS_BIOS:-}" ]; then
        # the PS app's own rule (platform/ps/systems/sms_cart.zig, check): 16 KiB
        # times a power of two, up to 256 KiB; a bad dump stops here, before
        # make sd writes the card, not in the menu
        size=$(stat -L -c %s "$SMS_BIOS")
        banks=$((size / 16384))
        if [ "$size" -eq 0 ] || [ $((size % 16384)) -ne 0 ] || [ $((banks & (banks - 1))) -ne 0 ] || [ "$size" -gt 262144 ]; then
            echo "mksd.sh: $SMS_BIOS: $size bytes; a Master System BIOS is 16 KiB x 2^n, up to 256 KiB" >&2
            exit 1
        fi
        mcopy -i "$img" "$SMS_BIOS" ::/sms/bios.bin
        echo "mksd.sh: the Master System BIOS $SMS_BIOS as /sms/bios.bin"
    fi
fi

# The Master System's battery saves (M6), as the Game Gear's.
if [ -n "${LINK:-}" ] && [ -n "${SMS_DIRS:-}" ]; then
    stage=$(mktemp -d)
    slots=0 restored=0
    for d in $SMS_DIRS; do
        [ -d "$d" ] || continue
        name=$(basename "$d")
        while IFS=$'\t' read -r rel size; do
            sav="sms/$name/${rel%.*}.sav"
            mkdir -p "$stage/saves/$(dirname "$sav")"
            stored="${SAVES_DIR:-}/$sav"
            if [ -n "${SAVES_DIR:-}" ] && [ -f "$stored" ] && [ "$(stat -L -c %s "$stored")" = "$size" ]; then
                cp "$stored" "$stage/saves/$sav"
                restored=$((restored + 1))
            else
                truncate -s "$size" "$stage/saves/$sav"
            fi
            slots=$((slots + 1))
        done < <("$LINK" --system sms --save-slots "$d")
    done
    if [ "$slots" -gt 0 ]; then
        mdir -i "$img" ::/saves >/dev/null 2>&1 || mmd -i "$img" ::/saves
        mcopy -s -Q -i "$img" "$stage/saves/sms" ::/saves/
        echo "mksd.sh: $slots Master System save file(s) in /saves/sms ($restored from ${SAVES_DIR:-nowhere}, the rest blank)"
    fi
    rm -rf "$stage"
fi
