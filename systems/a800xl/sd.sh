# The Atari 800XL's part of the SD card image (side track K, D4:
# one fragment per system, its track's file). Sourced by tools/mksd.sh,
# in sorted order, after the image is made: it sees "$img", "$src"
# (CHIP8_DIR), the arguments (GB_DIR...) and the environment mksd.sh
# documents. Not a script of its own.

# Atari 800XL (M4 step 4.8): each folder in A800_DIRS (space-separated,
# `make sd` exports SD_A800_ROMS) goes into /a800xl as a folder of its own
# name.
if [ -n "${A800_DIRS:-}" ]; then
    mmd -i "$img" ::/a800xl
    for d in $A800_DIRS; do
        if [ ! -d "$d" ]; then
            echo "mksd.sh: no folder $d, skipped" >&2
            continue
        fi
        name=$(basename "$d")
        mmd -i "$img" "::/a800xl/$name"
        mcopy -s -Q -i "$img" "$d"/* "::/a800xl/$name/"
        echo "mksd.sh: $(find -L "$d" -type f | wc -l) file(s) from $d in /a800xl/$name"
    done
fi
