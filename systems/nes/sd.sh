# The NES's part of the SD card image (side track K, D4:
# one fragment per system, its track's file). Sourced by tools/mksd.sh,
# in sorted order, after the image is made: it sees "$img", "$src"
# (CHIP8_DIR), the arguments (GB_DIR...) and the environment mksd.sh
# documents. Not a script of its own.

# NES (M3 step 3.8): each folder in NES_DIRS (space-separated, `make sd`
# passes SD_NES_ROMS) goes into /nes as a folder of its own name.
if [ -n "${NES_DIRS:-}" ]; then
    mmd -i "$img" ::/nes
    for d in $NES_DIRS; do
        if [ ! -d "$d" ]; then
            echo "mksd.sh: no folder $d, skipped" >&2
            continue
        fi
        name=$(basename "$d")
        mmd -i "$img" "::/nes/$name"
        mcopy -s -Q -i "$img" "$d"/* "::/nes/$name/"
        echo "mksd.sh: $(find -L "$d" -type f | wc -l) file(s) from $d in /nes/$name"
    done
fi
