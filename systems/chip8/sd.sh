# CHIP-8's part of the SD card image (side track K, D4:
# one fragment per system, its track's file). Sourced by tools/mksd.sh,
# in sorted order, after the image is made: it sees "$img", "$src"
# (CHIP8_DIR), the arguments (GB_DIR...) and the environment mksd.sh
# documents. Not a script of its own.

mmd -i "$img" ::/chip8
# -s: subfolders too; -Q: stop at the first error
mcopy -s -Q -i "$img" "$src"/* ::/chip8/
echo "mksd.sh: $out (64 MiB, FAT32): $(find -L "$src" -type f | wc -l) file(s) from $src in /chip8"

# Side track J: CHIP8_DIRS="dir ..." go into /chip8 as folders of their
# own names (the boot splash's demo, /chip8/rota); missing ones skipped.
for d in ${CHIP8_DIRS:-}; do
    if [ ! -d "$d" ]; then
        echo "mksd.sh: no folder $d, skipped" >&2
        continue
    fi
    name=$(basename "$d")
    mmd -i "$img" "::/chip8/$name"
    mcopy -s -Q -i "$img" "$d"/* "::/chip8/$name/"
    echo "mksd.sh: $(find -L "$d" -type f | wc -l) file(s) from $d in /chip8/$name"
done
