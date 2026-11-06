#!/usr/bin/env bash
# Test images for the PS app's FAT reader (platform/ps/fat.zig):
#
#   tools/mkfat-test.sh [DIR]      (default build/ps-test)
#
# Made with the real tools (mkfs.fat, mtools, sfdisk), so the reader is
# checked against an independent implementation:
#
#   fat12.img  1.44 MB superfloppy (no partition table), FAT12, /CHIP8
#   fat16.img  16 MiB, MBR partition at LBA 2048 (type 0x06), FAT16, /chip8
#   fat32.img  48 MiB, MBR partition at LBA 2048 (type 0x0C), FAT32 with one
#              sector per cluster, /Chip8 and /Chip8/many (100 files, so
#              the directory spans many clusters)
#
# Every image holds the same CHIP-8 directory (long names, an upper-case
# 8.3 name, a lower-case 8.3 name, a subdirectory), /BIG.BIN (many
# clusters) and /FRAG.BIN (written into the hole left by a deleted file, so
# its cluster chain isn't contiguous). File contents are byte i =
# (i * m + a) & 0xFF; fat.zig's tests regenerate them. Keep the two in step.
set -euo pipefail

out=${1:-build/ps-test}
mkdir -p "$out"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export MTOOLS_SKIP_CHECK=1

# pattern <file> <length> <m> <a>
pattern() {
    python3 -c 'import sys; n, m, a = map(int, sys.argv[2:]); open(sys.argv[1], "wb").write(bytes((i * m + a) & 255 for i in range(n)))' "$@"
}

# populate <mtools image spec> <chip8 dir name> <big.bin length>
populate() {
    local img=$1 dir=$2 big=$3
    local src="$tmp/src"
    rm -rf "$src"
    mkdir -p "$src"
    pattern "$src/Space Invaders [David Winter].ch8" 1000 7 3
    pattern "$src/PONG.CH8" 246 13 5
    pattern "$src/brix.ch8" 280 3 1
    pattern "$src/Tetris [Fran Dachille, 1991].ch8" 494 7 3
    pattern "$src/BIG.BIN" "$big" 7 3
    pattern "$src/A.BIN" 1536 1 0
    pattern "$src/B.BIN" 512 1 0
    pattern "$src/FRAG.BIN" 5000 13 5

    mmd -i "$img" "::/$dir" "::/$dir/games"
    mcopy -i "$img" "$src/Space Invaders [David Winter].ch8" "$src/PONG.CH8" "$src/brix.ch8" "::/$dir/"
    mcopy -i "$img" "$src/Tetris [Fran Dachille, 1991].ch8" "::/$dir/games/"
    mcopy -i "$img" "$src/BIG.BIN" "::/"
    # fragmentation: A, B, delete A, then FRAG fills A's hole and continues
    # after B
    mcopy -i "$img" "$src/A.BIN" "::/"
    mcopy -i "$img" "$src/B.BIN" "::/"
    mdel -i "$img" "::/A.BIN"
    # FAT32: mtools allocates after FSInfo's "next free" hint, which would
    # skip the hole; set the hint to unknown (0xFFFFFFFF, fatgen103 section 5)
    if [ -n "${fsinfo:-}" ]; then
        printf '\xff\xff\xff\xff' | dd of="${img%@@*}" bs=1 seek="$fsinfo" conv=notrunc status=none
    fi
    mcopy -i "$img" "$src/FRAG.BIN" "::/"
}

# --- FAT12 superfloppy ------------------------------------------------------
rm -f "$out/fat12.img"
mkfs.fat -C -F 12 -n FAT12TEST "$out/fat12.img" 1440 >/dev/null
populate "$out/fat12.img" CHIP8 20000

# --- FAT16 in an MBR partition ----------------------------------------------
rm -f "$out/fat16.img"
truncate -s 16M "$out/fat16.img"
echo 'start=2048, type=6' | sfdisk -q "$out/fat16.img"
mkfs.fat -F 16 -s 2 -n FAT16TEST --offset 2048 "$out/fat16.img" >/dev/null
populate "$out/fat16.img@@1M" chip8 60000

# --- FAT32 in an MBR partition ----------------------------------------------
rm -f "$out/fat32.img"
truncate -s 48M "$out/fat32.img"
echo 'start=2048, type=c' | sfdisk -q "$out/fat32.img"
mkfs.fat -F 32 -s 1 -n FAT32TEST --offset 2048 "$out/fat32.img" >/dev/null
# FSInfo is sector 1 of the volume (BPB_FSInfo); its next-free field is at 492
fsinfo=$((1048576 + 512 + 492)) populate "$out/fat32.img@@1M" Chip8 200000
mkdir -p "$tmp/many"
for i in $(seq -f '%03g' 0 99); do printf 'game %s\n' "$i" > "$tmp/many/game number $i.ch8"; done
mmd -i "$out/fat32.img@@1M" "::/Chip8/many"
mcopy -i "$out/fat32.img@@1M" "$tmp"/many/*.ch8 "::/Chip8/many/"

# The Master System's companion file (M6 step 6.7, menu.zig `companion`):
# /sms holds a BIOS of a good size (32 KiB) and a cartridge, /smsbad a BIOS
# of a size the loader refuses (20000 bytes), /smsnone a cartridge alone.
pattern "$tmp/BIOS.BIN" 32768 11 7
pattern "$tmp/GAME.SMS" 65536 5 1
pattern "$tmp/BAD.SMS" 20000 3 2
mmd -i "$out/fat32.img@@1M" "::/sms" "::/smsbad" "::/smsnone"
mcopy -i "$out/fat32.img@@1M" "$tmp/BIOS.BIN" "$tmp/GAME.SMS" "::/sms/"
mcopy -i "$out/fat32.img@@1M" "$tmp/GAME.SMS" "::/smsbad/"
mcopy -i "$out/fat32.img@@1M" "$tmp/BAD.SMS" "::/smsbad/BIOS.BIN"
mcopy -i "$out/fat32.img@@1M" "$tmp/GAME.SMS" "::/smsnone/"

# --- Broken images ----------------------------------------------------------
pattern "$out/garbage.img" 65536 97 11
head -c 4096 "$out/fat16.img" > "$out/truncated.img"

echo "mkfat-test: images in $out"
