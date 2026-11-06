#!/usr/bin/env bash
# The SD card through the desktop's card reader (side track K, the user's
# request 2026-10-16): make sd-sync. Instead of an image sent through the
# board (make sd), the card is mounted by the desktop and its files kept
# in step with an image tools/mksd.sh built: only what changed is copied,
# what is gone removed, and the whole card is usable. The card must be
# FAT32 (or FAT16) in an MBR partition: the board's reader (fat.zig) takes
# the first FAT partition; exFAT (the default on cards over 32 GB) won't do.
#
#   tools/sd-sync.sh mount                print the card's mount point: SD_MOUNT,
#                                         else the only vfat mount under
#                                         /run/media or /media
#   tools/sd-sync.sh pull MOUNT SAVES STATES MOVIES [SCREENS]
#                                         the card's /saves, /states, /movies
#                                         (and /screens)
#                                         into those folders (what make sd
#                                         pulls through the board first)
#   tools/sd-sync.sh push MOUNT IMAGE     the image's files onto the card
#
# SD_SYNC_TEST=1 skips the checks that MOUNT is a mounted vfat file system
# (tools/test_sd_sync.sh runs on a plain folder).
set -euo pipefail
export MTOOLS_SKIP_CHECK=1

die() { echo "sd-sync: $*" >&2; exit 1; }

# folders desktop systems put on a card: never removed, never ours
keep=(--exclude '/System Volume Information' --exclude '/.Trash-*' --exclude '/.Spotlight-V100'
      --exclude '/.fseventsd' --exclude '/.Trashes' --exclude '/.metadata_never_index')

check_mount() {
    local m=$1
    [ -d "$m" ] || die "$m: no such folder (is the card mounted? SD_MOUNT=...)"
    [ -w "$m" ] || die "$m: not writable"
    [ "${SD_SYNC_TEST:-0}" = 1 ] && return 0
    local target fstype
    target=$(findmnt -n -o TARGET --target "$m") || die "$m: not on a mounted file system"
    [ "$target" = "$(realpath "$m")" ] || die "$m is a folder on $target, not the card's mount point"
    fstype=$(findmnt -n -o FSTYPE --target "$m")
    [ "$fstype" = vfat ] || die "$m is $fstype, not FAT (vfat): the board reads FAT32 only (format the card FAT32: one MBR partition)"
}

case ${1:-} in
mount)
    if [ -n "${SD_MOUNT:-}" ]; then echo "$SD_MOUNT"; exit 0; fi
    mapfile -t found < <(findmnt -rn -t vfat -o TARGET | grep -E '^/(run/)?media/' || true)
    case ${#found[@]} in
        1) echo "${found[0]}" ;;
        0) die "no FAT card mounted under /run/media or /media: mount it, or SD_MOUNT=<its folder>" ;;
        *) die "${#found[@]} FAT file systems mounted (${found[*]}): SD_MOUNT=<the card's folder>" ;;
    esac
    ;;
pull)
    m=${2:?usage: sd-sync.sh pull MOUNT SAVES STATES MOVIES [SCREENS]}
    check_mount "$m"
    n=0
    for pair in "saves:${3:?}" "states:${4:?}" "movies:${5:?}" ${6:+"screens:$6"}; do
        dir=${pair%%:*} dst=${pair#*:}
        # any case on the card (FAT): the folder as the board names it, or not
        src=$(find "$m" -maxdepth 1 -iname "$dir" -type d | head -1)
        [ -n "$src" ] || continue
        mkdir -p "$dst"
        rsync -rt --modify-window=2 --no-perms --no-owner --no-group "$src/" "$dst/"
        n=$((n + $(find "$src" -type f | wc -l)))
        echo "sd-sync: the card's /$dir into $dst"
    done
    echo "sd-sync: pulled $n file(s) off the card"
    ;;
push)
    m=${2:?usage: sd-sync.sh push MOUNT IMAGE}
    img=${3:?usage: sd-sync.sh push MOUNT IMAGE}
    check_mount "$m"
    [ -f "$img" ] || die "$img: no such image"
    stage=$(mktemp -d)
    trap 'rm -rf "$stage"' EXIT
    mcopy -s -n -Q -m -i "$img@@1M" '::/*' "$stage/"
    [ -f "$stage/BOOT.BIN" ] || [ -f "$stage/boot.bin" ] || echo "sd-sync: warning: the image has no BOOT.BIN (the board won't boot from this card)" >&2
    changes=$(rsync -rt --delete --modify-window=2 --no-perms --no-owner --no-group --itemize-changes "${keep[@]}" "$stage/" "$m/" | grep -c '' || true)
    sync -f "$m" 2>/dev/null || sync
    echo "sd-sync: $m now holds the image's $(find "$stage" -type f | wc -l) file(s) ($(du -sh "$stage" | cut -f1)); $changes change(s); written to the card (sync)"
    ;;
*)
    die "usage: tools/sd-sync.sh mount | pull MOUNT SAVES STATES MOVIES | push MOUNT IMAGE"
    ;;
esac
