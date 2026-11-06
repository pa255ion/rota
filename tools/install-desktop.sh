#!/usr/bin/env bash
# make install-desktop (side track Q, the user 2026-11-01): the desktop
# emulator installed outside the repo, runnable from anywhere.
#
#   tools/install-desktop.sh PREFIX BINDIR SDL3_LIBDIR LIB...
#
# Run by the Makefile inside the dev shell, after the libraries are built.
# PREFIX gets the front end, the six system libraries with the project's
# own Haskell libraries they need (their RUNPATH's build-tree entries
# rewritten to $ORIGIN), and the files the desktop finds by default (the
# 800XL's OS, BASIC and loader, the SNES's IPL). BINDIR gets the command,
# `rota`, and the debugger `rota-dbg`.
#
# NixOS: the libraries and SDL3 come from the Nix store (the dev shell's).
# A `nix-collect-garbage` would delete them, so PREFIX/nix-profile is a GC
# root of the dev shell (`nix develop --profile`), replaced at each install.
# Rerun this to update; nothing outside PREFIX, BINDIR/rota and
# BINDIR/rota-dbg is written.
set -euo pipefail

prefix=$1 bindir=$2 sdl=$3
shift 3
repo=$(pwd)
new=$prefix.new
rm -rf "$new"
mkdir -p "$new/lib" "$new/data"

# the system libraries and the project's libraries they load (those ldd
# finds in this repo's build tree; the rest are the Nix store's)
for lib in "$@"; do
  [ -f "$lib" ] || { echo "install-desktop: $lib missing (make desktop-lib-llvm)" >&2; exit 1; }
  cp "$lib" "$new/lib/"
  ldd "$lib" | awk -v r="$repo/" 'index($3, r) == 1 { print $3 }' | while read -r dep; do
    [ -e "$new/lib/$(basename "$dep")" ] || cp "$dep" "$new/lib/"
  done
done
# RUNPATH: $ORIGIN for the copies, the Nix store's entries as they were
for so in "$new"/lib/*.so; do
  rp=$(patchelf --print-rpath "$so" | tr ':' '\n' | grep -v "^$repo/" | paste -sd: -)
  patchelf --set-rpath "\$ORIGIN${rp:+:$rp}" "$so"
done
# every copy loads with nothing from the repo and nothing missing
for so in "$new"/lib/*.so; do
  if ldd "$so" | grep -e "not found" -e "$repo/" >&2; then
    echo "install-desktop: $so still needs the above" >&2
    exit 1
  fi
done

# the files the desktop reads by default (the 800XL's ROMs are your own
# fetched copies: tools/fetch-roms.sh)
for f in roms/a800xl/os/altirraos_xl.rom roms/a800xl/os/altirra_basic.rom build/a800xl/xexload.bin build/snes/ipl.bin; do
  if [ -f "$f" ]; then
    mkdir -p "$new/data/$(dirname "$f")"
    cp "$f" "$new/data/$f"
  else
    echo "install-desktop: no $f (that system needs --os/--basic/--ipl then)" >&2
  fi
done

# the front end, its libraries and data where they will be
libs=()
for sys in gb nes chip8 sms a800xl snes; do
  opt=$sys; [ "$sys" = a800xl ] && opt=a800
  libs+=("-D$opt-lib=$prefix/lib/lib$sys-desktop.so")
done
zig build --cache-dir build/zig-cache desktop "${libs[@]}" -Ddesktop-data="$prefix/data" -Dsdl3-libdir="$sdl" --prefix "$new"
# (its RUNPATH: SDL3's store path; the dev shell's linker flags add an
# entry under the repo, dropped)
rp=$(patchelf --print-rpath "$new/desktop" | tr ':' '\n' | grep -v "^$repo/" | paste -sd: -)
patchelf --set-rpath "$rp" "$new/desktop"

# in place of the old one
rm -rf "$prefix.old"
[ -e "$prefix" ] && mv "$prefix" "$prefix.old"
mv "$new" "$prefix"
rm -rf "$prefix.old"

# the GC root (made at the final path: the root is its symlink)
nix develop "$repo" --profile "$prefix/nix-profile" -c true

mkdir -p "$bindir"
# (the command was emulation-clash before 2026-11-01: an old one of ours goes)
if [ -f "$bindir/emulation-clash" ] && grep -q "make install-desktop" "$bindir/emulation-clash"; then
  rm "$bindir/emulation-clash"
fi
cat > "$bindir/rota" <<EOF
#!/bin/sh
# the desktop emulator ($repo, make install-desktop)
exec env -u LD_LIBRARY_PATH "$prefix/desktop" --saves "\${SAVES_DIR:-\${XDG_DATA_HOME:-\$HOME/.local/share}/emulation_clash/saves}" "\$@"
EOF
chmod +x "$bindir/rota"
# the debugger in a terminal (side track R): rota --debug GAME, then rota-dbg
ln -sf "$prefix/rota-dbg" "$bindir/rota-dbg"

# a check: the installed command loads every library and runs a few frames
for rom in build/gb/asm/save.gb roms/gb/games/libbet.gb; do
  if [ -f "$rom" ]; then
    SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy "$bindir/rota" --no-save --keymap /dev/null --unpaced --frames 5 "$rom" >/dev/null 2>&1 \
      || { echo "install-desktop: the installed command failed on $rom" >&2; exit 1; }
    break
  fi
done
echo "install-desktop: $bindir/rota -> $prefix ($(git -C "$repo" log -1 --format=%h))"
case ":$PATH:" in
  *":$bindir:"*) ;;
  *) echo "install-desktop: $bindir is not on your PATH; on NixOS: environment.localBinInPath = true; (NixOS) or home.sessionPath = [ \"$bindir\" ]; (home-manager)" >&2 ;;
esac
