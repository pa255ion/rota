#!/bin/sh
# side track W: the browser build. Run inside web/'s dev shell:
#   nix develop path:. -c ./build.sh [SYSTEM ...]
# (path: — web/ is its own flake, not the repository's). Writes
# www/rota-<system>.wasm for each system named (default: all of them).
set -eu
cd "$(dirname "$0")"
systems=${*:-sms gb chip8 nes a800 snes}
# (BUILDDIR: another build folder, e.g. for another GHC's dev shell)
bd="--builddir=${BUILDDIR:-dist-newstyle}"
wasm32-wasi-cabal update >/dev/null
[ -d tools/node_modules ] || (cd tools && npm ci --no-audit --no-fund >/dev/null)
for s in $systems; do
  # neither cabal nor GHC links again when only the export list (-optl)
  # changed: a .cabal file newer than the binary rebuilds that system whole
  old=$(wasm32-wasi-cabal "$bd" list-bin -v0 "rota-$s" 2>/dev/null) || old=
  if [ -n "$old" ] && { [ ! -f "$old" ] || [ rota-web.cabal -nt "$old" ]; }; then
    rm -rf "$(dirname "$(dirname "$(dirname "$old")")")"
  fi
  wasm32-wasi-cabal "$bd" build -v1 "rota-$s"
  # GHC's returns to its trampoline (StgRun) made WebAssembly tail calls
  # (tools/tailcall.mjs: about 1.5x faster)
  node tools/tailcall.mjs "$(wasm32-wasi-cabal "$bd" list-bin -v0 "rota-$s")" "www/rota-$s.wasm"
done
ls -la www/*.wasm
