#!/bin/sh
# side track W, W.4: the page into a static site's checkout (the user's
# Neocities site), as the 4d project's tools/publish.sh:
#   web/publish.sh SITE_DIR [SUBDIR]        (SUBDIR defaults to rota)
# Needs the systems' www/rota-<lib>.wasm (./build.sh) and the splash ROMs
# (make demos: build/demos/rota.gg, rota.sms, rota.gb, our own). The commit is stamped into every
# URL the page loads, so caches never mix versions. Then commit the site
# and upload it with its ./deploy.sh (never `neocities push .`: it uploads
# .git/).
set -eu
site=${1:?usage: web/publish.sh SITE_DIR [SUBDIR]}
sub=${2:-rota}
cd "$(dirname "$0")"
libs="sms gb chip8 nes a800 snes"
splashes="rota.gg rota.sms rota.gb rota.nes rota.xex rota.sfc"
# the 800XL's OS and BASIC (Altirra's, all-permissive: www/altirra-notice.txt)
# and our loader (tools/fetch-roms.sh; make test builds xexload.bin); the
# Super Nintendo's boot ROM, our own (make snes-asm)
a8files="../roms/a800xl/os/altirraos_xl.rom ../roms/a800xl/os/altirra_basic.rom ../build/a800xl/xexload.bin ../build/snes/ipl.bin"
for f in $(for l in $libs; do echo "www/rota-$l.wasm"; done) $(for s in $splashes; do echo "../build/demos/$s"; done) $a8files; do
  [ -f "$f" ] || { echo "publish: no $f (./build.sh; make demos)" >&2; exit 1; }
done
version=$(git rev-parse --short HEAD)$(git diff --quiet HEAD -- . ../systems ../cpus ../platform/src || echo "-dirty")
dest="$site/$sub"
mkdir -p "$dest"
# (the W.1 build, the Game Gear alone, is gone: rota-sms runs it)
rm -f "$dest/rota-gg.wasm"
cp www/index.html www/wasi.js www/altirra-notice.txt $a8files "$dest/"
for l in $libs; do cp "www/rota-$l.wasm" "$dest/"; done
for s in $splashes; do cp "../build/demos/$s" "$dest/"; done
# the commit stamped into every URL a page or worker loads (caches never mix
# versions): the modules, the worker, the wasm builds
stamp() { sed -e "s|\"./wasi.js\"|\"./wasi.js?v=$version\"|; s|\"./systems.js\"|\"./systems.js?v=$version\"|; s|new URL(\"worker.js\", import.meta.url)|new URL(\"worker.js?v=$version\", import.meta.url)|; s|new URL(\"render.js\", import.meta.url)|new URL(\"render.js?v=$version\", import.meta.url)|; s|fetch(\`rota-\${lib}.wasm\`)|fetch(\`rota-\${lib}.wasm?v=$version\`)|" "$1"; }
stamp www/app.js > "$dest/app.js"
stamp www/worker.js > "$dest/worker.js"
stamp www/systems.js > "$dest/systems.js"
stamp www/render.js > "$dest/render.js"
grep -q "render.js?v=$version" "$dest/worker.js" || { echo "publish: the drawing worker's URL wasn't stamped" >&2; exit 1; }
grep -q "wasm?v=$version" "$dest/render.js" || { echo "publish: the drawing worker's wasm URL wasn't stamped" >&2; exit 1; }
grep -q "wasm?v=$version" "$dest/worker.js" || { echo "publish: the wasm URL wasn't stamped" >&2; exit 1; }
grep -q "worker.js?v=$version" "$dest/app.js" || { echo "publish: the worker's URL wasn't stamped" >&2; exit 1; }
sed -i "s|src=\"app.js\"|src=\"app.js?v=$version\"|" "$dest/index.html"
echo "published $version to $dest"
echo "next: commit the site, then its ./deploy.sh --dry-run and ./deploy.sh"
