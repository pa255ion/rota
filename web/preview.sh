#!/bin/sh
# side track W: the page on this machine, as the site will serve it:
#   web/preview.sh [PORT]     then open http://127.0.0.1:PORT/
set -eu
cd "$(dirname "$0")"
port=${1:-8080}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
./publish.sh "$dir" site >/dev/null
echo "rota: http://127.0.0.1:$port/ (Ctrl-C stops)"
cd "$dir/site" && python3 -m http.server "$port" --bind 127.0.0.1
