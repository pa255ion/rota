#!/usr/bin/env bash
# Run Xilinx bootgen (ships with Vivado, Vivado/bin) in Vivado's
# environment, like tools/xsdb.sh: through the FHS wrapper's vivado-env if
# there is one (NixOS, docs/TOOLCHAIN.md), otherwise by sourcing
# settings64.sh directly.
#
#   VIVADO_SETTINGS=<settings64.sh> tools/bootgen.sh [bootgen args...]
set -euo pipefail
settings=${VIVADO_SETTINGS:?set VIVADO_SETTINGS to the path of settings64.sh}
if command -v vivado-env >/dev/null; then
    exec vivado-env -c 'source "$1" >/dev/null 2>&1; shift; exec bootgen "$@"' -- "$settings" "$@"
else
    # shellcheck disable=SC1090
    source "$settings"
    exec bootgen "$@"
fi
