#!/usr/bin/env python3
"""tools/snes-refs.py ZIP DIR: the OpenSNES examples' reference screenshots
(examples/*/NAME/NAME.png in the release zip) into DIR (M7 step 7.5; called
by tools/fetch-roms.sh; the zip's hash is checked there)."""
import os, sys, zipfile
zipname, out = sys.argv[1], sys.argv[2]
os.makedirs(out, exist_ok=True)
with zipfile.ZipFile(zipname) as z:
    for n in z.namelist():
        parts = n.split('/')
        if n.endswith('.png') and len(parts) >= 2 and parts[-1][:-4] == parts[-2]:
            with open(os.path.join(out, parts[-1]), 'wb') as f:
                f.write(z.read(n))
