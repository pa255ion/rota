#!/usr/bin/env python3
# side track W: two checkpoints of the long run (web/test/longrun.sh)
# compared: the picture (0xFFRRGGBB words) and the SHA-1 of every sample
# so far; the exit status 1 when either differs. (Save states aren't
# compared: the browser's clocks are rebased, as WebAssembly's Int is 32
# bits, and the installed native library may predate that.)
import sys
a, w = open(sys.argv[1], "rb").read(), open(sys.argv[2], "rb").read()
bad = 0
if a[-20:] != w[-20:]:
    print("  the sound differs (the hash of every sample so far)"); bad = 1
pa, pw = a[:-20], w[:-20]
if len(pa) != len(pw):
    print(f"  the picture's size differs: {len(pa) // 4} vs {len(pw) // 4} pixels"); bad = 1
else:
    d = sum(1 for i in range(0, len(pa), 4) if pa[i:i + 4] != pw[i:i + 4])
    if d: print(f"  {d} pixels differ"); bad = 1
sys.exit(bad)
