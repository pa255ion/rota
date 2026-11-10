#!/usr/bin/env python3
# side track W: the long-run check's native side (web/test/longrun.sh): the
# installed desktop library (make install) through ctypes, the case's
# machine run for the checkpoints' frames with no keys; at each, its
# picture and a hash of every sample so far: OUT-<frame>.native.
import ctypes, hashlib, json, os, sys
case, out, points = sys.argv[1], sys.argv[2], [int(n) for n in sys.argv[3:]]
c = json.load(open("web/test/longrun-cases.json"))[case]
lib = ctypes.CDLL(os.path.expanduser(f"~/.local/share/emulation_clash/desktop/lib/lib{ {'a800': 'a800xl'}.get(c['lib'], c['lib']) }-desktop.so"), mode=os.RTLD_GLOBAL)
argc = ctypes.c_int(1); argv = (ctypes.c_char_p * 2)(b"x", None); pargv = ctypes.pointer(argv)
lib.hs_init(ctypes.byref(argc), ctypes.byref(pargv))
args, keep = [], []
for a in c["args"]:
    if isinstance(a, list) and a[0] == "file":
        b = open(a[1], "rb").read(); keep.append(b); args += [ctypes.c_char_p(b), ctypes.c_size_t(len(b))]
    elif isinstance(a, list): args += [ctypes.c_void_p(0), ctypes.c_size_t(0)]
    else: args.append(ctypes.c_int(a))
if c["lib"] == "chip8": args[2] = ctypes.c_uint8(args[2].value)
h = ctypes.c_void_p()
rc = getattr(lib, c["lib"] + "_new")(*args, ctypes.byref(h))
assert rc == 0, rc
frame = getattr(lib, c["lib"] + "_frame")
keys = (ctypes.c_uint8 * 11)()
arg = {"u8": ctypes.c_uint8(0), "u16": ctypes.c_uint16(0), "u32": ctypes.c_uint32(0), "keys": keys}[c["frame"]]
pixels = getattr(lib, c["lib"] + "_pixels"); px = (ctypes.c_uint32 * (512 * 512))()
audio = getattr(lib, c["lib"] + "_audio"); abuf = (ctypes.c_int16 * 8192)()
def size():
    if c["size"]: return c["size"]
    return getattr(lib, c["lib"] + "_width")(h), getattr(lib, c["lib"] + "_height")(h)
hsh = hashlib.sha1()
k = 0
for n in points:
    while k < n:
        frame(h, arg); m = audio(h, abuf, ctypes.c_int(4096)); k += 1
        hsh.update(bytes(abuf)[:4 * m])
    w, ht = size(); pixels(h, px)
    open(f"{out}-{n}.native", "wb").write(bytes(px)[:4 * w * ht] + hsh.copy().digest())
