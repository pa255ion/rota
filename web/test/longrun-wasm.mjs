// side track W: the long-run check's browser side (web/test/longrun.sh):
// the case's wasm build in Node, the same frames as longrun-native.py;
// at each checkpoint its picture and a hash of every sample so far:
// OUT-<frame>.wasm.
import { readFileSync, writeFileSync } from "node:fs";
import { WASI } from "node:wasi";
import { createHash } from "node:crypto";
const [kase, out, ...points] = process.argv.slice(2);
const c = JSON.parse(readFileSync("web/test/longrun-cases.json", "utf8"))[kase];
const wasi = new WASI({ version: "preview1", args: [], env: {} });
const { instance } = await WebAssembly.instantiate(readFileSync(`web/www/rota-${c.lib}.wasm`), { wasi_snapshot_preview1: wasi.wasiImport });
wasi.initialize(instance);
const x = instance.exports;
x.hs_init(0, 0);
const args = [];
// (wasm_args: the browser's own, e.g. the fast version where the native
// library runs the reference)
for (const a of c.wasm_args ?? c.args) {
  if (Array.isArray(a) && a[0] === "file") {
    const b = readFileSync(a[1]), p = x.web_alloc(b.length);
    new Uint8Array(x.memory.buffer, p, b.length).set(b);
    args.push(p, b.length);
  } else if (Array.isArray(a)) args.push(0, 0);
  else args.push(a);
}
const o = x.web_alloc(4);
const rc = x[`${c.lib}_new`](...args, o);
if (rc !== 0) throw new Error(`${c.lib}_new: ${rc}`);
const h = new DataView(x.memory.buffer).getUint32(o, true);
const keys = x.web_alloc(11);
new Uint8Array(x.memory.buffer, keys, 11).fill(0);
const arg = c.frame === "keys" ? keys : 0;
const frame = x[`${c.lib}_frame`], audio = x[`${c.lib}_audio`], abuf = x.web_alloc(4 * 4096);
const px = x.web_alloc(4 * 512 * 512);
const size = () => c.size || [x[`${c.lib}_width`](h), x[`${c.lib}_height`](h)];
const hsh = createHash("sha1");
let k = 0;
for (const n of points.map(Number)) {
  // (the samples taken each frame, as the page does: left, they pile up)
  while (k < n) {
    frame(h, arg);
    const m = audio(h, abuf, 4096);
    hsh.update(new Uint8Array(x.memory.buffer, abuf, 4 * m));
    k++;
  }
  const [w, ht] = size();
  x[`${c.lib}_pixels`](h, px);
  const tail = hsh.copy().digest();
  writeFileSync(`${out}-${n}.wasm`, Buffer.concat([new Uint8Array(x.memory.buffer, px, 4 * w * ht).slice(), tail]));
}
