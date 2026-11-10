// side track W: the Super Nintendo's drawing split off (the page's second
// worker): a machine making its render log for export (snes_new's -1
// threads) and a replayer in a second instance of the library, fed each
// frame's log, show the same picture as a machine drawing inline, frame by
// frame (snes_hash against snes_render_hash), with buttons pressed now and
// then and frame skipping on in stretches (the log goes on without draws).
//   node web/test/split.mjs [FRAMES]   (from the repository's root)
import { existsSync, readFileSync } from "node:fs";
import { WASI } from "node:wasi";

async function lib() {
  const wasi = new WASI({ version: "preview1", args: [], env: {} });
  const { instance } = await WebAssembly.instantiate(readFileSync("web/www/rota-snes.wasm"), { wasi_snapshot_preview1: wasi.wasiImport });
  wasi.initialize(instance);
  const x = instance.exports;
  x.hs_init(0, 0);
  const put = (b) => { const p = x.web_alloc(Math.max(1, b.length)); new Uint8Array(x.memory.buffer, p, b.length).set(b); return [p, b.length]; };
  return { x, put };
}
const frames = Number(process.argv[2] || 600);
const hex = (v) => BigInt.asUintN(64, v).toString(16);
let failed = 0;
for (const rom of ["roms/snes/games/unnamed-tech-demo-v3.sfc", "roms/snes/games/horizontal-shooter.sfc", "roms/snes/opensnes/mode5_hires.sfc", "roms/snes/opensnes/extbg.sfc"]) {
  if (!existsSync(rom)) { console.log(`${rom}: skipped (missing)`); continue; }
  const machine = async (threads) => {
    const { x, put } = await lib();
    const out = x.web_alloc(4);
    if (x.snes_new(...put(readFileSync("build/snes/ipl.bin")), ...put(readFileSync(rom)), threads, 0, out)) throw new Error("refused");
    return { x, h: new DataView(x.memory.buffer).getUint32(out, true), ab: x.web_alloc(16384) };
  };
  const a = await machine(0), b = await machine(-1);
  const r = await lib(), ro = r.x.web_alloc(4);
  r.x.snes_render_new(ro);
  const rh = new DataView(r.x.memory.buffer).getUint32(ro, true);
  let parted = null, compared = 0;
  for (let f = 0; f < frames && parted === null; f++) {
    const keys = (f % 37) < 5 ? [0x100, 0x8, 0x800, 0x80, 0][(f / 37 | 0) % 5] : 0;
    // (frame skipping in stretches: both machines' pictures go undrawn)
    const skip = (f % 150) >= 120;
    a.x.snes_skip(a.h, skip ? 1 : 0); b.x.snes_skip(b.h, skip ? 1 : 0);
    a.x.snes_frame(a.h, keys); a.x.snes_audio(a.h, a.ab, 4096);
    b.x.snes_frame(b.h, keys); b.x.snes_audio(b.h, b.ab, 4096);
    const n = b.x.snes_log_size(b.h), p = b.x.web_alloc(Math.max(4, 4 * n));
    b.x.snes_log_get(b.h, p);
    const q = r.x.web_alloc(Math.max(4, 4 * n));
    new Int32Array(r.x.memory.buffer, q, n).set(new Int32Array(b.x.memory.buffer, p, n));
    const ended = r.x.snes_render_feed(rh, q, n);
    b.x.web_free(p); r.x.web_free(q);
    // (a log usually holds one frame's end; the first after power-on can
    // hold two: the replayer keeps the last, the machine's frame now)
    if (ended < 1) { parted = `frame ${f}: no frame ended in its log`; break; }
    if (!skip) { compared++; if (hex(a.x.snes_hash(a.h)) !== hex(r.x.snes_render_hash(rh))) parted = `frame ${f}: the pictures differ`; }
  }
  if (parted) { console.log(`${rom}: FAIL, ${parted}`); failed = 1; }
  else console.log(`${rom}: OK, the same picture at all ${compared} drawn frames of ${frames}`);
}
process.exit(failed);
