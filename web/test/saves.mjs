// side track W: the page's battery saves through each library's <lib>_cram_*
// calls (the NES's and Super Nintendo's, new; the Game Boy's): a game run
// a while, its battery RAM taken; a fresh machine given it before its first
// frame hands the same bytes back, and runs on from it as the first did
// from the same RAM (the same frame hashes where the library has them).
// A cartridge without a battery has none (size 0).
//   node web/test/saves.mjs      (from the repository's root; after build.sh)
import { existsSync, readFileSync } from "node:fs";
import { WASI } from "node:wasi";

async function lib(name) {
  const wasi = new WASI({ version: "preview1", args: [], env: {} });
  const { instance } = await WebAssembly.instantiate(readFileSync(`web/www/rota-${name}.wasm`), { wasi_snapshot_preview1: wasi.wasiImport });
  wasi.initialize(instance);
  const x = instance.exports;
  x.hs_init(0, 0);
  const put = (b) => { const p = x.web_alloc(Math.max(1, b.length)); new Uint8Array(x.memory.buffer, p, b.length).set(b); return [p, b.length]; };
  return { x, put };
}

// the buttons a run holds at frame i: the saving run's, the runs' from the save
const every = (word) => (i) => (i % 40 < 4 ? word[(i / 40 | 0) % word.length] : 0);
const at = (word, from) => (i) => (i >= from && i < from + 4 ? word : 0);
const cases = [
  // [library, file, frames, the library's new call's arguments after the
  // file, a battery, the saving run's buttons, the later runs']
  // (Kirby: Start and A now and then; its file select shows the save)
  ["nes", "roms/nes/games/local/mmc3-game.nes", 900, () => [2], true, every([0x8, 0x1, 0, 0]), every([0x8, 0x1, 0, 0])],
  // (OpenSNES's SRAM test: A writes slot 1, B reads it back and shows it)
  ["snes", "roms/snes/opensnes/save_game.sfc", 300, null, true, at(0x8, 120), at(0x800, 120)],
  ["nes", "roms/nes/nestest/nestest.nes", 30, () => [2], false, every([0]), every([0])],
  ["snes", "roms/snes/games/unnamed-tech-demo-v3.sfc", 30, null, false, every([0]), every([0])],
];
let failed = 0;
for (const [name, file, frames, extra, battery, saving, later] of cases) {
  if (!existsSync(file)) { console.log(`${name} ${file}: skipped (missing)`); continue; }
  const make = async (keys) => {
    const { x, put } = await lib(name);
    const out = x.web_alloc(4), rom = put(readFileSync(file));
    const rc = name === "snes" ? x.snes_new(...put(readFileSync("build/snes/ipl.bin")), ...rom, 0, 0, out) : x.nes_new(...rom, ...extra(), out);
    if (rc !== 0) throw new Error(`${file}: refused`);
    const h = new DataView(x.memory.buffer).getUint32(out, true), ab = x.web_alloc(16384);
    let k = 0;
    const frame = () => { x[`${name}_frame`](h, keys(k++)); x[`${name}_audio`](h, ab, 4096); };
    const size = () => Number(x[`${name}_cram_size`](h));
    const get = () => { const n = size(), p = x.web_alloc(Math.max(1, n)); x[`${name}_cram_get`](h, p, n); return new Uint8Array(x.memory.buffer, p, n).slice(); };
    const set = (d) => { const [p, n] = put(d); x[`${name}_cram_set`](h, p, n); };
    // (the library's frame hash, or FNV-1a of its picture)
    const px = x.web_alloc(4 * 512 * 480);
    const hash = () => {
      if (x[`${name}_hash`]) return BigInt.asUintN(64, x[`${name}_hash`](h)).toString(16);
      x[`${name}_pixels`](h, px);
      const w = new Uint32Array(x.memory.buffer, px, 256 * 240);
      let v = 0x811c9dc5;
      for (let i = 0; i < w.length; i++) v = Math.imul(v ^ w[i], 0x01000193) >>> 0;
      return v.toString(16);
    };
    return { frame, size, get, set, hash };
  };
  const a = await make(saving);
  for (let i = 0; i < frames; i++) a.frame();
  const n = a.size();
  if (!battery) {
    const ok = n === 0;
    console.log(`${name} ${file}: ${ok ? "OK, no battery: no save" : `FAIL, a save of ${n} bytes without a battery`}`);
    if (!ok) failed = 1;
    continue;
  }
  const saved = a.get();
  const used = saved.filter((b) => b !== 0 && b !== 0xFF).length;
  const b = await make(later);
  b.set(saved);
  const back = b.get();
  const same = back.length === saved.length && back.every((v, i) => v === saved[i]);
  // (both on for a while from the same RAM: the same pictures, where the
  // library hashes them)
  let parted = null;
  const c = await make(later);
  c.set(saved);
  const d = await make(later);
  d.set(saved);
  for (let i = 0; i < 120 && parted === null; i++) { c.frame(); d.frame(); if (c.hash() !== d.hash()) parted = i; }
  // (and the game reads it: from the save, the pictures differ from a
  // machine's without it, where the library hashes them)
  let reads = null;
  if (a.hash() !== null) {
    const e = await make(later), f = await make(later);
    f.set(saved);
    reads = false;
    for (let i = 0; i < frames && !reads; i++) { e.frame(); f.frame(); if (e.hash() !== f.hash()) reads = true; }
  }
  const ok = n > 0 && same && parted === null && reads !== false;
  console.log(`${name} ${file}: ${ok ? "OK" : "FAIL"}, ${n} bytes of battery RAM (${used} written by the game), handed back ${same ? "the same" : "different"}${parted === null ? "" : `; the twins parted at frame ${parted}`}${reads === null ? "" : reads ? "; the game shows its save" : "; the game doesn't show its save"}`);
  if (!ok) failed = 1;
}
process.exit(failed);
