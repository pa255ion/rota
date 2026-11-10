// side track W: the page's sound ring (app.js's WORKLET) and the worker's
// pacing (worker.js's framesNow) run offline with stand-ins: a game's
// samples (a raw 48 kHz stereo s16 dump) produced a frame at a time, as
// the worker's loop asks, on a display of DISPLAY_HZ that stalls now and
// then, as a busy tab does. Counts the steps between output samples larger
// than any in the input (clicks), the times the ring ran dry, and the
// display frames that ran other than one frame (the picture's stutter).
//   node web/test/ring-sim.mjs PCM [RATE]
//   (SYSTEM_HZ 59.73, DISPLAY_HZ 60, STALLS 0.05, STALL_MIN 30, STALL_SPAN 50;
//    a slow machine: COST, a drawn frame's ms with its picture, SKIP_COST a
//    frame not drawn, JITTER ms either way: a display frame still busy at
//    its vsync waits for the next, and the pictures' spacing is counted)
import { readFileSync } from "node:fs";
const app = readFileSync("web/www/app.js", "utf8");
const worker = readFileSync("web/www/worker.js", "utf8");
const consts = {};
for (const k of ["AUDIO_TARGET", "AUDIO_RESUME", "AUDIO_RATE"]) consts[k] = Number(app.match(new RegExp(`const ${k} = (\\d+)`))[1]);
let src = app.slice(app.indexOf("const WORKLET = `") + 17);
src = src.slice(0, src.indexOf("`;"));
src = src.replace(/\$\{(\w+)\}/g, (_, k) => consts[k]);
// the AudioWorklet scope, stood in for; the worker's port to it
const rate = Number(process.argv[3] || 48000);
let Ring, reported = 0;
new Function("AudioWorkletProcessor", "registerProcessor", "sampleRate", src)(
  class { constructor() { this.port = {}; } }, (_, c) => { Ring = c; }, rate);
const ring = new Ring();
const link = { postMessage: (v) => { reported = v; } };
ring.port.onmessage({ data: { port: link } });
// the worker's pacing, its own code
const systemHz = Number(process.env.SYSTEM_HZ ?? 4194304 / 70224);
let written = 0;
const pace = worker.slice(worker.indexOf("let tickMs"), worker.indexOf("function loop("));
// (the page's measure of the display's refresh, as it would send it)
const paceCode = pace.replace("let displayMs = 0;", `let displayMs = ${1000 / Number(process.env.DISPLAY_HZ ?? 60)};`);
const [framesNow, noteDrawn, noteWork] = new Function("SYSTEMS", "core", "ring", "queued", "AUDIO_TARGET", `${paceCode}; return [framesNow, noteDrawn, noteWork];`)(
  { s: { hz: systemHz } }, { sys: "s", lib: "s", x: { s_skip: () => {} } }, link, () => written - reported, consts.AUDIO_TARGET);
const COST = Number(process.env.COST ?? 0), SKIP_COST = Number(process.env.SKIP_COST ?? COST), JITTER = Number(process.env.JITTER ?? 0);
let busyUntil = 0, lastPic = -1;
const gaps = new Map();

const pcm = new Int16Array(readFileSync(process.argv[2]).buffer.slice(0));
const perFrame = consts.AUDIO_RATE / systemHz;
let pos = 0, frac = 0, outMax = 0, clicks = 0, dry = 0, prev = 0, wasDry = false, stall = 0, ticks = 0, uneven = 0;
let seed = 1; const rnd = () => (seed = (seed * 1103515245 + 12345) & 0x7fffffff) / 0x7fffffff;
let inMax = 0; for (let i = 2; i < pcm.length; i += 2) inMax = Math.max(inMax, Math.abs(pcm[i] - pcm[i - 2]) / 32768);
const displayMs = 1000 / Number(process.env.DISPLAY_HZ ?? 60);
const outBlocks = Math.floor(rate * 30 / 128);   // 30 s
let nextDisplay = 0, stalledFrom = -1;
for (let blk = 0; blk < outBlocks; blk++) {
  const now = blk * 128 / rate * 1000;
  while (now >= nextDisplay) {
    if (stall > 0) stall -= displayMs;
    else if (nextDisplay < busyUntil) { /* still working: this vsync is missed */ }
    else {
      if (rnd() < Number(process.env.STALLS ?? 0.05)) { stall = Number(process.env.STALL_MIN ?? 30) + Number(process.env.STALL_SPAN ?? 50) * rnd(); stalledFrom = ticks; }
      const n = framesNow(nextDisplay);
      if (!COST) noteWork(0);
      ticks++;
      if (COST && n > 0) {
        const j = () => (rnd() * 2 - 1) * JITTER;
        const drawn = COST + j();
        busyUntil = nextDisplay + (n - 1) * (SKIP_COST + j()) + drawn;
        if (process.env.HALF !== "0") noteDrawn(drawn);
        noteWork(busyUntil - nextDisplay);
        const k = Math.round(nextDisplay / displayMs);
        if (lastPic >= 0) gaps.set(k - lastPic, (gaps.get(k - lastPic) || 0) + 1);
        lastPic = k;
      }
      // (the display frames right after a stall may catch up: not counted)
      if (n !== 1 && ticks > 30 && ticks - stalledFrom > 30) uneven++;
      for (let k = 0; k < n && pos < pcm.length; k++) {
        frac += perFrame; const m = Math.floor(frac); frac -= m;
        const chunk = new Float32Array(2 * m);
        for (let i = 0; i < 2 * m && pos + i < pcm.length; i++) chunk[i] = pcm[pos + i] / 32768;
        pos += 2 * m; written += m;
        link.onmessage({ data: chunk });
      }
    }
    nextDisplay += displayMs;
  }
  const L = new Float32Array(128), R = new Float32Array(128);
  ring.process([], [[L, R]]);
  for (let i = 0; i < 128; i++) {
    const j = Math.abs(L[i] - prev); prev = L[i];
    if (j > inMax * 1.01) clicks++;
    outMax = Math.max(outMax, j);
  }
  if (ring.dry && !wasDry) dry++;
  wasDry = ring.dry;
}
if (COST) {
  const all = [...gaps].reduce((a, [, c]) => a + c, 0), [mode, mc] = [...gaps].sort((a, b) => b[1] - a[1])[0];
  console.log(`pictures every ${mode} display frames mostly; ${(100 * (all - mc) / all).toFixed(1)}% of the gaps otherwise (${[...gaps].sort((a, b) => a[0] - b[0]).map(([g, c]) => `${g}:${c}`).join(" ")})`);
}
console.log(`rate ${rate}, system ${systemHz.toFixed(2)} Hz, display ${(1000 / displayMs).toFixed(2)} Hz: input's largest step ${inMax.toFixed(3)}, output's ${outMax.toFixed(3)}; steps above the input's: ${clicks}; times run dry: ${dry}; display frames not running one frame (stalls aside): ${uneven} of ${ticks}`);
