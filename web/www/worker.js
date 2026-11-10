// rota in the browser (side track W): the emulator in a Web Worker, off
// the page's thread (the page draws nothing, runs no frame, collects no
// garbage of ours): the systems' libraries (rota-<lib>.wasm), the splash
// and the game, battery saves (IndexedDB), the frames paced by the sound's
// queue, the picture on the page's canvas (an OffscreenCanvas handed over),
// the sound straight to the page's AudioWorklet (a MessagePort). The page
// (app.js) sends the keys and what the player picks; this sends back the
// status, the picture's size and the speed.

import { makeWasi } from "./wasi.js";
import { SYSTEMS } from "./systems.js";

const SPLASH_FRAMES = 600;         // the splash's music, then the game (or a key sooner)
const SAVE_EVERY = 600;            // frames between battery-save checks
const AUDIO_RATE = 48000;          // every library samples at 48 kHz
const AUDIO_CAP = 2048;            // stereo frames a frame's call may hand over
const AUDIO_TARGET = 4800;         // the sound's queue the frames keep (100 ms; the ring's own)

const post = (m) => postMessage(m);
const ZERO_KEYS = new Uint8Array(11);
const status = (text, error = false) => post({ type: "status", text, error });

// --- the cores: one wasm instance per library, loaded when first wanted

const instances = {};
async function instance(lib) {
  if (!instances[lib]) instances[lib] = (async () => {
    const { wasi, setMemory } = makeWasi();
    let i;
    try {
      ({ instance: i } = await WebAssembly.instantiateStreaming(fetch(`rota-${lib}.wasm`), { wasi_snapshot_preview1: wasi }));
    } catch (e) {
      // (the builds use WebAssembly tail calls: web/tools/tailcall.mjs)
      if (e instanceof WebAssembly.CompileError) throw new Error("this browser is too old for the emulator (it needs WebAssembly tail calls: Chrome 112, Firefox 121, Safari 18.2 or newer)");
      throw e;
    }
    setMemory(i.exports.memory);
    i.exports._initialize();
    i.exports.hs_init(0, 0);
    return i.exports;
  })();
  return instances[lib];
}

// a system's extra files (the 800XL's OS, BASIC and loader), fetched once
const extraFiles = {};
async function filesOf(def) {
  return Promise.all((def.files || []).map((f) => extraFiles[f] ??= (async () => {
    const r = await fetch(f);
    if (!r.ok) throw new Error(`${f} didn't load`);
    return new Uint8Array(await r.arrayBuffer());
  })()));
}

// a machine of one system: its library's calls on its handle
class Core {
  constructor(sys, x) {
    this.sys = sys; this.def = SYSTEMS[sys]; this.x = x; this.lib = this.def.lib; this.h = 0;
    this.out = x.web_alloc(4);
    this.audioPtr = x.web_alloc(4 * AUDIO_CAP);
    this.keys = x.web_alloc(16);
  }
  call(name, ...args) { return this.x[name](...args); }
  bytes(data) {
    const p = this.x.web_alloc(Math.max(1, data.length));
    new Uint8Array(this.x.memory.buffer, p, data.length).set(data);
    return p;
  }
  load(data, name, files, opts = {}) {
    this.free();
    const p = this.bytes(data);
    const fs = files.map((f) => [this.bytes(f), f.length]);
    // the library's <lib>_new: the system's arguments, then where the handle goes
    const rc = this.call(`${this.lib}_new`, ...this.def.args([p, data.length], data, name, fs, opts), this.out);
    this.split = Boolean(opts.split);
    this.x.web_free(p);
    for (const [q] of fs) this.x.web_free(q);
    if (rc !== 0) throw new Error(`this file isn't a ${this.def.title} ROM`);
    this.h = new DataView(this.x.memory.buffer).getUint32(this.out, true);
    this.skipping = false;
    this.w = this.def.sized ? this.call("sms_width", this.h) : this.def.w;
    this.h2 = this.def.sized ? this.call("sms_height", this.h) : this.def.h;
    this.pic = this.x.web_alloc(4 * 512 * 512);
  }
  free() {
    if (this.h) { this.call(`${this.lib}_free`, this.h); this.h = 0; }
    if (this.pic) { this.x.web_free(this.pic); this.pic = 0; }
  }
  // frame skipping: a frame whose picture won't be shown (not the last a
  // display frame runs) isn't drawn, where the library can skip it (the
  // NES and Super Nintendo: nothing the game sees changes)
  skip(on) {
    const f = this.x[`${this.lib}_skip`];
    if (f && this.h && on !== this.skipping) { f(this.h, on ? 1 : 0); this.skipping = on; }
  }
  frame(word, bytes) {
    // (the 800XL: its key bytes, at this.keys)
    if (this.def.keyBytes) new Uint8Array(this.x.memory.buffer, this.keys, 11).set(bytes || ZERO_KEYS);
    const n = this.call(`${this.lib}_frame`, this.h, this.def.keyBytes ? this.keys : word);
    if (this.def.sized) { this.w = this.call("sms_width", this.h); this.h2 = this.call("sms_height", this.h); }
    return n;
  }
  audio() {
    const n = this.call(`${this.lib}_audio`, this.h, this.audioPtr, AUDIO_CAP);
    return new Int16Array(this.x.memory.buffer, this.audioPtr, 2 * n);
  }
  // the picture as RGBA (the libraries hand 0xFFRRGGBB words over)
  // (the canvas's rows: a picture whose lines come twice, the Super
  // Nintendo's, is drawn at half its height and stretched by the page)
  get ch() { return this.h2 / (this.def.rows || 1); }
  image() {
    const w = this.w, ch = this.ch, step = this.def.rows || 1;
    // (one ImageData kept while the size stays: not a new megabyte for the
    // garbage collector every frame)
    if (!this.img || this.img.width !== w || this.img.height !== ch) { this.img = new ImageData(w, ch); this.img32 = new Uint32Array(this.img.data.buffer); }
    // (a library that hands the canvas's own picture over, RGBA at its
    // height: one block copy, the Super Nintendo's)
    const rgba = this.x[`${this.lib}_pixels_rgba`];
    if (rgba) {
      rgba(this.h, this.pic);
      this.img.data.set(new Uint8Array(this.x.memory.buffer, this.pic, 4 * w * ch));
      return this.img;
    }
    this.call(`${this.lib}_pixels`, this.h, this.pic);
    const px = new Uint32Array(this.x.memory.buffer, this.pic, w * this.h2);
    const out = this.img32;
    for (let y = 0; y < ch; y++) {
      const s = y * step * w, d = y * w;
      for (let i = 0; i < w; i++) { const p = px[s + i]; out[d + i] = (p & 0xFF00FF00) | ((p >>> 16) & 0xFF) | ((p & 0xFF) << 16); }
    }
    return this.img;
  }
  hash() {
    // as the goldens' frame hash where the library has one, else FNV-1a of the RGBA
    if (this.x[`${this.lib}_hash`]) return BigInt.asUintN(64, this.call(`${this.lib}_hash`, this.h)).toString(16).padStart(16, "0");
    let h = 0xcbf29ce484222325n;
    for (const b of this.image().data) h = BigInt.asUintN(64, (h ^ BigInt(b)) * 0x100000001b3n);
    return h.toString(16).padStart(16, "0");
  }
  cram() {
    const n = Number(this.call(`${this.lib}_cram_size`, this.h));
    const p = this.x.web_alloc(Math.max(1, n));
    this.call(`${this.lib}_cram_get`, this.h, p, n);
    const v = new Uint8Array(this.x.memory.buffer, p, n).slice();
    this.x.web_free(p);
    return v;
  }
  setCram(data) {
    const p = this.bytes(data);
    this.call(`${this.lib}_cram_set`, this.h, p, data.length);
    this.x.web_free(p);
  }
}

// --- the session: a splash, then the game

let canvas = null, ctx2d = null;
let core = null;                   // the running machine
let game = null;                   // { name, data, sys }
let phase = "idle";                // idle | splash | game
let frames = 0, paused = false, hidden = false;
let word = 0, keyBytes = null, anyInput = false;    // the keys held (the page's key word); a new press since the last frame
let splashOn = true, debug = false, testing = false;

async function start(sys, data, name) {
  const def = SYSTEMS[sys];
  if (!core || core.sys !== sys) {
    if (core) core.free();
    status(`loading the ${def.title}…`);
    core = new Core(sys, await instance(def.lib));
  }
  // the drawing on its own core where the system can split it (not in the
  // page's test: its hashes are the inline drawing's)
  const split = def.split && !testing ? splitter(def.lib) : null;
  core.load(data, name, await filesOf(def), { split: Boolean(split) });
  if (split) { split.w.postMessage({ type: "reset" }); split.pic = null; split.fresh = false; }
  frames = 0;
  half = false; drawnMs = 0;
  sizeCanvas();
}

function sizeCanvas() {
  if (canvas.width !== core.w || canvas.height !== core.ch) {
    canvas.width = core.w; canvas.height = core.ch;
    post({ type: "size", w: core.w, h: core.ch, sy: core.def.rows || 1 });
  }
}

async function playGame(g) {
  game = g;
  const splash = SYSTEMS[g.sys].splash;
  if (splashOn && splash) {
    const res = await fetch(splash);
    if (res.ok) {
      await start(g.sys, new Uint8Array(await res.arrayBuffer()), splash);
      phase = "splash";
      anyInput = false;
      status(`${g.name}: after the splash (any key skips it)`);
      return;
    }
  }
  await enterGame();
}

async function enterGame(kept = null) {
  // held while the battery save is looked up: it goes in before the game's
  // first frame (a game reads its save RAM as it starts)
  phase = "idle";
  await start(game.sys, game.data, game.name);
  const saves = SYSTEMS[game.sys].saves;
  // (@kept: the running game's own save RAM, on a reset: the latest, and
  // the copy in the database may not be written yet)
  const saved = !saves ? null : kept ? kept : await within(2000, loadSave(saveKey()), null);
  if (saved) core.setCram(saved);
  phase = "game";
  lastSave = saves ? core.cram() : null;
  const any = saved && saved.some((b) => b !== 0);
  status(`${game.name}${kept ? ": reset" : any ? " (its battery save loaded)" : !saves || dbOk ? "" : " (this browser keeps no saves here)"}`);
}

// --- battery saves (IndexedDB, one per game: its name, size and system)

let lastSave = null;
const saveKey = () => `${game.sys}:${game.name}:${game.data.length}`;
// (storage that doesn't answer, as in some private windows: no saves,
// never a game held up)
let dbOk = true;
// (the timer only counts when it wins: it fired every time before, so a
// save looked up in time still marked the storage as absent, and every
// save after it was dropped)
const within = (ms, p, fallback) => new Promise((ok) => {
  let done = false;
  const timer = setTimeout(() => { if (!done) { done = true; dbOk = false; ok(fallback); } }, ms);
  p.then((v) => { if (!done) { done = true; clearTimeout(timer); ok(v); } }, () => { if (!done) { done = true; clearTimeout(timer); ok(fallback); } });
});
function openDb() {
  return new Promise((ok, fail) => {
    const r = indexedDB.open("rota", 1);
    r.onupgradeneeded = () => r.result.createObjectStore("saves");
    r.onsuccess = () => ok(r.result);
    r.onerror = () => fail(r.error);
  });
}
async function loadSave(key) {
  try {
    const db = await openDb();
    return await new Promise((ok) => {
      const q = db.transaction("saves").objectStore("saves").get(key);
      q.onsuccess = () => ok(q.result ? new Uint8Array(q.result) : null);
      q.onerror = () => ok(null);
    });
  } catch { return null; }
}
async function storeSave(key, data) {
  if (!dbOk) return;
  try {
    const db = await openDb();
    db.transaction("saves", "readwrite").objectStore("saves").put(data, key);
  } catch { /* private window: no saves */ }
}
function flushSave() {
  if (phase !== "game" || !SYSTEMS[game.sys].saves) return;
  const now = core.cram();
  if (now.length === 0 || now.every((b) => b === 0)) return;
  if (lastSave && now.length === lastSave.length && now.every((b, i) => b === lastSave[i])) return;
  lastSave = now;
  storeSave(saveKey(), now);
}

// --- sound: the page's ring (an AudioWorklet), reached through a port

let ring = null;                   // the port, once the page's sound runs
let written = 0, played = 0;       // 48 kHz stereo frames handed over, and used (the ring's report)
const queued = () => written - played;
function pushAudio() {
  // (taken every frame, sound or not: left in the library, they pile up)
  const s16 = core.audio(), n = s16.length / 2;
  if (!ring || n === 0) return;
  // (most of a second queued: the page fell far ahead somehow; those are
  // dropped rather than played late, and the ring never wraps)
  if (queued() + n > 0.9 * AUDIO_RATE) return;
  const f = new Float32Array(2 * n);
  for (let i = 0; i < f.length; i++) f[i] = s16[i] / 32768;
  written += n;
  ring.postMessage(f, [f.buffer]);
}

// --- the loop: frames at the system's rate (or the sound's queue), the
// picture once a display frame

// the speed readout: what a frame costs, as times real time; how often the
// sound ran dry
let costMs = 0, costFrames = 0, shownAt = 0, dryFrames = 0;
function report(t) {
  if (t - shownAt < 1000 || !costFrames) return;
  post({ type: "speed", ratio: 1000 / SYSTEMS[core.sys].hz / (costMs / costFrames), ms: costMs / costFrames, dry: ring ? dryFrames : null, half, drawnMs });
  costMs = 0; costFrames = 0; shownAt = t;
}

// --- the drawing on its own core (render.js; the Super Nintendo's)

let splitW = null;
function splitter(lib) {
  if (splitW) return splitW;
  try {
    const w = new Worker(new URL("render.js", import.meta.url), { type: "module" });
    splitW = { w, sent: 0, done: 0, pic: null, fresh: false };
    w.onmessage = (e) => {
      const m = e.data;
      if (m.type === "pic") { splitW.done = m.seq; splitW.pic = new ImageData(m.rgba, 512, 239); splitW.fresh = true; }
      else if (m.type === "done") splitW.done = m.seq;
      else if (m.type === "error") status(`the drawing stopped: ${m.text}`, true);
    };
    w.postMessage({ type: "init", lib });
    return splitW;
  } catch {
    // (no nested workers here: drawn inline)
    return null;
  }
}
// a frame's render log to the drawing worker
function sendLog() {
  const x = core.x, n = x.snes_log_size(core.h);
  const p = x.web_alloc(Math.max(4, 4 * n));
  x.snes_log_get(core.h, p);
  const log = new Int32Array(n);
  log.set(new Int32Array(x.memory.buffer, p, n));
  x.web_free(p);
  splitW.sent++;
  splitW.w.postMessage({ type: "log", seq: splitW.sent, log }, [log.buffer]);
}
// (the drawing worker more than two frames behind: the next frames' draws
// left out of the log, so it catches up; the game runs on)
const splitBehind = () => core.split && splitW && splitW.sent - splitW.done > 2;

const nextTick = self.requestAnimationFrame ? (f) => self.requestAnimationFrame(f) : (f) => setTimeout(() => f(performance.now()), 4);
function draw() {
  sizeCanvas();
  if (core.split) {
    // (the drawing worker's latest picture, when a new one has come)
    if (splitW.pic && splitW.fresh) { ctx2d.putImageData(splitW.pic, 0, 0); splitW.fresh = false; }
  } else ctx2d.putImageData(core.image(), 0, 0);
}
// How many frames a display frame runs. The picture stutters when that
// count wobbles (0, 2, 0, 2: the sound's queue, reported every 21 ms, read
// a display frame at a time did that), so where the display's rate is the
// system's within 1.2% (a 60 Hz display: every system but CHIP-8's 60 and
// PAL), it is exactly one, always, and the ring's rate control (±1%)
// absorbs the difference; only a queue far off its target (under 40%,
// over 180%) adds or drops a frame (two under 20%), until it is back at
// the target. Other
// displays (120 Hz, 144 Hz, 50 Hz) run the frames due by the system's
// clock, nudged by the queue the same way.
let tickMs = 0, lastT = 0, acc = 0, mend = 0;   // the worker's display frames (averaged); frames owed; +1/+2 filling, -1 draining
// the display's refresh, as the page measures it on its own thread (the
// worker's own display frames stretch when it is busy, and a busy worker
// took 30 Hz for its display and ran frames in pairs from then on)
let displayMs = 0;
// the last display frame's own work (its frames and picture), ms: a gap
// longer than that is the browser's stall, made up with frames; one this
// work made isn't (pairs of frames would only miss the next vsync too)
let workMs = 0;
function noteWork(ms) { workMs = ms; }
function framesNow(t) {
  const frameMs = 1000 / SYSTEMS[core.sys].hz;
  const dt = lastT ? Math.min(t - lastT, 250) : frameMs;   // (after a stall: no catching up)
  lastT = t;
  tickMs = tickMs ? tickMs + 0.05 * (dt - tickMs) : dt;
  if (ring) {
    const q = queued();
    if (q < 0.2 * AUDIO_TARGET) mend = 2;
    else if (q < 0.4 * AUDIO_TARGET) mend = Math.max(mend, 1);
    else if (q > 1.8 * AUDIO_TARGET) mend = -1;
    else if ((mend > 0 && q >= AUDIO_TARGET) || (mend < 0 && q <= AUDIO_TARGET)) mend = 0;
  } else mend = 0;
  const disp = displayMs || tickMs;
  let n;
  if (half) {
    // (half rate: frames in pairs, the second drawn: an even 30 pictures a
    // second where 60 don't fit, rather than bursts and misses)
    acc += dt / frameMs; n = 2 * Math.floor(acc / 2); acc -= n;
  } else if (Math.abs(disp / frameMs - 1) < 0.012) {
    // (one a display frame; after the browser's own stall, the frames its
    // vsyncs missed (4 at most); a vsync this work missed is made up
    // through the sound's queue)
    n = dt > 1.5 * disp && workMs < 0.9 * disp ? Math.min(4, Math.round(dt / disp)) : 1;
    acc = 0;
  }
  else { acc += dt / frameMs; n = Math.floor(acc); acc -= n; }
  return Math.max(0, Math.min(4, n + mend));
}
// Half rate: a drawn frame (its emulation and the picture) costing more
// than 90% of the system's frame on average can't keep a display frame's
// pace without misses, and the misses and catch-up bursts stutter; under
// 85% the full rate comes back (the average moves slowly: no flipping). Where the library can't skip the drawing
// (only the NES and Super Nintendo can), the half rate gains nothing.
let half = false, drawnMs = 0;
function noteDrawn(ms) {
  drawnMs = drawnMs ? drawnMs + 0.05 * (ms - drawnMs) : ms;
  const frameMs = 1000 / SYSTEMS[core.sys].hz;
  if (!half && drawnMs > 0.9 * frameMs && core.x[`${core.lib}_skip`]) { half = true; acc = 0; }
  else if (half && drawnMs < 0.85 * frameMs) { half = false; acc = 0; }
}
function loop(t) {
  nextTick(loop);
  const t0 = performance.now();
  if (phase === "idle" || paused || hidden || testing) { lastT = 0; return; }
  const want = framesNow(t);
  let ran = 0;
  try {
    let lastMs = 0;
    while (ran < want) {
      if (ring && queued() <= 0) dryFrames++;
      const c0 = performance.now();
      core.skip(ran < want - 1 || splitBehind());
      core.frame(phase === "splash" ? 0 : word, phase === "splash" ? null : keyBytes);
      if (core.split) sendLog();
      pushAudio();
      lastMs = performance.now() - c0;
      costMs += lastMs; costFrames++;
      frames++; ran++;
      if (phase === "game" && frames % SAVE_EVERY === 0) flushSave();
    }
    if (ran) {
      const d0 = performance.now();
      draw();
      // (the last frame was drawn: its cost and the picture's)
      noteDrawn(lastMs + performance.now() - d0);
      report(t);
    }
    noteWork(performance.now() - t0);
  } catch (e) {
    // a trap inside the emulator: stopped (its runtime can't be entered again)
    phase = "idle";
    status(`the emulator stopped: ${e.message || e}`, true);
    return;
  }
  if (debug && ran) post({ type: "debug", text: `${phase} frame ${frames}${ring ? ` queued ${queued()}` : ""}` });
  // (a press in the splash's first half second doesn't count: the one that
  // picked the file may still be on its way)
  if (phase === "splash" && (frames >= SPLASH_FRAMES || (anyInput && frames > 30))) { anyInput = false; enterGame(); }
  anyInput = false;
}

// The page's reset: in the splash, the game at once; the NES, its reset
// button (nes_reset, between two frames); the others (no reset button
// modelled, or none on the console), the game started again without the
// splash, its battery save written first and loaded again.
async function reset() {
  if (!game || phase === "idle") return;
  if (phase === "game" && core.x[`${core.lib}_reset`]) {
    core.call(`${core.lib}_reset`, core.h);
    status(`${game.name}: reset`);
    return;
  }
  const was = phase;
  let kept = null;
  if (was === "game" && SYSTEMS[game.sys].saves) { kept = core.cram(); flushSave(); }
  await enterGame(kept);
  if (was === "game" && !kept) status(`${game.name}: reset`);
}

// ?test=N (the page's headless test): N frames at once, the picture drawn,
// its hash back
async function runTest(n) {
  core.skip(false);
  for (let i = 0; i < n; i++) {
    core.frame(0);
    core.audio();
    frames++;
    if (phase === "splash" && frames >= SPLASH_FRAMES) await enterGame();
  }
  draw();
  post({ type: "test", hash: core.hash(), phase });
  phase = "idle";
}

// --- the page's messages

onmessage = async (e) => {
  const m = e.data;
  try {
    switch (m.type) {
      case "init":
        canvas = m.canvas; ctx2d = canvas.getContext("2d"); debug = m.debug;
        nextTick(loop);
        break;
      case "audio":
        ring = m.port;
        ring.onmessage = (ev) => { played = ev.data; };
        written = 0; played = 0;
        break;
      case "open":
        splashOn = m.splash;
        testing = Boolean(m.test);
        await playGame({ name: m.name, data: m.data, sys: m.sys });
        if (m.test) await runTest(m.test);
        break;
      case "display": displayMs = m.ms; break;
      case "keys": word = m.word; keyBytes = m.bytes || null; break;
      case "input": anyInput = true; break;
      case "pause": paused = m.on; break;
      case "hidden": hidden = m.on; if (m.on) flushSave(); break;
      case "flush": flushSave(); break;
      case "reset": await reset(); break;
    }
  } catch (err) {
    phase = "idle";
    status(String(err.message || err), true);
  }
};
