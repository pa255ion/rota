// rota in the browser (side track W): the page. The emulator runs in a
// Web Worker (worker.js: the systems' libraries, the frames, saves, the
// picture on this page's canvas, the sound into this page's AudioWorklet);
// here: the controls, the keyboard and gamepads (as the key word, sent when
// it changes), the sound's output (the context, the ring, the volume) and
// what the worker reports.

import { SYSTEMS, systemOf, a800Bytes, a800Typed } from "./systems.js";

const AUDIO_RATE = 48000;          // every library samples at 48 kHz
const AUDIO_TARGET = 4800;         // the ring's target queue (100 ms; the worker keeps it filled)
const AUDIO_RESUME = 2400;         // after running dry: 50 ms queued before it plays again

const $ = (id) => document.getElementById(id);
const params = new URLSearchParams(location.search);
const DEBUG = params.has("debug");
// ?test=N (the page's headless test): N frames run at once after loading
// (the splash's first, if on), the picture drawn, its hash in data-hash
const TEST = Number(params.get("test") || 0);
const canvas = $("screen");
const status = (text, error = false) => { $("status").textContent = text; $("status").className = error ? "error" : ""; };

// --- the worker

const worker = new Worker(new URL("worker.js", import.meta.url), { type: "module" });
const offscreen = canvas.transferControlToOffscreen();
worker.postMessage({ type: "init", canvas: offscreen, debug: DEBUG }, [offscreen]);
let game = null;                   // the game's system, once one is picked (the keys panel's)
let paused = false;
worker.onmessage = (e) => {
  const m = e.data;
  switch (m.type) {
    case "status": status(m.text, m.error); break;
    case "size": picture = { w: m.w, h: m.h * (m.sy || 1) }; fitCanvas(); break;
    case "speed":
      $("speed-out").textContent = `${m.ratio.toFixed(2)}x real time (${m.ms.toFixed(1)} ms a frame; a drawn one ${m.drawnMs.toFixed(1)} ms)${m.half ? ", half rate (every other picture drawn)" : ""}${m.dry !== null ? `, sound ran dry ${m.dry} times` : ""}`;
      break;
    case "debug": document.body.dataset.debug = m.text; break;
    case "test": document.body.dataset.hash = m.hash; document.body.dataset.phase = m.phase; break;
  }
};
worker.onerror = (e) => status(`the emulator didn't load: ${e.message || e}`, true);

// --- input: the keyboard (rebindable, per system) and gamepads, as the key word

const PAUSE = { id: "pause", name: "Pause the emulator", bit: 0, keys: ["KeyP"] };
let keysSys = "gg";                // the system the Keys panel shows (the game's)
// (the 800XL types on the whole keyboard: its pause key is F9)
const actionsOf = (sys) => [...SYSTEMS[sys].actions, SYSTEMS[sys].pauseKeys ? { ...PAUSE, keys: SYSTEMS[sys].pauseKeys } : PAUSE];
const storeOf = (sys) => SYSTEMS[sys].keyStore || `rota.keys.${sys}`;
const binds = {};                  // system -> action id -> [key codes]
function loadBinds(sys) {
  const b = Object.fromEntries(actionsOf(sys).map((a) => [a.id, [...a.keys]]));
  try {
    const saved = JSON.parse(localStorage.getItem(storeOf(sys)) || "null");
    if (saved) for (const a of actionsOf(sys)) if (Array.isArray(saved[a.id])) b[a.id] = saved[a.id].filter((k) => typeof k === "string");
  } catch { /* storage off, or a bad value: the defaults */ }
  binds[sys] = b;
}
const bindsOf = (sys) => { if (!binds[sys]) loadBinds(sys); return binds[sys]; };
function saveBinds(sys) { try { localStorage.setItem(storeOf(sys), JSON.stringify(binds[sys])); } catch { /* storage off */ } }
const playing = () => game ?? keysSys;
const actionOf = (code) => { const b = bindsOf(playing()); return actionsOf(playing()).find((a) => b[a.id].includes(code)); };

const held = new Set();
let rebinding = null;              // the action waiting for its new key
window.addEventListener("keydown", (e) => {
  if (rebinding) { e.preventDefault(); finishRebind(e.code); return; }
  if (e.target.tagName === "SELECT") return;
  // the 800XL's keyboard: typed keys go to the machine (the joystick's and
  // pause key's bindings first)
  if (SYSTEMS[playing()].keyBytes && !actionOf(e.code) && a800Typed(e.code)) {
    e.preventDefault();
    held.add(e.code); sendKeys();
    if (!e.repeat) worker.postMessage({ type: "input" });
    return;
  }
  const a = actionOf(e.code);
  if (a) {
    e.preventDefault();
    if (a.id === "pause") { if (!e.repeat) { paused = !paused; worker.postMessage({ type: "pause", on: paused }); } }
    else { held.add(e.code); sendKeys(); }
  }
  if (!e.repeat) worker.postMessage({ type: "input" });
});
window.addEventListener("keyup", (e) => { held.delete(e.code); sendKeys(); });
canvas.addEventListener("pointerdown", () => worker.postMessage({ type: "input" }));

// the Keys panel: the system's actions, their keys, a button to change them
function showKeys(sys) { keysSys = sys; rebinding = null; drawKeys(); }
function drawKeys() {
  const sys = keysSys, b = bindsOf(sys);
  $("keys-title").textContent = SYSTEMS[sys].title;
  const t = $("keys");
  t.replaceChildren();
  for (const a of actionsOf(sys)) {
    const row = document.createElement("tr");
    const name = document.createElement("td");
    name.textContent = a.name;
    const keys = document.createElement("td");
    keys.textContent = rebinding === a.id ? "press a key… (Esc: cancel, Backspace: none)" : b[a.id].map(keyName).join(", ") || "(none)";
    const act = document.createElement("td");
    const btn = document.createElement("button");
    btn.type = "button";
    btn.textContent = "change";
    btn.addEventListener("click", () => { rebinding = a.id; drawKeys(); btn.blur(); });
    act.append(btn);
    row.append(name, keys, act);
    t.append(row);
  }
}
function finishRebind(code) {
  const id = rebinding, sys = keysSys, b = bindsOf(sys);
  rebinding = null;
  if (code === "Backspace") b[id] = [];
  else if (code !== "Escape") {
    // the key leaves any other action it was on
    for (const a of actionsOf(sys)) b[a.id] = b[a.id].filter((k) => k !== code);
    b[id] = [code];
  }
  saveBinds(sys);
  drawKeys();
}
const keyName = (code) => code.replace(/^Key/, "").replace(/^Digit/, "").replace(/^Arrow/, "");
$("keys-reset").addEventListener("click", () => { try { localStorage.removeItem(storeOf(keysSys)); } catch { /* */ } loadBinds(keysSys); drawKeys(); });

let padWas = false;
function keyWord() {
  let w = 0;
  for (const k of held) { const a = actionOf(k); if (a) w |= a.bit; }
  for (const pad of navigator.getGamepads ? navigator.getGamepads() : []) {
    if (!pad) continue;
    const pressed = (i) => pad.buttons[i] && pad.buttons[i].pressed;
    for (const a of actionsOf(playing())) {
      if ((a.pad || []).some(pressed)) w |= a.bit;
      if (a.axis && pad.axes[a.axis[0]] * a.axis[1] > 0.5) w |= a.bit;
    }
    const any = pad.buttons.some((b) => b.pressed);
    if (any && !padWas) worker.postMessage({ type: "input" });
    padWas = any;
  }
  return w;
}

// the key word to the worker when it changes (the keyboard's at once, the
// gamepads' polled once a display frame)
let sentWord = -1, sentBytes = "";
function sendKeys() {
  const w = keyWord();
  const bytes = SYSTEMS[playing()].keyBytes ? a800Bytes(held, w) : null;
  const b = bytes ? bytes.join() : "";
  if (w !== sentWord || b !== sentBytes) { sentWord = w; sentBytes = b; worker.postMessage({ type: "keys", word: w, bytes }); }
}
// the display's refresh (the median of the last 60 frames here, on the
// page's thread: the worker's own frames stretch when it is busy)
let lastPoll = 0, sentDisplay = 0;
const pollGaps = [];
function poll(t) {
  requestAnimationFrame(poll);
  if (lastPoll) {
    pollGaps.push(t - lastPoll);
    if (pollGaps.length > 60) pollGaps.shift();
    if (pollGaps.length >= 20) {
      const ms = [...pollGaps].sort((a, b) => a - b)[pollGaps.length >> 1];
      if (Math.abs(ms - sentDisplay) > 0.005 * ms) { sentDisplay = ms; worker.postMessage({ type: "display", ms }); }
    }
  }
  lastPoll = t;
  if (navigator.getGamepads && [...navigator.getGamepads()].some((p) => p)) sendKeys();
}
requestAnimationFrame(poll);

// --- sound: the context and the ring here, the worker feeding the ring

let audio = null;
// The ring (an AudioWorklet): 48 kHz stereo in, the context's rate out,
// linearly interpolated, at a step that follows the queue (1% at most, a
// sixth of a semitone, reached slowly: the worker runs one frame a display
// frame, and this takes up the display's rate against the system's). It reports the input frames it has used every 8
// blocks of 128. Running dry, it holds the last sample and lets it fade
// (5 ms) instead of dropping to zero; it plays again once AUDIO_RESUME is
// queued, fading in: a late frame makes a soft dip, not a click.
const WORKLET = `
const TARGET = ${AUDIO_TARGET}, RESUME = ${AUDIO_RESUME}, STEP = ${AUDIO_RATE} / sampleRate;
const FADE = 1 / (0.005 * sampleRate), DECAY = Math.exp(-1 / (0.001 * sampleRate));
class Ring extends AudioWorkletProcessor {
  constructor() {
    super();
    this.n = 48000;                           // a second of stereo frames
    this.buf = new Float32Array(2 * this.n);
    this.r = 0; this.w = 0; this.frac = 0; this.used = 0; this.blocks = 0;
    this.dry = true; this.env = 0; this.lastL = 0; this.lastR = 0;
    // the worker's port (the page hands it over first): samples in,
    // reports out
    this.link = null;
    this.port.onmessage = (e) => {
      this.link = e.data.port;
      this.link.onmessage = (m) => {
        const s = m.data, b = this.buf, len = b.length;
        let w = 2 * this.w;
        for (let i = 0; i < s.length; i++) { b[w] = s[i]; w = (w + 1) % len; }
        this.w = w / 2;
      };
    };
  }
  process(_in, outs) {
    const [left, right] = outs[0], n = this.n, b = this.buf;
    let queued = (this.w - this.r + n) % n;
    if (this.dry && queued >= RESUME) this.dry = false;
    const lead = Math.max(-1, Math.min(1, (queued - TARGET) / TARGET));
    const step = STEP * (1 + 0.01 * lead);
    for (let i = 0; i < left.length; i++) {
      if (!this.dry && queued >= 2) {
        const j = 2 * this.r, k = 2 * ((this.r + 1) % n), f = this.frac;
        const l = b[j] + (b[k] - b[j]) * f, r = b[j + 1] + (b[k + 1] - b[j + 1]) * f;
        this.env = Math.min(1, this.env + FADE);
        this.lastL = l * this.env; this.lastR = r * this.env;
        this.frac += step;
        while (this.frac >= 1) { this.frac -= 1; this.r = (this.r + 1) % n; queued--; this.used++; }
      } else {
        // dry: the last sample fades out; the ring waits for RESUME
        this.dry = true; this.env = 0;
        this.lastL *= DECAY; this.lastR *= DECAY;
      }
      left[i] = this.lastL; right[i] = this.lastR;
    }
    if (++this.blocks % 8 === 0 && this.link) this.link.postMessage(this.used);
    return true;
  }
}
registerProcessor("ring", Ring);`;
// (sound that doesn't start within 2 s, as in a browser without an output:
// the game runs without it, paced by the clock)
async function startAudio() {
  if (audio) return;
  await Promise.race([openAudio(), new Promise((ok) => setTimeout(ok, 2000))]);
}
async function openAudio() {
  try {
    const ctx = new AudioContext({ sampleRate: AUDIO_RATE, latencyHint: "interactive" });
    await ctx.audioWorklet.addModule(URL.createObjectURL(new Blob([WORKLET], { type: "text/javascript" })));
    const node = new AudioWorkletNode(ctx, "ring", { outputChannelCount: [2] });
    const gain = ctx.createGain();
    node.connect(gain).connect(ctx.destination);
    // the worker talks to the ring directly: samples in, reports out
    const ch = new MessageChannel();
    node.port.postMessage({ port: ch.port1 }, [ch.port1]);
    worker.postMessage({ type: "audio", port: ch.port2 }, [ch.port2]);
    audio = { ctx, node, gain };
    setVolume();
  } catch (e) { console.warn("rota: no sound", e); }
}
function setVolume() { if (audio) audio.gain.gain.value = $("mute").checked ? 0 : 1; }
$("mute").addEventListener("change", setVolume);

// --- the page

let picture = { w: 160, h: 144 };
function fitCanvas() {
  // the largest whole scale that fits (the canvas's own size is the worker's)
  const s = Math.max(1, Math.floor(Math.min((innerWidth - 32) / picture.w, (innerHeight - 200) / picture.h)));
  canvas.style.width = `${picture.w * s}px`;
  canvas.style.height = `${picture.h * s}px`;
}
window.addEventListener("resize", fitCanvas);

const chosen = (name) => ($("system").value === "auto" ? systemOf(name) : $("system").value);
async function openData(name, data, test = 0) {
  const sys = chosen(name);
  if (!sys) return status(`${name}: which system? (pick one above)`, true);
  game = sys;
  showKeys(sys);
  if (document.activeElement && document.activeElement !== document.body) document.activeElement.blur();
  held.clear(); sentWord = -1; sendKeys();
  worker.postMessage({ type: "open", name, data, sys, splash: $("splash").checked, test }, [data.buffer]);
}
async function openFile(file) {
  if (!file) return;
  await startAudio();
  await openData(file.name, new Uint8Array(await file.arrayBuffer()));
}
$("file").addEventListener("change", (e) => openFile(e.target.files[0]));
document.addEventListener("dragover", (e) => e.preventDefault());
document.addEventListener("drop", (e) => { e.preventDefault(); openFile(e.dataTransfer.files[0]); });
$("reset").addEventListener("click", () => { worker.postMessage({ type: "reset" }); $("reset").blur(); });
$("fullscreen").addEventListener("click", () => canvas.requestFullscreen && canvas.requestFullscreen());
$("system").addEventListener("change", () => { if ($("system").value !== "auto" && !game) showKeys($("system").value); });
document.addEventListener("visibilitychange", () => worker.postMessage({ type: "hidden", on: document.hidden }));
window.addEventListener("pagehide", () => worker.postMessage({ type: "flush" }));

// the speed readout, remembered (off unless turned on)
try { $("speed").checked = localStorage.getItem("rota.speed") === "on"; } catch { /* storage off */ }
if (params.has("speed")) $("speed").checked = true;  // (?speed: this visit)
const speedShown = () => { $("speed-out").hidden = !$("speed").checked; };
$("speed").addEventListener("change", () => { try { localStorage.setItem("rota.speed", $("speed").checked ? "on" : "off"); } catch { /* */ } speedShown(); });
speedShown();

// the splash toggle, remembered (on unless turned off)
try { $("splash").checked = localStorage.getItem("rota.splash") !== "off"; } catch { /* storage off */ }
// (?splash=0 or 1: this visit only, not remembered)
if (params.get("splash") !== null) $("splash").checked = params.get("splash") !== "0";
$("splash").addEventListener("change", () => { try { localStorage.setItem("rota.splash", $("splash").checked ? "on" : "off"); } catch { /* storage off */ } });
if (params.get("system") && SYSTEMS[params.get("system")]) $("system").value = params.get("system");

showKeys("gg");
fitCanvas();
status("pick a ROM (Game Gear, Master System, Game Boy or Color, NES, CHIP-8), or drop one here");
// ?rom=URL (a file on this site, e.g. a free homebrew game): loaded at once
const url = params.get("rom");
if (url) (async () => {
  const res = await fetch(url);
  if (!res.ok) return status(`no ${url} here`, true);
  // (?sound: the sound started without a click, for a test with
  // --autoplay-policy=no-user-gesture-required)
  if (params.has("sound")) await startAudio();
  await openData(url.split("/").pop(), new Uint8Array(await res.arrayBuffer()), TEST);
})().catch((e) => status(`the emulator didn't load: ${e.message || e}`, true));
