// rota in the browser (side track W): the Super Nintendo's drawing on its
// own core. The emulator's worker (worker.js) runs the machine with its
// render log exported (snes_new's -1 render threads: every change the
// drawing reads, in order, and each frame's end) and sends each frame's
// log here; a second instance of the library replays it on its own copy of
// the PPU ("Snes.SpecFast.RenderLog"'s replayer) and sends each finished
// frame's picture back, RGBA at the canvas's size. The same pictures as
// drawn inline (web/test/split.mjs), one frame later.

import { makeWasi } from "./wasi.js";

let x = null, h = 0, pic = 0, buf = 0, bufLen = 0;
const W = 512, H = 239;

async function start(lib) {
  const { wasi, setMemory } = makeWasi();
  const { instance } = await WebAssembly.instantiateStreaming(fetch(`rota-${lib}.wasm`), { wasi_snapshot_preview1: wasi });
  setMemory(instance.exports.memory);
  x = instance.exports;
  x._initialize();
  x.hs_init(0, 0);
  pic = x.web_alloc(4 * W * H);
  fresh();
}

// a replayer as the machine is at power-on (a new game, a restart)
function fresh() {
  if (h) x.snes_render_free(h);
  const out = x.web_alloc(4);
  x.snes_render_new(out);
  h = new DataView(x.memory.buffer).getUint32(out, true);
  x.web_free(out);
}

function feed(log) {
  if (log.length > bufLen) { if (buf) x.web_free(buf); bufLen = Math.max(log.length, 2 * bufLen, 65536); buf = x.web_alloc(4 * bufLen); }
  new Int32Array(x.memory.buffer, buf, log.length).set(log);
  return x.snes_render_feed(h, buf, log.length);
}

let ready = null;
onmessage = async (e) => {
  const m = e.data;
  try {
    if (m.type === "init") { ready = start(m.lib); await ready; postMessage({ type: "ready" }); return; }
    await ready;
    if (m.type === "reset") fresh();
    else if (m.type === "log") {
      const ended = feed(m.log);
      // the frame finished last (the earlier ones in the same log are
      // never shown): its picture, handed over
      if (ended > 0) {
        x.snes_render_pixels_rgba(h, pic);
        const rgba = new Uint8ClampedArray(4 * W * H);
        rgba.set(new Uint8Array(x.memory.buffer, pic, 4 * W * H));
        postMessage({ type: "pic", seq: m.seq, rgba }, [rgba.buffer]);
      } else postMessage({ type: "done", seq: m.seq });
    }
  } catch (err) {
    postMessage({ type: "error", text: String(err.message || err) });
  }
};
