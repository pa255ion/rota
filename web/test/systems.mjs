// side track W, W.3's gate: each system's browser build in Node (its WASI),
// against the goldens the desktop libraries' Zig tests use
// (platform/desktop/<sys>_spec.zig), and its speed.
//   node web/test/systems.mjs [SYSTEM ...]      (from the repository's root)
// A case whose files are missing (roms/, build/: tools/fetch-roms.sh, the
// systems' asm targets) is skipped and said so; a mismatch fails.
import { existsSync, readFileSync } from "node:fs";
import { WASI } from "node:wasi";
import { readPng } from "./png.mjs";

async function load(sys) {
  const wasi = new WASI({ version: "preview1", args: [], env: {} });
  const { instance } = await WebAssembly.instantiate(readFileSync(`web/www/rota-${sys}.wasm`), { wasi_snapshot_preview1: wasi.wasiImport });
  wasi.initialize(instance);
  const x = instance.exports;
  x.hs_init(0, 0);
  const put = (bytes) => {
    const p = x.web_alloc(bytes.length);
    new Uint8Array(x.memory.buffer, p, bytes.length).set(bytes);
    return [p, bytes.length];
  };
  const out = x.web_alloc(4);
  const handle = (rc) => {
    if (rc !== 0) throw new Error(`${sys}_new: ${rc}`);
    return new DataView(x.memory.buffer).getUint32(out, true);
  };
  return { x, put, out, handle };
}

const REF = Number(process.env.REF ?? 0); // 1: the reference models (slow), for a mismatch
// (the NES: NES_MODE 0 the fast version, 1 the reference, 2 the performance version, the default;
// the Game Boy: GB_FAST 2 its fast version, the default, 0 the reference)
const file = (p) => readFileSync(p);
const none = new Uint8Array(0);
const hex = (v) => BigInt.asUintN(64, v).toString(16).padStart(16, "0");

// each case: the files it needs, how to make the machine, its frames and the
// key word at frame k, and what to compare
const cases = {
  gb: [
    ["dmg-acid2", ["roms/gb/dmg-acid2/dmg-acid2.gb"], 0, 30, () => 0, "systems/gb/test/golden/spec/dmg-acid2.png"],
    ["buttons right+A", ["build/gb/asm/buttons.gb"], 0, 30, () => 0x11, "systems/gb/test/golden/spec/buttons-right-a.png"],
    ["cgb-acid2", ["roms/gbc/cgb-acid2/cgb-acid2.gbc"], 1, 30, () => 0, "systems/gb/test/golden/spec/cgb-acid2.png"],
  ].map(([name, [rom], model, n, keys, png]) => ({
    name, needs: [rom, png], png, n, w: 160, h: 144,
    make: (e) => { const [p, l] = e.put(file(rom)); return e.handle(e.x.gb_new(p, l, model + Number(process.env.GB_FAST ?? 2), e.out)); },
    frame: (e, h, k) => e.x.gb_frame(h, keys(k)),
  })),
  chip8: ["2-ibm-logo:60", "3-corax+:300"].map((s) => {
    const [name, n] = s.split(":"), rom = `roms/chip8/timendus/${name}.ch8`, png = `systems/chip8/test/golden/spec/${name}.png`;
    return {
      name, needs: [rom, png], png, n: Number(n), w: 64, h: 32,
      make: (e) => { const [p, l] = e.put(file(rom)); return e.handle(e.x.chip8_new(p, l, 0, 200, e.out)); },
      frame: (e, h) => e.x.chip8_frame(h, 0),
    };
  }),
  nes: [
    // the pad as $4016 reads it: bit 0 A, 1 B, 2 Select, 3 Start, 4 Up, 5 Down, 6 Left, 7 Right
    ["nestest", "roms/nes/nestest/nestest.nes", 0, 30, "systems/nes/test/golden/spec/nestest.png"],
    ["ppu_scene", "build/nes/asm/ppu_scene.nes", 0, 10, "systems/nes/test/golden/spec/ppu_scene.png"],
    ["mmc5test exattr (Select)", "roms/nes/mmc5/mmc5test_v2.nes", 0x04, 120, "systems/nes/test/golden/spec/mmc5test_v2-exattr.png"],
  ].map(([name, rom, pad, n, png]) => ({
    name, needs: [rom, png], png, n, w: 256, h: 240,
    make: (e) => { const [p, l] = e.put(file(rom)); return e.handle(e.x.nes_new(p, l, Number(process.env.NES_MODE ?? 2), e.out)); },
    frame: (e, h, k) => e.x.nes_frame(h, k >= 30 ? pad : 0),
  })),
  a800: [
    ["READY", null, 300, "systems/a800xl/test/golden/ready.png"],
    ["rasterbars.xex", "build/a800xl/asm/rasterbars.xex", 100, "systems/a800xl/test/golden/rasterbars.png"],
  ].map(([name, xex, n, png]) => {
    const os = "roms/a800xl/os/altirraos_xl.rom", basic = "roms/a800xl/os/altirra_basic.rom", loader = "build/a800xl/xexload.bin";
    return {
      name, needs: [os, basic, loader, png, ...(xex ? [xex] : [])], png, n, w: 320, h: 240,
      make: (e) => {
        const [op, ol] = e.put(file(os)), [bp, bl] = e.put(file(basic));
        const [lp, ll] = e.put(xex ? file(loader) : none), [fp, fl] = e.put(xex ? file(xex) : none);
        e.keys = e.x.web_alloc(11);
        new Uint8Array(e.x.memory.buffer, e.keys, 11).fill(0);
        return e.handle(e.x.a800_new(op, ol, bp, bl, lp, ll, fp, fl, xex ? 1 : 0, 0, e.out));
      },
      frame: (e, h) => e.x.a800_frame(h, e.keys),
    };
  }),
  snes: [["krom/8x8BG1Map2BPP32x328PAL", 60], ["opensnes/extbg", 60], ["opensnes/mode5_hires", 120], ["games/unnamed-tech-demo-v3", 120]].map(([rel, n]) => {
    const rom = `roms/snes/${rel}.sfc`, ipl = "build/snes/ipl.bin", golden = `systems/snes/test/golden/frames/${rel.split("/").pop()}-${n}.txt`;
    return {
      name: rel, needs: [rom, ipl, golden], n, hash: () => readFileSync(golden, "utf8").trim(),
      make: (e) => { const [ip, il] = e.put(file(ipl)), [p, l] = e.put(file(rom)); return e.handle(e.x.snes_new(ip, il, p, l, 0, REF, e.out)); },
      frame: (e, h) => e.x.snes_frame(h, 0),
      got: (e, h) => hex(e.x.snes_hash(h)),
    };
  }),
};

// the Master System with its FM unit (Sms.Desktop): the script's frame
// hashes, as sms_spec.zig's expectHashes
const bits = { R: 1, L: 2, U: 4, D: 8, "2": 16, "1": 32, S: 128, r: 256, l: 512, u: 1024, d: 2048, "4": 4096, "3": 8192 };
cases.sms = [
  ["Waternet (GG)", "roms/gg/games/waternet.gg", 0, "systems/gg/test/input/waternet-start.txt", "systems/gg/test/golden/waternet.txt"],
  ["Raposa do Sol (SMS, no FM)", "roms/sms/games/raposadosol_v20.sms", 1, "systems/sms/test/input/raposa-start.txt", "systems/sms/test/golden/raposadosol.txt"],
].map(([name, rom, mode, scriptPath, golden]) => ({
  name, needs: [rom, scriptPath, golden], script: true,
  run: (e) => {
    const [p, l] = e.put(file(rom));
    const h = e.handle(e.x.sms_new(p, l, mode, 0, 0, 0, 0, e.out));
    const script = readFileSync(scriptPath, "utf8").split("\n").map((s) => s.split("#")[0].trim().split(/\s+/)).filter((t) => t.length >= 2 && /^\d+$/.test(t[0])).map(([f, b]) => [Number(f), [...b].reduce((w, c) => w | (bits[c] ?? 0), 0)]);
    const inputAt = (k) => { let w = 0; for (const [f, v] of script) { if (f > k) break; w = v; } return w; };
    let k = 0, bad = 0, n = 0;
    for (const line of readFileSync(golden, "utf8").trim().split("\n")) {
      const [at, want] = line.split(" ");
      while (k <= Number(at)) e.x.sms_frame(h, inputAt(k++));
      const got = hex(e.x.sms_hash(h));
      n++;
      if (got !== want) { console.log(`  frame ${at}: ${got}, want ${want}`); bad++; }
    }
    return [bad, `${n} frame hashes`, h];
  },
}));

// the frames a second each system's speed is measured over, and its rate
const rate = { gb: 59.73, chip8: 60, nes: 60.1, a800: 59.92, snes: 60.1, sms: 59.92 };
const speedFrame = { gb: (e, h) => e.x.gb_frame(h, 0), chip8: (e, h) => e.x.chip8_frame(h, 0), nes: (e, h) => e.x.nes_frame(h, 0), a800: (e, h) => e.x.a800_frame(h, e.keys), snes: (e, h) => e.x.snes_frame(h, 0), sms: (e, h) => e.x.sms_frame(h, 0) };

let failed = 0;
for (const sys of process.argv.length > 2 ? process.argv.slice(2) : Object.keys(cases)) {
  console.log(`== ${sys}`);
  let last = null;
  for (const c of cases[sys]) {
    const missing = c.needs.filter((p) => !existsSync(p));
    if (missing.length) { console.log(`  ${c.name}: skipped (missing ${missing.join(", ")})`); continue; }
    const e = await load(sys);
    let bad, what, h;
    if (c.run) [bad, what, h] = c.run(e);
    else {
      h = c.make(e);
      for (let k = 0; k < c.n; k++) c.frame(e, h, k);
      if (c.hash) {
        const got = c.got(e, h), want = c.hash();
        bad = got === want ? 0 : 1; what = `hash ${got}${bad ? `, want ${want}` : ""}`;
      } else {
        const img = readPng(c.png);
        if (img.w !== c.w || img.h !== c.h) throw new Error(`${c.png}: ${img.w}x${img.h}`);
        const p = e.x.web_alloc(4 * c.w * c.h);
        e.x[`${sys}_pixels`](h, p);
        const px = new Uint32Array(e.x.memory.buffer, p, c.w * c.h);
        bad = 0;
        for (let i = 0; i < px.length; i++) if (px[i] !== img.rgb[i]) bad++;
        what = bad ? `${bad} pixels differ` : "the golden's pixels";
      }
    }
    console.log(`  ${c.name}: ${bad ? "FAIL" : "OK"} (${what})`);
    if (bad) failed++;
    last = [e, h];
  }
  if (last) {
    const [e, h] = last, n = 600, t0 = performance.now();
    for (let i = 0; i < n; i++) speedFrame[sys](e, h);
    const s = (performance.now() - t0) / 1000;
    console.log(`  speed: ${n} frames in ${s.toFixed(2)} s, ${(n / rate[sys] / s).toFixed(2)}x real time`);
  }
}
process.exit(failed ? 1 : 0);
