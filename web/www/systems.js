// rota in the browser (side track W): the systems, as the page (its keys
// panel, the key word) and the emulator's worker (worker.js: the library,
// its arguments, rate and picture) both use them.

// --- the systems: their library, its <lib>_new's arguments after the ROM,
// rate, picture, buttons (the key word's bits, as the library's frame call
// takes them) and splash

// the standard gamepad's buttons (w3c "standard" mapping)
export const PAD = { south: 0, east: 1, west: 2, north: 3, l: 4, r: 5, select: 8, start: 9, up: 12, down: 13, left: 14, right: 15 };
const dpad = (up, down, left, right) => [
  { id: "up", name: "Up", bit: up, keys: ["ArrowUp"], pad: [PAD.up], axis: [1, -1] },
  { id: "down", name: "Down", bit: down, keys: ["ArrowDown"], pad: [PAD.down], axis: [1, 1] },
  { id: "left", name: "Left", bit: left, keys: ["ArrowLeft"], pad: [PAD.left], axis: [0, -1] },
  { id: "right", name: "Right", bit: right, keys: ["ArrowRight"], pad: [PAD.right], axis: [0, 1] },
];
const SEGA = [
  ...dpad(4, 8, 2, 1),
  { id: "b1", name: "Button 1", bit: 32, keys: ["KeyZ", "KeyK"], pad: [PAD.south, PAD.west] },
  { id: "b2", name: "Button 2", bit: 16, keys: ["KeyX", "KeyJ"], pad: [PAD.east, PAD.north] },
  { id: "start", name: "Start / Pause", bit: 128, keys: ["Enter"], pad: [PAD.start] },
];
const CHIP8_KEYS = ["1", "2", "3", "C", "4", "5", "6", "D", "7", "8", "9", "E", "A", "0", "B", "F"];
const CHIP8_CODES = ["Digit1", "Digit2", "Digit3", "Digit4", "KeyQ", "KeyW", "KeyE", "KeyR", "KeyA", "KeyS", "KeyD", "KeyF", "KeyZ", "KeyX", "KeyC", "KeyV"];

// The Atari 800XL's keyboard (platform/ps/a800_keys.zig's layout, the
// board's and the desktop's): a key's code (KeyboardEvent.code) -> its
// POKEY key code (k), the same with CONTROL (c), SHIFT, CONTROL, BREAK, or
// a console key (0 START, 1 SELECT, 2 OPTION). Positional where the 800XL
// has the key; ' is +, \\ is *, [ ] are < >, ` is ESC; F1 HELP, F2-F4 the
// console keys, F6 the inverse key, F8 and Pause BREAK; the arrows are
// CONTROL with - = + *, Delete CONTROL+Backspace, Insert CONTROL+>.
const A8K = {
  KeyL: 0x00, KeyJ: 0x01, Semicolon: 0x02, KeyK: 0x05, Quote: 0x06, Backslash: 0x07, KeyO: 0x08, KeyP: 0x0A,
  KeyU: 0x0B, Enter: 0x0C, NumpadEnter: 0x0C, KeyI: 0x0D, Minus: 0x0E, Equal: 0x0F, KeyV: 0x10, F1: 0x11,
  KeyC: 0x12, KeyB: 0x15, KeyX: 0x16, KeyZ: 0x17, Digit4: 0x18, Digit3: 0x1A, Digit6: 0x1B, Backquote: 0x1C,
  Digit5: 0x1D, Digit2: 0x1E, Digit1: 0x1F, Comma: 0x20, Space: 0x21, Period: 0x22, KeyN: 0x23, KeyM: 0x25,
  Slash: 0x26, F6: 0x27, KeyR: 0x28, KeyE: 0x2A, KeyY: 0x2B, Tab: 0x2C, KeyT: 0x2D, KeyW: 0x2E, KeyQ: 0x2F,
  Digit9: 0x30, Digit0: 0x32, Digit7: 0x33, Backspace: 0x34, Digit8: 0x35, BracketLeft: 0x36,
  BracketRight: 0x37, KeyF: 0x38, KeyH: 0x39, KeyD: 0x3A, CapsLock: 0x3C, KeyG: 0x3D, KeyS: 0x3E, KeyA: 0x3F,
};
const A8C = { ArrowUp: 0x0E, ArrowDown: 0x0F, ArrowLeft: 0x06, ArrowRight: 0x07, Delete: 0x34, Insert: 0x37 };
const A8M = { ShiftLeft: 1, ShiftRight: 1, ControlLeft: 2, ControlRight: 2, F8: 4, Pause: 4 };
const A8CON = { F2: 0, F3: 1, F4: 2 };
export const a800Typed = (code) => code in A8K || code in A8C || code in A8M || code in A8CON;
// the 11 bytes a800_frame takes: 0-7 the key matrix (bit n % 8 of byte
// n / 8 = key code n), 8 SHIFT/CONTROL/BREAK, 9 START/SELECT/OPTION, 10 the
// joystick (from the key word)
export function a800Bytes(codes, word) {
  const out = new Array(11).fill(0);
  const key = (k) => { out[k >> 3] |= 1 << (k & 7); };
  for (const c of codes) {
    if (c in A8K) key(A8K[c]);
    else if (c in A8C) { key(A8C[c]); out[8] |= 2; }
    else if (c in A8M) out[8] |= A8M[c];
    else if (c in A8CON) out[9] |= 1 << A8CON[c];
  }
  out[10] = word & 0x1F;
  return out;
}

export const SYSTEMS = {
  gg: {
    title: "Game Gear", lib: "sms", ext: /\.gg$/i, splash: "rota.gg", hz: 59.92, sized: true, saves: true,
    args: (rom) => [...rom, 0, 0, 0, 0, 0], actions: SEGA, keyStore: "rota.keys",
  },
  sms: {
    title: "Master System", lib: "sms", ext: /\.sms$/i, splash: "rota.sms", hz: 59.92, sized: true, saves: true,
    // with its FM unit (the desktop's default; games that don't use it sound the same)
    args: (rom) => [...rom, 1, 1, 0, 0, 0], actions: SEGA, keyStore: "rota.keys",
  },
  gb: {
    title: "Game Boy / Color", lib: "gb", ext: /\.gbc?$/i, splash: "rota.gb", hz: 4194304 / 70224, w: 160, h: 144, saves: true,
    // a cartridge that says it's for the Color (0143h bit 7) runs on one
    // the fast version ("Gb.SpecFast": gb_new's models 2 and 3)
    args: (rom, data) => [...rom, data.length > 0x143 && data[0x143] & 0x80 ? 3 : 2],
    actions: [
      ...dpad(4, 8, 2, 1),
      { id: "a", name: "A", bit: 16, keys: ["KeyX", "KeyK"], pad: [PAD.east, PAD.north] },
      { id: "b", name: "B", bit: 32, keys: ["KeyZ", "KeyJ"], pad: [PAD.south, PAD.west] },
      { id: "select", name: "Select", bit: 64, keys: ["ShiftLeft", "ShiftRight"], pad: [PAD.select] },
      { id: "start", name: "Start", bit: 128, keys: ["Enter"], pad: [PAD.start] },
    ],
  },
  nes: {
    title: "NES", lib: "nes", ext: /\.nes$/i, splash: "rota.nes", hz: 1789773 / 29780.5, w: 256, h: 240, saves: true,
    // the performance version ("Nes.SpecPerf": nes_new's 2)
    args: (rom) => [...rom, 2],
    // the pad as $4016 reads it: A, B, Select, Start, Up, Down, Left, Right
    actions: [
      ...dpad(16, 32, 64, 128),
      { id: "a", name: "A", bit: 1, keys: ["KeyX", "KeyK"], pad: [PAD.east, PAD.north] },
      { id: "b", name: "B", bit: 2, keys: ["KeyZ", "KeyJ"], pad: [PAD.south, PAD.west] },
      { id: "select", name: "Select", bit: 4, keys: ["ShiftLeft", "ShiftRight"], pad: [PAD.select] },
      { id: "start", name: "Start", bit: 8, keys: ["Enter"], pad: [PAD.start] },
    ],
  },
  a800: {
    title: "Atari 800XL", lib: "a800", ext: /\.(xex|car|rom|cas)$/i, splash: "rota.xex", hz: 59.92, w: 320, h: 240,
    // AltirraOS and Altirra BASIC (Avery Lee's, all-permissive: their
    // notice in altirra-notice.txt) and our .xex loader, fetched with the
    // library; the file's kind by its name: 1 a program (.xex, through the
    // loader), 2 a cartridge (.car, or a raw .rom), 3 a cassette (.cas,
    // through the OS's own loader); 0: the fast version
    files: ["altirraos_xl.rom", "altirra_basic.rom", "xexload.bin"],
    args: (rom, data, name, [os, basic, loader]) =>
      [...os, ...basic, ...loader, ...rom, /\.xex$/i.test(name) ? 1 : /\.cas$/i.test(name) ? 3 : 2, 0],
    // the keyboard as itself (a800Bytes), the joystick on the keypad (or a gamepad)
    keyBytes: true, pauseKeys: ["F9"],
    actions: [
      { id: "up", name: "Joystick up", bit: 1, keys: ["Numpad8"], pad: [PAD.up], axis: [1, -1] },
      { id: "down", name: "Joystick down", bit: 2, keys: ["Numpad2", "Numpad5"], pad: [PAD.down], axis: [1, 1] },
      { id: "left", name: "Joystick left", bit: 4, keys: ["Numpad4"], pad: [PAD.left], axis: [0, -1] },
      { id: "right", name: "Joystick right", bit: 8, keys: ["Numpad6"], pad: [PAD.right], axis: [0, 1] },
      { id: "trigger", name: "Joystick trigger", bit: 16, keys: ["Numpad0", "AltRight"], pad: [PAD.south, PAD.east] },
    ],
  },
  snes: {
    title: "Super Nintendo", lib: "snes", ext: /\.(sfc|smc)$/i, splash: "rota.sfc", hz: 21477272 / 357366, w: 512, h: 478, saves: true,
    // (snes_pixels gives each line twice: the canvas takes every other one, the page stretches it)
    rows: 2,
    // our own SPC700 boot ROM (systems/snes/ipl), never Nintendo's; drawn
    // inline (no render threads), the fast version
    files: ["ipl.bin"],
    // (split: the drawing on its own core, the page's second worker,
    // render.js: snes_new's -1 render threads exports the render log)
    split: true,
    args: (rom, data, name, [ipl], opts) => [...ipl, ...rom, opts && opts.split ? -1 : 0, 0],
    // pad 1 as snes_frame takes it: 11 B, 10 Y, 9 Select, 8 Start, 7 up,
    // 6 down, 5 left, 4 right, 3 A, 2 X, 1 L, 0 R
    actions: [
      ...dpad(128, 64, 32, 16),
      { id: "b", name: "B", bit: 2048, keys: ["KeyZ"], pad: [PAD.south] },
      { id: "a", name: "A", bit: 8, keys: ["KeyX"], pad: [PAD.east] },
      { id: "y", name: "Y", bit: 1024, keys: ["KeyA"], pad: [PAD.west] },
      { id: "x", name: "X", bit: 4, keys: ["KeyS"], pad: [PAD.north] },
      { id: "l", name: "L", bit: 2, keys: ["KeyQ"], pad: [PAD.l] },
      { id: "r", name: "R", bit: 1, keys: ["KeyW"], pad: [PAD.r] },
      { id: "select", name: "Select", bit: 512, keys: ["ShiftLeft", "ShiftRight"], pad: [PAD.select] },
      { id: "start", name: "Start", bit: 256, keys: ["Enter"], pad: [PAD.start] },
    ],
  },
  chip8: {
    title: "CHIP-8", lib: "chip8", ext: /\.(ch8|c8)$/i, hz: 60, w: 64, h: 32,
    // the COSMAC VIP's quirks, 12 instructions a frame (the board's slow speed)
    args: (rom) => [...rom, 0, 12],
    actions: CHIP8_KEYS.map((k, i) => ({ id: `k${k}`, name: `Key ${k}`, bit: 1 << parseInt(k, 16), keys: [CHIP8_CODES[i]] })),
  },
};
export const systemOf = (name) => Object.keys(SYSTEMS).find((s) => SYSTEMS[s].ext.test(name));
