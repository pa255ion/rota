// side track W: a PNG reader for the tests (the goldens: 8-bit grey, RGB or
// RGBA, not interlaced); each pixel as 0xFFRRGGBB, as the libraries hand them.
import { readFileSync } from "node:fs";
import { inflateSync } from "node:zlib";

export function readPng(path) {
  const b = readFileSync(path);
  let pos = 8, w = 0, h = 0, type = 0;
  const idat = [];
  while (pos < b.length) {
    const len = b.readUInt32BE(pos), tag = b.toString("latin1", pos + 4, pos + 8);
    const d = b.subarray(pos + 8, pos + 8 + len);
    if (tag === "IHDR") {
      w = d.readUInt32BE(0); h = d.readUInt32BE(4); type = d[9];
      if (d[8] !== 8 || d[12] !== 0 || (type !== 2 && type !== 6 && type !== 0)) throw new Error(`${path}: unsupported PNG`);
    } else if (tag === "IDAT") idat.push(d);
    pos += 12 + len;
  }
  const bpp = type === 6 ? 4 : type === 2 ? 3 : 1, stride = w * bpp;
  const raw = inflateSync(Buffer.concat(idat)), out = new Uint8Array(stride * h);
  for (let y = 0; y < h; y++) {
    const f = raw[y * (stride + 1)], src = raw.subarray(y * (stride + 1) + 1, (y + 1) * (stride + 1));
    for (let x = 0; x < stride; x++) {
      const a = x >= bpp ? out[y * stride + x - bpp] : 0, up = y ? out[(y - 1) * stride + x] : 0;
      const c = x >= bpp && y ? out[(y - 1) * stride + x - bpp] : 0;
      let v = src[x];
      if (f === 1) v += a; else if (f === 2) v += up; else if (f === 3) v += (a + up) >> 1;
      else if (f === 4) { const p = a + up - c, pa = Math.abs(p - a), pb = Math.abs(p - up), pc = Math.abs(p - c); v += pa <= pb && pa <= pc ? a : pb <= pc ? up : c; }
      out[y * stride + x] = v;
    }
  }
  const rgb = new Uint32Array(w * h);
  for (let i = 0; i < w * h; i++) {
    const r = out[i * bpp], g = out[i * bpp + (bpp > 1 ? 1 : 0)], bl = out[i * bpp + (bpp > 1 ? 2 : 0)];
    rgb[i] = (0xFF000000 | (r << 16) | (g << 8) | bl) >>> 0;
  }
  return { w, h, rgb };
}

