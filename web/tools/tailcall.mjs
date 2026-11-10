// side track W: GHC's WebAssembly code returns the next block's table index
// to StgRun's loop, which calls it. Here every such return becomes a
// WebAssembly tail call: a constant index a direct return_call, a computed
// one a return_call_indirect (0, "back to the scheduler", still returned).
// Only functions of type () -> i32 in the table (GHC's code blocks) are
// touched; a return left as it was still works (StgRun calls it).
//   node tailcall.mjs IN.wasm OUT.wasm   (web/build.sh runs it; npm ci here first)
// WebAssembly tail calls: Chrome 112, Firefox 121, Safari 18.2, Node 20.
import binaryen from "binaryen";
import { readFileSync, writeFileSync } from "node:fs";
const [inp, outp] = process.argv.slice(2);
const m = binaryen.readBinary(readFileSync(inp));
m.setFeatures(binaryen.Features.All);
const i32 = binaryen.i32, none = binaryen.none;

// the table: index -> function name
const table = new Map();
let tableName = null;
for (let i = 0; i < m.getNumElementSegments(); i++) {
  const s = binaryen.getElementSegmentInfo(m.getElementSegmentByIndex(i));
  tableName = s.table;
  const off = binaryen.getExpressionInfo(s.offset);
  if (off.id !== binaryen.ConstId) throw new Error("element offset not constant");
  s.data.forEach((n, k) => table.set(off.value + k, n));
}
// GHC's code blocks: () -> i32, in the table (all named _blk_..., ..._entry,
// ..._slow or stg_... in our builds)
const inTable = new Set(table.values());
const code = new Set();
for (let i = 0; i < m.getNumFunctions(); i++) {
  const f = binaryen.getFunctionInfo(m.getFunctionByIndex(i));
  if (f.params === none && f.results === i32 && inTable.has(f.name)) code.add(f.name);
}

let direct = 0, indirect = 0;
// the replacement of a value returned to StgRun, in function fn
function tail(fn, value) {
  const v = binaryen.getExpressionInfo(value);
  if (v.id === binaryen.ConstId && v.type === i32) {
    const target = table.get(v.value);
    if (target && code.has(target)) { direct++; return m.return_call(target, [], i32); }
    return null;
  }
  indirect++;
  const tmp = binaryen._BinaryenFunctionAddVar(fn, i32);
  return m.block(null, [
    m.local.set(tmp, value),
    m.if(m.i32.eqz(m.local.get(tmp, i32)), m.return(m.i32.const(0))),
    m.return_call_indirect(tableName, m.local.get(tmp, i32), [], none, i32),
  ], binaryen.unreachable);
}
// returns inside the control flow (blocks, ifs, loops)
function walk(fn, e) {
  const info = binaryen.getExpressionInfo(e);
  switch (info.id) {
    case binaryen.ReturnId: {
      if (!info.value) return e;
      const r = tail(fn, info.value);
      return r ?? e;
    }
    case binaryen.BlockId: {
      const n = binaryen._BinaryenBlockGetNumChildren(e);
      for (let k = 0; k < n; k++) {
        const c = binaryen._BinaryenBlockGetChildAt(e, k);
        const c2 = walk(fn, c);
        if (c2 !== c) binaryen._BinaryenBlockSetChildAt(e, k, c2);
      }
      return e;
    }
    case binaryen.IfId: {
      const t = info.ifTrue, t2 = walk(fn, t);
      if (t2 !== t) binaryen._BinaryenIfSetIfTrue(e, t2);
      if (info.ifFalse) { const f = info.ifFalse, f2 = walk(fn, f); if (f2 !== f) binaryen._BinaryenIfSetIfFalse(e, f2); }
      return e;
    }
    case binaryen.LoopId: {
      const b = info.body, b2 = walk(fn, b);
      if (b2 !== b) binaryen._BinaryenLoopSetBody(e, b2);
      return e;
    }
    default:
      return e;
  }
}
let fns = 0;
for (const name of code) {
  const fn = m.getFunction(name);
  const body = binaryen._BinaryenFunctionGetBody(fn);
  let b = walk(fn, body);
  // the body's own value (the function's implicit return)
  if (binaryen.getExpressionInfo(b).type === i32) {
    const r = tail(fn, b);
    if (r) b = r;
  }
  binaryen._BinaryenFunctionSetBody(fn, b);
  fns++;
}
m.setFeatures(binaryen.Features.All);
if (!m.validate()) throw new Error("the rewritten module doesn't validate");
writeFileSync(outp, m.emitBinary());
console.log(`tailcall: ${fns} functions, ${direct} direct and ${indirect} indirect tail calls`);
