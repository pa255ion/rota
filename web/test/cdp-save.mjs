// side track W: the page's battery saves end to end (web/test/save-page.sh):
// OpenSNES's SRAM test opened, A (write slot 1) pressed, the periodic save
// waited for; the page opened again in the same browser profile, B (read
// slot 1) pressed. Prints the status lines and the database's keys, and
// writes a screenshot; the shell script checks the picture's hash.
//   node web/test/cdp-save.mjs PORT URL SCREENSHOT
const [port, url, out] = process.argv.slice(2);
const v = await (await fetch(`http://127.0.0.1:${port}/json/version`)).json();
const ws = new WebSocket(v.webSocketDebuggerUrl); let id = 0; const pend = new Map();
ws.onmessage = (e) => { const m = JSON.parse(e.data); if (m.id && pend.has(m.id)) { pend.get(m.id)(m); pend.delete(m.id); } };
await new Promise((ok) => { ws.onopen = ok; });
const send = (method, params = {}, sessionId) => new Promise((ok) => { const i = ++id; pend.set(i, ok); ws.send(JSON.stringify({ id: i, method, params, ...(sessionId ? { sessionId } : {}) })); });
const wait = (ms) => new Promise((ok) => setTimeout(ok, ms));
const st = async (s) => (await send("Runtime.evaluate", { expression: "document.getElementById('status').textContent", returnByValue: true }, s)).result.result.value;
const key = async (s, code, k) => { await send("Input.dispatchKeyEvent", { type: "rawKeyDown", code, key: k }, s); await wait(300); await send("Input.dispatchKeyEvent", { type: "keyUp", code, key: k }, s); };
async function open() {
  const { result: { targetId } } = await send("Target.createTarget", { url: "about:blank" });
  const { result: { sessionId } } = await send("Target.attachToTarget", { targetId, flatten: true });
  await send("Page.navigate", { url }, sessionId);
  await wait(4000);
  return { targetId, sessionId };
}
// first: write slot 1 (A: X), then the periodic save (600 frames)
let t = await open();
console.log("first:", await st(t.sessionId));
await key(t.sessionId, "KeyX", "x");
await wait(14000);
const ev = async (sid, x) => (await send("Runtime.evaluate", { expression: x, returnByValue: true, awaitPromise: true }, sid)).result.result.value;
console.log("debug:", await ev(t.sessionId, "document.body.dataset.debug || ''"), "hidden:", await ev(t.sessionId, "document.hidden"));
console.log("db:", await ev(t.sessionId, "new Promise((ok) => { const r = indexedDB.open('rota', 1); r.onupgradeneeded = () => r.result.createObjectStore('saves'); r.onsuccess = () => { const q = r.result.transaction('saves').objectStore('saves').getAllKeys(); q.onsuccess = () => ok(JSON.stringify(q.result)); q.onerror = () => ok('err'); }; r.onerror = () => ok('open err'); })"));
await send("Target.closeTarget", { targetId: t.targetId });
// again: its save loaded; read slot 1 (B: Z)
t = await open();
console.log("again:", await st(t.sessionId));
await key(t.sessionId, "KeyZ", "z");
await wait(1500);
const shot = await send("Page.captureScreenshot", { format: "png" }, t.sessionId);
(await import("node:fs")).writeFileSync(out, Buffer.from(shot.result.data, "base64"));
await send("Target.closeTarget", { targetId: t.targetId }); ws.close();
