// side track W: the page test's browser driver. Loads a URL in a headless
// Chromium already listening on a debugging port, in real time (the
// emulator runs in a Web Worker, which headless Chromium's virtual time
// doesn't wait for), and prints document.body.dataset.hash once the page
// has set it (empty after the timeout).
//   node web/test/cdp.mjs PORT URL [TIMEOUT_MS]
const [port, url, timeoutArg] = process.argv.slice(2);
const timeout = Number(timeoutArg || 60000);

const version = await (await fetch(`http://127.0.0.1:${port}/json/version`)).json();
const ws = new WebSocket(version.webSocketDebuggerUrl);
let id = 0;
const pending = new Map();
ws.onmessage = (e) => {
  const m = JSON.parse(e.data);
  if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); }
};
await new Promise((ok) => { ws.onopen = ok; });
const send = (method, params = {}, sessionId) => new Promise((ok) => {
  const i = ++id;
  pending.set(i, ok);
  ws.send(JSON.stringify({ id: i, method, params, ...(sessionId ? { sessionId } : {}) }));
});

const { result: { targetId } } = await send("Target.createTarget", { url: "about:blank" });
const { result: { sessionId } } = await send("Target.attachToTarget", { targetId, flatten: true });
await send("Page.navigate", { url }, sessionId);
const t0 = Date.now();
let hash = "";
while (Date.now() - t0 < timeout) {
  const r = await send("Runtime.evaluate", { expression: "document.body ? (document.body.dataset.hash || '') : ''", returnByValue: true }, sessionId);
  hash = r.result?.result?.value || "";
  if (hash) break;
  await new Promise((ok) => setTimeout(ok, 250));
}
console.log(hash);
await send("Target.closeTarget", { targetId });
ws.close();
