// The WASI functions GHC's runtime imports (side track W), for a browser:
// a clock, stdout and stderr to the console, and nothing else (no files,
// no environment, no preopened folders). A call outside that answers
// ENOSYS, so the runtime sees an unsupported feature, never a crash.

const ESUCCESS = 0, EBADF = 8, ENOSYS = 52;

export function makeWasi() {
  let memory = null;
  const view = () => new DataView(memory.buffer);
  const lines = ["", "", ""];
  const imports = {
    environ_sizes_get(countPtr, sizePtr) {
      view().setUint32(countPtr, 0, true);
      view().setUint32(sizePtr, 0, true);
      return ESUCCESS;
    },
    environ_get() { return ESUCCESS; },
    clock_time_get(_id, _precision, outPtr) {
      // nanoseconds, from the page's monotonic clock
      view().setBigUint64(outPtr, BigInt(Math.round(performance.now() * 1e6)), true);
      return ESUCCESS;
    },
    fd_write(fd, iovs, iovsLen, nwrittenPtr) {
      let n = 0;
      const mem = new Uint8Array(memory.buffer);
      for (let i = 0; i < iovsLen; i++) {
        const p = view().getUint32(iovs + 8 * i, true);
        const len = view().getUint32(iovs + 8 * i + 4, true);
        if (fd === 1 || fd === 2) {
          lines[fd] += new TextDecoder().decode(mem.subarray(p, p + len));
          let nl;
          while ((nl = lines[fd].indexOf("\n")) >= 0) {
            (fd === 2 ? console.warn : console.log)("rota: " + lines[fd].slice(0, nl));
            lines[fd] = lines[fd].slice(nl + 1);
          }
        }
        n += len;
      }
      view().setUint32(nwrittenPtr, n, true);
      return fd === 1 || fd === 2 ? ESUCCESS : EBADF;
    },
    fd_fdstat_get(fd, statPtr) {
      if (fd > 2) return EBADF;
      // a character device, no rights needed by the runtime
      for (let i = 0; i < 24; i++) view().setUint8(statPtr + i, 0);
      view().setUint8(statPtr, 2);
      return ESUCCESS;
    },
    fd_prestat_get() { return EBADF; },
    fd_prestat_dir_name() { return EBADF; },
    fd_close() { return ESUCCESS; },
    poll_oneoff(_in, _out, _n, neventsPtr) {
      view().setUint32(neventsPtr, 0, true);
      return ESUCCESS;
    },
    proc_exit(code) { throw new Error(`rota: the emulator exited (${code})`); },
  };
  // the rest (files, folders): not here
  const wasi = new Proxy(imports, {
    get: (t, name) => (name in t ? t[name] : () => ENOSYS),
  });
  return { wasi, setMemory: (m) => { memory = m; } };
}
