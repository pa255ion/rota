#!/usr/bin/env python3
"""Check derived board XDC files against the board's Digilent/Avnet master.

For every port constrained in the derived files:
  * its PACKAGE_PIN must equal the master's pin for the same signal
    (derived bus names map back to master names: LD[n] -> LDn,
    SW[n] -> SWn, VGA_R[n] -> VGA_R<n+1>, likewise G and B,
    HD_D[n] -> HD_Dn, except HD_D[15] -> HS_D15, the master's spelling),
  * no two ports share a pin,
  * it must have exactly one IOSTANDARD, and that IOSTANDARD must match
    the bank voltage: LVCMOS33 for the fixed 3.3 V banks 13/33, LVCMOS25
    for the VADJ banks 34/35 (VADJ = 2.5 V on this board).

Banks come from the master's "# <section> - Bank NN" headings.

Masters in Digilent's newer format (the Arty Z7-20's) set each pin with
"set_property -dict { PACKAGE_PIN P IOSTANDARD S } [get_ports { name }]";
for those the IOSTANDARD to match is the master's own for that signal
(every Arty bank is 3.3 V: LVCMOS33, TMDS_33 for the HDMI pairs), and
port names are the master's as they are (led[0] stays led[0]).

Usage: check-xdc.py MASTER DERIVED...
Exits non-zero on any mismatch.
"""

import fnmatch
import re
import sys

BANK_IOSTANDARD = {13: "LVCMOS33", 33: "LVCMOS33", 34: "LVCMOS25", 35: "LVCMOS25"}

PIN_RE = re.compile(r"set_property\s+PACKAGE_PIN\s+(\S+)\s+\[get_ports\s+\{([^}]*)\}\]")
IOSTD_RE = re.compile(r"set_property\s+IOSTANDARD\s+(\S+)\s+\[get_ports\s+\{([^}]*)\}\]")
BANK_RE = re.compile(r"^#+\s.*-\s*Bank\s+(\d+)\s*$")
DICT_RE = re.compile(r"set_property\s+-dict\s+\{([^}]*)\}\s+\[get_ports\s+\{([^}]*)\}\]")
BUS_RE = re.compile(r"^(LD|SW|VGA_[RGB]|HD_D)\[(\d+)\]$")

# Master names that don't follow the pattern (typos kept as Digilent wrote them).
MASTER_ALIASES = {"HD_D15": "HS_D15"}


def read_master(path):
    """Signal name -> (pin, bank, IOSTANDARD or None), from the
    commented-out master lines."""
    pins, bank = {}, None
    with open(path) as f:
        for line in f:
            m = BANK_RE.match(line.strip())
            if m:
                bank = int(m.group(1))
                continue
            m = PIN_RE.search(line)
            if m:
                pins[m.group(2).strip()] = (m.group(1), bank, None)
                continue
            m = DICT_RE.search(line)
            if m:
                props = dict(zip(*[iter(m.group(1).split())] * 2))
                if "PACKAGE_PIN" in props:
                    pins[m.group(2).strip()] = (props["PACKAGE_PIN"], bank, props.get("IOSTANDARD"))
    return pins


def master_name(port):
    m = BUS_RE.match(port)
    if not m:
        return port
    base, idx = m.group(1), int(m.group(2))
    name = f"{base}{idx + 1}" if base.startswith("VGA_") else f"{base}{idx}"
    return MASTER_ALIASES.get(name, name)


def check(master_path, derived_paths):
    master = read_master(master_path)
    errors, ports, iostd = [], {}, {}

    for path in derived_paths:
        with open(path) as f:
            text = [l for l in f if not l.lstrip().startswith("#")]
        for line in text:
            m = PIN_RE.search(line)
            if m:
                pin, port = m.group(1), m.group(2).strip()
                if port in ports:
                    errors.append(f"{path}: {port} constrained twice")
                ports[port] = (pin, path)
        for line in text:
            m = IOSTD_RE.search(line)
            if m:
                std = m.group(1)
                for pattern in m.group(2).split():
                    hits = [p for p in ports if fnmatch.fnmatchcase(p, pattern.replace("[", "[[]"))]
                    if not hits:
                        errors.append(f"{path}: IOSTANDARD pattern {pattern} matches no port")
                    for p in hits:
                        iostd.setdefault(p, []).append(std)

    by_pin = {}
    for port, (pin, _) in ports.items():
        by_pin.setdefault(pin, []).append(port)
    for pin, ps in sorted(by_pin.items()):
        if len(ps) > 1:
            errors.append(f"pin {pin} used by {len(ps)} ports: {' '.join(sorted(ps))}")

    for port, (pin, path) in sorted(ports.items()):
        name = master_name(port)
        if name not in master:
            errors.append(f"{path}: {port} ({name}) not in master XDC")
            continue
        mpin, bank, mstd = master[name]
        if pin != mpin:
            errors.append(f"{path}: {port} on {pin}, master has {name} on {mpin}")
        stds = iostd.get(port, [])
        want = mstd or BANK_IOSTANDARD.get(bank)
        if len(stds) != 1:
            errors.append(f"{path}: {port} has {len(stds)} IOSTANDARD settings, want 1")
        elif want is None:
            errors.append(f"{path}: {port} is in bank {bank}, no known voltage")
        elif stds[0] != want:
            where = "the master's" if mstd else f"bank {bank}"
            errors.append(f"{path}: {port} is {stds[0]}, want {want} ({where})")

    for e in errors:
        print("check-xdc: " + e, file=sys.stderr)
    print(f"check-xdc: {len(ports)} ports checked, {len(errors)} errors")
    return not errors


if __name__ == "__main__":
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    sys.exit(0 if check(sys.argv[1], sys.argv[2:]) else 1)
