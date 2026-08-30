#!/usr/bin/env python3
"""Attribute a SLOW-32 PC histogram to functions, or list the hot
instructions inside one function.

    s32-ld --print-map ... -o prog.s32x            # prog.s32x.map beside it
    slow32 -t -c 50000000 prog.s32x args 2>/dev/null \\
        | tail -n +30000000 | grep -o 'PC=[0-9A-F]*' | sort | uniq -c > hist
    s32-hotspots.py prog.s32x.map hist             # per function
    s32-hotspots.py prog.s32x.map hist cob_get_num # per instruction (needs slow32dis)

The map lists global symbols only: a static function is charged to the
global before it.  The trace window skips the program's start-up so the
steady state is what gets counted.
"""
import bisect, os, re, subprocess, sys

def load_map(path):
    syms, codelim = [], None
    for l in open(path):
        m = re.match(r'\s+0x[0-9A-F]+ 0x([0-9A-F]+) \.text', l)
        if m: codelim = int(m.group(1), 16)
        m = re.match(r'\s+0x([0-9A-F]+)\s+0x[0-9A-F]+\s+(\S+)', l)
        if m and codelim is not None and int(m.group(1), 16) < codelim:
            syms.append((int(m.group(1), 16), m.group(2)))
    syms.sort()
    return syms

def load_hist(path):
    h = {}
    for l in open(path):
        c, pc = l.split()
        h[int(pc[3:], 16)] = h.get(int(pc[3:], 16), 0) + int(c)
    return h

def main():
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    syms = load_map(sys.argv[1]); hist = load_hist(sys.argv[2])
    addrs = [a for a, _ in syms]
    total = sum(hist.values())
    if len(sys.argv) == 3:
        per = {}
        for pc, c in hist.items():
            i = bisect.bisect_right(addrs, pc) - 1
            name = syms[i][1] if i >= 0 else '?'
            per[name] = per.get(name, 0) + c
        for k, v in sorted(per.items(), key=lambda x: -x[1])[:30]:
            print(f"{100 * v / total:6.2f}%  {v:10d}  {k}")
        return
    func = sys.argv[3]
    lo = [a for a, n in syms if n == func]
    if not lo: sys.exit(f"{func}: not in the map")
    lo = lo[0]; hi = min(a for a, _ in syms if a > lo)
    inside = {pc: c for pc, c in hist.items() if lo <= pc < hi}
    sub = sum(inside.values())
    print(f"{func} 0x{lo:x}-0x{hi:x}: {sub} of {total} ({100 * sub / total:.2f}%)")
    dis = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'slow32dis')
    prog = sys.argv[1][:-4] if sys.argv[1].endswith('.map') else sys.argv[1]
    out = subprocess.run([dis, prog, hex(lo), hex(hi)], capture_output=True, text=True).stdout
    for l in out.splitlines():
        m = re.match(r'\s+0x([0-9a-f]+):\s+[0-9a-f]+\s+(.*)', l)
        if m and inside.get(int(m.group(1), 16), 0):
            print(f"{inside[int(m.group(1), 16)]:10d}  0x{m.group(1)}  {m.group(2)}")

if __name__ == '__main__':
    main()
