import struct, sys, subprocess, re
binf = sys.argv[1]
d = open(binf, 'rb').read()
nsec, secoff, stroff, strsz = struct.unpack_from('<IIII', d, 0x0C)
secs = {}
for i in range(nsec):
    no, ty, va, off, sz, msz, fl = struct.unpack_from('<IIIIIII', d, secoff + 28*i)
    name = d[stroff+no:d.index(b'\0', stroff+no)].decode('latin1')
    secs[name] = (off, sz, va)
so, ssz, _ = secs['.symtab']
to, tsz, _ = secs['.sym_strtab']
syms = []
for i in range(ssz // 16):
    no, val, sec, ty, bind, sz = struct.unpack_from('<IIHBBI', d, so + 16*i)
    name = d[to+no:d.index(b'\0', to+no)].decode('latin1')
    if not name.startswith('__') and name not in ('_start',):
        syms.append((val, name))
syms.sort()
addrs = [s[0] for s in syms]
names = [s[1] for s in syms]
import bisect
counts = {}
proc = subprocess.Popen(['./tools/emulator/slow32', '-t', binf],
                        stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True)
pat = re.compile(r'PC=([0-9A-Fa-f]{8})')
for line in proc.stdout:
    m = pat.search(line)
    if not m: continue
    pc = int(m.group(1), 16)
    i = bisect.bisect_right(addrs, pc) - 1
    fn = names[i] if i >= 0 else '???'
    counts[fn] = counts.get(fn, 0) + 1
proc.wait()
tot = sum(counts.values())
print("total %d" % tot)
for fn, c in sorted(counts.items(), key=lambda x: -x[1])[:16]:
    print("%10d  %5.1f%%  %s" % (c, 100.0*c/tot, fn))
