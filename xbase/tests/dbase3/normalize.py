#!/usr/bin/env python3
"""normalize.py IN.NDX OUT.NDX -- an index dBASE III PLUS wrote, with the
bytes it leaves as it finds them made 0, as xbase writes them: the header
past the key expression's NUL; in each page the tree reaches, the high
half of the count (dBASE reads and writes 16 bits), each key's padding
out to its item, an internal page's last item past its pointer, and the
page past its items; and every page the tree does not reach (dBASE never
uses a page again once it is out of the tree).  The fixtures here were
made so: dBASE III PLUS 1.1 ran each .PRG on its .DBF under an emulated
PC, and this made its indexes what run-tests.sh compares xbase's with.
ndxfuzz.py does the same for random programs."""
import struct, sys


def normalized(f):
    out = bytearray(len(f))
    klen, isz = struct.unpack_from('<H', f, 12)[0], struct.unpack_from('<H', f, 18)[0]
    end = f.index(0, 24)
    out[:end + 1] = f[:end + 1]
    todo = [struct.unpack_from('<I', f, 0)[0]]
    while todo:
        p = todo.pop(); o = p * 512
        n = struct.unpack_from('<H', f, o)[0]
        out[o:o + 2] = f[o:o + 2]
        left = struct.unpack_from('<I', f, o + 4)[0]
        leaf = left == 0 or (n == 0 and (left >= len(f) // 512 or left == p))
        for i in range(n):
            it = o + 4 + i * isz
            out[it:it + 8 + klen] = f[it:it + 8 + klen]
            if not leaf: todo.append(struct.unpack_from('<I', f, it)[0])
        if not leaf:
            it = o + 4 + n * isz
            out[it:it + 4] = f[it:it + 4]
            todo.append(struct.unpack_from('<I', f, it)[0])
    return bytes(out)


if __name__ == '__main__':
    with open(sys.argv[1], 'rb') as f:
        data = f.read()
    with open(sys.argv[2], 'wb') as f:
        f.write(normalized(data))
