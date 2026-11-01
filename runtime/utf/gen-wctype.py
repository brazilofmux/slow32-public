#!/usr/bin/env python3
"""Generate runtime/utf/wctype_tables.h: the <wctype.h> classes and the
simple case mappings for the UTF-8 locales, from the Unicode Character
Database that libutf (~/utf) is generated from -- the same Unicode version
as the rest of runtime/utf.

    runtime/utf/gen-wctype.py [UTF_DIR]          (default ~/utf)
    runtime/utf/gen-wctype.py --dump [UTF_DIR]   one line per code point
                                                 with a class or a mapping,
                                                 for comparing with glibc

The classes, as C11 7.30.2.1 asks of them and close to what glibc's
C.UTF-8 does with the same data:
  alpha   Alphabetic, and the decimal digits beyond ASCII (Nd), which C's
          iswdigit may not take but which belong among the alphanumerics
  upper   Uppercase, or having a simple lowercase mapping
  lower   Lowercase, or having a simple uppercase mapping
  digit   0-9 only; xdigit 0-9 A-F a-f only (7.30.2.1.5, .12)
  space   the ASCII white space, the separators Zs other than the no-break
          ones (U+00A0, U+2007, U+202F), and U+2028/U+2029
  blank   tab, and those same separators
  cntrl   Cc, U+2028, U+2029
  graph   every assigned character that is not a control, a surrogate or
          a space -- private use included (libutf colours with the private
          use area; to C it is just printable), and the no-break spaces,
          which iswspace leaves out (7.30.2.1.6 makes graph print less space)
  print   graph, and the spaces among the Zs separators
  punct   graph and not alphanumeric
The mappings are UnicodeData's simple ones (fields 12 and 13); the special,
one-to-many mappings have no place in a towupper that returns one wchar_t.
"""
import os
import sys

args = [a for a in sys.argv[1:] if a != '--dump']
DUMP = '--dump' in sys.argv
UTF = args[0] if args else os.path.expanduser('~/utf')
DATA = os.path.join(UTF, 'gen', 'data')
HERE = os.path.dirname(os.path.abspath(__file__))


def unicode_version():
    first = open(os.path.join(DATA, 'DerivedCoreProperties.txt')).readline()
    return first.strip().split('-')[-1].replace('.txt', '')

ALPHA, UPPER, LOWER, DIGIT, XDIGIT, SPACE, BLANK, CNTRL, PUNCT, GRAPH, PRINT = (1 << i for i in range(11))
NAMES = ['ALPHA', 'UPPER', 'LOWER', 'DIGIT', 'XDIGIT', 'SPACE', 'BLANK', 'CNTRL', 'PUNCT', 'GRAPH', 'PRINT']
N = 0x110000

cat = ['Cn'] * N
upmap = {}
lomap = {}
first = None
for line in open(os.path.join(DATA, 'UnicodeData.txt')):
    f = line.rstrip('\n').split(';')
    cp = int(f[0], 16)
    if f[1].endswith(', First>'):
        first = cp
        continue
    if f[1].endswith(', Last>'):
        for c in range(first, cp + 1):
            cat[c] = f[2]
        continue
    cat[cp] = f[2]
    if f[12]:
        upmap[cp] = int(f[12], 16)
    if f[13]:
        lomap[cp] = int(f[13], 16)

props = {'Alphabetic': set(), 'Uppercase': set(), 'Lowercase': set()}
for line in open(os.path.join(DATA, 'DerivedCoreProperties.txt')):
    line = line.split('#')[0].strip()
    if not line:
        continue
    rng, prop = [x.strip() for x in line.split(';')[:2]]
    if prop not in props:
        continue
    if '..' in rng:
        lo, hi = (int(x, 16) for x in rng.split('..'))
    else:
        lo = hi = int(rng, 16)
    props[prop].update(range(lo, hi + 1))

NOBREAK = {0x00A0, 0x2007, 0x202F}


def classes(c):
    k = cat[c]
    b = 0
    if c in props['Alphabetic'] or (k == 'Nd' and not 0x30 <= c <= 0x39):
        b |= ALPHA
    if c in props['Uppercase'] or c in lomap:
        b |= UPPER
    if c in props['Lowercase'] or c in upmap:
        b |= LOWER
    if 0x30 <= c <= 0x39:
        b |= DIGIT
    if 0x30 <= c <= 0x39 or 0x41 <= c <= 0x46 or 0x61 <= c <= 0x66:
        b |= XDIGIT
    zs = k == 'Zs' and c not in NOBREAK
    if 0x09 <= c <= 0x0D or zs or k in ('Zl', 'Zp'):
        b |= SPACE
    if c == 0x09 or zs:
        b |= BLANK
    if k in ('Cc', 'Zl', 'Zp'):
        b |= CNTRL
    if k not in ('Cn', 'Cc', 'Cs', 'Zl', 'Zp') and not zs:
        b |= GRAPH | PRINT
        if not b & (ALPHA | DIGIT):
            b |= PUNCT
    if zs:
        b |= PRINT
    return b


bits = [classes(c) for c in range(N)]

if DUMP:
    for c in range(N):
        b = bits[c]
        ob = b & 0x7FF
        if b & (ALPHA | DIGIT):
            ob |= 1 << 11
        u, l = upmap.get(c, c), lomap.get(c, c)
        if ob or u != c or l != c:
            print('%X %X %X %X' % (c, ob, u, l))
    sys.exit(0)

# --- the classes: one word per run of equal classes, (start << 11) | bits,
# the run ending where the next begins
runs = []
for c in range(N):
    if c == 0 or bits[c] != bits[c - 1]:
        runs.append((c, bits[c]))


# --- a mapping: runs of (lo, hi, stride, delta), each c = lo, lo+stride, ..
# up to hi mapping to c + delta
def map_runs(m):
    out = []
    for c in sorted(m):
        d = m[c] - c
        if out:
            lo, hi, st, dd = out[-1]
            if dd == d and ((lo == hi and c - hi in (1, 2)) or c - hi == st):
                out[-1] = (lo, c, c - hi, d)
                continue
        out.append((c, c, 1, d))
    return out


ups = map_runs(upmap)
los = map_runs(lomap)
for m, rs in ((upmap, ups), (lomap, los)):          # the runs say what m says
    back = {}
    for lo, hi, st, d in rs:
        for c in range(lo, hi + 1, st):
            back[c] = c + d
    assert back == m


def emit_map(o, name, rs):
    o.write('static const struct __s32_wcmap %s[%d] = {\n' % (name, len(rs)))
    for lo, hi, st, d in rs:
        o.write('    {0x%05X, 0x%05X, %d, %d},\n' % (lo, hi, st, d))
    o.write('};\n\n')


with open(os.path.join(HERE, 'wctype_tables.h'), 'w') as o:
    o.write('''/* Generated by runtime/utf/gen-wctype.py from the Unicode Character
 * Database libutf is built from (gen/data, Unicode %s) -- do not edit.
 * The classes and simple case mappings of the UTF-8 locales; see the
 * generator for how each class is drawn.  Checked code point for code
 * point against glibc's C.UTF-8 (2.41, the same Unicode). */
''' % unicode_version())
    for i, n in enumerate(NAMES):
        o.write('#define __S32_WC_%s 0x%03X\n' % (n, 1 << i))
    o.write('\nstruct __s32_wcmap { unsigned lo, hi; short stride; int delta; };\n\n')
    o.write('/* (first code point << 11) | classes, a run each */\n')
    o.write('static const unsigned __s32_wc_runs[%d] = {\n' % len(runs))
    for i in range(0, len(runs), 6):
        o.write('    ' + ' '.join('0x%08X,' % ((c << 11) | b) for c, b in runs[i:i + 6]) + '\n')
    o.write('};\n\n')
    emit_map(o, '__s32_wc_toupper', ups)
    emit_map(o, '__s32_wc_tolower', los)
print('%d class runs, %d toupper runs, %d tolower runs' % (len(runs), len(ups), len(los)))
