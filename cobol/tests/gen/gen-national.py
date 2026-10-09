#!/usr/bin/env python3
"""Generate a random COBOL 2014 program over national (PIC N) items, with
its expected output written out from the text's code-unit model -- the
reference for tests/gen/run-ref.sh (no oracle: GnuCOBOL's national data
is not UTF-16).

    gen-national.py SEED [STATEMENTS] > prog.cbl 2> prog.ref

The model (2023 8.5.1.4, 8.4.2.4, 14.6.8, 15.26, 15.70): a national
character position is one UTF-16 code unit; a supplementary character is
two positions; alphanumeric text is UTF-8; a receiving national item is
filled left-justified with national spaces and truncated on the right by
positions (JUSTIFIED: on the left); reference modification, LENGTH,
INSPECT and STRING/UNSTRING count positions; DISPLAY-OF gives UTF-8, a
lone surrogate becoming U+FFFD; comparison pads the shorter with spaces
and orders by code unit.  One deviation from the text, the owner's ruling
of 2026-10-09: a cut never parts a surrogate pair -- a MOVE's truncation
or a STRING's overflow that would keep one half drops the pair, its
position a space (or, in STRING, not transferred).  The reference marks
the lines where a cut met a pair (" split"), so the run counts how often
the generator exercises the rule.

The alphabet mixes one-unit characters of one, two and three UTF-8 bytes
(ASCII, Latin-1, Greek, CJK -- the last two columns wide on a terminal),
two-unit characters (emoji, a CJK extension B ideograph), and a combining
mark that follows a base letter (e + U+0301): every intersection the user
named -- UTF-8 length, UTF-16 units, code points, visual width, and
COBOL's positions -- is in the pool.
"""
import os
import random
import sys

# characters: (text, note)
POOL = ["a", "Z", "7", " ", "é", "ñ", "Ω", "ж", "漢", "字", "日", "😀", "𝄞", "𠀋", "é", "x"]
WEIGHTS = [6, 4, 3, 3, 4, 2, 3, 2, 4, 3, 2, 5, 3, 3, 3, 4]


def units(s):
    """UTF-16 code units of a Python string (surrogate pairs as two)"""
    b = s.encode("utf-16-be", "surrogatepass")
    return [b[i] << 8 | b[i + 1] for i in range(0, len(b), 2)]


def from_units(u):
    """a Python string from code units; a lone surrogate stays a lone surrogate"""
    return bytes(x for v in u for x in (v >> 8, v & 255)).decode("utf-16-be", "surrogatepass")


def utf8_of_units(u):
    """DISPLAY-OF / DISPLAY of national: UTF-8, a lone surrogate as U+FFFD"""
    out = bytearray()
    i = 0
    while i < len(u):
        v = u[i]
        if 0xD800 <= v <= 0xDBFF and i + 1 < len(u) and 0xDC00 <= u[i + 1] <= 0xDFFF:
            cp = 0x10000 + ((v - 0xD800) << 10) + (u[i + 1] - 0xDC00); i += 2
        elif 0xD800 <= v <= 0xDFFF:
            cp = 0xFFFD; i += 1
        else:
            cp = v; i += 1
        out += chr(cp).encode("utf-8")
    return bytes(out)


def fit(u, n, just=False):
    """a national receiver of n positions: truncated, padded with spaces; a
    pair the cut would part is dropped, its position a space"""
    if len(u) >= n:
        kept = u[len(u) - n:] if just else u[:n]
        sp = len(u) > n and splits(u, n, just)
        if sp:
            kept = kept[:]
            if just: kept[0] = 0x20
            else: kept[-1] = 0x20
        return kept, sp
    pad = [0x20] * (n - len(u))
    return (pad + u) if just else (u + pad), False


def splits(u, n, just):
    """does cutting u to n positions part a surrogate pair?"""
    if just:
        k = len(u) - n
        return k > 0 and 0xDC00 <= u[k] <= 0xDFFF and 0xD800 <= u[k - 1] <= 0xDBFF
    return 0xD800 <= u[n - 1] <= 0xDBFF and n < len(u) and 0xDC00 <= u[n] <= 0xDFFF


def text(r, lo, hi):
    return "".join(r.choices(POOL, WEIGHTS)[0] for _ in range(r.randint(lo, hi)))


def lit(s):
    return 'N"' + s.replace('"', '""') + '"'


def main():
    seed = int(sys.argv[1])
    nstmt = int(sys.argv[2]) if len(sys.argv) > 2 else 40
    r = random.Random(seed * 2654435761 + 7)
    out, refs = [], []
    w = out.append
    w("identification division.")
    w("program-id. gnat%d." % seed)
    w("data division.")
    w("working-storage section.")
    nlen = [r.randint(1, 12) for _ in range(4)]
    for i in range(4):
        w("01 N%d pic n(%d)%s." % (i, nlen[i], " justified right" if i == 3 else ""))
    xlen = [r.randint(1, 16) for _ in range(2)]
    for i in range(2):
        w("01 X%d pic x(%d)." % (i, xlen[i]))
    w("01 NN pic 9(5) usage national.")
    w("01 K pic 9(4).")
    w("01 K2 pic 9(4).")
    w("01 P pic 99.")
    w("01 O pic 9(5).")
    w("01 V pic -(6)9.99.")
    w("procedure division.")
    cur = [[0x20] * n for n in nlen]            # the national items' positions

    def display_nat(k, i):
        """DISPLAY k "[" N "]" LENGTH BYTE-LENGTH -- the reference line's bytes"""
        u = cur[i]
        return b"%d [" % k + utf8_of_units(u) + b"] %d %d" % (len(u), 2 * len(u))

    for k in range(1, nstmt + 1):
        kind = r.choice(["move", "move", "movex", "refmod", "cmp", "cmp", "inspect", "string", "unstring", "upper", "natof",
                         "moven", "rmrecv", "replace", "natnum", "cmpx", "tox", "convert", "count",
                         "reverse", "ord", "numval", "trim"])
        i = r.randrange(4)
        if kind == "move":
            s = text(r, 0, 14)
            w('    move %s to N%d.' % (lit(s), i))
            u, sp = fit(units(s), nlen[i], i == 3)
            cur[i] = u
            w('    display "%d [" N%d "] " function length(N%d) " " function byte-length(N%d).' % (k, i, i, i))
            refs.append(display_nat(k, i) + (b" split" if sp else b""))
        elif kind == "movex":
            # alphanumeric text to a national item: UTF-8 decoded (14.9.25); and back through DISPLAY-OF
            s = text(r, 0, 10)
            j = r.randrange(2)
            xb = s.encode("utf-8")
            xv = (xb + b" " * xlen[j])[:xlen[j]]        # the alphanumeric item: bytes, truncated (a sequence may be cut)
            w('    move %s to X%d.' % ('"' + s.replace('"', '""') + '"', j))
            w('    move X%d to N%d.' % (j, i))
            # the item's bytes decoded: a cut sequence is malformed -> U+FFFD per bad byte
            dec = xv.decode("utf-8", "replace")
            u, sp = fit(units(dec), nlen[i], i == 3)
            cur[i] = u
            w('    display "%d [" N%d "] " function length(N%d).' % (k, i, i))
            refs.append(b"%d [" % k + utf8_of_units(u) + b"] %d" % len(u) + (b" split" if sp else b""))
        elif kind == "refmod":
            n = nlen[i]
            st = r.randint(1, n); ln = r.randint(1, n - st + 1)
            part = cur[i][st - 1:st - 1 + ln]
            w('    display "%d [" function display-of(N%d(%d:%d)) "] " function length(N%d(%d:%d)) " " function length(N%d(%d:)).' % (k, i, st, ln, i, st, ln, i, st))
            refs.append(b"%d [" % k + utf8_of_units(part) + b"] %d %d" % (ln, n - st + 1))
        elif kind == "cmp":
            j = r.randrange(4)
            if r.random() < 0.5:
                s = text(r, 0, 8); other = units(s); rhs = lit(s)
            else:
                other = cur[j]; rhs = "N%d" % j
            a, b = cur[i][:], other[:]
            m = max(len(a), len(b)); a += [0x20] * (m - len(a)); b += [0x20] * (m - len(b))
            res = "E" if a == b else ("L" if a < b else "G")
            w('    if N%d = %s display "%d E" else if N%d < %s display "%d L" else display "%d G" end-if end-if.' % (i, rhs, k, i, rhs, k, k))
            refs.append(b"%d %s" % (k, res.encode()))
        elif kind == "inspect":
            ch = r.choice(POOL[:-1])
            cu = units(ch)
            u = cur[i]
            cnt = 0
            p = 0
            while p + len(cu) <= len(u):
                if u[p:p + len(cu)] == cu: cnt += 1; p += len(cu)
                else: p += 1
            w('    move 0 to K. inspect N%d tallying K for all %s.' % (i, lit(ch)))
            w('    move 0 to K2. inspect N%d tallying K2 for characters.' % i)
            w('    display "%d " K " " K2.' % k)
            refs.append(b"%d %04d %04d" % (k, cnt, len(u)))
        elif kind == "string":
            i = r.randrange(3)                      # not N3: a JUSTIFIED item is no STRING receiver (14.9.43.3)
            a, b = text(r, 0, 5), text(r, 0, 5)
            p = r.randint(1, nlen[i] + 1)
            w('    move %d to P. string %s %s delimited by size into N%d with pointer P.' % (p, lit(a), lit(b), i))
            u = cur[i][:]; pos = p; ov = False; sp = False
            for src in (units(a), units(b)):            # one transfer per sending item
                for t, v in enumerate(src):
                    if pos < 1 or pos > len(u): ov = True; break
                    if 0xD800 <= v <= 0xDBFF and pos == len(u) and t + 1 < len(src):
                        ov = True; sp = True; break     # the pair does not fit whole: not transferred
                    u[pos - 1] = v; pos += 1
                if ov: break
            cur[i] = u
            w('    display "%d [" N%d "] " P.' % (k, i))
            refs.append(b"%d [" % k + utf8_of_units(u) + b"] %02d" % pos + (b" split" if sp else b""))
        elif kind == "unstring":
            j = (i + 1) % 4
            d = r.choice(["a", "é", "漢", "😀", " "])
            du = units(d)
            u = cur[i]
            # the first field up to the delimiter, into N(j) (14.9.48: a delimiter is a national literal here)
            p = 0; found = -1
            while p + len(du) <= len(u):
                if u[p:p + len(du)] == du: found = p; break
                p += 1
            field = u[:found] if found >= 0 else u[:]
            w('    unstring N%d delimited by %s into N%d.' % (i, lit(d), j))
            fu, sp = fit(field, nlen[j], j == 3)
            cur[j] = fu
            w('    display "%d [" N%d "]".' % (k, j))
            refs.append(b"%d [" % k + utf8_of_units(fu) + b"]" + (b" split" if sp else b""))
        elif kind == "upper":
            u = cur[i]
            res = []
            s = from_units(u)
            for ch in s:
                m = ch.upper()
                if len(m) == 1 and len(units(m)) == len(units(ch)) and ord(ch) < 0x10000 and not (0xD800 <= ord(ch) <= 0xDFFF):
                    res += units(m)
                else:
                    res += units(ch)
            w('    display "%d [" function upper-case(N%d) "]".' % (k, i))
            refs.append(b"%d [" % k + utf8_of_units(res) + b"]")
        elif kind == "moven":
            # national item to national item: positions, truncated or space-filled (JUSTIFIED: from the left)
            j = r.randrange(4)
            w('    move N%d to N%d.' % (j, i))
            u, sp = fit(cur[j], nlen[i], i == 3)
            cur[i] = u
            w('    display "%d [" N%d "] " function length(N%d).' % (k, i, i))
            refs.append(b"%d [" % k + utf8_of_units(u) + b"] %d" % len(u) + (b" split" if sp else b""))
        elif kind == "rmrecv":
            # a part as the receiver: an item of ln positions, no JUSTIFIED (8.4.3.3.4 rule 6)
            n = nlen[i]
            st = r.randint(1, n); ln = r.randint(1, n - st + 1)
            s = text(r, 0, 6)
            w('    move %s to N%d(%d:%d).' % (lit(s), i, st, ln))
            part, sp = fit(units(s), ln, False)
            u = cur[i][:]; u[st - 1:st - 1 + ln] = part; cur[i] = u
            w('    display "%d [" N%d "]".' % (k, i))
            refs.append(b"%d [" % k + utf8_of_units(u) + b"]" + (b" split" if sp else b""))
        elif kind == "replace":
            # INSPECT REPLACING ALL x BY y, both one character of one or two positions (the sizes must agree: 14.9.22.3)
            pairs = [("a", "b"), ("é", "ñ"), ("漢", "字"), ("😀", "𠀋"), (" ", "x")]
            x, y = r.choice(pairs)
            xu, yu = units(x), units(y)
            u = cur[i][:]; p = 0
            while p + len(xu) <= len(u):
                if u[p:p + len(xu)] == xu: u[p:p + len(xu)] = yu; p += len(xu)
                else: p += 1
            cur[i] = u
            w('    inspect N%d replacing all %s by %s.' % (i, lit(x), lit(y)))
            w('    display "%d [" N%d "]".' % (k, i))
            refs.append(b"%d [" % k + utf8_of_units(u) + b"]")
        elif kind == "natnum":
            # a numeric USAGE NATIONAL item: the digits as national characters; MOVE in, DISPLAY, LENGTH
            v = r.randint(0, 99999)
            w('    move %d to NN.' % v)
            w('    display "%d [" NN "] " function length(NN) " " function byte-length(NN).' % k)
            refs.append(b"%d [%05d] 5 10" % (k, v))
        elif kind == "cmpx":
            # national against an alphanumeric literal: the literal as national characters (8.8.4.2.3)
            s = text(r, 0, 6)
            a, b = cur[i][:], units(s)
            m = max(len(a), len(b)); a += [0x20] * (m - len(a)); b += [0x20] * (m - len(b))
            res = "E" if a == b else ("L" if a < b else "G")
            q = '"' + s.replace('"', '""') + '"'
            w('    if N%d = %s display "%d E" else if N%d < %s display "%d L" else display "%d G" end-if end-if.' % (i, q, k, i, q, k, k))
            refs.append(b"%d %s" % (k, res.encode()))
        elif kind == "tox":
            # DISPLAY-OF into an alphanumeric item: UTF-8 bytes, cut at the item's size (a sequence may be cut: the bytes as they are)
            j = r.randrange(2)
            w('    move function display-of(N%d) to X%d.' % (i, j))
            w('    display "%d [" X%d "] " function length(X%d).' % (k, j, j))
            xb = (utf8_of_units(cur[i]) + b" " * xlen[j])[:xlen[j]]
            refs.append(b"%d [" % k + xb + b"] %d" % xlen[j])
        elif kind == "convert":
            # CONVERTING: each character of the first operand to the one at its position in the second (14.9.22.4
            # rule 24); here both of one-position characters, so positions are characters
            frm, to = r.choice([("a7", "7a"), ("é ", " é"), ("漢字", "字漢"), ("Ωж", "жΩ")])
            fu, tu = units(frm), units(to)
            u = [tu[fu.index(v)] if v in fu else v for v in cur[i]]
            cur[i] = u
            w('    inspect N%d converting %s to %s.' % (i, lit(frm), lit(to)))
            w('    display "%d [" N%d "]".' % (k, i))
            refs.append(b"%d [" % k + utf8_of_units(u) + b"]")
        elif kind == "count":
            # UNSTRING ... COUNT IN: the examined positions; DELIMITER IN the delimiter found
            j = (i + 1) % 4
            d = r.choice(["a", "é", "漢", "😀"])
            du = units(d); u = cur[i]
            p = 0; found = -1
            while p + len(du) <= len(u):
                if u[p:p + len(du)] == du: found = p; break
                p += 1
            field = u[:found] if found >= 0 else u[:]
            w('    move 0 to K. unstring N%d delimited by %s into N%d count in K.' % (i, lit(d), j))
            fu, sp = fit(field, nlen[j], j == 3)
            cur[j] = fu
            w('    display "%d [" N%d "] " K.' % (k, j))
            refs.append(b"%d [" % k + utf8_of_units(fu) + b"] %04d" % len(field) + (b" split" if sp else b""))
        elif kind == "reverse":
            # REVERSE: the positions in reverse order, a surrogate pair kept in its order (national.md)
            u = cur[i]; res = []; p = len(u)
            while p > 0:
                if p >= 2 and 0xDC00 <= u[p - 1] <= 0xDFFF and 0xD800 <= u[p - 2] <= 0xDBFF: res += u[p - 2:p]; p -= 2
                else: res.append(u[p - 1]); p -= 1
            w('    display "%d [" function reverse(N%d) "] " function length(function reverse(N%d)).' % (k, i, i))
            refs.append(b"%d [" % k + utf8_of_units(res) + b"] %d" % len(u))
        elif kind == "ord":
            # ORD of the first position: its place in the national collating sequence, code unit + 1 (15.70)
            w('    move function ord(N%d(1:1)) to O. display "%d " O.' % (i, k))
            refs.append(b"%d %05d" % (k, cur[i][0] + 1))
        elif kind == "trim":
            # TRIM of national text: the spaces at both ends, or one end, gone; nothing left is a zero-length result
            u = cur[i]; mode = r.choice(["", " leading", " trailing"])
            a, b = 0, len(u)
            if mode != " trailing":
                while a < b and u[a] == 0x20: a += 1
            if mode != " leading":
                while b > a and u[b - 1] == 0x20: b -= 1
            w('    display "%d [" function trim(N%d%s) "] " function length(function trim(N%d%s)).' % (k, i, mode, i, mode))
            refs.append(b"%d [" % k + utf8_of_units(u[a:b]) + b"] %d" % (b - a))
        elif kind == "numval":
            # NUMVAL and TEST-NUMVAL of a national literal: digits in UTF-16 (15.67, 15.93); a character
            # out of place (one of two code units, perhaps) is reported at its position
            whole = r.randint(0, 99999); dec = r.choice(["", ".%d" % r.randint(0, 9), ".%02d" % r.randint(0, 99)])
            neg = r.random() < 0.3
            body = ("-" if neg else "") + " " * r.randint(0, 1) + str(whole) + dec
            txt = " " * r.randint(0, 2) + body + " " * r.randint(0, 2)
            val = -float(str(whole) + dec) if neg else float(str(whole) + dec)
            w('    compute V = function numval(%s). display "%d [" V "] " function test-numval(%s).' % (lit(txt), k, lit(txt)))
            refs.append(b"%d [%10.2f] +%018d" % (k, val, 0))
            bad = r.choice(["x", "é", "😀", "漢"])
            btxt = " " * r.randint(0, 2) + body + bad
            w('    display "%d " function test-numval(%s).' % (k, lit(btxt)))
            refs.append(b"%d +%018d" % (k, len(units(btxt)) - len(units(bad)) + 1))
        elif kind == "natof":
            # NATIONAL-OF of an alphanumeric literal, its LENGTH, and DISPLAY-OF back (not of a zero-length literal: 15.70)
            s = text(r, 1, 8)
            u = units(s)
            w('    display "%d " function length(function national-of(%s)) " [" function display-of(function national-of(%s)) "]".' % (k, '"' + s.replace('"', '""') + '"', '"' + s.replace('"', '""') + '"'))
            refs.append(b"%d %d [" % (k, len(u)) + utf8_of_units(u) + b"]")
    w("    stop run.")
    sys.stdout.buffer.write(("\n".join(out) + "\n").encode("utf-8"))
    sys.stderr.buffer.write(b"\n".join(refs) + b"\n")


if __name__ == "__main__":
    main()
