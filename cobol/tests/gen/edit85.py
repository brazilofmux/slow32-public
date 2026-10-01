#!/usr/bin/env python3
"""MOVE into an edited item as X3.23-1985 describes it (VI-33 to VI-35,
editing rules 4 to 8; the sign table; MOVE alignment, VI-103), written
out from the text independently of either compiler: a reference oracle
for tests/gen (gen-edit.py).  Pictures are without parentheses, as
gen-edit.py writes them.

edit_numeric(pic, value, bwz) and edit_alnum(pic, text) return the
item's content.
"""
from decimal import Decimal, ROUND_DOWN, getcontext

getcontext().prec = 60
INSERT = ",B0/"


def symbols(pic):
    out, i = [], 0
    while i < len(pic):
        if pic[i:i + 2] in ("CR", "DB"):
            out.append(pic[i:i + 2]); i += 2
        else:
            out.append(pic[i]); i += 1
    return out


def edit_numeric(pic, value, bwz=False):
    sy = symbols(pic)
    # the floating symbol: a string of two or more of + - $ (rule 7)
    fl = None
    for c in "+-$":
        if sy.count(c) >= 2:
            fl = c
    first_fl = sy.index(fl) if fl else -1
    point = sy.index(".") if "." in sy else len(sy)
    # digit positions: 9, Z, *, and each floating symbol but the first
    dpos = [i for i, c in enumerate(sy)
            if c in "9Z*" or (fl and c == fl and i != first_fl)]
    ints = sum(1 for i in dpos if i < point)
    decs = len(dpos) - ints
    # MOVE: aligned on the point, truncated at both ends (VI-103); the
    # value edited is the value after truncation (rule 7), and a zero is
    # "positive or zero" in the sign table
    v = value.quantize(Decimal(1).scaleb(-decs), rounding=ROUND_DOWN)
    neg = v < 0
    mag = abs(v) % (Decimal(10) ** ints) if ints else abs(v) - int(abs(v))
    mag = mag.quantize(Decimal(1).scaleb(-decs), rounding=ROUND_DOWN)
    zero = mag == 0
    if zero:
        neg = False
    digs = str(int(mag * (Decimal(10) ** decs))).zfill(len(dpos))
    width = sum(len(c) for c in sy)
    if bwz and zero:
        return " " * width
    dig_at = dict(zip(dpos, digs))
    supp = "Z" if "Z" in sy else ("*" if "*" in sy else None)
    fill = "*" if supp == "*" else " "
    all_suppressed = all(sy[i] in "Z*" or (fl and sy[i] == fl) for i in dpos)

    def sign_char(c):
        if c == "+":
            return "-" if neg else "+"
        if c == "-":
            return "-" if neg else " "
        if c == "CR":
            return "CR" if neg else "  "
        if c == "DB":
            return "DB" if neg else "  "
        return c

    # every digit position suppressed or floating, and the value zero
    if zero and all_suppressed and (supp or fl):
        if supp == "*":
            return "".join("." if c == "." else ("**" if len(c) == 2 else "*") for c in sy)
        return " " * width

    out = [None] * len(sy)
    if fl:
        # the floating string: the floating symbols, with the simple
        # insertion characters embedded in it or immediately right of it
        last_fl = max(i for i, c in enumerate(sy) if c == fl)
        end = last_fl
        while end + 1 < len(sy) and sy[end + 1] in INSERT:
            end += 1
        fdig = [i for i in dpos if first_fl < i <= last_fl]
        nz = next((i for i in fdig if dig_at[i] != "0"), None)
        # the symbol goes immediately left of the first nonzero digit the
        # string represents, or else at the string's right limit -- its
        # rightmost character, which may be an insertion character
        # immediately right of the last symbol (they are part of it)
        at = (nz - 1) if nz is not None else end
        for i in range(first_fl, end + 1):
            if i < at:
                out[i] = " "
            elif i == at:
                out[i] = "$" if fl == "$" else ("-" if neg else ("+" if fl == "+" else " "))
            else:
                c = sy[i]
                out[i] = dig_at[i] if i in dig_at else ("," if c == "," else " " if c == "B" else c)
    sig = False
    started = False
    for i, c in enumerate(sy):
        if out[i] is not None:       # the floating string, done above; zero
            continue                 # suppression and floating are exclusive (rule 3)
        if c in "Z*":
            started = True
            d = dig_at[i]
            if not sig and d == "0":
                out[i] = fill
            else:
                sig = True
                out[i] = d
        elif c == "9":
            sig = True
            out[i] = dig_at[i]
        elif c == ".":
            sig = True
            out[i] = "."
        elif c in INSERT:
            if started and not sig:
                out[i] = fill
            else:
                out[i] = " " if c == "B" else c
        else:
            out[i] = sign_char(c)
    return "".join(out)


def edit_alnum(pic, text, width_src=None):
    """X positions take the source's characters left to right (a MOVE:
    left-justified, space-filled); B 0 / are inserted"""
    xs = sum(1 for c in pic if c in "XA9")
    src = (text + " " * xs)[:xs]
    out, k = [], 0
    for c in pic:
        if c in "XA9":
            out.append(src[k]); k += 1
        else:
            out.append(" " if c == "B" else c)
    return "".join(out)
