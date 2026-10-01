#!/usr/bin/env python3
"""Conditions as X3.23-1985 describes them (VI-54 to VI-61: relation,
class, sign, combined and abbreviated combined relation conditions),
written out independently of either compiler: a reference oracle for
tests/gen (gen-cond.py).

An operand is ("n", Decimal) -- numeric, compared algebraically -- or
("x", str) -- nonnumeric.  A numeric integer DISPLAY item compared with
a nonnumeric operand is given as ("x", its characters): it is compared
"as though moved to an alphanumeric item of the same size" (VI-55).
The native collating sequence is the implementor's; on SLOW-32 it is
ASCII, so characters compare by their code.
"""
from decimal import Decimal

OPS = {"=": lambda c: c == 0, "<": lambda c: c < 0, ">": lambda c: c > 0,
       "<=": lambda c: c <= 0, ">=": lambda c: c >= 0,
       "NOT =": lambda c: c != 0, "NOT <": lambda c: not c < 0, "NOT >": lambda c: not c > 0}


def cmp(a, b):
    """-1, 0 or 1 comparing a with b (VI-55)"""
    if a[0] == "n" and b[0] == "n":
        return (a[1] > b[1]) - (a[1] < b[1])
    x, y = a[1], b[1]
    # nonnumeric: the shorter is extended with spaces on the right
    n = max(len(x), len(y))
    x, y = x.ljust(n), y.ljust(n)
    return (x > y) - (x < y)


def relation(a, op, b):
    return OPS[op](cmp(a, b))


def figurative(fig, other):
    """a figurative constant as the operand compared with `other`: the
    size of the other operand (VI-55; IV-11 for the figurative constants)"""
    if other[0] == "n":
        assert fig == "ZERO"
        return ("n", Decimal(0))
    n = len(other[1])
    if fig in ("SPACE", "SPACES"):
        return ("x", " " * n)
    if fig == "ZERO":
        return ("x", "0" * n)
    if fig == "HIGH-VALUE":
        return ("x", "\xff" * n)
    if fig == "LOW-VALUE":
        return ("x", "\x00" * n)
    if fig.startswith("ALL "):
        lit = fig[5:-1]
        return ("x", (lit * n)[:n])
    raise ValueError(fig)


def klass(text, cls, unsigned_numeric=False):
    """class condition (VI-56)"""
    if cls == "NUMERIC":
        return len(text) > 0 and all(c in "0123456789" for c in text)
    if cls == "ALPHABETIC":
        return all(c == " " or ("A" <= c <= "Z") or ("a" <= c <= "z") for c in text)
    if cls == "ALPHABETIC-UPPER":
        return all(c == " " or ("A" <= c <= "Z") for c in text)
    if cls == "ALPHABETIC-LOWER":
        return all(c == " " or ("a" <= c <= "z") for c in text)
    raise ValueError(cls)


def sign(v, s):
    """sign condition (VI-58): POSITIVE greater than zero, NEGATIVE less"""
    return {"POSITIVE": v > 0, "NEGATIVE": v < 0, "ZERO": v == 0}[s]


# ---- the condition grammar, with abbreviated combined relations ----

RELWORDS = ["NOT =", "NOT <", "NOT >", "<=", ">=", "=", "<", ">"]
CLASSES = ["NUMERIC", "ALPHABETIC-UPPER", "ALPHABETIC-LOWER", "ALPHABETIC"]
SIGNS = ["POSITIVE", "NEGATIVE", "ZERO"]
FIGS = ["SPACES", "SPACE", "HIGH-VALUE", "LOW-VALUE", "ZERO"]


def tokens(s):
    out, i = [], 0
    while i < len(s):
        c = s[i]
        if c == " ":
            i += 1
        elif c == '"':
            j = s.index('"', i + 1)
            out.append(s[i:j + 1]); i = j + 1
        elif c in "()":
            out.append(c); i += 1
        elif c in "<>=":
            if s[i:i + 2] in ("<=", ">="):
                out.append(s[i:i + 2]); i += 2
            else:
                out.append(c); i += 1
        else:
            j = i
            while j < len(s) and s[j] not in ' ()"':
                j += 1
            out.append(s[i:j]); i = j
    return out


class Cond:
    """evaluate(cond) over env: name -> ("n", Decimal) | ("x", str) |
    ("i", digits) for an unsigned integer DISPLAY item"""

    def __init__(self, text, env):
        self.t = tokens(text)
        self.p = 0
        self.env = env
        self.subj = None          # the last stated subject (VI-61)
        self.op = None            # and relational operator

    def peek(self, k=0):
        return self.t[self.p + k] if self.p + k < len(self.t) else None

    def relop(self):
        """a relational operator at the cursor, NOT included; or None"""
        a, b = self.peek(), self.peek(1)
        if a == "NOT" and b in ("=", "<", ">"):
            return "NOT " + b, 2
        if a in ("=", "<", ">", "<=", ">="):
            return a, 1
        return None, 0

    def operand(self):
        x = self.t[self.p]
        if x == "ALL":
            self.p += 2
            return ("fig", "ALL " + self.t[self.p - 1])
        self.p += 1
        if x in FIGS:
            return ("fig", x)
        if x.startswith('"'):
            return ("x", x[1:-1])
        if x[0] in "-+.0123456789":
            return ("n", Decimal(x))
        return self.env[x]

    def value(self, a, other):
        if a[0] == "fig":
            o = other if other[0] != "fig" else ("x", "")
            if o[0] == "i":
                o = ("x", o[1])
            return figurative(a[1], o)
        if a[0] == "i":
            # an integer DISPLAY item: numeric against a numeric operand,
            # its characters against a nonnumeric one (VI-55)
            if other[0] in ("n", "i"):
                return ("n", Decimal(a[1]))
            return ("x", a[1])
        return a

    def rel(self, a, op, b):
        return relation(self.value(a, b), op, self.value(b, a))

    def or_(self):
        v = self.and_()
        while self.peek() == "OR":
            self.p += 1
            w = self.and_()
            v = v or w
        return v

    def and_(self):
        v = self.not_()
        while self.peek() == "AND":
            self.p += 1
            w = self.not_()
            v = v and w
        return v

    def not_(self):
        # NOT immediately before a relational operator belongs to it (VI-61)
        if self.peek() == "NOT" and self.relop()[0] is None:
            self.p += 1
            return not self.not_()
        return self.primary()

    def primary(self):
        if self.peek() == "(":
            self.p += 1
            v = self.or_()
            assert self.peek() == ")", self.t[self.p:]
            self.p += 1
            return v
        op, n = self.relop()
        if op:                                   # the subject omitted
            self.p += n
            self.op = op
            return self.rel(self.subj, op, self.operand())
        if self.subj is not None and self.after_operand_is_end():
            return self.rel(self.subj, self.op, self.operand())   # subject and operator omitted
        a = self.operand()
        neg = False
        if self.peek() == "NOT" and self.peek(1) in CLASSES + SIGNS:
            neg = True
            self.p += 1
        w = self.peek()
        if w in CLASSES:
            self.p += 1
            assert a[0] in ("x", "i")
            return klass(a[1], w) != neg
        if w in SIGNS:
            self.p += 1
            assert a[0] == "n"
            return sign(a[1], w) != neg
        op, n = self.relop()
        self.p += n
        b = self.operand()
        self.subj, self.op = a, op
        return self.rel(a, op, b)

    def after_operand_is_end(self):
        """an operand here not followed by an operator or a class or
        sign word is an abbreviated object"""
        q = self.p + (2 if self.peek() == "ALL" else 1)
        nxt = self.t[q] if q < len(self.t) else None
        nxt2 = self.t[q + 1] if q + 1 < len(self.t) else None
        if nxt in ("=", "<", ">", "<=", ">=") or nxt in CLASSES + SIGNS:
            return False
        if nxt == "NOT" and (nxt2 in ("=", "<", ">") or nxt2 in CLASSES + SIGNS):
            return False
        return True

    def evaluate(self):
        v = self.or_()
        assert self.p == len(self.t), self.t[self.p:]
        return v


def evaluate(text, env):
    return Cond(text, env).evaluate()
