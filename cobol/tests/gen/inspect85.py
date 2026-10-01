#!/usr/bin/env python3
"""INSPECT as X3.23-1985 VI-96 to VI-99 describe it, written out
independently of either compiler: a third oracle for tests/gen.

Phrases are (kind, literal, replacement, where) with kind one of
"characters", "all", "leading", "first"; where None or ("before"|"after",
literal-2).  inspect() returns the tallies (format 1) and the new content
(format 2).  Format 3 is format 1 then format 2 (rule 15); format 4 is a
series of one-character ALL phrases (rule 16).

- Rule 6: the comparison cycle runs left to right; at each position the
  phrases are tried in the order written, the first match wins, and the
  next cycle starts right of the match (or one position on, when none
  matched).  CHARACTERS matches one character (6e).
- Rule 7: BEFORE and AFTER boundaries come from the first occurrence of
  literal-2, found before the first cycle; outside its region a phrase
  does not match; AFTER with no occurrence never matches; BEFORE with
  none is as if not written.
- Rules 10 and 13: ALL each match; LEADING contiguous matches beginning
  where comparison began in the first cycle the phrase was eligible for;
  FIRST the leftmost match, each FIRST phrase once.
"""


def region(subject, where):
    n = len(subject)
    if not where:
        return 0, n
    which, lit2 = where
    i = subject.find(lit2)
    if which == "before":
        return 0, (i if i >= 0 else n)
    return (i + len(lit2), n) if i >= 0 else (n + 1, n)   # AFTER, absent: never eligible


def inspect(subject, phrases, replacing):
    s = list(subject)
    regs = [region(subject, p[3]) for p in phrases]
    tallies = [0] * len(phrases)
    lead_next = [None] * len(phrases)     # where a LEADING phrase must match next
    lead_dead = [False] * len(phrases)
    first_done = [False] * len(phrases)
    pos, n = 0, len(s)
    while pos < n:
        for i, p in enumerate(phrases):     # a leading run the scan jumped past is over
            if p[0] == "leading" and lead_next[i] is not None and lead_next[i] < pos:
                lead_dead[i] = True
        # rule 13c: a LEADING run begins where comparison began in the
        # first cycle the phrase was eligible for -- eligibility is the
        # region's, whether or not an earlier phrase matches first
        for i, (kind, lit, rep, where) in enumerate(phrases):
            if kind == "leading" and lead_next[i] is None:
                lo, hi = regs[i]
                if lo <= pos and pos + len(lit) <= hi:
                    lead_next[i] = pos
        matched, who = 0, -1
        for i, (kind, lit, rep, where) in enumerate(phrases):
            lo, hi = regs[i]
            ln = 1 if kind == "characters" else len(lit)
            if not (lo <= pos and pos + ln <= hi):
                continue
            if kind == "characters":
                ok = True
            else:
                ok = "".join(s[pos:pos + ln]) == lit
                if kind == "leading":
                    ok = ok and not lead_dead[i] and lead_next[i] == pos
                if kind == "first":
                    ok = ok and not first_done[i]
            if ok:
                tallies[i] += 1
                if replacing:
                    s[pos:pos + ln] = list(rep[0] if kind == "characters" else rep)
                if kind == "leading":
                    lead_next[i] = pos + ln
                if kind == "first":
                    first_done[i] = True
                matched, who = ln, i
                break
        for i, p in enumerate(phrases):     # its cycle came, and it did not match
            if p[0] == "leading" and lead_next[i] == pos and who != i:
                lead_dead[i] = True
        pos += matched if matched else 1
    return tallies, "".join(s)


def converting(subject, frm, to, where=None):
    lo, hi = region(subject, where)
    m = {}
    for a, b in zip(frm, to):
        m.setdefault(a, b)
    return "".join(m.get(c, c) if lo <= i < hi else c for i, c in enumerate(subject))
