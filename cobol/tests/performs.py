#!/usr/bin/env python3
"""Read the PERFORM census a compile leaves (S32_CENSUS_DIR; src/cobc/pcensus.h)
and say, corpus by corpus, how paragraphs are entered and left: which
performed ranges are procedures, and what keeps the others from being one.

    performs.py DIR [--by-file] [--corpus NAME] [--ranges VERDICT]

A performed range is the paragraphs from a PERFORM's first name to its
THRU name (a section: all of its paragraphs).  It is a

    procedure     entered at its first paragraph by PERFORM and nothing
                  else: no GO TO from outside it to any paragraph of it,
                  no GO TO from inside it to a paragraph outside, not
                  fallen into from the paragraph before, not the program's
                  entry, and not partly overlapping another range
    fallen into   the paragraph before its first falls through into it
                  and that paragraph itself runs in line (or the range
                  begins at the program's entry), so its code also runs in
                  line, outside any PERFORM of it
    nested        it lies inside another performed range (PERFORM A THRU C
                  and PERFORM B): its code runs in line when the outer
                  range is performed
    GO TO in      a GO TO from outside the range reaches a paragraph of it
    GO TO out     a GO TO inside the range leaves it (an error exit, say)
    overlapping   another range begins or ends inside it without being
                  nested in it

A range may be several of the last four; each is counted.  A GO TO inside
a procedure that stays inside it is a branch of that procedure and is
counted apart as "GO TO within".  Every count comes by ranges and by the
PERFORM statements that name them.
"""
import glob
import os
import sys
from collections import Counter, defaultdict


def corpus_of(path):
    p = path.replace("\\", "/")
    if "/majesty/" in p:
        return "majesty"
    if "/open/" in p:
        return "open-systems"
    if "xcobol" in p.lower() or "x-cobol" in p.lower():
        return "x-cobol"
    if "ccvs" in p.lower() or "/cobol85/" in p.lower():
        return "ccvs-85"
    if "/tests/" in p:
        return "harness"
    return "other"


class Unit:
    def __init__(self, name, src):
        self.name = name; self.src = src
        self.paras = {}          # id -> dict(name, section, secid, decl, line, ends)
        self.performs = []       # (from, lo, thru, kind)
        self.gotos = []          # (from, target, kind)
        self.decl = set()


def load(d):
    units = []
    for fn in sorted(glob.glob(os.path.join(d, "*.perform"))):
        src = "?"
        u = Unit("?", src)
        with open(fn, errors="replace") as fh:
            for line in fh:
                p = line.rstrip("\n").split("\t")
                if p[0] == "#file":
                    src = p[1]; u.src = src
                elif p[0] == "P":
                    u.paras[int(p[1])] = dict(name=p[2], section=p[3] == "section", secid=int(p[4]), decl=int(p[5]), line=int(p[6]), ends=p[7])
                elif p[0] == "F":
                    u.performs.append((int(p[1]), int(p[2]), int(p[3]), p[4]))
                elif p[0] == "G":
                    u.gotos.append((int(p[1]), int(p[2]), p[3]))
                elif p[0] == "D":
                    u.decl.add(int(p[1]))
                elif p[0] == "U":
                    u.name = p[1]
                    if u.paras or u.performs:
                        units.append(u)
                    u = Unit("?", src)
    return units


def analyse(u):
    ids = sorted(u.paras)
    code = ids              # paragraphs in order; a section header too, since sentences may follow it directly
    entry = next((i for i in code if not u.paras[i]["decl"]), None)
    pos = {i: k for k, i in enumerate(code)}

    def section_last(sid):
        members = [i for i in code if u.paras[i]["secid"] == sid or i == sid]
        return max(members)

    def prev_falls(p):
        """the paragraph before p, when control falls out of its end into p"""
        k = pos.get(p)
        if k is None or k == 0:
            return None
        q = code[k - 1]
        if u.paras[q]["decl"] != u.paras[p]["decl"] or u.paras[q]["ends"] != "fall":
            return None
        return q

    ranges = Counter()            # (lo, hi) -> PERFORM statements naming it
    kinds = defaultdict(Counter)
    for frm, lo, thru, kind in u.performs:
        if lo not in u.paras:
            continue
        if thru >= 0:
            hi = thru if not u.paras.get(thru, {}).get("section") else section_last(thru)
        else:
            hi = section_last(lo) if u.paras[lo]["section"] else lo
        if hi < lo:
            lo, hi = hi, lo
        ranges[(lo, hi)] += 1
        kinds[(lo, hi)][kind] += 1

    def inside(r, p):
        return r[0] <= p <= r[1]

    # does a paragraph's code ever run outside every performed range that
    # holds it -- in line?  The entry does; a GO TO's target does when the
    # GO TO comes from code running in line, or from outside every range
    # the target is in (a GO TO out of a range abandons it); the paragraph
    # after one that runs in line and falls through does.  A GO TO to a
    # range's own exit paragraph from inside the range keeps control in
    # the range, whose end returns.  A fixed point, since a GO TO may come
    # from later in the text.
    def share_range(a, b):
        return any(r[0] <= a <= r[1] and r[0] <= b <= r[1] for r in ranges)
    runs = {p: p == entry for p in code}
    changed = True
    while changed:
        changed = False
        for p in code:
            if runs[p]:
                continue
            q = prev_falls(p)
            v = (q is not None and runs[q]) or any(g[1] == p and (runs.get(g[0], g[0] == -1) or not share_range(g[0], p)) for g in u.gotos)
            if v:
                runs[p] = True; changed = True

    out = []
    for r, n in ranges.items():
        lo, hi = r
        why = []
        q = prev_falls(lo)
        if lo == entry or (q is not None and runs[q]):
            why.append("fallen into")
        nested = any(r2 != r and r2[0] <= lo and hi <= r2[1] and not (r2[0] == lo and r2[1] == hi) for r2 in ranges)
        if nested and "fallen into" not in why:
            why.append("nested in a range")
        gin = [g for g in u.gotos if inside(r, g[1]) and not inside(r, g[0])]
        gout = [g for g in u.gotos if inside(r, g[0]) and not inside(r, g[1])]
        within = [g for g in u.gotos if inside(r, g[0]) and inside(r, g[1])]
        if gin:
            why.append("GO TO in")
        if gout:
            why.append("GO TO out")
        for r2 in ranges:
            if r2 == r:
                continue
            if (r2[0] < lo <= r2[1] < hi) or (lo < r2[0] <= hi < r2[1]):
                why.append("overlapping"); break
        stop = any(u.paras[p]["ends"] == "stop" for p in code if inside(r, p))
        out.append(dict(range=r, n=n, why=why, within=len(within), stop=stop, kinds=kinds[r],
                        paras=sum(1 for p in code if inside(r, p))))
    # paragraphs: how each is entered
    pclass = {}
    goto_targets = {g[1] for g in u.gotos}
    starts = {r[0] for r in ranges}
    members = {p for r in ranges for p in code if inside(r, p)}
    for p in code:
        ways = []
        if p == entry:
            ways.append("entry")
        if p in starts:
            ways.append("performed")
        elif p in members:
            ways.append("in a range")
        if p in goto_targets:
            ways.append("GO TO")
        q = prev_falls(p)
        if q is not None and runs[q]:
            ways.append("fall-through")
        if u.paras[p]["secid"] in u.decl or p in u.decl:
            ways.append("declarative")
        pclass[p] = ways
    return dict(ranges=out, paras=pclass, code=code, entry=entry)


def pct(a, b):
    return "%5.1f%%" % (100.0 * a / b) if b else "     -"


def report(name, units):
    print("=" * 78)
    print("%s: %d units, %d paragraphs, %d out-of-line PERFORMs, %d GO TOs"
          % (name, len(units), sum(len(a["code"]) for _, a in units), sum(r["n"] for _, a in units for r in a["ranges"]),
             sum(len(u.gotos) for u, _ in units)))
    print("=" * 78)
    rng = [r for _, a in units for r in a["ranges"]]
    nr, ns = len(rng), sum(r["n"] for r in rng)
    proc = [r for r in rng if not r["why"]]
    print("performed ranges: %d, named by %d PERFORM statements" % (nr, ns))
    print("    %-28s %8s %7s   %9s %7s" % ("", "ranges", "", "PERFORMs", ""))
    print("    %-28s %8d %s   %9d %s" % ("procedure", len(proc), pct(len(proc), nr), sum(r["n"] for r in proc), pct(sum(r["n"] for r in proc), ns)))
    pw = [r for r in proc if r["within"]]
    print("    %-28s %8d %s   %9d %s" % ("  ... with GO TO within", len(pw), pct(len(pw), nr), sum(r["n"] for r in pw), pct(sum(r["n"] for r in pw), ns)))
    for w in ("fallen into", "nested in a range", "GO TO in", "GO TO out", "overlapping"):
        rs = [r for r in rng if w in r["why"]]
        print("    %-28s %8d %s   %9d %s" % (w, len(rs), pct(len(rs), nr), sum(r["n"] for r in rs), pct(sum(r["n"] for r in rs), ns)))
    multi = [r for r in rng if len(r["why"]) > 1]
    print("    %-28s %8d %s   %9d %s" % ("(more than one of those)", len(multi), pct(len(multi), nr), sum(r["n"] for r in multi), pct(sum(r["n"] for r in multi), ns)))
    k = Counter()
    for r in rng:
        for kk, v in r["kinds"].items():
            k[kk] += v
    print("    PERFORM forms: " + ", ".join("%s %d" % kv for kv in k.most_common()))
    sizes = Counter(min(r["paras"], 5) for r in rng)
    print("    range lengths, in paragraphs: " + ", ".join("%s %d" % ("5+" if s == 5 else s, sizes[s]) for s in sorted(sizes)))
    # GO TOs
    gt = Counter()
    for u, a in units:
        rs = [r["range"] for r in a["ranges"]]
        for frm, tgt, kind in u.gotos:
            encl = [r for r in rs if r[0] <= frm <= r[1]]
            if kind != "goto":
                gt[kind] += 1
            elif not encl:
                gt["from the mainline"] += 1
            elif all(r[0] <= tgt <= r[1] for r in encl):
                gt["within every range it is in"] += 1
            else:
                gt["leaving a range"] += 1
    print("    GO TO: " + ", ".join("%s %d" % kv for kv in gt.most_common()))
    # paragraphs
    pc = Counter()
    for _, a in units:
        for p, ways in a["paras"].items():
            key = "+".join(ways) if ways else "unreachable by name"
            pc[key] += 1
    tot = sum(pc.values())
    print("paragraphs, by how they are entered:")
    for key, n in pc.most_common(14):
        print("    %-44s %6d %s" % (key, n, pct(n, tot)))
    print()


def main():
    args = sys.argv[1:]
    if not args:
        print(__doc__); sys.exit(2)
    units = load(args[0])
    only = args[args.index("--corpus") + 1] if "--corpus" in args else None
    rows = []
    for u in units:
        c = corpus_of(os.path.abspath(u.src) if u.src != "?" else u.src)
        if only and c != only:
            continue
        rows.append((c, u, analyse(u)))
    if "--ranges" in args:
        v = args[args.index("--ranges") + 1]
        for c, u, a in rows:
            for r in a["ranges"]:
                verdict = "procedure" if not r["why"] else ", ".join(r["why"])
                if v in verdict:
                    print("%s\t%s\t%s\t%s..%s\t%d PERFORMs\t%s" % (c, os.path.basename(u.src), u.name,
                          u.paras[r["range"][0]]["name"], u.paras[r["range"][1]]["name"], r["n"], verdict))
        return
    if "--by-file" in args:
        for c, u, a in rows:
            rng = a["ranges"]; proc = [r for r in rng if not r["why"]]
            print("%-12s %-24s %-14s paras %4d  ranges %3d  procedures %3d  GO TOs %3d" % (c, os.path.basename(u.src)[:24], u.name[:14], len(a["code"]), len(rng), len(proc), len(u.gotos)))
        return
    by = defaultdict(list)
    for c, u, a in rows:
        by[c].append((u, a))
    for c in sorted(by):
        report(c, by[c])
    if len(by) > 1:
        report("all corpora", [x for c in by for x in by[c]])


main()
