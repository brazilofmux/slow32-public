#!/usr/bin/env python3
"""Read the census a compile leaves (S32_CENSUS_DIR; src/cobc/census.h) and
say, corpus by corpus, how many elementary items stand alone.

    census.py DIR [--by-file] [--items VERDICT] [--corpus NAME]

The compiler writes one line per elementary item: what it is, how the
statements named it, and whether a group over it or a redefinition of it
was named.  The verdict is drawn here, the first of these that applies:

    not named     no statement names it (it may still live under a group
                  that is named; nothing reads or writes it by itself)
    storage       its storage is not the program's to lay out: a file's
                  record, LINKAGE, EXTERNAL, BASED, ANY LENGTH, or a
                  GLOBAL item a contained program names
    address       its address is given away or kept: a CALL argument BY
                  REFERENCE or RETURNING item, ADDRESS OF, a host
                  variable, a FILE STATUS / RELATIVE KEY / ASSIGN /
                  DEPENDING ON / LINAGE / CRT STATUS item, a screen item,
                  the PROCEDURE DIVISION header, or a use the census has
                  no rule for
    group         a group over it is named by a statement ...
    group, item by item
                  ... but only by INITIALIZE, which is defined as a MOVE
                  to each elementary item, or by SEARCH, which names the
                  table and looks at what its WHEN phrases name: the
                  group's bytes as a whole are never looked at
    alias         a redefinition or a renaming of its bytes is named
    alone         none of those: the item's layout is the compiler's

Each count comes twice: the items, and the references to them (how many
times statements name them) -- the second is what the generated code is
made of.
"""
import glob
import os
import sys
from collections import Counter, defaultdict

FIELDS = ["unit", "line", "level", "name", "shape", "section", "category", "usage", "picture",
          "size", "refs", "flags", "verbs", "gverbs", "averbs", "value", "pins", "native"]
VERDICTS = ["alone", "group, item by item", "group", "alias", "address", "storage", "not named"]
NUMERIC = {"binary-int", "binary-int8", "binary-dec", "binary-other", "display-int", "display-sint",
           "display-dec", "packed-int", "packed-dec", "float", "national-num"}


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


def verdict(it):
    f = it["flagset"]
    if "unref" in f:
        return "not named"
    if it["section"] in ("file", "linkage", "external", "based") or "anylen" in f or "inner" in f:
        return "storage"
    if f & {"call", "address", "sql", "runtime", "screen", "header", "other"}:
        return "address"
    if "group" in f:
        gv = set(it["gverbs"].split(",")) - {"-"}
        if "alias" not in f and gv and gv <= {"INITIALIZE", "SEARCH"}:
            return "group, item by item"
        return "group"
    if "alias" in f:
        return "alias"
    return "alone"


def load(d):
    items = []
    for fn in sorted(glob.glob(os.path.join(d, "*.census"))):
        src = "?"
        with open(fn, errors="replace") as fh:
            for line in fh:
                line = line.rstrip("\n")
                if line.startswith("#file\t"):
                    src = line.split("\t", 1)[1]
                    continue
                parts = line.split("\t")
                if len(parts) != len(FIELDS):
                    continue
                it = dict(zip(FIELDS, parts))
                it["refs"] = int(it["refs"]); it["size"] = int(it["size"])
                it["flagset"] = set(it["flags"].split(",")) - {"-"}
                it["file"] = src; it["corpus"] = corpus_of(os.path.abspath(src) if src != "?" else src)
                it["verdict"] = verdict(it)
                items.append(it)
    return items


def pct(a, b):
    return "%5.1f%%" % (100.0 * a / b) if b else "     -"


def table(title, rows, total_items, total_refs):
    print(title)
    print("    %-28s %8s %7s   %9s %7s" % ("", "items", "", "refs", ""))
    for name, ni, nr in rows:
        if name == "not named":
            print("    %-28s %8d" % (name, ni))
        else:
            print("    %-28s %8d %s   %9d %s" % (name, ni, pct(ni, total_items), nr, pct(nr, total_refs)))
    print()


def report(name, items):
    files = len({it["file"] for it in items})
    print("=" * 78)
    print("%s: %d programs, %d elementary items" % (name, files, len(items)))
    print("=" * 78)
    for shape in ("scalar", "table"):
        sel = [it for it in items if it["shape"] == shape and it["category"] != "index-name"]
        if not sel:
            continue
        named = [it for it in sel if it["verdict"] != "not named"]
        ti, tr = len(named), sum(it["refs"] for it in named)
        rows = []
        for v in VERDICTS:
            vs = [it for it in sel if it["verdict"] == v]
            rows.append((v, len(vs), sum(it["refs"] for it in vs)))
        table("%s items (%s): %d, of which %d are named by a statement; shares are of the named"
              % (shape, "not in a table" if shape == "scalar" else "under an OCCURS", len(sel), ti), rows, ti, tr)
        # the alone ones, by what they are
        alone = [it for it in sel if it["verdict"] in ("alone", "group, item by item")]
        ai, ar = len(alone), sum(it["refs"] for it in alone)
        cats = defaultdict(lambda: [0, 0, 0, 0])
        for it in alone:
            c = cats[it["category"]]
            c[0] += 1; c[1] += it["refs"]
            if it["category"] in NUMERIC and it["flagset"] & {"refmod", "class"}:
                c[2] += 1
            if it["category"] in NUMERIC and it["value"] == "-":
                c[3] += 1
        if alone:
            print("    standing alone (with the item-by-item groups), by category:")
            print("    %-16s %8s %7s   %9s %7s   %s" % ("", "items", "", "refs", "", "numeric: bytes looked at / no VALUE"))
            for cat, c in sorted(cats.items(), key=lambda kv: -kv[1][1]):
                tail = "   %d / %d" % (c[2], c[3]) if cat in NUMERIC else ""
                print("    %-16s %8d %s   %9d %s%s" % (cat, c[0], pct(c[0], ai), c[1], pct(c[1], ar), tail))
            print()
    # written the machine's way (src/cobc/native.h): the compiler's own verdict
    nat = [it for it in items if it.get("native") == "y"]
    named = [it for it in items if it["verdict"] != "not named" and it["category"] != "index-name"]
    if nat:
        tr = sum(it["refs"] for it in named)
        print("    written the machine's way: %d items (%s of the named), %d references (%s)"
              % (len(nat), pct(len(nat), len(named)).strip(), sum(it["refs"] for it in nat), pct(sum(it["refs"] for it in nat), tr).strip()))
        c = Counter(it["category"] for it in nat)
        print("        " + ", ".join("%s %d" % kv for kv in c.most_common()))
        held = [it for it in items if it.get("native") not in ("y", "-", "table", "unnamed")]
        if held:
            c = Counter(); ci = Counter(); p = Counter()
            for it in held:
                c[it["native"]] += it["refs"]; ci[it["native"]] += 1
                if it["native"] == "bytes":
                    for w in set(it["pins"].split(",")) - {"-"}:
                        p[w.split("/")[0] if w.startswith(("inline/", "noaddr/")) else w] += it["refs"]
            print("        integers of those kinds, not in tables, left as written: %d items, %d references, kept by:"
                  % (len(held), sum(it["refs"] for it in held)))
            print("        " + ", ".join("%s %d (%d)" % (k, ci[k], c[k]) for k, _ in c.most_common()))
            if p:
                print("        'bytes', by what used them (references): " + ", ".join("%s %d" % kv for kv in p.most_common(10)))
        print()
    ix = [it for it in items if it["category"] == "index-name"]
    if ix:
        print("    index-names (the compiler's own cells already): %d, %d references\n" % (len(ix), sum(it["refs"] for it in ix)))
    # why the address ones are: the flags
    addr = [it for it in items if it["verdict"] == "address"]
    if addr:
        c = Counter()
        for it in addr:
            for fl in ("call", "address", "sql", "runtime", "screen", "header", "other"):
                if fl in it["flagset"]:
                    c[fl] += 1
        print("    'address', by reason (an item may have several): " + ", ".join("%s %d" % kv for kv in c.most_common()))
    stor = [it for it in items if it["verdict"] == "storage"]
    if stor:
        c = Counter(it["section"] if it["section"] != "working" and it["section"] != "local" else
                    ("contained program" if "inner" in it["flagset"] else "any length") for it in stor)
        print("    'storage', by reason: " + ", ".join("%s %d" % kv for kv in c.most_common()))
    grp = [it for it in items if it["verdict"] == "group"]
    if grp:
        c = Counter()
        for it in grp:
            for v in it["gverbs"].split(","):
                c[v] += 1
        print("    'group', by the statements naming the group (an item may have several): "
              + ", ".join("%s %d" % kv for kv in c.most_common(12)))
    print()


def main():
    args = sys.argv[1:]
    if not args:
        print(__doc__); sys.exit(2)
    d = args[0]
    items = load(d)
    only = None
    if "--corpus" in args:
        only = args[args.index("--corpus") + 1]
        items = [it for it in items if it["corpus"] == only]
    if "--items" in args:
        v = args[args.index("--items") + 1]
        for it in items:
            if it["verdict"] == v:
                print("\t".join([it["corpus"], os.path.basename(it["file"])] + [str(it[f]) for f in FIELDS]))
        return
    if "--by-file" in args:
        byf = defaultdict(list)
        for it in items:
            byf[(it["corpus"], it["file"])].append(it)
        print("%-12s %-28s %7s %7s %7s %9s %9s" % ("corpus", "program", "items", "named", "alone", "refs", "alone"))
        for (c, f), its in sorted(byf.items()):
            its = [it for it in its if it["shape"] == "scalar" and it["category"] != "index-name"]
            named = [it for it in its if it["verdict"] != "not named"]
            alone = [it for it in named if it["verdict"] in ("alone", "group, item by item")]
            print("%-12s %-28s %7d %7d %7d %9d %9d" % (c, os.path.basename(f)[:28], len(its), len(named), len(alone),
                                                         sum(it["refs"] for it in named), sum(it["refs"] for it in alone)))
        return
    by = defaultdict(list)
    for it in items:
        by[it["corpus"]].append(it)
    for name in sorted(by):
        report(name, by[name])
    if len(by) > 1:
        report("all corpora", items)


main()
