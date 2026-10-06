#!/usr/bin/env python3
"""Generate the conformance coverage matrix: which language elements of
ISO/IEC 1989:2023 have been swept, and with what dispositions.

    gen-coverage.py [outline.txt] > coverage.md

The denominator comes from the standard's own bookmark outline, taken
from the licensed PDF on this machine (docs/standards.md) with
`mutool show <pdf> outline`: an element is a numbered section that has
"Syntax rules" or "General rules" subsections.  Only section numbers and
titles are used, as the pages here already cite them; no rule text.

The numerator comes from the pages beside this script: the 2023 section
numbers each page names, and the disposition marks in its tables
(README.md's legend).  A page's counts are credited to the sections it
names; an element no page names is unswept.
"""
import os
import re
import subprocess
import sys
import tempfile
from collections import OrderedDict

HERE = os.path.dirname(os.path.abspath(__file__))
PDF = os.path.expanduser(
    "~/Documents/standards/cobol/cobol-2023-incits-iso-iec-1989-2023.pdf")
CLAUSES = OrderedDict([          # our own descriptions, not the standard's titles
    ("7", "COPY, REPLACE and directives"),
    ("8", "characters, names, data, expressions, conditions"),
    ("9", "files and objects, general"),
    ("10", "the compilation group"),
    ("11", "IDENTIFICATION DIVISION"),
    ("12", "ENVIRONMENT DIVISION"),
    ("13", "DATA DIVISION"),
    ("14", "PROCEDURE DIVISION"),
    ("15", "intrinsic functions"),
    ("16", "standard classes"),
])
RULE_HEADS = ("Syntax rules", "Syntax rule", "General rules", "General rule",
              "General format", "General formats")
MARKS = ["test", "refused", "gap", "n/a", "ruling"]
NUM = re.compile(r"^(\d+(?:\.\d+)*)\s+(.*)$")


def outline_lines(path):
    if path:
        with open(path, encoding="utf-8") as f:
            return f.read().splitlines()
    out = subprocess.run(["mutool", "show", PDF, "outline"],
                         capture_output=True, text=True, check=True)
    return out.stdout.splitlines()


def elements(lines):
    """number -> title for sections with a rules subsection, in order."""
    titles = OrderedDict()
    for ln in lines:
        m = re.search(r'"([^"]*)"', ln)
        if not m:
            continue
        nm = NUM.match(m.group(1).strip())
        if nm:
            titles[nm.group(1)] = nm.group(2).strip()
    elems = OrderedDict()
    for num, title in titles.items():
        if title in RULE_HEADS:
            parent = num.rsplit(".", 1)[0]
            if parent in titles and parent.split(".")[0] in CLAUSES:
                elems[parent] = titles[parent]
        elif num.split(".")[0] == "15" and title.endswith(" function"):
            elems[num] = title
    return OrderedDict(sorted(elems.items(),
                              key=lambda kv: [int(x) for x in kv[0].split(".")]))


def pages():
    """page -> (set of 2023 section numbers named, {mark: count})."""
    out = {}
    for fn in sorted(os.listdir(HERE)):
        if not fn.endswith(".md") or fn in ("README.md", "coverage.md"):
            continue
        with open(os.path.join(HERE, fn), encoding="utf-8") as f:
            text = f.read()
        head = []
        for ln in text.splitlines():
            if ln.startswith("#"):
                head.append(ln)
        # section numbers in headings, and in the README's table row
        nums = set()
        for h in head:
            nums.update(re.findall(r"\b(1[0-6]|[7-9])\.(\d+(?:\.\d+)*)", h))
        nums = {a + "." + b for a, b in nums}
        counts = {m: 0 for m in MARKS}
        for ln in text.splitlines():
            if not ln.startswith("|") or ln.startswith("|---"):
                continue
            cells = [c.strip() for c in ln.strip("|").split("|")]
            if len(cells) < 3:
                continue
            disp = cells[-1].lower()
            if re.search(r"\*\*new\*\*", disp):     # a feature added, with its test
                counts["test"] += 1
                continue
            for m in MARKS:
                if re.search(r"(^|\W)\*{0,2}" + re.escape(m) + r"\*{0,2}\b", disp):
                    counts[m] += 1
                    break
        # "Not swept here: 12.4.5.9, ..." -- sections a page names by an
        # ancestor but does not sweep
        excl = set()
        for ln in text.splitlines():
            if ln.startswith("Not swept here:"):
                excl.update(re.findall(r"\b(?:1[0-6]|[7-9])\.\d+(?:\.\d+)*", ln))
        out[fn] = (nums, counts, text, excl)
    return out


def readme_sections():
    """page -> section numbers from README's index table."""
    res = {}
    with open(os.path.join(HERE, "README.md"), encoding="utf-8") as f:
        for ln in f:
            m = re.search(r"\]\(([a-z0-9-]+\.md)\)", ln)
            if not m or not ln.startswith("|"):
                continue
            cell = ln.split("|")[1]
            nums = set()
            # 14.9.6/.10/.27: the shorthand continues the first number;
            # 13.18.32, .33, .52: so does a comma-separated list
            for full, tail in re.findall(
                    r"\b((?:1[0-6]|[7-9])\.\d+(?:\.\d+)*)((?:(?:/|,\s*)\.\d+)*)", cell):
                nums.add(full)
                stem = full.rsplit(".", 1)[0]
                for t in re.findall(r"(?:/|,\s*)\.(\d+)", tail):
                    nums.add(stem + "." + t)
            words = set(re.findall(r"\b[A-Z][A-Z0-9-]{1,}\b", cell))
            ent = res.setdefault(m.group(1), [set(), set()])
            ent[0].update(nums)
            ent[1].update(words)
    return res


KINDS = (" statement", " clause", " phrase", " entry")


def label(title):
    """What the matrix may show for an element: the COBOL keyword when the
    element is one (a statement, clause, phrase, entry or function -- the
    language's own words), else nothing.  The standard's descriptive
    section titles are its text, and stay out of the tree."""
    for k in KINDS + (" function",):
        if title.endswith(k):
            kw = title[:-len(k)]
            if re.fullmatch(r"[A-Z][A-Z0-9-]*(?: [A-Z][A-Z0-9-]*)*", kw):
                return kw + k.rstrip()
    return ""


def keyword(title):
    """ADD statement -> ADD; the keyword a README row would name."""
    for k in KINDS:
        if title.endswith(k):
            return title[:-len(k)].upper()
    return None


COBC = os.path.join(HERE, "..", "..", "out", "s32-cobc")


def probe_function(name, tmp):
    """Ask the compiler about FUNCTION name: ('impl', ''), ('gap', why)
    or ('unknown', msg).  An argument-count complaint means the name is
    known, which is all this asks."""
    src = os.path.join(tmp, "p.cbl")
    with open(src, "w") as f:
        f.write("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. P.\n"
                "       PROCEDURE DIVISION.\n           DISPLAY FUNCTION %s.\n"
                "           STOP RUN.\n" % name)
    r = subprocess.run([COBC, "-fixed", "-std=2002", "-o",
                        os.path.join(tmp, "p.s"), src],
                       capture_output=True, text=True)
    msg = r.stderr.strip().splitlines()[0] if r.stderr.strip() else ""
    msg = msg.split("error: ", 1)[-1]
    if "not implemented" in msg:
        why = re.sub(r"^FUNCTION \S+ (is )?", "", msg)
        return "gap", why
    if "not an intrinsic function" in msg:
        return "unknown", msg
    return "impl", ""


def probe_statement(verb, tmp):
    """Ask the compiler about a statement verb, in statement position (a
    bare word alone would parse as a paragraph name): ('impl', ''),
    ('gap', why) or ('unknown', msg)."""
    src = os.path.join(tmp, "s.cbl")
    with open(src, "w") as f:
        f.write("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. P.\n"
                "       PROCEDURE DIVISION.\n           CONTINUE\n"
                "           %s.\n           STOP RUN.\n" % verb)
    r = subprocess.run([COBC, "-fixed", "-std=2002", "-o",
                        os.path.join(tmp, "s.s"), src],
                       capture_output=True, text=True)
    msg = r.stderr.strip().splitlines()[0] if r.stderr.strip() else ""
    msg = msg.split("error: ", 1)[-1]
    if "not implemented" in msg or "not supported" in msg:
        return "gap", msg
    if "is not a COBOL verb" in msg:
        return "unknown", msg
    return "impl", ""


def covered_by(num, named):
    """True if a named section is num or an ancestor/descendant of it."""
    for n in named:
        if num == n or num.startswith(n + ".") or n.startswith(num + "."):
            return True
    return False


def main():
    elems = elements(outline_lines(sys.argv[1] if len(sys.argv) > 1 else None))
    pg = pages()
    rwords = {}
    for fn, (nums, words) in readme_sections().items():
        if fn in pg:
            pg[fn] = (pg[fn][0] | nums, pg[fn][1], pg[fn][2], pg[fn][3])
            rwords[fn] = words

    print("# Conformance coverage (generated)")
    print()
    print("Generated by gen-coverage.py from the section outline of ISO/IEC")
    print("1989:2023 and the pages in this directory; do not edit by hand.  An")
    print("element is a section of the standard with syntax or general rules.")
    print("Counts are the disposition marks of the page(s) naming the section;")
    print("a page that sweeps several sections credits each of them.  Intrinsic")
    print("functions are classified by asking the compiler: the implemented ones")
    print("are swept by functions.md as a whole (no counts of their own), the rest")
    print("carry the compiler's own reason.")
    print()
    tmp = tempfile.mkdtemp()
    tot = swept = 0
    rows = OrderedDict((c, []) for c in CLAUSES)
    for num, title in elems.items():
        named = [fn for fn, (nums, _, _, _) in pg.items() if covered_by(num, nums)]
        kw = keyword(title)
        if not named and kw:
            named = [fn for fn, words in rwords.items() if kw in words]
        named = [fn for fn in named if num not in pg[fn][3]]     # the page's "Not swept here"
        byname = []
        if title.endswith(" function") and os.path.exists(COBC):
            kind, why = probe_function(title[:-len(" function")], tmp)
            if kind == "impl":
                byname = ["functions.md"]
            else:
                named = []
                byname = (kind, why)
        if (not named and not byname and title.endswith(" statement")
                and os.path.exists(COBC)):
            kind, why = probe_statement(title[:-len(" statement")], tmp)
            if kind != "impl":
                byname = (kind, why)
            else:
                byname = ("impl", "")
        rows[num.split(".")[0]].append((num, title, named, byname))
    print("| clause | elements | swept | not implemented | unswept |")
    print("|---|---|---|---|---|")
    gaps = 0
    for c, lst in rows.items():
        g = sum(1 for r in lst if isinstance(r[3], tuple) and r[3][0] != "impl")
        s = sum(1 for r in lst if r[2] or (r[3] and not isinstance(r[3], tuple)))
        tot += len(lst)
        swept += s
        gaps += g
        print("| %s %s | %d | %d | %d | %d |" % (c, CLAUSES[c], len(lst), s, g, len(lst) - s - g))
    print("| **all** | **%d** | **%d** | **%d** | **%d** |" % (tot, swept, gaps, tot - swept - gaps))
    for c, lst in rows.items():
        if not lst:
            continue
        print()
        print("## %s %s" % (c, CLAUSES[c]))
        print()
        print("| section | element | page | test | refused | gap | n/a | ruling |")
        print("|---|---|---|---|---|---|---|---|")
        for num, title, named, byname in lst:
            if isinstance(byname, tuple) and byname[0] == "impl":
                print("| %s | %s | *unswept* (implemented) | | | | | |" % (num, label(title)))
            elif isinstance(byname, tuple):
                print("| %s | %s | **%s**: %s | | | | | |" % (num, label(title),
                      "not implemented" if byname[0] == "gap" else "UNKNOWN to the compiler",
                      byname[1]))
            elif byname:
                links = ", ".join("[%s](%s)" % (fn[:-3], fn) for fn in byname)
                print("| %s | %s | %s (implemented) | | | | | |" % (num, label(title), links))
            elif named:
                c2 = {m: sum(pg[fn][1][m] for fn in named) for m in MARKS}
                links = ", ".join("[%s](%s)" % (fn[:-3], fn) for fn in named)
                print("| %s | %s | %s | %s |" % (num, label(title), links,
                      " | ".join(str(c2[m]) for m in MARKS)))
            else:
                print("| %s | %s | *unswept* | | | | | |" % (num, label(title)))


if __name__ == "__main__":
    main()
