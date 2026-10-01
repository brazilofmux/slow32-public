#!/usr/bin/env python3
"""xcobol-survey.py -- compile every program of the X-COBOL dataset
(X-COBOL: A Dataset of COBOL Repositories, Zenodo 10.5281/zenodo.7968845,
CC-BY-4.0; 84 GitHub projects, 844 .cbl/.cob files) with s32-cobc and
record what each one does: compiles, or the first error.  Third-party
COBOL, none of it written for us -- where the harness, CCVS-85 and the
generators are ours.  ISSUES.md 120 has the first survey.

  xcobol-survey.py OUT.tsv            one row per file: project, file,
                                      OK or FAIL, the flags or the error
  XCOBOL=~/refs/x-cobol/X-COBOL       the unpacked dataset (never in a
                                      git tree)
  XCOBOL_FLAGS=-dialect=mf            further s32-cobc flags for every
                                      compile (Micro Focus's dialect)

Each file is compiled in the reference format its own text declares (a
>>SOURCE or $SET directive, else the sequence area and indicator column
in use or not), -std=85 then -std=2002, with every directory of its
project on -I.  The dataset kept each repository's COBOL files but not
its tree, so a COPY naming a path ("shared/copybooks/x.cpy") is pointed
back at the project's file of that name through a directory of
symlinks.  Two passes: a FUNCTION-ID's signature file, written beside
its output, is there for the programs that call it on the second."""
import os, re, shutil, subprocess, sys, tempfile
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(os.environ.get("XCOBOL", os.path.expanduser("~/refs/x-cobol/X-COBOL")), "COBOL_Files")
COBC = os.path.join(HERE, "..", "out", "s32-cobc")
OUT = sys.argv[1]
EXTRA = os.environ.get("XCOBOL_FLAGS", "").split()
if not os.path.isdir(ROOT):
    sys.exit("no X-COBOL dataset at %s (set XCOBOL)" % ROOT)
W = tempfile.mkdtemp(prefix="xcobol.")

def copy_dirs(proj):
    """every directory of the project, its root first: copybooks come
    with any extension or none, and COPY names paths from the root"""
    dirs = [proj]
    for d, sub, _ in os.walk(proj):
        sub[:] = [x for x in sub if x not in ("venv", ".git", "node_modules")]
        if d != proj:
            dirs.append(d)
    return dirs

def shadow(proj, pd):
    """the dataset's flattening undone: an -I directory where each path a
    COPY names is a symlink to the project's file of that name"""
    base = {}
    for d, _, fs in os.walk(pd):
        for f in fs:
            base.setdefault(f.lower(), os.path.join(d, f))
    sd = os.path.join(W, "shadow", proj)
    for d, _, fs in os.walk(pd):
        for f in fs:
            if not f.lower().endswith((".cbl", ".cob", ".cpy")):
                continue
            for m in re.finditer(r'(?i)\bCOPY\s+["\']([^"\']*/[^"\']*)["\']', open(os.path.join(d, f), errors="replace").read()):
                rel = os.path.normpath(m.group(1))
                tgt = base.get(os.path.basename(rel).lower())
                if tgt and not rel.startswith(".."):
                    link = os.path.join(sd, rel)
                    os.makedirs(os.path.dirname(link), exist_ok=True)
                    if not os.path.lexists(link):
                        os.symlink(tgt, link)
    return sd

def detect(src):
    """the source's reference format, as its own text says"""
    fixed = free = 0
    for l in open(src, errors="replace").read().splitlines()[:400]:
        u = l.upper().replace(" ", "")
        if ">>SOURCEFORMAT" in u or "SOURCEFORMAT\"" in u or "SOURCEFORMAT(" in u:
            return "-free" if "FREE" in u else "-fixed"
        if not l.strip():
            continue
        if len(l) > 6 and l[6] in "*/-dD" and (not l[:6].strip() or l[:6].strip().isdigit()):
            fixed += 1
        elif l[:6].strip().isdigit():
            fixed += 1
        elif l[:7].strip() and not l.lstrip().startswith("*>"):
            free += 1
    return "-fixed" if fixed >= free else "-free"

def attempt(src, fmt, std, incs):
    od = os.path.join(W, "out", os.path.basename(os.path.dirname(src)))
    os.makedirs(od, exist_ok=True)
    cmd = [COBC, fmt, std] + EXTRA + sum((["-I", d] for d in incs), []) + ["-I", od, "-o", os.path.join(od, os.path.basename(src) + ".s"), src]
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, errors="replace", timeout=60)
    except subprocess.TimeoutExpired:
        return False, "TIMEOUT", 0
    if p.returncode == 0:
        return True, "", 0
    err = next((l for l in p.stderr.splitlines() if "error" in l), p.stderr.strip().splitlines()[0] if p.stderr.strip() else "rc %d" % p.returncode)
    m = re.match(r"^[^:]*:(\d+):", err)
    return False, re.sub(r"^[^:]*:\d+: (error: )?", "", err), int(m.group(1)) if m else 0

def survey(job):
    proj, src, incs = job
    fmt = detect(src)
    errs = []
    for std in ("-std=85", "-std=2002"):
        ok, err, line = attempt(src, fmt, std, incs)
        if ok:
            return (proj, src, "OK", fmt + " " + std)
        errs.append("%s (line %d, %s %s)" % (err, line, fmt, std))
    # 85's refusal, unless all it says is that the feature is 2002's
    return (proj, src, "FAIL", errs[1] if "2002" in errs[0].split(" (line")[0] else errs[0])

jobs = []
for proj in sorted(os.listdir(ROOT)):
    pd = os.path.join(ROOT, proj)
    if not os.path.isdir(pd):
        continue
    incs = copy_dirs(pd) + [shadow(proj, pd)]
    for d, _, fs in os.walk(pd):
        for f in sorted(fs):
            if f.lower().endswith((".cbl", ".cob")):
                jobs.append((proj, os.path.join(d, f), incs))
try:
    for _ in range(2):
        with ThreadPoolExecutor(max_workers=os.cpu_count() or 4) as ex:
            rows = list(ex.map(survey, jobs))
finally:
    shutil.rmtree(W, ignore_errors=True)
with open(OUT, "w") as o:
    for r in rows:
        o.write("\t".join([r[0], os.path.relpath(r[1], ROOT), r[2], r[3].replace("\t", " ")]) + "\n")
print("files", len(rows), "compile", sum(r[2] == "OK" for r in rows))
