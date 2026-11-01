# C11 clause 7: the two C libraries

> Status 2026-11-01 (evening): rt 296 PASS / 19 DIFFER / 24 NOBUILD, sh
> 276 / 17 / 46, of 339 probes; coverage.md marks what is fixed.  The
> inventory: rt 638, sh 553 of 898 identifiers (morning: 445, 301).

How far SLOW-32's two C libraries are from C11's clause 7, the standard
library: `runtime/` (what clang-built programs link) and
`selfhost/stage08/libc` with `selfhost/stage08/include` (the kit's, what
stage08 cc programs link).  The companion of
`selfhost/c-conformance/`, which did chapter 6, the language, for stage08
cc.

The text is read from WG14's N1570, the final committee draft of C11.  It
is free from open-std.org but remains (c) ISO/IEC, so it is not in the
tree: these pages cite clause and paragraph (`7.21.6.1p8`) and
paraphrase, never quote.  `tools/annexb.py` reads a local
`pdftotext -layout` copy and keeps only identifiers.

## Step 1: the inventory (2026-11-01)

[`inventory.md`](inventory.md): for every identifier Annex B (the library
summary) gives a header, does each library provide it?  `inventory.tsv`
holds the per-identifier answer for three legs:

| leg | compiler | headers and library |
|---|---|---|
| clang | clang `-std=c11` for slow32 | `runtime/include`, `libc_mmio.s32a` + `libs32.s32a` |
| stage08 | `selfhost/stage08/cc.s32x` under the DBT | `selfhost/stage08/include`, `selfhost/stage08/lib` |
| host | the host's cc `-std=c11` | the host's (a check on the tool, not a target) |

    python3 tools/annexb.py n1570.txt > annexb.tsv
    python3 tools/inventory.py annexb.tsv inventory.tsv [clang stage08 host]

Each identifier is compiled (and, where it names a function or object,
linked) in a program of its own:

| answer | meaning |
|---|---|
| MACRO | `#ifdef` sees it |
| FUNC, OBJ | `&(name)` compiles and links (the parentheses get past a function-like macro to the function 7.1.4p1 requires beside it) |
| TYPE | a complete type |
| TYPE-INCOMPLETE | a type, incomplete (`FILE` in the selfhost library, as in musl; C11 allows it) |
| DECL-ONLY | declared, but nothing in the library defines it |
| MACRO-ONLY | a function present only as a macro: wrong unless the clause specifies a macro (`assert`, `va_arg`, `isgreater`, ...) |
| MISSING | none of the above |

The optional parts are left out: `<complex.h>`, `<stdatomic.h>`,
`<threads.h>` and Annex K's bounds-checked interfaces (`*_s`, `errno_t`,
`rsize_t`).  The host leg misses only `NDEBUG` (the program's to define,
not the library's) and `<uchar.h>` (macOS has none), which is how the
tool was checked.

## Step 2: behaviour

    python3 tools/probe.py [-v] [probes/7.21.*.c ...]   # every probe if none named
    python3 tools/probe.py --check                      # the gate: against verdicts.txt
    python3 tools/probe.py --update                     # record this run
    python3 tools/ngram.py n1570.txt probes/*.c *.md    # nothing copied from the standard

A probe (`probes/<clause>-<slug>.c`, first line naming the clause) is
built and run four ways: clang + the runtime and stage08 + the selfhost
library, both on the reference emulator; the host's cc and libc; and gcc
with glibc in a Linux container (podman, `docker.io/library/gcc`; one
container run for every probe not already in `~/.cache`).  The expected
output is what macOS's and glibc's libraries both print; where they
differ, a `<probe>.expect` holds the ruling and the page says why.
Output is stdout plus `[exit N]`; every leg runs with
`TZ=America/Chicago`, `LC_ALL=C`, stdin from `<probe>.in` or nothing.

| verdict, per library | meaning |
|---|---|
| PASS | prints what the oracle prints |
| DIFFER | builds, prints something else: wrong behaviour |
| NOBUILD | does not compile or link: usually something the library lacks |
| TIMEOUT | did not finish in 60 s |
| ORACLE-SPLIT | the two host libraries disagree and no ruling is recorded |
