#!/bin/bash
# cobol/ test harness.
#
# Gate 1 (pictest): pic_analyse over tests/pictures.txt against an expected
#   file checked by hand against the 1985 PICTURE clause text.
# Gate 2 (programs): every tests/fixed/*.cbl and tests/free/*.cbl (and
#   tests/2002/*.cbl, under -std=2002 and the oracle's -std=cobol2002; tests/2014/*.cbl
#   likewise under -std=2014 and -std=cobol2014; tests/2023/*.cbl under -std=2023, the oracle's
#   -std=cobol2014, its nearest) compiled by
#   s32-cobc, assembled, linked with libcob and the SLOW-32 libc, run on the
#   emulator; stdout must match the .expected file.  The same source is also
#   compiled and run under GnuCOBOL and diffed, so the .expected files are
#   checked against the oracle as well as against us (docs/oracles.md:
#   -std=cobol85 for portable programs; a program whose first comment names
#   "default dialect" uses GnuCOBOL's default, because it exercises
#   implementor usages -std=cobol85 rejects).  GnuCOBOL is no longer
#   installed on any host: the oracle is the gnucobol:$ORACLE_TAG-builder image
#   (ORACLE_TAG=4.0, trunk, by default; 3.3 for the 3.x branch -- docs/oracles.md
#   says which revision each is; GCOBOL_IMAGE=gcobol:17 likewise for the second)
#   (cobc) and gnucobol:4.0-runtime (the built program), under podman or
#   docker, with the repo bind-mounted at its own path.  A host cobc, if
#   one exists, is used instead.  No oracle at all is reported, not hidden.
#   A SECOND oracle, gcobol (GCC 15's COBOL front end, the gcobol:15 image
#   from ~/gnucobol/gcobol), compiles and runs the same program when its
#   image is present: its agreement is a note, its disagreement or refusal
#   is counted and listed (the run's gcobol-differs.txt is printed at the
#   end) but does not fail the test -- gcobol is young and its gaps are
#   being surveyed (docs/oracles.md); GCOBOL=strict makes them failures,
#   GCOBOL=0 leaves it out.  A test's "no gcobol" comment skips it alone;
#   .gcobol-expected beside a test holds a documented gcobol divergence.
# Gate 3 (refusals): every tests/bad/*.cbl must be refused with exactly one
#   error per line of its .expected, each containing that line's text, and
#   leave no output file.  Unimplemented is a diagnostic, never silence.
# Gate 4 (behavior points): every tests/warn/*.cbl must compile under
#   -warn-74 with exactly the [BP-..] ids its .expected lists (an empty
#   file: none), and must compile with no stderr at all without the flag.
#   The ids are docs/behavior-points.md's.  flag14-*.cbl: >>FLAG-14's
#   [F14-OPTION] warnings under -std=2023 (2023 7.3.15), no flag.
# Both paths (in Gates 2 and 5): every program is compiled a second time
#   with -fno-hot-arith, the register arithmetic and its peepholes off, and
#   must print the same; CCVS runs twice and every program's tally, report
#   and console output must be the same bytes (docs/performance.md).
# Gate 5 (NIST): the CCVS-85 totals must equal tests/ccvs-baseline.txt
#   exactly (CCVS85 names the tree; CCVS=0 skips it, and a missing
#   tree is reported as NOT RUN, never passed over).
# Gate 7 (generated): a fixed batch of each tests/gen generator (arith,
#   edit, cond, string, table, flow, pos), run here and under the oracle by run-gen.sh: every
#   line must agree, apart from the oracle disagreements docs/oracles.md
#   records -- and those only in the shapes run-gen.sh checks (ours equal
#   to a computed truth or to inspect85.py).  Needs the oracle's container
#   image; without it the summary says the gate did not run.
# Gate 8 (sanitizers): tests/sanitize.sh -- the compiler built with the
#   address and undefined-behavior sanitizers compiles every test, the
#   exception sites, generated programs and majesty's sources without a
#   report.  A host that cannot build so is reported, not passed over.
# Gate 6 (exception sites): tests/ecsites/template.cbl compiled once per
#   line of sites.txt with the statement put in; each must print what the
#   line expects -- RAISED where the statement references invalid numeric
#   content under EC-DATA-INCOMPATIBLE checking, "not raised" where it does
#   not.  A fatal condition ends the run, so one program per site.
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
CDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$CDIR/.." && pwd)"
# Tree paths by default; the same S32_* knobs compile.sh honours point the
# suite at an installed copy (the slow32:cobol image: /opt/slow32).
AS="${S32_AS:-$ROOT/tools/assembler/slow32asm}"
LD="${S32_LD:-$ROOT/tools/linker/s32-ld}"
EMU="${EMU:-$ROOT/tools/emulator/slow32}"
COBC="${S32_COBC:-$CDIR/out/s32-cobc}"
LIBCOB="${S32_LIBCOB:-$CDIR/libcob/libcob.s32o}"
HOSTCC="${HOSTCC:-cc}"
# Is there a SLOW-32 C compiler at all (cctool.sh's two backends)?  A test
# whose .link names a .c file needs one; without it the test is SKIPPED,
# named, and counted in the summary, not failed.
HAVE_S32_CC=0
[ -x "${LLVM_BIN:-$HOME/llvm-project/build/bin}/clang" ] && HAVE_S32_CC=1
[ -f "${S32_KIT:-$HOME/s32x}/cc.s32x" ] && HAVE_S32_CC=1
# The oracle: host cobc if present, else GnuCOBOL in a container.
# ORACLE=0 turns it off.  The container oracle is TWO `docker run`s per
# test, ~200 for the suite, and what that costs is a property of the HOST
# rather than of the suite: measured the same day on the same images and
# the same harness, one box starts a container in 0.145s and finishes the
# oracle-backed suite in 29 seconds, while another takes ~24s per start
# and over an hour.  On the second, the oracle is not part of the run, it
# is the run.  So two machines can split the gate: the fast one keeps the
# oracle, the slow one drops it, and between them the suite is covered.
#
# What is left without it is still the full comparison against our own
# expected outputs.  What is dropped is "and GnuCOBOL agrees", so the
# summary line says so -- an ORACLE=0 run must not be mistakable in a log
# for a full one, which is the same trap as an oracle that refuses and
# reports a pass.
# Which oracle images: gnucobol:$ORACLE_TAG-builder and -runtime (4.0 = the
# trunk build, 3.3 = the 3.x branch; docs/oracles.md says which revisions),
# and the gcobol image (gcobol:15 by default; 17 is the one upstream backports to)
: "${ORACLE_TAG:=4.0}"
: "${GCOBOL_IMAGE:=gcobol:15}"
ORACLE_ENGINE=""
if [ "${ORACLE:-1}" = 0 ]; then
    :
elif command -v cobc >/dev/null 2>&1; then
    ORACLE_ENGINE=host
else
    for e in podman docker; do
        if command -v "$e" >/dev/null 2>&1 && "$e" image inspect "gnucobol:$ORACLE_TAG-builder" >/dev/null 2>&1; then
            ORACLE_ENGINE="$e"
            ORACLE_RUN_IMAGE="gnucobol:$ORACLE_TAG-builder"
            "$e" image inspect "gnucobol:$ORACLE_TAG-runtime" >/dev/null 2>&1 && ORACLE_RUN_IMAGE="gnucobol:$ORACLE_TAG-runtime"
            break
        fi
    done
fi

# the second oracle: gcobol (GCC 15), one image that compiles and runs
GCOBOL_ENGINE=""
if [ "${GCOBOL:-1}" != 0 ]; then
    for e in podman docker; do
        if command -v "$e" >/dev/null 2>&1 && "$e" image inspect "$GCOBOL_IMAGE" >/dev/null 2>&1; then GCOBOL_ENGINE="$e"; break; fi
    done
fi
GC_AGREE=0; GC_DIFF=0; GC_REFUSED=0; GC_SKIP=0

# The work directory lives under cobol/out (gitignored), not /tmp: a
# container engine on macOS can bind-mount the home directory but not
# /tmp, and the oracle compiles and runs inside the container on the
# same absolute paths the host sees.
mkdir -p "$CDIR/out"
W="$(mktemp -d "$CDIR/out/tests.XXXXXX")"
# build.sh refuses to rebuild under a running harness (a rebuild mid-run
# mixes compilers); this run's own builds pass
echo $$ > "$CDIR/out/harness.lock"; export S32_HARNESS=1
trap 'rm -rf "$W"; rm -f "$CDIR/out/harness.lock"' EXIT

oracle_cc() {   # oracle_cc out.orc [cobc args...]: compile under GnuCOBOL, cwd $W
    out="$1"; shift
    case "$ORACLE_ENGINE" in
        host) (cd "$W" && cobc -x "$@" -o "$out") ;;
        *)    "$ORACLE_ENGINE" run --rm -v "$ROOT:$ROOT" -w "$W" "gnucobol:$ORACLE_TAG-builder" cobc -x "$@" -o "$out" ;;
    esac
}
oracle_run() {  # oracle_run prog.orc [args...]: run the oracle's program in $W/run,
                # standard input from $keys (the test's .keys file, or nothing)
    case "$ORACLE_ENGINE" in
        host) (cd "$W/run" && env ${PROG_ENV[@]+"${PROG_ENV[@]}"} "$@" < "$keys") ;;
        *)    local ef=(); for x in ${PROG_ENV[@]+"${PROG_ENV[@]}"}; do ef+=(-e "$x"); done
              "$ORACLE_ENGINE" run --rm -i ${ef[@]+"${ef[@]}"} -v "$ROOT:$ROOT" -w "$W/run" "$ORACLE_RUN_IMAGE" timeout 60 "$@" < "$keys" ;;
    esac
}
# (timeout: an oracle that hangs -- GnuCOBOL 4.0-early-dev does on an OPEN
# failure without FILE STATUS inside a contained program -- counts as a
# disagreement, not a stalled harness)
gcobol_cc() {   # gcobol_cc out [gcobol args...]: compile under gcobol, cwd $W
    out="$1"; shift
    "$GCOBOL_ENGINE" run --rm -v "$ROOT:$ROOT" -w "$W" "$GCOBOL_IMAGE" gcobol "$@" -o "$out"
}
gcobol_run() {  # gcobol_run prog [args...]: in $W/run, stdin from $keys
    local ef=(); for x in ${PROG_ENV[@]+"${PROG_ENV[@]}"}; do ef+=(-e "$x"); done
    "$GCOBOL_ENGINE" run --rm -i ${ef[@]+"${ef[@]}"} -v "$ROOT:$ROOT" -w "$W/run" "$GCOBOL_IMAGE" timeout 60 "$@" < "$keys"
}
# the second oracle on one test: a note for the report, the counts kept;
# GCOBOL=strict turns a disagreement or refusal into a failure (returns 1)
gcobol_check() {   # gcobol_check name src flag exp extra...
    local name="$1" src="$2" flag="$3" exp="$4"; shift 4
    local gflag="-ffree-form"; [ "$flag" = "-fixed" ] && gflag="-ffixed-form"
    if grep -qi "no gcobol" "$src"; then GC_SKIP=$((GC_SKIP+1)); GC_NOTE="gcobol skipped"; return 0; fi
    if ! gcobol_cc "$W/$name.gc" $gflag -I "$HERE/copy" "$src" "$@" >"$W/$name.gclog" 2>&1; then
        GC_REFUSED=$((GC_REFUSED+1)); GC_NOTE="gcobol refused it"
        echo "$name: refused: $(grep -m1 -i error "$W/$name.gclog" | cut -c1-100)" >> "$W/gcobol-differs.txt"
        [ "${GCOBOL:-1}" = strict ] && return 1; return 0
    fi
    fresh_workdir
    gcobol_run "$W/$name.gc" $PROG_ARGS > "$W/$name.gcout" 2>/dev/null
    local gexp="$exp"; [ -f "${src%.cbl}.gcobol-expected" ] && gexp="${src%.cbl}.gcobol-expected"
    if diff -q "$W/$name.gcout" "$gexp" >/dev/null; then
        GC_AGREE=$((GC_AGREE+1)); GC_NOTE="gcobol agrees"; [ "$gexp" != "$exp" ] && GC_NOTE="gcobol agrees with its documented divergence"
        return 0
    fi
    GC_DIFF=$((GC_DIFF+1)); GC_NOTE="gcobol differs"
    { echo "$name: differs:"; diff "$exp" "$W/$name.gcout" | head -6; } >> "$W/gcobol-differs.txt"
    [ "${GCOBOL:-1}" = strict ] && return 1; return 0
}

PASS=0; FAIL=0

report() {
    if [ "$2" = "0" ]; then printf "  %-28s PASS%s\n" "$1:" "${3:+ ($3)}"; PASS=$((PASS+1))
    else printf "  %-28s FAIL%s\n" "$1:" "${3:+ ($3)}"; FAIL=$((FAIL+1)); fi
}

# Every program runs with a fresh copy of tests/data as its working
# directory (fixtures in data/, outputs to tmp/), for us and for the oracle.
fresh_workdir() {
    rm -rf "$W/run"; mkdir -p "$W/run/tmp"
    [ -d "$HERE/data" ] && cp -R "$HERE/data/." "$W/run/"
}

emu_run() {   # emu_run prog.s32x > stdout: the guest's output only
    # Everything between the emulator's "Starting execution" line and its
    # halt report is the program's.  Blank lines inside are the program's
    # too (DISPLAY of nothing), so this is a capture, not a grep -v.
    # The emulator writes one empty line of its own before "Program halted.";
    # hold each line back one step so that line can be dropped and a
    # program's own trailing blank line kept.
    # a .keys file beside the test is typed into the program (the term
    # service reads keys from the emulator's stdin)
    # a .args file beside the test is the program's command line;
    # a .env file beside it is the guest's environment (S32_SORT_MEMORY=24K ...),
    # and the oracle's: GnuCOBOL ignores the S32_ names and reads COB_ ones
    # the program's exit status (the emulator's own) goes to $W/lastrc,
    # for a test with a .exitcode file (STOP RUN WITH STATUS)
    rm -f "$W/lastrc"
    (cd "$W/run" && env ${PROG_ENV[@]+"${PROG_ENV[@]}"} "$EMU" "$1" $PROG_ARGS 2>/dev/null < "${2:-/dev/null}"; echo $? > "$W/lastrc") | awk '
        /^Starting execution/ { capture = 1; held = 0; next }
        /^HALT at|^Program halted|^Exit code/ { if (held && prev != "") print prev; capture = 0; held = 0 }
        capture { if (held) print prev; prev = $0; held = 1 }
        END { if (held) print prev }'
}

if [ ! -x "$COBC" ] || [ ! -f "$LIBCOB" ]; then
    "$CDIR/build.sh" >/dev/null || { echo "build failed"; exit 1; }
fi

# --- Gate 1: PICTURE ---------------------------------------------------
# Gates 1 and 1b build host programs.  Without a host C compiler (the
# slow32:cobol image has none) they are SKIPPED, and the summary says so:
# a run that silently dropped two gates would read as a full one.
SKIPPED=""; GEN_SKIPPED=""
if ! command -v "$HOSTCC" >/dev/null 2>&1; then
    SKIPPED=" pictest bt_test wide_test ieee_test scredit_test scrnum_test scram_test"
    echo "SKIP  pictest  (no host C compiler: $HOSTCC)"
    echo "SKIP  bt_test  (no host C compiler: $HOSTCC)"
elif ! "$HOSTCC" -std=c99 -I"$CDIR/src" -O1 -w -o "$W/pictest" "$HERE/pictest.c" \
        "$CDIR/src/picture.c" "$CDIR/src/picture_scan.c" "$CDIR/src/lex_scan.c" 2>"$W/cc.log"; then
    report "pictest" 1 "host build"
else
    "$W/pictest" "$HERE/pictures.txt" > "$W/pictures.out" 2>&1
    if diff -q "$W/pictures.out" "$HERE/pictures.expected" >/dev/null 2>&1; then
        report "pictest" 0
    else
        report "pictest" 1 "mismatch"
        diff "$HERE/pictures.expected" "$W/pictures.out" | head -12
    fi
fi

# --- Gate 1c: the 31-digit arithmetic core (host, libcob/wide.h) -------
# Every limb operation against the host's unsigned __int128 (docs/wide.md)
if [ -z "$SKIPPED" ]; then
    if ! "$HOSTCC" -std=gnu99 -I"$CDIR/libcob" -O1 -w -o "$W/wide_test" "$HERE/wide_test.c" 2>"$W/cc.log"; then
        report "wide_test" 1 "host build"
    elif "$W/wide_test" > "$W/wide.out" 2>&1; then
        report "wide_test" 0 "$(tail -1 "$W/wide.out" | sed 's/^wide_test: //')"
    else
        report "wide_test" 1 "$(tail -1 "$W/wide.out")"
    fi
    # --- Gate 1d: the IEEE formats (host, libcob/ieee.h) against the exact
    # rational arithmetic of tests/ieee_vectors.py (binary128 both ways, the
    # decimal formats in both encodings and byte orders)
    if ! "$HOSTCC" -std=gnu99 -I"$CDIR/libcob" -O1 -w -o "$W/ieee_test" "$HERE/ieee_test.c" 2>"$W/cc.log"; then
        report "ieee_test" 1 "host build"
    elif ! python3 -I "$HERE/ieee_vectors.py" > "$W/ieee_vectors.txt" 2>"$W/ieee.err"; then
        report "ieee_test" 1 "the vectors: $(tail -1 "$W/ieee.err")"
    elif "$W/ieee_test" < "$W/ieee_vectors.txt" > "$W/ieee.out" 2>&1; then
        report "ieee_test" 0 "$(tail -1 "$W/ieee.out" | sed 's/^ieee: //')"
    else
        report "ieee_test" 1 "$(tail -1 "$W/ieee.out")"
    fi
fi

# --- Gate 1i: the screen field editor's core (host, libcob/scredit.h) ---
# tests/scredit.txt's pictures and keys through the core, the field and
# cursor after every key against scredit.expected (docs/plans/
# screen-input.md; scredit-differential.sh checks the same core against
# Micro Focus's ADIS when the oracle rig is there)
if [ -z "$SKIPPED" ]; then
    if ! "$HOSTCC" -std=gnu99 -I"$CDIR/libcob" -O1 -w -o "$W/scredit_test" "$HERE/scredit_test.c" 2>"$W/cc.log"; then
        report "scredit_test" 1 "host build"
    else
        grep -v '^#' "$HERE/scredit.txt" | while IFS='~' read -r pic val keys; do
            echo "== $pic ~ $val ~ $keys"; "$W/scredit_test" "$pic" "$val" "$keys{ENTER}"
        done > "$W/scredit.out" 2>&1
        if cmp -s "$W/scredit.out" "$HERE/scredit.expected"; then
            report "scredit_test" 0 "$(grep -c '^==' "$W/scredit.out") cases"
        else
            report "scredit_test" 1 "$(diff "$W/scredit.out" "$HERE/scredit.expected" | head -1)"
        fi
    fi
fi

# --- Gate 1j: the same core's numeric fields (tests/scrnum.txt) ---------
if [ -z "$SKIPPED" ]; then
    if ! "$HOSTCC" -std=gnu99 -I"$CDIR/libcob" -I"$CDIR/src" -O1 -w -o "$W/scrnum_test" "$HERE/scrnum_test.c" \
            "$CDIR/src/picture.c" "$CDIR/src/picture_scan.c" "$CDIR/src/lex_scan.c" 2>"$W/cc.log"; then
        report "scrnum_test" 1 "host build"
    else
        grep -v '^#' "$HERE/scrnum.txt" | while IFS='~' read -r pic val keys; do
            echo "== $pic ~ $val ~ $keys"; "$W/scrnum_test" "$pic" "$val" "$keys{ENTER}"
        done > "$W/scrnum.out" 2>&1
        if cmp -s "$W/scrnum.out" "$HERE/scrnum.expected"; then
            report "scrnum_test" 0 "$(grep -c '^==' "$W/scrnum.out") cases"
        else
            report "scrnum_test" 1 "$(diff "$W/scrnum.out" "$HERE/scrnum.expected" | head -1)"
        fi
    fi
fi

# --- Gate 1e: SCRAM-SHA-256 for the PostgreSQL client (libcob/scram.c) --
# SHA-256, HMAC, PBKDF2, base64 and a whole SCRAM exchange against the
# FIPS and RFC vectors (docs/esql.md, the PostgreSQL backend)
if [ -z "$SKIPPED" ]; then
    if ! "$HOSTCC" -std=gnu99 -O1 -w -o "$W/scram_test" "$HERE/scram_test.c" 2>"$W/cc.log"; then
        report "scram_test" 1 "host build"
    elif "$W/scram_test" > "$W/scram.out" 2>&1; then
        report "scram_test" 0 "$(tail -1 "$W/scram.out" | sed 's/^scram_test: //')"
    else
        report "scram_test" 1 "$(grep -m1 FAIL "$W/scram.out")"
    fi
fi

# --- Gate 1f: the loop analysis (host, src/cobc/loopreg.h) --------------
# The reading of a loop's code that decides what may be kept in a register
# across it, on lines written for it: what it must refuse and the compiler
# does not happen to emit
if [ -z "$SKIPPED" ]; then
    if ! "$HOSTCC" -std=gnu99 -O1 -w -o "$W/loopreg_test" "$HERE/loopreg_test.c" "$CDIR/src/picture.c" "$CDIR/src/picture_scan.c" "$CDIR/src/lex_scan.c" 2>"$W/cc.log"; then
        report "loopreg_test" 1 "host build"
    elif "$W/loopreg_test" > "$W/loopreg.out" 2>&1; then
        report "loopreg_test" 0 "$(tail -1 "$W/loopreg.out" | sed 's/^loopreg_test: //')"
    else
        report "loopreg_test" 1 "$(grep -m1 FAIL "$W/loopreg.out")"
    fi
fi

# --- Gate 1h: what is done with an item's bytes (src/cobc/symtab.h) -----
# The rule that decides whether an item may be written the machine's way
# (src/cobc/native.h), on events written for it: the cases it must
# refuse and the compiler does not happen to emit
if [ -z "$SKIPPED" ]; then
    if ! "$HOSTCC" -std=gnu99 -O1 -w -o "$W/census_test" "$HERE/census_test.c" "$CDIR/src/picture.c" "$CDIR/src/picture_scan.c" "$CDIR/src/lex_scan.c" 2>"$W/cc.log"; then
        report "census_test" 1 "host build"
    elif "$W/census_test" > "$W/census_test.out" 2>&1; then
        report "census_test" 0 "$(tail -1 "$W/census_test.out" | sed 's/^census_test: //')"
    else
        report "census_test" 1 "$(grep -m1 FAIL "$W/census_test.out")"
    fi
fi

# --- Gate 1g: the census (src/cobc/census.h, docs/plans/census.md) ------
# Which items stand alone, on two programs written to hold every shape
# that decides it: groups named and not, redefinitions named and not,
# records redefining records, tables in tables, renamings, a GLOBAL item
# a contained program names, CALL arguments by reference and by content
# ... and performs.cbl for the PERFORM census (src/cobc/pcensus.h): a
# mainline that leaves by GO TO, ranges performed and fallen into, GO TOs
# within a range and out of one, a paragraph nothing reaches
for n in alone shapes performs; do
    mkdir -p "$W/census-$n"
    ext=census; [ $n = performs ] && ext=perform
    if ! (cd "$HERE/census" && S32_CENSUS_DIR="$W/census-$n" "$COBC" -free -std=2002 "$n.cbl" -o "$W/census-$n/$n.s") >"$W/census-$n/err" 2>&1; then
        report "census/$n" 1 "compile"
    elif cat "$W/census-$n"/*.$ext | diff - "$HERE/census/$n.expect" >"$W/census-$n/diff" 2>&1; then
        report "census/$n" 0 "$(($(wc -l < "$HERE/census/$n.expect") - 1)) lines"
    else
        report "census/$n" 1 "$(grep -m1 '^[<>]' "$W/census-$n/diff" | cut -c1-80)"
    fi
done

# --- Gate 1d: the DBT hooks over libcob/kern.h (docs/dbt-hooks.md) ------
# slow32-fast (no hooks) against slow32-dbt (hooks) on random descriptors;
# needs both engines built in the tree.
if [ -x "$CDIR/../tools/dbt/slow32-dbt" ] && [ -x "$CDIR/../tools/emulator/slow32-fast" ]; then
    if "$HERE/kern-differential.sh" > "$W/kern.out" 2>&1; then
        report "kern-differential" 0 "$(tail -1 "$W/kern.out" | sed 's/^kern-differential: //; s/, hooks called.*//')"
    else
        report "kern-differential" 1 "$(head -1 "$W/kern.out")"
    fi
fi

# --- Gate 1b: the key-file B+tree (host, libcob/btree.h) ---------------
# Six shapes, ~8s of silence on a fast host and more on a busy one -- say so,
# or the pause after pictest reads as a hang (it was reported as one).
[ -n "$SKIPPED" ] || printf "  %-28s running 6 shapes (host, ~10s)...\n" "bt_test:"
if [ -n "$SKIPPED" ]; then
    :
elif ! "$HOSTCC" -std=c99 -I"$CDIR/libcob" -O1 -w -o "$W/bt_test" "$HERE/bt_test.c" 2>"$W/cc.log"; then
    report "bt_test" 1 "host build"
else
    btfail=0
    for ex in 0 8; do for args in "200 4000 20000" "20 30000 40000" "3 5000 20000"; do
        (cd "$W" && EXTRA=$ex ./bt_test $args) > "$W/bt.out" 2>&1 || btfail=1
        grep -q 'root-is-empty-leaf=1' "$W/bt.out" || btfail=1
    done; done
    if [ $btfail = 0 ]; then report "bt_test" 0 "6 shapes"; else report "bt_test" 1 "$(tail -1 "$W/bt.out")"; fi
fi

# --- Gate 2: programs --------------------------------------------------
# tests/2002 is Stage B (docs/standards.md): free format, compiled with
# -std=2002, the oracle with -std=cobol2002.  fixed/ and free/ are -std=85.
for fmt in fixed free 2002 2014 2023; do
    for src in "$HERE/$fmt"/*.cbl; do
        [ -e "$src" ] || continue
        name="$(basename "$src" .cbl)"
        exp="${src%.cbl}.expected"
        flag="-$fmt"; stdflag=""; ostd="-std=cobol85"
        [ "$fmt" = 2002 ] && { flag="-free"; stdflag="-std=2002"; ostd="-std=cobol2002"; }
        [ "$fmt" = 2014 ] && { flag="-free"; stdflag="-std=2014"; ostd="-std=cobol2014"; }
        [ "$fmt" = 2023 ] && { flag="-free"; stdflag="-std=2023"; ostd="-std=cobol2014"; }
        # mf-*: Micro Focus's dialect (-dialect=mf), the oracle in GnuCOBOL's -std=mf
        case "$name" in mf-*) stdflag="$stdflag -dialect=mf"; ostd="-std=mf" ;; esac
        # gnu-*: GnuCOBOL's own forms (-dialect=gnucobol), the oracle in its default dialect
        case "$name" in gnu-*) stdflag="$stdflag -dialect=gnucobol"; ostd="-std=default" ;; esac
        # a .link file beside the test names further sources (subprogram
        # .cbl, .c) relative to tests/, for us and for the oracle
        extra=(); needs_cc=0
        if [ -f "${src%.cbl}.link" ]; then
            for e in $(cat "${src%.cbl}.link"); do
                extra+=("$HERE/$e")
                case "$e" in *.c) needs_cc=1 ;; esac
            done
        fi
        if [ "$needs_cc" = 1 ] && [ "$HAVE_S32_CC" = 0 ]; then
            echo "SKIP  $fmt/$name  (links a .c file; no SLOW-32 C compiler here)"
            SKIPPED="$SKIPPED $fmt/$name"
            continue
        fi
        if ! "$CDIR/compile.sh" $flag $stdflag -I "$HERE/copy" "$src" "${extra[@]+"${extra[@]}"}" -o "$W/$name.s32x" >"$W/$name.log" 2>"$W/$name.err"; then
            report "$fmt/$name" 1 "$(grep -m1 -i "error" "$W/$name.err" "$W/$name.log" | head -1 | sed 's/^[^:]*://')"; continue
        fi
        fresh_workdir
        keys=/dev/null; [ -f "${src%.cbl}.keys" ] && keys="${src%.cbl}.keys"
        PROG_ARGS=""; [ -f "${src%.cbl}.args" ] && PROG_ARGS="$(cat "${src%.cbl}.args")"
        # one VAR=value per line, kept whole: a value may hold spaces
        # (COB_CURRENT_DATE=2026/09/07 13:45:10); GnuCOBOL gets it too
        PROG_ENV=(); if [ -f "${src%.cbl}.env" ]; then while IFS= read -r l || [ -n "$l" ]; do [ -n "$l" ] && PROG_ENV+=("$l"); done < "${src%.cbl}.env"; fi
        emu_run "$W/$name.s32x" "$keys" > "$W/$name.out"
        if [ ! -f "$exp" ]; then
            report "$fmt/$name" 1 "no .expected file"; continue
        fi
        if ! diff -q "$W/$name.out" "$exp" >/dev/null; then
            report "$fmt/$name" 1 "output mismatch"
            diff "$exp" "$W/$name.out" | head -8
            continue
        fi
        if [ -f "${src%.cbl}.exitcode" ] && [ "$(cat "$W/lastrc" 2>/dev/null)" != "$(cat "${src%.cbl}.exitcode")" ]; then
            report "$fmt/$name" 1 "exit status $(cat "$W/lastrc" 2>/dev/null), want $(cat "${src%.cbl}.exitcode")"; continue
        fi
        # both paths: the same program with the register arithmetic and its
        # peepholes off (-fno-hot-arith, the code the decimal stack and
        # cob_move run) must print the same -- the fast paths are exact by
        # construction, and this is where that is checked on every program
        if ! "$CDIR/compile.sh" $flag $stdflag -fno-hot-arith -I "$HERE/copy" "$src" "${extra[@]+"${extra[@]}"}" -o "$W/$name.stack.s32x" >"$W/$name.stack.log" 2>&1; then
            report "$fmt/$name" 1 "does not compile with -fno-hot-arith: $(grep -m1 -i error "$W/$name.stack.log")"; continue
        fi
        fresh_workdir
        emu_run "$W/$name.stack.s32x" "$keys" > "$W/$name.stack.out"
        if ! diff -q "$W/$name.stack.out" "$exp" >/dev/null; then
            report "$fmt/$name" 1 "the stack path (-fno-hot-arith) prints otherwise"
            diff "$exp" "$W/$name.stack.out" | head -8
            continue
        fi
        # a .tapemgr file lists "file maxlen" pairs the program wrote in
        # mode V: each goes through majesty's tapemgr (create a binary-V
        # dataset from it, extract it again) and must come back byte for
        # byte -- the RDW on disk is IBM's, not a private length word
        if [ -f "${src%.cbl}.tapemgr" ] && [ -x "$HOME/majesty/tapemgr" ]; then
            tm_ok=1
            while read -r vf vlen; do
                [ -n "$vf" ] || continue
                cat > "$W/tm.json" <<JSON
{ "volume_serial": "S32V01", "owner_code": "SLOW32", "files": [ { "dataset_name": "S32.VREC", "local_file": "$W/run/$vf", "record_format": "V", "record_length": $vlen, "block_size": 4096, "binary": true } ] }
JSON
                cat > "$W/tmx.json" <<JSON
{ "volume_serial": "S32V01", "owner_code": "SLOW32", "files": [ { "dataset_name": "S32.VREC", "local_file": "$W/tm-back.dat", "record_format": "V", "record_length": $vlen, "block_size": 4096, "binary": true } ] }
JSON
                rm -f "$W/tm.aws" "$W/tm-back.dat"
                # (tapemgr create drops a RESTORE.JCL in its cwd; keep that in $W)
                if ! (cd "$W" && "$HOME/majesty/tapemgr" create --volser=S32V01 -o "$W/tm.aws" -c "$W/tm.json") >"$W/tm.log" 2>&1 ||
                   ! (cd "$W" && "$HOME/majesty/tapemgr" extract -c "$W/tmx.json" "$W/tm.aws") >>"$W/tm.log" 2>&1 ||
                   ! cmp -s "$W/run/$vf" "$W/tm-back.dat"; then
                    tm_ok=0; report "$fmt/$name" 1 "tapemgr round trip of $vf failed: $(tail -1 "$W/tm.log")"; break
                fi
            done < "${src%.cbl}.tapemgr"
            [ "$tm_ok" = 1 ] || continue
        fi
        # oracle: GnuCOBOL on the same source, when present.  A program whose
        # comments say "no oracle" (screens need a tty there) is ours alone.
        note=""
        if grep -qi "no oracle" "$src"; then note="no oracle: reviewed by hand"; ORACLE_SKIP=1; else ORACLE_SKIP=0; fi
        if [ -n "$ORACLE_ENGINE" ] && [ "$ORACLE_SKIP" = 0 ]; then
            std="$ostd"
            grep -qi "default dialect" "$src" && std=""
            if oracle_cc "$W/$name.orc" $std $flag -I "$HERE/copy" "$src" "${extra[@]+"${extra[@]}"}" >"$W/$name.orclog" 2>&1; then
                fresh_workdir
                oracle_run "$W/$name.orc" $PROG_ARGS > "$W/$name.orcout" 2>/dev/null
                # a documented divergence from GnuCOBOL (docs/oracles.md) keeps
                # GnuCOBOL's own output beside the standard's in .oracle-expected
                oexp="$exp"; [ -f "${src%.cbl}.oracle-expected" ] && oexp="${src%.cbl}.oracle-expected"
                if diff -q "$W/$name.orcout" "$oexp" >/dev/null; then
                    note="oracle agrees"; [ "$oexp" != "$exp" ] && note="oracle agrees with its documented divergence"
                    [ -f "${src%.cbl}.tapemgr" ] && note="$note; tapemgr round trip"
                else
                    report "$fmt/$name" 1 "GnuCOBOL disagrees with .expected"
                    diff "$exp" "$W/$name.orcout" | head -8
                    continue
                fi
            else
                # An oracle refusal used to be a note on a PASS, which is how
                # free/identmove -- the test carrying a conformance argument
                # that is *about* what GnuCOBOL does -- went green for a day
                # without GnuCOBOL ever compiling it (COMP-3 under
                # -std=cobol85).  A test the oracle cannot build is a test
                # with no oracle, and that has to be a decision someone wrote
                # down: say "default dialect" if it needs GnuCOBOL's own
                # dialect, or "no oracle" if it cannot be checked there at all.
                report "$fmt/$name" 1 "oracle refused it: $(grep -m1 error "$W/$name.orclog" | cut -c1-60)"
                sed -n '1,3p' "$W/$name.orclog"
                continue
            fi
        fi
        if [ -n "$GCOBOL_ENGINE" ] && [ "$ORACLE_SKIP" = 0 ]; then
            if ! gcobol_check "$name" "$src" "$flag" "$exp" "${extra[@]+"${extra[@]}"}"; then
                report "$fmt/$name" 1 "$GC_NOTE (GCOBOL=strict)"; tail -7 "$W/gcobol-differs.txt"; continue
            fi
            note="${note:+$note; }$GC_NOTE"
        fi
        report "$fmt/$name" 0 "$note"
    done
done

# --- Gate 3: refusals --------------------------------------------------
for src in "$HERE/bad"/*.cbl; do
    [ -e "$src" ] || continue
    name="$(basename "$src" .cbl)"
    exp="${src%.cbl}.expected"
    flag="-fixed"; grep -q "^identification division" "$src" && flag="-free"
    [ "$name" = "mixed-format" ] && flag="-fixed"
    stdflag=""; case "$name" in std2002-*) stdflag="-std=2002" ;; std2014-*) stdflag="-std=2014" ;; std2023-*) stdflag="-std=2023" ;; esac   # a Stage B refusal; a 2014 one; a 2023 one
    case "$name" in mf-*) stdflag="$stdflag -dialect=mf" ;; esac         # refused even under Micro Focus's dialect
    case "$name" in gnu-*) stdflag="$stdflag -dialect=gnucobol" ;; esac  # refused even under GnuCOBOL's
    if "$COBC" $flag $stdflag -I "$HERE/copy" -o "$W/$name.s" "$src" 2>"$W/$name.err"; then
        report "bad/$name" 1 "was accepted"; continue
    fi
    # one line of .expected per error, and no
    # more errors than that: a cascade after the first is a failure too
    # (ISSUES-41).  A refused compile leaves no output behind.
    want=$(grep -c . "$exp"); got=$(grep -c ': error: ' "$W/$name.err")
    miss=""
    while IFS= read -r line; do
        [ -n "$line" ] && ! grep -qF -- "$line" "$W/$name.err" && miss="$line" && break
    done < "$exp"
    if [ -e "$W/$name.s" ]; then
        report "bad/$name" 1 "left a partial $name.s behind"
    elif [ -n "$miss" ]; then
        report "bad/$name" 1 "wrong message: $(head -1 "$W/$name.err")"
    elif [ "$got" != "$want" ]; then
        report "bad/$name" 1 "$got errors, want $want: $(sed -n "$((want + 1))p" "$W/$name.err" | cut -c1-60)"
    else
        report "bad/$name" 0 "$(cut -d: -f3- "$W/$name.err" | head -1 | cut -c1-50)$([ "$want" -gt 1 ] && echo " (+$((want - 1)) more)")"
    fi
done

# --- Gate 4: behavior points -------------------------------------------
for src in "$HERE/warn"/*.cbl; do
    [ -e "$src" ] || continue
    name="$(basename "$src" .cbl)"
    exp="${src%.cbl}.expected"
    flag="-fixed"; grep -q "^identification division" "$src" && flag="-free"
    # ext-*: the extensions (class E) under -warn-extensions, not -warn-74
    wflag="-warn-74"; case "$name" in ext-*) wflag="-warn-extensions" ;; esac
    stdflag=""; case "$name" in *std2002*) stdflag="-std=2002" ;; esac   # a point that exists only under 2002
    case "$name" in *-mf-*|mf-*|ext-mf*) stdflag="$stdflag -dialect=mf" ;; esac   # a dialect point (class D)
    # flag14-*: >>FLAG-14's warnings (2023 7.3.15), [F14-OPTION] ids, the
    # directive in the source turning them on -- no flag, no silent run
    idpat='\[BP-[A-Z][0-9]*\]'; case "$name" in flag14-*) wflag=""; stdflag="-std=2023"; idpat='\[F14-[A-Z0-9-]*\]' ;; esac
    if ! "$COBC" $flag $stdflag $wflag -I "$HERE/copy" -o "$W/$name.s" "$src" 2>"$W/$name.warn"; then
        report "warn/$name" 1 "refused: $(head -1 "$W/$name.warn")"; continue
    fi
    got="$(grep -o "$idpat" "$W/$name.warn" | sort -u)"
    want="$(sort -u "$exp")"
    if [ "$got" != "$want" ]; then
        report "warn/$name" 1 "ids: got [$(echo $got)] want [$(echo $want)]"; continue
    fi
    case "$name" in flag14-*) report "warn/$name" 0 "$(echo $got | wc -w) option(s) flagged"; continue ;; esac
    "$COBC" $flag $stdflag -I "$HERE/copy" -o "$W/$name.s" "$src" 2>"$W/$name.quiet"
    # silent: no behavior point's warning without the flag (a warning the
    # compiler always gives -- BP-D7's cut literal -- is not one)
    if grep -q '\[BP-' "$W/$name.quiet"; then
        report "warn/$name" 1 "not silent without $wflag: $(head -1 "$W/$name.quiet")"; continue
    fi
    report "warn/$name" 0 "$(echo $got | wc -w) point(s), silent by default"
done

# Gate 6 (exception sites): one program per line of a sites file, its
# statement put into a template, all counted as one report so the tally
# stays readable; the first failure is named.  template.cbl + sites.txt:
# EC-DATA-INCOMPATIBLE; argfn.cbl + argfn.txt: EC-ARGUMENT-FUNCTION.
run_sites() {   # name template sites
    local esn=0 esbad="" l want stmt got
    while IFS= read -r l || [ -n "$l" ]; do
        case "$l" in ""|"#"*) continue ;; esac
        want="${l%%|*}"; stmt="${l#*|}"; esn=$((esn + 1))
        awk -v s="$stmt" '{ i = index($0, "@STMT@"); if (i) $0 = substr($0, 1, i - 1) s substr($0, i + 6); print }' \
            "$HERE/ecsites/$2" > "$W/ecsite.cbl"
        if ! "$CDIR/compile.sh" -free -std=2002 "$W/ecsite.cbl" -o "$W/ecsite.s32x" >"$W/ecsite.log" 2>&1; then
            esbad="compile: $stmt"; break
        fi
        fresh_workdir
        got="$(emu_run "$W/ecsite.s32x" /dev/null | grep -m1 -E '^(RAISED|not raised)$')"
        [ "$got" = "$want" ] || { esbad="${got:-nothing} for: $stmt"; break; }
    done < "$HERE/ecsites/$3"
    if [ -z "$esbad" ]; then report "$1" 0 "$esn sites"; else report "$1" 1 "$esbad"; fi
}
run_sites ecsites template.cbl sites.txt
run_sites ecsites/argfn argfn.cbl argfn.txt

# Gate 5 (NIST): the CCVS-85 totals line must equal tests/ccvs-baseline.txt.
# The suite runs in seconds, and outside this gate a MERGE regression sat
# unseen for three weeks (cobol ISSUES-42).  Equality is a ratchet both
# ways: a better total fails too, until the baseline is updated on purpose.
# No tree is reported, never silent; CCVS=0 switches the gate off.
CCVS_NOTE=""
CCVS_TREE=${CCVS85:-$HOME/gnucobol-svn/tests/cobol85}
if [ "${CCVS:-1}" = 0 ]; then
    CCVS_NOTE="cobol: CCVS-85 NOT RUN -- switched off by CCVS=0"
elif [ -d "$CCVS_TREE" ] && ! ls "$CCVS_TREE"/NC/*.CBL >/dev/null 2>&1; then
    # the harness without the programs (no newcob.val split into NC/, SQ/ ...):
    # nothing to run, which is not zero programs passing (kagura, 2026-10-03)
    CCVS_NOTE="cobol: CCVS-85 NOT RUN -- $CCVS_TREE holds the harness but no split modules (NC/*.CBL)"
elif [ -d "$CCVS_TREE" ]; then
    CCVS_KEEP=1 "$HERE/ccvs-run.sh" > "$W/ccvs.fast" 2>/dev/null
    cd1="$(ls -dt "$CDIR"/out/ccvsrun.* 2>/dev/null | head -1)"
    got="$(tail -1 "$W/ccvs.fast")"
    want="$(cat "$HERE/ccvs-baseline.txt")"
    if [ "$got" = "$want" ]; then
        report "ccvs/totals" 0 "$(echo "$got" | sed 's/.*tests \([0-9]* of [0-9]*\) pass.*/\1/') pass, as recorded"
    else
        report "ccvs/totals" 1 "got [$got] want [$want]; if better, update tests/ccvs-baseline.txt"
    fi
    # both paths: the suite again with -fno-hot-arith; every program's
    # tally, report and console output must be the same bytes.  NC214M
    # prints the time of day (ACCEPT FROM TIME) and differs between any
    # two runs, so its report is left out.  (A null dereference that only
    # the stack path's SEARCH reached was found this way, 2026-09-30.)
    CCVS_KEEP=1 CCVS_FLAGS=-fno-hot-arith "$HERE/ccvs-run.sh" > "$W/ccvs.stack" 2>/dev/null
    cd2="$(ls -dt "$CDIR"/out/ccvsrun.* 2>/dev/null | head -1)"
    bpd=""; bpn=0
    diff -q "$W/ccvs.fast" "$W/ccvs.stack" >/dev/null || bpd="tallies differ: $(diff "$W/ccvs.fast" "$W/ccvs.stack" | grep -m1 '^>' )"
    if [ -z "$bpd" ] && [ -n "$cd1" ] && [ "$cd1" != "$cd2" ]; then
        for f in $(cd "$cd1" && find . \( -name '*.report' -o -name '*.out' \) ! -name 'NC214M.report' | sort); do
            bpn=$((bpn+1))
            cmp -s "$cd1/$f" "$cd2/$f" || { bpd="$f differs"; break; }
        done
    fi
    if [ -z "$bpd" ]; then report "ccvs/both-paths" 0 "$bpn reports and outputs identical with -fno-hot-arith"
    else report "ccvs/both-paths" 1 "$bpd"; fi
    rm -rf "$cd1" "$cd2"
    # SQ101M's 57 tests are the suite's visual inspection of WRITE
    # ADVANCING; the program states where every line must land, so the
    # print file is rendered as a printer would and checked (ISSUES-46)
    if command -v python3 >/dev/null 2>&1; then
        CCVS_KEEP=1 CCVS_ONLY=SQ101M "$HERE/ccvs-run.sh" SQ >/dev/null 2>&1
        sqd="$(ls -dt "$CDIR"/out/ccvsrun.* 2>/dev/null | head -1)"
        if [ -n "$sqd" ] && [ -f "$sqd/SQ/REPORT" ]; then
            if lay="$(python3 "$HERE/sq101m-layout.py" "$sqd/SQ/REPORT")"; then report "ccvs/sq101m-layout" 0 "$(echo "$lay" | tail -1)"
            else report "ccvs/sq101m-layout" 1 "$(echo "$lay" | tail -1): $(echo "$lay" | head -1)"; fi
        else report "ccvs/sq101m-layout" 1 "SQ101M wrote no REPORT"; fi
        [ -n "$sqd" ] && rm -rf "$sqd"
    else
        CCVS_NOTE="cobol: SQ101M layout NOT CHECKED -- no python3"
    fi
else
    CCVS_NOTE="cobol: CCVS-85 NOT RUN -- no tree at $CCVS_TREE (set CCVS85)"
fi

# Gate 5b (NIST): the NIST SQL Test Suite's embedded COBOL (docs/esql.md),
# totals equal to tests/nist-sql-baseline.txt, a ratchet as CCVS's is.
# The suite lives outside the tree (NISTSQL, default ~/refs/nist-sql); no
# tree is reported, and NISTSQL_RUN=0 switches the gate off.
NSQL_NOTE=""
NSQL_TREE=${NISTSQL:-$HOME/refs/nist-sql}
if [ "${NISTSQL_RUN:-1}" = 0 ]; then
    NSQL_NOTE="cobol: NIST SQL NOT RUN -- switched off by NISTSQL_RUN=0"
elif [ -d "$NSQL_TREE/pco" ] && command -v python3 >/dev/null 2>&1; then
    got="$(NISTSQL="$NSQL_TREE" "$HERE/nist-sql-run.sh" 2>/dev/null | tail -1)"
    want="$(cat "$HERE/nist-sql-baseline.txt")"
    if [ "$got" = "$want" ]; then
        report "nist-sql/totals" 0 "$(echo "$got" | sed 's/.*tests \([0-9]* pass, [0-9]* fail\).*/\1/'), as recorded"
    else
        report "nist-sql/totals" 1 "got [$got] want [$want]; if better, update tests/nist-sql-baseline.txt"
    fi
else
    NSQL_NOTE="cobol: NIST SQL NOT RUN -- no suite at $NSQL_TREE (set NISTSQL), or no python3"
fi

# Gate 7 (generated): fixed seeds, so a run is repeatable and a failure
# names its program; run-gen.sh keeps the work directory when anything
# disagrees.
GEN_NOTE=""
if [ "$ORACLE_ENGINE" = podman ] || [ "$ORACLE_ENGINE" = docker ]; then
    for g in arith:70 edit:60 cond:60 string:40 table:50 flow:25 pos:40; do
        gname=${g%%:*}; gn=${g##*:}
        gout="$(GEN=$gname "$HERE/gen/run-gen.sh" 1 40 "$gn" 2>&1 | tail -1)"
        case "$gout" in
            "all 40 agree") report "gen/$gname" 0 "40 programs, $gn statements each" ;;
            *)              report "gen/$gname" 1 "$gout" ;;
        esac
    done
else
    GEN_NOTE="cobol: GENERATED PROGRAMS NOT RUN -- they need the oracle's container image (tests/gen/README.md)"
fi

# ... and the generators that need no oracle, the same compiler both times
# (gen/run-flag.sh): the loops' items in registers (src/cobc/loopreg.h)
# and not; the values held outside loops alone (-fno-avail-reg), on other
# seeds; the items written the machine's way (src/cobc/native.h) and
# as their entries say; and the statements compiled through HIR as
# islands (src/cobc/lower.h) and by the text emitter, on the generator
# whose programs have the most such statements.  They need python3 to write the programs, which
# the slow32:cobol image does not carry: without it they are skipped and
# said to be, as the host-compiler gates are (a missing generator made an
# empty program and a bare FAIL, and cost the build fleet a round).
if command -v python3 >/dev/null 2>&1 || [ -n "${GENDIR:-}" ]; then      # GENDIR: the programs pre-generated (gen/run-flag.sh)
    fout="$(GEN=loop "$HERE/gen/run-flag.sh" -fno-loop-reg 1 60 2>&1 | tail -1)"
    case "$fout" in
        "all 60 the same"*) report "gen/loop" 0 "60 programs, with the registers and without" ;;
        *)                  report "gen/loop" 1 "$fout" ;;
    esac
    fout="$(GEN=loop "$HERE/gen/run-flag.sh" -fno-avail-reg 101 60 2>&1 | tail -1)"
    case "$fout" in
        "all 60 the same"*) report "gen/held" 0 "60 programs, with values held between statements and without" ;;
        *)                  report "gen/held" 1 "$fout" ;;
    esac
    fout="$(GEN=native "$HERE/gen/run-flag.sh" -fno-native-items 1 60 2>&1 | tail -1)"
    case "$fout" in
        "all 60 the same"*) report "gen/native" 0 "60 programs, with items written the machine's way and as written" ;;
        *)                  report "gen/native" 1 "$fout" ;;
    esac
    fout="$(GEN=loop "$HERE/gen/run-flag.sh" -fno-hir 201 60 2>&1 | tail -1)"
    case "$fout" in
        "all 60 the same"*) report "gen/hir" 0 "60 programs, with islands compiled through HIR and without" ;;
        *)                  report "gen/hir" 1 "$fout" ;;
    esac
    # national data against the text's code-unit model written out in the
    # generator itself (gen-national.py; no oracle has UTF-16 national data)
    if command -v python3 >/dev/null 2>&1 || [ -f "${GENDIR:-/nonexistent}/national-1.ref" ]; then
        fout="$(GEN=national "$HERE/gen/run-ref.sh" 1 40 40 2>&1 | tail -1)"
        case "$fout" in
            "40 of 40 agree"*) report "gen/national" 0 "40 programs against the code-unit model (${fout#40 of 40 agree, })" ;;
            *)                 report "gen/national" 1 "$fout" ;;
        esac
    else echo "SKIP  gen/national  (no python3, and GENDIR holds no national-N.ref)"; GEN_SKIPPED="${GEN_SKIPPED:-} gen/national"; fi
else
    for g in gen/loop gen/held gen/native gen/hir gen/national; do echo "SKIP  $g  (no python3 to write the programs)"; done
    GEN_SKIPPED=" gen/loop gen/held gen/native gen/hir gen/national"
fi

# Gate 8 (sanitizers): the compiler itself, built with the address and
# undefined-behavior sanitizers, over every source there is (sanitize.sh)
SAN_NOTE=""
sout="$("$HERE/sanitize.sh" 2>&1 | tail -1)"
case "$sout" in
    *"no findings")  report "sanitize" 0 "${sout#sanitize: }" ;;
    *"NOT RUN"*)     SAN_NOTE="cobol: $sout" ;;
    *)               report "sanitize" 1 "$sout" ;;
esac

echo
case "$ORACLE_ENGINE" in
    "")   if [ "${ORACLE:-1}" = 0 ]; then
              echo "cobol: NO ORACLE -- switched off by ORACLE=0; .expected files were checked against us alone"
          else
              echo "cobol: NO ORACLE -- neither a host cobc nor a gnucobol:$ORACLE_TAG-builder image; .expected files were checked against us alone"
          fi ;;
    host) echo "cobol: oracle is the host cobc" ;;
    *)    echo "cobol: oracle is gnucobol:$ORACLE_TAG-builder / $ORACLE_RUN_IMAGE under $ORACLE_ENGINE" ;;
esac
if [ -n "$GCOBOL_ENGINE" ]; then
    echo "cobol: second oracle $GCOBOL_IMAGE under $GCOBOL_ENGINE: $GC_AGREE agree, $GC_DIFF differ, $GC_REFUSED refused, $GC_SKIP skipped${GC_DIFF:+}"
    if [ -s "$W/gcobol-differs.txt" ]; then cp "$W/gcobol-differs.txt" "$CDIR/out/gcobol-differs.txt"; echo "cobol: gcobol's disagreements are in out/gcobol-differs.txt"; fi
elif [ "${GCOBOL:-1}" != 0 ]; then
    echo "cobol: no $GCOBOL_IMAGE image: the second oracle was not consulted"
fi
if [ "${ORACLE:-1}" = 0 ]; then
    echo "cobol: $PASS passed, $FAIL failed (ORACLE=0: expected output only, GnuCOBOL not consulted)"
else
    echo "cobol: $PASS passed, $FAIL failed"
fi
[ -z "$CCVS_NOTE" ] || echo "$CCVS_NOTE"
[ -z "$NSQL_NOTE" ] || echo "$NSQL_NOTE"
[ -z "$GEN_NOTE" ] || echo "$GEN_NOTE"
[ -z "$SAN_NOTE" ] || echo "$SAN_NOTE"
[ -z "$SKIPPED" ] || echo "cobol: SKIPPED:$SKIPPED -- no C compiler for them here; this is not a full run"
[ -z "${GEN_SKIPPED:-}" ] || echo "cobol: SKIPPED:$GEN_SKIPPED -- no python3 for them here; this is not a full run"
[ "$FAIL" = "0" ]
