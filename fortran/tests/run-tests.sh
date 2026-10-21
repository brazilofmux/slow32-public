#!/bin/bash
# fortran/ test harness.
#
# Gate 1 (backend-slice): drives the COPIED SLOW-32 backend with
# hand-built HIR and no Fortran frontend, then runs the result on the
# emulator.  This is the load-bearing test for the copy: if a re-sync
# from selfhost/ breaks the frontend contract in f77_contract.h, this
# fails immediately and specifically, rather than surfacing later as a
# mysterious miscompile in Fortran code.
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
FDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$FDIR/.." && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
# Tree paths by default; the S32_* knobs compile.sh honours point the suite
# at an installed copy (the slow32:fortran image: /opt/slow32).
AS="${S32_AS:-$ROOT/tools/assembler/slow32asm}"
LD="${S32_LD:-$ROOT/tools/linker/s32-ld}"
RT="${S32_RT:-$ROOT/runtime}"
HOSTCC="${HOSTCC:-gcc}"
EMU="${EMU:-$ROOT/tools/emulator/slow32}"
SKIPPED=""

W="$(mktemp -d /tmp/f77-tests.XXXXXX)"
# The oracle runs on a COPY in a $HOME-based scratch dir: podman's
# macOS VM does not share /tmp, and a test that OPENs files must never
# scribble them into tests/f77/ (the mounted directory).
OW="$(mktemp -d "$HOME/.f77-oracle.XXXXXX")"
trap 'rm -rf "$W" "$OW"' EXIT

PASS=0; FAIL=0

report() {
    if [ "$2" = "0" ]; then printf "  %-24s PASS\n" "$1:"; PASS=$((PASS+1))
    else printf "  %-24s FAIL%s\n" "$1:" "${3:+ ($3)}"; FAIL=$((FAIL+1)); fi
}

# --- Gate 1: backend slice -------------------------------------------
# Gates 1 and 2 build host programs, and gate 1 also needs clang for its
# driver.  Without them (the slow32:fortran image has neither) they are
# SKIPPED by name, and the summary says so.
if ! command -v "$HOSTCC" >/dev/null 2>&1 || [ ! -x "$LLVM_BIN/clang" ]; then
    printf "  %-24s SKIP (no host C compiler / clang)\n" "backend-slice:"
    SKIPPED="$SKIPPED backend-slice"
elif ! "$HOSTCC" -I"$FDIR/src" -O1 -w -o "$W/slice" "$HERE/backend_slice.c" 2>"$W/cc.log"; then
    report "backend-slice" 1 "host build"
else
    "$W/slice" "$W/slice.s" 2>/dev/null
    "$AS" "$W/slice.s" "$W/slice.s32o" >/dev/null 2>&1
    "$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm -O1 \
        -I"$RT/include" "$HERE/backend_slice_drv.c" -o "$W/drv.ll" 2>/dev/null
    "$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$W/drv.ll" -o "$W/drv.s" 2>/dev/null
    "$AS" "$W/drv.s" "$W/drv.s32o" >/dev/null 2>&1
    "$LD" -o "$W/slice.s32x" "$RT/crt0.s32o" "$W/drv.s32o" "$W/slice.s32o" \
        "$RT/libc_debug.s32a" "$RT/libs32.s32a" >/dev/null 2>&1
    "$EMU" "$W/slice.s32x" 2>/dev/null \
        | grep -vE "^Starting execution|^HALT at|^$|^Program halted|^Instructions|^Cycles|^Wall|^Performance|^MMIO" \
        > "$W/slice.out"
    if diff -q "$W/slice.out" "$HERE/backend_slice.expected" >/dev/null 2>&1; then
        report "backend-slice" 0
    else
        report "backend-slice" 1 "output mismatch"
        diff "$HERE/backend_slice.expected" "$W/slice.out" | head -10
    fi
fi

# --- Gate 2: card image + tokenizer ----------------------------------
if ! command -v "$HOSTCC" >/dev/null 2>&1; then
    printf "  %-24s SKIP (no host C compiler)\n" "lex-torture:"
    SKIPPED="$SKIPPED lex-torture"
elif ! "$HOSTCC" -I"$FDIR/src" -O1 -w -o "$W/lexdump" "$HERE/lexdump.c" 2>"$W/lex.log"; then
    report "lex-torture" 1 "host build"
else
    "$W/lexdump" "$HERE/torture.f" > "$W/torture.out" 2>&1
    if diff -q "$W/torture.out" "$HERE/torture.expected" >/dev/null 2>&1; then
        report "lex-torture" 0
    else
        report "lex-torture" 1 "token stream mismatch"
        diff "$HERE/torture.expected" "$W/torture.out" | head -12
    fi
fi

# --- Gate 3: differential vs the gfortran oracle ----------------------
# Each tests/f77/*.f is run under gfortran-in-a-container (expected) and,
# once fortran/out/f77 exists, under our compiler on the emulator
# (actual).  stdout and exit status must both match.  gfortran sends the
# `STOP n` message to stderr and the code to the exit status, so stdout
# stays clean for diffing.
F77="${S32_F77:-$FDIR/out/f77}"
LIBF77="${S32_LIBF77:-$FDIR/runtime/libf77.s32o}"
# Without the oracle (no podman, or no slow32:fortran-oracle image -- the
# case inside the slow32:fortran image) the programs still run: each is
# self-checking and ends in STOP 0, so "compiles, runs, exits 0" is a real
# gate, just not the differential.  Reported as such, never as the full one.
ORACLE=1
if ! "$HERE/oracle.sh" --check >/dev/null 2>&1 && \
   ! podman image exists slow32:fortran-oracle 2>/dev/null; then
    ORACLE=0
fi
if [ ! -x "$F77" ]; then
    printf "  %-24s SKIP (no compiler yet -- milestone 3)\n" "differential:"
else
    if [ "$ORACLE" = 0 ]; then
        printf "  %-24s SELF-CHECK ONLY (oracle image absent: exit 0 required, output not diffed)\n" "differential:"
        SKIPPED="$SKIPPED oracle-diff"
    fi
    for f in "$HERE"/f77/*.f; do
        [ -e "$f" ] || continue
        b="$(basename "$f" .f)"
        # A sibling <name>.in is the program's stdin (for READ tests),
        # fed identically to the oracle and to the emulator.  /dev/null
        # otherwise, so a runaway READ cannot hang on the terminal.
        IN="/dev/null"
        [ -f "$HERE/f77/$b.in" ] && IN="$HERE/f77/$b.in"
        # A sibling <name>.expected pins the program to that output instead
        # of gfortran's, where slow-32 differs from gfortran on purpose; the
        # .f says why.  It must end in STOP 0 like any other.
        PINNED=0
        if [ -f "$HERE/f77/$b.expected" ]; then
            PINNED=1
            cp "$HERE/f77/$b.expected" "$W/$b.want"; wrc=0
        elif [ "$ORACLE" = 1 ]; then
            cp "$f" "$OW/$b.f"
            "$HERE/oracle.sh" "$OW/$b.f" < "$IN" > "$W/$b.want" 2>/dev/null; wrc=$?
        fi
        # our compiler -> .s -> .s32o -> .s32x -> emulator
        if ! "$F77" "$f" "$W/$b.s" >"$W/$b.cc.log" 2>&1; then
            report "diff:$b" 1 "f77 compile"; continue
        fi
        "$AS" "$W/$b.s" "$W/$b.s32o" >/dev/null 2>&1 || { report "diff:$b" 1 "assemble"; continue; }
        # --mmio + libc_mmio is what propagates the guest exit status out
        # of the emulator, which is how STOP n is checked.
        "$LD" -o "$W/$b.s32x" --mmio 64K "$RT/crt0.s32o" "$W/$b.s32o" \
              "$LIBF77" \
              "$RT/libc_mmio.s32a" "$RT/libs32.s32a" \
              >/dev/null 2>&1 || { report "diff:$b" 1 "link"; continue; }
        # cwd inside $W so files a test OPENs land in scratch space.
        (cd "$W" && "$EMU" "$W/$b.s32x" < "$IN" 2>/dev/null) \
            | grep -vE "^Starting execution|^HALT at|^$|^Program halted|^Instructions|^Cycles|^Wall|^Performance|^MMIO|^Exit code" \
            > "$W/$b.got"
        (cd "$W" && "$EMU" "$W/$b.s32x" < "$IN" >/dev/null 2>&1); grc=$?
        # Agreement is necessary but NOT sufficient.  Every program in
        # f77/ is self-checking and ends in STOP 0, so a non-zero exit
        # means an assertion tripped -- and if the oracle trips the same
        # one, a plain diff calls that a PASS while the test has in fact
        # stopped early and never exercised what it was written for.
        # That happened: slice6 asserted FLAT(2)==21 against a fill of
        # I+10*J, stopped at its second check on BOTH compilers, and
        # reported PASS for several commits.  So require exit 0 too.
        if [ "$PINNED" = 1 ]; then
            if ! diff -q "$W/$b.want" "$W/$b.got" >/dev/null 2>&1 || [ "$grc" != "0" ]; then
                report "pin:$b" 1 "output differs from $b.expected, or STOP $grc"
                diff "$W/$b.want" "$W/$b.got" | head -8
            else
                report "pin:$b" 0
            fi
        elif [ "$ORACLE" = 0 ]; then
            if [ "$grc" = "0" ]; then report "self:$b" 0; else report "self:$b" 1 "STOP $grc -- an assertion tripped"; fi
        elif ! diff -q "$W/$b.want" "$W/$b.got" >/dev/null 2>&1 || [ "$wrc" != "$grc" ]; then
            report "diff:$b" 1 "output/exit differs (ours=$grc oracle=$wrc)"
            diff "$W/$b.want" "$W/$b.got" | head -8
        elif [ "$grc" != "0" ]; then
            report "diff:$b" 1 "both stopped early at STOP $grc -- test never completed"
        else
            report "diff:$b" 0
        fi
    done
fi

# --- Gate: program units the compiler must refuse ----------------------
# How the compiler divides a source into units is where a mistake loses
# a unit without a word.  A PROGRAM statement inside a subprogram (its
# END missing), a second main program, a label referred to but never
# defined, and text a statement leaves unread (DIMENSION A(3) B(2):
# the guard that would have caught INTEGER N/9000/ being skipped) are
# refused, each with its message; f77/mainlast.f has a main program after its subprograms.
# The message names the statement's own line even when the statement
# called a subprogram whose text was read in its place (callline).
# Found by fort370 (reported on H390-MVS), whose unit scan is this one's.
# COMPLEX*16 is refused rather than compiled as single precision, which
# it was without a word -- in a declaration and in a FUNCTION header,
# said once (lenfn).  Other lengths are read as fort370 reads them.
# END DO (MIL-STD-1753, from fort370): a DO left waiting for its END
# DO, an IF block without END IF, and an END DO with no DO open.  A
# labelled DO that its label never closes -- no such statement (nodo),
# or the label inside another DO's range (crossdo) -- compiled silently.
# Two subprograms of one name, a FUNCTION and a SUBROUTINE of one name,
# and an ENTRY with a subprogram's name compiled silently too, and a
# CALL reached whichever came first (from fort370).
# A function whose type in its caller differs from its definition's --
# implicitly REAL there, INTEGER FUNCTION here (fntype), or INTEGER
# against LOGICAL (fnlog) -- compiled silently; gfortran refuses both.
# A format the compiler can see, a FORMAT statement or a character
# constant, is checked (from fort370): an nH running past the end
# (fmth), a count with no descriptor (fmtcnt), a missing width (fmtwid),
# a letter that is no descriptor (fmtlet), unpaired parentheses (fmtpar),
# a constant format (fmtcon).  The runtime used to read each some other way.
if [ -x "$F77" ]; then
    printf '      SUBROUTINE PRINTIT\n      RETURN\n      PROGRAM HELLO\n      CALL PRINTIT\n      END\n' > "$W/noend.f"
    printf '      PROGRAM A\n      END\n      PROGRAM B\n      END\n' > "$W/twomain.f"
    printf '      GO TO 99\n      END\n' > "$W/nolabel.f"
    printf '      DIMENSION A(3) B(2)\n      END\n' > "$W/nocomma.f"
    printf '      X = 1.0\n      CALL FOO(X) Z\n      END\n      SUBROUTINE FOO(Y)\n      Y = 2.0\n      END\n' > "$W/callline.f"
    printf '      COMPLEX*16 Z\n      Z = (1.0, 2.0)\n      END\n' > "$W/lencx.f"
    printf '      X = G(1.0)\n      END\n      COMPLEX*16 FUNCTION G(Y)\n      G = Y\n      END\n' > "$W/lenfn.f"
    printf '      DO I = 1, 3\n      X = I\n      END\n' > "$W/opendo.f"
    printf '      IF (X .GT. 0.0) THEN\n      X = 1.0\n      END\n' > "$W/openif.f"
    printf '      X = 1.0\n      END DO\n      END\n' > "$W/straydo.f"
    printf '      DO 10 I = 1, 3\n      X = I\n      END\n' > "$W/nodo.f"
    printf '      DO 10 I = 1, 3\n      DO 20 J = 1, 2\n   10 X = I\n   20 CONTINUE\n      END\n' > "$W/crossdo.f"
    printf '      CALL PING\n      END\n      SUBROUTINE PING\n      END\n      SUBROUTINE PING\n      END\n' > "$W/twosub.f"
    printf '      X = TICK(1.0)\n      END\n      FUNCTION TICK(Y)\n      TICK = Y\n      END\n      SUBROUTINE TICK\n      END\n' > "$W/fnsub.f"
    printf '      CALL PONG\n      END\n      SUBROUTINE OUTER\n      ENTRY PONG\n      RETURN\n      END\n      SUBROUTINE PONG\n      END\n' > "$W/entsub.f"
    printf '      X = FTWO(1)\n      END\n      INTEGER FUNCTION FTWO(N)\n      FTWO = N + 1\n      END\n' > "$W/fntype.f"
    printf '      INTEGER LOK\n      I = LOK(1)\n      END\n      LOGICAL FUNCTION LOK(N)\n      LOK = N .GT. 0\n      END\n' > "$W/fnlog.f"
    printf '      WRITE (6, 10) 1\n   10 FORMAT (1X, 9HABC, I3)\n      END\n' > "$W/fmth.f"
    printf '      WRITE (6, 10) 1\n   10 FORMAT (1X, 4HWXYZ12, I3)\n      END\n' > "$W/fmtcnt.f"
    printf '      WRITE (6, 10) 1\n   10 FORMAT (1X, I)\n      END\n' > "$W/fmtwid.f"
    printf '      WRITE (6, 10) 1\n   10 FORMAT (1X, Q5)\n      END\n' > "$W/fmtlet.f"
    printf '      WRITE (6, 10) 1\n   10 FORMAT ((1X, I5)\n      END\n' > "$W/fmtpar.f"
    printf "      WRITE (6, '(1X, F)') 2.5\n      END\n" > "$W/fmtcon.f"
    for c in "noend:line 3: PROGRAM statement inside a subprogram: the END before it is missing" \
             "twomain:line 3: more than one main program" \
             "nolabel:line 2: statement label 99 is referred to but never defined" \
             "nocomma:line 1: unexpected text after the statement" \
             "callline:line 2: unexpected text after the statement" \
             "lencx:line 1: COMPLEX*16 needs a target with native DOUBLE PRECISION" \
             "lenfn:line 3: COMPLEX*16 needs a target with native DOUBLE PRECISION" \
             "opendo:DO without END DO" \
             "openif:IF block without END IF" \
             "straydo:line 2: END DO without DO" \
             "nodo:DO 10 is never closed by a statement labelled 10" \
             "crossdo:DO 10 is never closed by a statement labelled 10" \
             "twosub:line 5: PING is already a subprogram or ENTRY name (line 3)" \
             "fnsub:line 6: TICK is already a subprogram or ENTRY name (line 3)" \
             "entsub:line 7: PONG is already a subprogram or ENTRY name (line 4)" \
             "fntype:line 1: FTWO is REAL here but INTEGER where it is defined (line 3)" \
             "fnlog:line 2: LOK is INTEGER here but LOGICAL where it is defined (line 4)" \
             "fmth:line 2: FORMAT: the format ends without its )" \
             "fmtcnt:line 2: FORMAT: a count with no descriptor after it" \
             "fmtwid:line 2: FORMAT: I needs a width" \
             "fmtlet:line 2: FORMAT: Q is no edit descriptor" \
             "fmtpar:line 2: FORMAT: the format ends without its )" \
             "fmtcon:line 1: FORMAT: F needs a width"; do
        b="${c%%:*}"; msg="${c#*:}"
        if "$F77" "$W/$b.f" "$W/$b.s" >"$W/$b.err" 2>&1; then
            report "refuse:$b" 1 "compiled"
        elif ! grep -qF "$msg" "$W/$b.err"; then
            report "refuse:$b" 1 "said: $(head -1 "$W/$b.err")"
        elif [ "$(grep -cF "$msg" "$W/$b.err")" != 1 ]; then
            report "refuse:$b" 1 "said it $(grep -cF "$msg" "$W/$b.err") times"
        else
            report "refuse:$b" 0
        fi
    done
fi

# --- Report: math libcalls emitted -----------------------------------
# INFORMATIONAL, not a failure.  fortran/ lives in the tree's ordinary
# universe, not selfhost's closed one: the compiler may use the host to
# target SLOW-32, and the emulators may use their environment to run
# SLOW-32 code.  slow32-dbt linking host libm and intercepting ~37 math
# symbols is therefore sanctioned, not a cheat -- sbasic.s32x carries
# sqrt/atan2/floor and runs them on the host under the DBT by design.
#
# What this reports is a code-quality fact worth watching: the libm
# calls a program's OWN code makes.  Arithmetic and SQRT stay hardware
# instructions; EXP, LOG, ATAN2, the trig functions and real-exponent **
# emit libm names so the DBT can intercept them (trans1.f, sumsq.f).
# The LINPACK kernel has none.  Printed rather than enforced.
#
# Read from the program's assembly, not its linked .s32x: libf77 is one
# object that calls libm itself (complex arithmetic, the intrinsic
# wrappers), and one math symbol links the whole math_soft member, so
# every linked program carries every name (docs/plans/fortran77.md) --
# a report from the .s32x listed all ~40 names for all ~75 programs and
# said nothing about any of them.
INTERCEPTABLE="sqrt sqrtf sin cos tan asin acos atan atan2 atan2f sinh cosh tanh exp log log10 pow powf ceil floor round trunc fabs fmod fmodf sinf cosf tanf asinf acosf atanf sinhf coshf tanhf expf logf log10f ceilf floorf roundf truncf fabsf"
leaked=""
for f in "$HERE"/f77/*.f; do
    [ -e "$f" ] || continue
    b="$(basename "$f" .f)"
    "$F77" "$f" "$W/$b.lm.s" >/dev/null 2>&1 || continue
    for n in $INTERCEPTABLE; do
        # a call, or a tail call: jal rN, name
        if grep -Eq "^[[:space:]]*jal r[0-9]+, $n\$" "$W/$b.lm.s"; then
            leaked="$leaked $b:$n"
        fi
    done
done
if [ -z "$leaked" ]; then
    printf "  %-24s none (all FP is hardware instructions)\n" "math-libcalls:"
else
    printf "  %-24s%s\n" "math-libcalls:" "$leaked"
fi

echo
echo "$PASS passed, $FAIL failed"
[ -z "$SKIPPED" ] || echo "fortran: SKIPPED:$SKIPPED -- no host compiler / oracle for them here; this is not a full run"
[ "$FAIL" -eq 0 ]
