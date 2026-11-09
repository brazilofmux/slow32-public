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
    for f in "$HERE"/f77/*.f "$HERE"/f77/*.F "$HERE"/f77/*.f90 "$HERE"/f77/*.F90; do
        [ -e "$f" ] || continue
        b="$(basename "$f")"; x="${b##*.}"; b="${b%.*}"
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
            cp "$f" "$OW/$b.$x"
            "$HERE/oracle.sh" "$OW/$b.$x" < "$IN" > "$W/$b.want" 2>/dev/null; wrc=$?
        fi
        # our compiler -> .s -> .s32o -> .s32x -> emulator
        # from $W: a module's .mod file lands in the current directory
        if ! (cd "$W" && "$F77" "$f" "$W/$b.s") >"$W/$b.cc.log" 2>&1; then
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

# --- Gate: programs of several files ------------------------------------
# tests/multi/<name>/ holds the files of one program, compiled one at a
# time and linked, and <name>/expected what gfortran prints for them
# (recorded with gfortran on all the files at once).  The differential
# gate above builds every program from one file, so it never saw what
# only crosses files: COMMON blocks were a local label per file, two
# files naming /INFOC/ had two blocks (found by LAPACK's testing XERBLA),
# and a module reaches another file only through its .mod.
if [ -x "$F77" ]; then
    for d in "$HERE"/multi/*/; do
        [ -d "$d" ] || continue
        b="$(basename "$d")"
        mw="$W/multi-$b"
        mkdir -p "$mw"
        objs=()
        fail=""
        for f in "$d"*.f "$d"*.f90; do
            [ -e "$f" ] || continue
            o="$mw/$(basename "$f").s32o"
            if ! (cd "$mw" && "$F77" "$f" "$o.s") > "$mw/cc.log" 2>&1; then fail="f77 compile $(basename "$f")"; break; fi
            "$AS" "$o.s" "$o" > /dev/null 2>&1 || { fail="assemble"; break; }
            objs+=("$o")
        done
        if [ -z "$fail" ] && ! "$LD" -o "$mw/p.s32x" --mmio 64K "$RT/crt0.s32o" "${objs[@]}" "$LIBF77" \
                  "$RT/libc_mmio.s32a" "$RT/libs32.s32a" > "$mw/ld.log" 2>&1; then fail="link"; fi
        if [ -n "$fail" ]; then report "multi:$b" 1 "$fail"; continue; fi
        (cd "$mw" && "$EMU" "$mw/p.s32x" < /dev/null 2>/dev/null) \
            | grep -vE "^Starting execution|^HALT at|^$|^Program halted|^Instructions|^Cycles|^Wall|^Performance|^MMIO|^Exit code" \
            > "$mw/got"
        if diff -q "$d/expected" "$mw/got" > /dev/null 2>&1; then report "multi:$b" 0
        else report "multi:$b" 1 "output differs from expected"; diff "$d/expected" "$mw/got" | head -6; fi
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
# A COMPLEX*16 FUNCTION referenced where its name is implicitly REAL is
# the mismatch it is (lenfn); COMPLEX*16 itself was refused until
# 2026-11-08.  Other lengths are read as fort370 reads them.
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
# a constant format (fmtcon), an Iw.m with m past w (fmtiwm: I10.20 printed
# asterisks).  The runtime used to read each some other way.  A list after
# PRINT f or READ f needs its comma (prcomma, rdcomma): blanks do not count,
# so PRINT * 1234 was PRINT*1234 and printed 1234 (from fort370, MV410).
# Construct names (Fortran 90; cn*): each names one construct and no
# variable; END DO, END IF, ELSE and ELSE IF carry the construct's name or
# none, as it has one or not; EXIT and CYCLE name a DO around them (EXIT
# from an IF is Fortran 2008's).  Two ELSEs, ELSE IF after ELSE, and text
# after END IF were taken silently before (else2, elseelif, endifjunk).
if [ -x "$F77" ]; then
    printf '      SUBROUTINE PRINTIT\n      RETURN\n      PROGRAM HELLO\n      CALL PRINTIT\n      END\n' > "$W/noend.f"
    printf '      PROGRAM A\n      END\n      PROGRAM B\n      END\n' > "$W/twomain.f"
    printf '      GO TO 99\n      END\n' > "$W/nolabel.f"
    printf '      DIMENSION A(3) B(2)\n      END\n' > "$W/nocomma.f"
    printf '      X = 1.0\n      CALL FOO(X) Z\n      END\n      SUBROUTINE FOO(Y)\n      Y = 2.0\n      END\n' > "$W/callline.f"
    printf '      X = G(1.0)\n      END\n      COMPLEX*16 FUNCTION G(Y)\n      G = Y\n      END\n' > "$W/lenfn.f"
    printf '      K = KF(3)\n      END\n      INTEGER FUNCTION KF(N)\n      KF = N * KF(N - 1)\n      END\n' > "$W/recfn.f"
    printf '      INTEGER L(4), X\n      X = L\n      END\n' > "$W/wholearr.f"
    printf '      REAL, ALLOCATABLE :: A(:)\n      REAL B\n      EQUIVALENCE (A, B)\n      END\n' > "$W/alloceq.f"
    printf '      SUBROUTINE S(A)\n      REAL, ALLOCATABLE :: A(:)\n      END\n' > "$W/allocarg.f"
    printf '      REAL, ALLOCATABLE :: A(10)\n      END\n' > "$W/allocshape.f"
    printf '      INTEGER K(3)\n      K(1:3) = (/ 1, 2 /)\n      END\n' > "$W/aconshape.f"
    printf '      INTEGER K(2)\n      K = 2 * (/ 1, 2 /)\n      END\n' > "$W/aconexpr.f"
    printf '      INTEGER K(2), J\n      J = TRANSFER(1.0, 0)\n      END\n' > "$W/xferexpr.f"
    printf '#define N 3\n      X = N\n      END\n' > "$W/cppdef.F"
    printf '#if 1\n      X = 1\n      END\n' > "$W/cppopen.F"
    printf 'x = 1 & y = 2\nend\n' > "$W/freeamp.f90"
    printf "x = 1\nprint *, 'open\nend\n" > "$W/freestr.f90"
    printf 'real(16) :: q\nend\n' > "$W/kindq.f90"
    printf 'integer(8) :: i8\nend\n' > "$W/kindi8.f90"
    printf 'x = 1.0_k\nend\n' > "$W/kindlit.f90"
    printf 'y = 2.0_16\nend\n' > "$W/kindlit16.f90"
    printf 'module mv\nreal :: x\nend module mv\n' > "$W/modvar.f90"
    printf 'module mi\ninterface\nsubroutine s(x)\nend subroutine s\nend interface\nend module mi\n' > "$W/modibody.f90"
    printf 'use nosuch\nend\n' > "$W/usenone.f90"
    printf 'use, intrinsic :: iso_fortran_env\nend\n' > "$W/useintr.f90"
    printf 'x = 1.0\ncall s\ncontains\nsubroutine s\ny = x\nend subroutine s\nend\n' > "$W/hostvar.f90"
    printf 'module mg\ninterface g\nmodule procedure a\nend interface\ncontains\nsubroutine a(x)\nreal x\nend subroutine a\nend module mg\nuse mg\ncall g(1)\nend\n' > "$W/genmiss.f90"
    printf 'a: do i=1,2\nend do a\na: do i=1,2\nend do a\nend\n' > "$W/cndup.f90"
    printf 'integer a\na=1\na: do i=1,2\nend do a\nend\n' > "$W/cnvar.f90"
    printf 'a: do i=1,2\nend do a\na=1\nend\n' > "$W/cnvar2.f90"
    printf 'a: do 10 i=1,2\n10 continue\nend\n' > "$W/cnlab.f90"
    printf 'do i=1,2\nend do a\nend\n' > "$W/cnendx.f90"
    printf 'a: do i=1,2\nend do\nend\n' > "$W/cnendnone.f90"
    printf 'a: if (.true.) then\nelse b\nend if a\nend\n' > "$W/cnelse.f90"
    printf 'a: if (.true.) then\nelse if (.false.) then b\nend if a\nend\n' > "$W/cnelif.f90"
    printf 'a: if (.true.) then\nexit a\nend if a\nend\n' > "$W/cnexitif.f90"
    printf 'do i=1,2\na: if (.true.) then\ncycle a\nend if a\nend do\nend\n' > "$W/cncycif.f90"
    printf 'do i=1,2\nexit q\nend do\nend\n' > "$W/cnexitno.f90"
    printf 'a: if (.true.) i=1\nend\n' > "$W/cnlogif.f90"
    printf 'q: integer i\nend\n' > "$W/cndecl.f90"
    printf 'if (.true.) then\nelse\nelse\nend if\nend\n' > "$W/else2.f90"
    printf 'if (.true.) then\nelse\nelse if (.true.) then\nend if\nend\n' > "$W/elseelif.f90"
    printf 'if (.true.) then\nendif(3)\nend\n' > "$W/endifjunk.f90"
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
    printf '      WRITE (6, 10) 5\n   10 FORMAT (1X, I10.20)\n      END\n' > "$W/fmtiwm.f"
    printf '      PRINT * 1234\n      END\n' > "$W/prcomma.f"
    printf '      INTEGER I\n      READ * I\n      END\n' > "$W/rdcomma.f"
    printf "      READ ('(I4)') M\n      END\n" > "$W/intnofmt.f"
    printf '      CHARACTER*8 B\n      WRITE (B) 3.5\n      END\n' > "$W/intunf.f"
    printf "      CHARACTER*6 G\n      G = 'KLMNOP'\n      WRITE (6, *) G(0:4)\n      END\n" > "$W/sub0.f"
    printf "      CHARACTER*6 G\n      G(2:7) = 'R'\n      END\n" > "$W/sublen.f"
    printf "      CHARACTER*6 G\n      G = 'KLMNOP'\n      WRITE (6, *) G(4:3)\n      END\n" > "$W/subrev.f"
    printf "      INTEGER K\n      DATA K /Z'12G4'/\n      END\n" > "$W/bozdig.f"
    printf "      INTEGER K\n      DATA K /-O'17'/\n      END\n" > "$W/bozneg.f"
    printf "      COMPLEX C\n      DATA C /Z'3F800000'/\n      END\n" > "$W/bozcx.f"
    printf "      K = IAND(1, 2.0)\n      END\n" > "$W/bitty.f"
    printf "      K = IBITS(5, 1)\n      END\n" > "$W/bitnarg.f"
    printf "      K = 1\n      CALL MVBITS(K, 0, 1, K)\n      END\n" > "$W/mvbnarg.f"
    printf "      INCLUDE 'nosuch'\n      END\n" > "$W/incnone.f"
    printf "      INCLUDE 'incloop'\n      END\n" > "$W/incself.f"
    printf "C     the text includes itself\n      INCLUDE 'incloop'\n" > "$W/incloop.inc"
    printf "      X = 1.0\n      INCLUDE (INCBAD)\n      END\n" > "$W/incerr.f"
    printf "      Y = 2.0\n      DIMENSION A(3) B(2)\n" > "$W/incbad.inc"
    printf "      INCLUDE 'incok'\n      GO TO 99\n      END\n" > "$W/incafter.f"
    printf "      I = 1\n      J = 2\n      K = 3\n" > "$W/incok.inc"
    for c in "noend:line 3: PROGRAM statement inside a subprogram: the END before it is missing" \
             "twomain:line 3: more than one main program" \
             "nolabel:line 2: statement label 99 is referred to but never defined" \
             "nocomma:line 1: unexpected text after the statement" \
             "callline:line 2: unexpected text after the statement" \
             "lenfn:line 1: G is REAL here but COMPLEX*16 where it is defined (line 3)" \
             "recfn:line 4: a FUNCTION's name is its result here: a recursive reference needs RESULT, which is not supported yet" \
             "wholearr:line 2: a whole array in an expression is not supported yet" \
             "alloceq:line 4: an ALLOCATABLE array cannot be in an EQUIVALENCE" \
             "allocarg:line 2: an ALLOCATABLE dummy argument is not supported" \
             "allocshape:line 1: an ALLOCATABLE array's shape is (:), (:,:) ..., its bounds given by ALLOCATE" \
             "aconshape:line 2: the array constructor has 2 values for 3 elements" \
             "aconexpr:line 2: an array constructor is supported only as the right side of an array assignment" \
             "xferexpr:line 2: TRANSFER is supported only as an actual argument" \
             "cppdef:line 1: only the preprocessor's conditionals are supported (#if, #ifdef, #ifndef, #elif, #else, #endif)" \
             "cppopen:line 3: #if without #endif" \
             "freeamp:line 1: & in the middle of a line" \
             "freestr:line 2: a character constant runs off the line" \
             "kindq:line 1: kind 16 of this type is not supported" \
             "kindi8:line 1: kind 8 of this type is not supported" \
             "kindlit:line 1: the kind of a constant must be digits or an INTEGER named constant, not K" \
             "kindlit16:line 1: REAL kind 16 is not supported" \
             "modvar:line 3: X: a module variable is not supported yet" \
             "modibody:line 2: an INTERFACE block of interface bodies is not supported yet" \
             "usenone:line 1: no module NOSUCH: neither in this file before the USE, nor a .mod file of it here" \
             "useintr:line 1: USE, INTRINSIC and USE :: are Fortran 2003, not supported" \
             "hostvar:line 6: X: host association with a variable is not supported yet" \
             "genmiss:line 11: no procedure of the generic G takes these arguments" \
             "cndup:line 3: the construct name A is used twice" \
             "cnvar:line 3: A is a variable; it cannot name a construct" \
             "cnvar2:line 3: A is a construct name, not a variable" \
             "cnlab:line 2: the end of a named DO needs the construct name A" \
             "cnendx:line 2: END DO names A; the construct has no name" \
             "cnendnone:line 2: END DO needs the construct name A" \
             "cnelse:line 2: ELSE names B; the construct is A" \
             "cnelif:line 2: ELSE IF names B; the construct is A" \
             "cnexitif:line 2: EXIT names A, an IF construct, not a DO (EXIT from an IF is Fortran 2008)" \
             "cncycif:line 3: CYCLE names A, an IF construct, not a DO" \
             "cnexitno:line 2: EXIT names Q, which is no construct around it" \
             "cnlogif:line 1: a construct name is on a block IF, not a logical IF" \
             "cndecl:line 1: a construct name is on DO or IF (...) THEN only" \
             "else2:line 3: a second ELSE in one IF construct" \
             "elseelif:line 3: ELSE IF after ELSE" \
             "endifjunk:line 2: unexpected text after END IF" \
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
             "fmtcon:line 1: FORMAT: F needs a width" \
             "fmtiwm:line 2: FORMAT: Iw.m needs m no more than w" \
             "prcomma:line 1: a comma after the format, before the list" \
             "rdcomma:line 2: a comma after the format, before the list" \
             "intnofmt:line 1: a CHARACTER unit is an internal file and needs a format" \
             "intunf:line 2: a CHARACTER unit is an internal file and needs a format" \
             "sub0:line 3: substring outside its string" \
             "sublen:line 2: substring outside its string" \
             "subrev:line 3: substring outside its string" \
             "bozdig:line 2: bad hexadecimal constant" \
             "bozneg:line 2: a sign cannot precede a hexadecimal or octal constant" \
             "bozcx:a hexadecimal or octal constant cannot initialize a COMPLEX item" \
             "bitty:line 1: this intrinsic needs INTEGER arguments" \
             "bitnarg:line 1: wrong number of arguments to a bit intrinsic" \
             "mvbnarg:line 2: MVBITS needs five arguments" \
             "incnone:f77: line 1: INCLUDE: no file nosuch" \
             "incself:f77: incloop line 2: INCLUDE of a text being read: incloop" \
             "incerr:f77: INCBAD line 2: unexpected text after the statement" \
             "incafter:f77: line 3: statement label 99 is referred to but never defined"; do
        b="${c%%:*}"; msg="${c#*:}"
        f="$W/$b.f"; for e in F f90; do [ -f "$W/$b.$e" ] && f="$W/$b.$e"; done
        if (cd "$W" && "$F77" "$f" "$W/$b.s") >"$W/$b.err" 2>&1; then
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

# --- Gate: programs the runtime must stop ----------------------------
# tests/stop/<name>.f must print exactly <name>.expected, then stop with
# exit status 2 and <name>.msg on stderr (stdin: <name>.in, if any).  Not
# in f77/: gfortran is no oracle for these -- iorecur.f, I/O inside I/O,
# is allowed by F2003 for an internal unit and gfortran does it; F77
# forbids it (fort370 issue 39).
if [ -x "$F77" ]; then
    for f in "$HERE"/stop/*.f; do
        [ -e "$f" ] || continue
        b="$(basename "$f" .f)"
        IN="/dev/null"
        [ -f "$HERE/stop/$b.in" ] && IN="$HERE/stop/$b.in"
        if ! "$F77" "$f" "$W/stop-$b.s" >"$W/stop-$b.cc.log" 2>&1 ||
           ! "$AS" "$W/stop-$b.s" "$W/stop-$b.s32o" >/dev/null 2>&1 ||
           ! "$LD" -o "$W/stop-$b.s32x" --mmio 64K "$RT/crt0.s32o" "$W/stop-$b.s32o" "$LIBF77" \
                 "$RT/libc_mmio.s32a" "$RT/libs32.s32a" >/dev/null 2>&1; then
            report "stop:$b" 1 "build"; continue
        fi
        (cd "$W" && "$EMU" "$W/stop-$b.s32x" < "$IN" 2>"$W/stop-$b.err") \
            | grep -vE "^Starting execution|^HALT at|^$|^Program halted|^Instructions|^Cycles|^Wall|^Performance|^MMIO|^Exit code" \
            > "$W/stop-$b.got"
        (cd "$W" && "$EMU" "$W/stop-$b.s32x" < "$IN" >/dev/null 2>&1); src=$?
        if [ "$src" != 2 ]; then
            report "stop:$b" 1 "exit $src, not 2"
        elif ! diff -q "$HERE/stop/$b.expected" "$W/stop-$b.got" >/dev/null 2>&1; then
            report "stop:$b" 1 "output differs from $b.expected"
            diff "$HERE/stop/$b.expected" "$W/stop-$b.got" | head -6
        elif ! grep -qF "$(cat "$HERE/stop/$b.msg")" "$W/stop-$b.err"; then
            report "stop:$b" 1 "said: $(head -1 "$W/stop-$b.err")"
        else
            report "stop:$b" 0
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
