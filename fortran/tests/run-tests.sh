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
# The reductions (Fortran 90) of an array expression: DIM= that would give
# an array in a scalar's place, an argument that is no array or of the
# wrong type, a keyword the intrinsic does not take, a DIM outside the rank.
# Assumed-shape dummies (Fortran 90; as*): an explicit interface, an array
# actual of the dummy's rank, and only for a dummy; an assumed-size
# actual is refused.
# A whole CHARACTER array was its first element wherever it was not an
# actual argument or I/O's: D = C assigned C(1) to D(1) and stopped
# (charwhole; CHARACTER array assignment since, a scalar's place still
# refused).  TRIM's argument is a scalar (it was trimmed elementwise);
# a constructor's values have one length; the shapes in an array
# expression conform where gfortran can tell they do not (shp*: N =
# N(1:2) compiled, reading past the section).  Array-valued functions
# (afn*): not in a scalar's place, with an explicit interface, of a known
# CHARACTER length, without ENTRY, not through a generic yet.
# The array-valued intrinsics' limits: a SHAPE of no constant size and an
# ORDER of the wrong size, MATMUL's and TRANSPOSE's ranks, SPREAD's DIM,
# MERGE's sources of two types, an array SHIFT of the wrong rank or
# shape, a BOUNDARY of another length; a constructor flattening its
# arrays to a count the target's shape does not have (aconshp).
# WHERE (Fortran 90): one inside another and the masked ELSEWHERE are
# Fortran 95's; its body is array assignments; its mask is a LOGICAL array
# of the assignments' rank (where*).
# Construct names (Fortran 90; cn*): each names one construct and no
# variable; END DO and END IF carry the construct's name or none, as it
# has one or not, ELSE and ELSE IF the construct's or none; EXIT and CYCLE name a DO around them (EXIT
# from an IF is Fortran 2008's).  Two ELSEs, ELSE IF after ELSE, and text
# after END IF were taken silently before (else2, elseelif, endifjunk).
# SELECT CASE (Fortran 90; sel*): no value in two CASEs, values constants
# of the selector's type, one CASE DEFAULT, only CASEs right after the
# SELECT, no LOGICAL range, the construct name on END SELECT, an INTEGER,
# LOGICAL or CHARACTER selector; EXIT from it is Fortran 2008's.
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
    printf '#define N 3\n      X = N\n      END\n' > "$W/cppdef.F"
    printf '#if 1\n      X = 1\n      END\n' > "$W/cppopen.F"
    printf 'x = 1 & y = 2\nend\n' > "$W/freeamp.f90"
    printf "x = 1\nprint *, 'open\nend\n" > "$W/freestr.f90"
    printf 'real(16) :: q\nend\n' > "$W/kindq.f90"
    printf 'integer(8) :: i8\nend\n' > "$W/kindi8.f90"
    printf 'x = 1.0_k\nend\n' > "$W/kindlit.f90"
    printf 'y = 2.0_16\nend\n' > "$W/kindlit16.f90"
    printf 'program p\ninterface\nsubroutine s(x)\nx = 1.0\nend subroutine\nend interface\nend\n' > "$W/ibexec.f90"
    printf 'program p\ninterface\nreal function f(x)\nend function\nend interface\nreal :: f\nend\n' > "$W/ibredecl.f90"
    printf 'use nosuch\nend\n' > "$W/usenone.f90"
    printf 'use, intrinsic :: iso_fortran_env\nend\n' > "$W/useintr.f90"
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
    printf 'x = 1.0\nprint *, sum(x)\nend\n' > "$W/sumscal.f90"
    printf 'logical l(2)\nl = .true.\nprint *, sum(l)\nend\n' > "$W/sumlog.f90"
    printf 'integer k(2)\nk = 1\nprint *, count(k)\nend\n' > "$W/cntnum.f90"
    printf 'real a(2,2)\na = 1.0\nprint *, dot_product(a, a)\nend\n' > "$W/dotrank.f90"
    printf 'integer k(2)\nk = 1\nprint *, sum(k, foo=1)\nend\n' > "$W/sumkw.f90"
    printf 'integer k(2)\nk = 1\nprint *, size(k, 3)\nend\n' > "$W/sizedim.f90"
    printf 'real a(2), b(3)\na = 1.0\nb = 1.0\nwhere (a > 0.0)\nwhere (b > 1.0) b = 2.0\nend where\nend\n' > "$W/wherenest.f90"
    printf 'real a(2)\na = 1.0\nwhere (a > 0.0)\nif (a(1) > 0.0) a(2) = 1.0\nend where\nend\n' > "$W/whereif.f90"
    printf 'real a(2)\na = 1.0\nwhere (a > 0.0) x = 2.0\nend\n' > "$W/wherescal.f90"
    printf 'real a(2), b(2,2)\na = 1.0\nb = 1.0\nwhere (a > 0.0)\na = 2.0\nelsewhere (b < 0.0)\na = 3.0\nend where\nend\n' > "$W/wheremask.f90"
    printf 'real a(2)\na = 1.0\nwhere (a > 0.0)\na = 2.0\nelsewhere\na = 3.0\nelsewhere\na = 4.0\nend where\nend\n' > "$W/where2else.f90"
    printf 'program a\nend program b\n' > "$W/endname.f90"
    printf 'subroutine s\nend function s\n' > "$W/endkind.f90"
    printf 'i = 1\nend program p\n' > "$W/endnoname.f90"
    printf 'block data\ncommon /c/ i\nend block data q\n' > "$W/endbdname.f90"
    printf 'module m\ncontains\nsubroutine s\nend subroutine s\nend module n\n' > "$W/endmodname.f90"
    printf 'y = 2.0\nprint *, sqrt(x=y)\nend\n' > "$W/kwfirst.f90"
    printf 'print *, f(3)\ncontains\nfunction f(n)\ncharacter(len=n) :: f\nf = "abc"\nend function\nend\n' > "$W/reslenref.f90"
    printf 'module k\ninteger, parameter :: n = 6\nend module\nmodule m\ninteger, parameter :: n = 4\ncontains\ncharacter(len=n) function f(i)\nuse k\nf = "abcdefgh"\nend function\nend module\n' > "$W/reslenhost.f90"
    printf 'module k\ninteger, parameter :: wp = 8\nend module\nmodule m\ninteger, parameter :: wp = 4\ncontains\nreal(wp) function f(i)\nuse k\nf = i\nend function\nend module\n' > "$W/reskindhost.f90"
    printf 'character*(n) function f(i)\nf = "abc"\nend\n' > "$W/reslenunk.f90"
    printf 'real a(2)\na = 1.0\nend where\nend\n' > "$W/wherestray.f90"
    printf 'real a(2)\na = 1.0\nwhere (a > 0.0)\na = 2.0\nend\n' > "$W/whereopen.f90"
    printf 'real a(2)\nwhere (1.0 > 0.0) a = 2.0\nend\n' > "$W/wherenoarr.f90"
    printf 'real a(2), m(2,2)\na = 1.0\nm = 1.0\nwhere (m > 0.0) a = 2.0\nend\n' > "$W/whereshape.f90"
    printf 'real a(3)\na = 1.0\ncall s(a)\nend\nsubroutine s(x)\nreal x(:)\nend\n' > "$W/asext.f90"
    printf 'module m\ncontains\nsubroutine s(x)\nreal x(:)\nend subroutine\nend module\nuse m\ny = 1.0\ncall s(y)\nend\n' > "$W/asscal.f90"
    printf 'module m\ncontains\nsubroutine s(x)\nreal x(:,:)\nend subroutine\nend module\nuse m\nreal a(3)\ncall s(a)\nend\n' > "$W/asrank.f90"
    printf 'subroutine s\nreal x(:)\nend\n' > "$W/aslocal.f90"
    printf 'module m\ncontains\nsubroutine s(x)\nreal x(:)\nend subroutine\nend module\nsubroutine t(y)\nuse m\nreal y(*)\ncall s(y)\nend\n' > "$W/asasz.f90"
    printf "character*2 c(2), s\nc(1) = 'ab'\nc(2) = 'cd'\ns = c\nend\n" > "$W/charwhole.f90"
    printf "character*2 c(2)\nc = 'ab'\nc = trim(c)\nend\n" > "$W/trimarr.f90"
    printf "character*2 c(3)\nc = (/ 'a', 'bb', 'cc' /)\nend\n" > "$W/aconlen.f90"
    printf 'integer n(3)\nn = 1\nn = n(1:2)\nend\n' > "$W/shpasg.f90"
    printf 'integer n(3)\nn = 1\nprint *, sum(n(1:2) + n)\nend\n' > "$W/shpop.f90"
    printf 'integer n(3)\nn = 1\nwhere (n(1:2) > 0) n = 2\nend\n' > "$W/shpwhere.f90"
    printf 'program p\nx = f()\ncontains\nfunction f()\ninteger f(3)\nf = 1\nend function\nend\n' > "$W/afnscal.f90"
    printf 'integer k(3)\nk = f()\nend\nfunction f()\ninteger f(3)\nf = 1\nend\n' > "$W/afnext.f90"
    printf 'function f(n)\ncharacter(len=*) f(2)\nf = "a"\nend\n' > "$W/afnclen.f90"
    printf 'function f()\ninteger f(2)\nf = 1\nentry g()\nf = 2\nend\n' > "$W/afnent.f90"
    printf 'module m\ninterface g\nmodule procedure f, h\nend interface\ncontains\nfunction f(x)\nreal x\nreal f(2)\nf = x\nend function\ninteger function h(i)\nh = i\nend function\nend module\nuse m\nreal a(2)\na = g(1.0)\nend\n' > "$W/afngen.f90"
    printf 'integer n(2)\nreal a(4), b(2,2)\nn = 2\na = 1.0\nb = reshape(a, n(1:2))\nend\n' > "$W/rshdyn.f90"
    printf 'real a(2), b(2)\na = 1.0\nb = 1.0\nprint *, matmul(a, b)\nend\n' > "$W/mmrank.f90"
    printf 'real a(2), b(2)\na = 1.0\nb = transpose(a)\nend\n' > "$W/trrank.f90"
    printf 'integer k(2), m(2,2)\nk = 1\nn = 1\nm = spread(k, n, 2)\nend\n' > "$W/sprdim.f90"
    printf 'real a(2)\na = merge(1.0, 2, .true.)\nend\n' > "$W/mrgty.f90"
    printf 'integer m(2,2), s(2)\nm = 1\nn = 1\ns = sum(m, n)\nend\n' > "$W/rddim.f90"
    printf 'integer m(9)\nm = 0\nm(1:5) = (/ (/ 1, 2 /), 3 /)\nend\n' > "$W/aconshp.f90"
    printf 'print *, f(3)\nend\nfunction f(n) result(r)\nr = f(n - 1)\nend\n' > "$W/recnot.f90"
    printf 'module m\ninteger :: x\nend module\nuse m\nreal :: x\nend\n' > "$W/modvredecl.f90"
    printf 'module m\nprivate\ninteger :: x\nend module\nuse m, only: x\nend\n' > "$W/privuse.f90"
    printf 'module m\npublic :: x\ninteger, private :: x\nend module\n' > "$W/privtwice.f90"
    printf 'private\nend\n' > "$W/privprog.f90"
    printf 'real :: x\nreal, pointer :: p\np => x\nend\n' > "$W/ptrtgt.f90"
    printf 'real, target :: t2(2,2)\nreal, pointer :: pv(:)\npv => t2\nend\n' > "$W/ptrrank.f90"
    printf 'integer, target :: i\nreal, pointer :: p\np => i\nend\n' > "$W/ptrtype.f90"
    printf 'program q\nreal :: x\ncall s(x)\ncontains\nsubroutine s(a)\nreal, pointer :: a\nend subroutine\nend\n' > "$W/ptrdum.f90"
    printf 'real, pointer :: p(5)\nend\n' > "$W/ptrshape.f90"
    printf 'type n\ntype(n) :: next\nend type\nend\n' > "$W/dtself.f90"
    printf 'type pt\nreal :: x, y\nend type\ntype(pt) :: p, q\nq = pt(1.0, 2.0)\np = q\nif (p == q) stop\nend\n' > "$W/dtcmp.f90"
    printf 'type pt\nreal :: x, y\nend type\ntype(pt) :: p\nx = p + 1.0\nend\n' > "$W/dtadd.f90"
    printf 'type pt\nreal :: x, y\nend type\ntype(pt) :: p\np%%z = 1.0\nend\n' > "$W/dtnocomp.f90"
    printf 'type pt\nreal :: x, y\nend type\ntype(pt) :: p\np = 1.0\nend\n' > "$W/dtasgty.f90"
    printf 'type pt\nreal :: x, y\nend type\ntype(pt) :: p\np = pt(1.0)\nend\n' > "$W/dtcons.f90"
    printf 'program p\ntype t\ninteger, save :: n = 5\nend type\nend\n' > "$W/dtattr.f90"
    printf 'program p\ntype t\nsequence\ninteger :: n = 5\nend type\ntype(t) :: x\ncommon /c/ x\nend\n' > "$W/dfltcom.f90"
    printf 'program p\ntype t\nsequence\ninteger :: n = 5\nend type\ntype(t) :: x\ninteger :: k\nequivalence (x, k)\nend\n' > "$W/dflteqv.f90"
    printf 'subroutine s(x)\ntype t\nsequence\ninteger :: n = 5\nend type\ntype(t), intent(out) :: x(:)\nend\n' > "$W/dfltout.f90"
    printf 'integer :: j => null()\nend\n' > "$W/nullnoptr.f90"
    printf 'integer, pointer :: n = 5\nend\n' > "$W/ptrinit.f90"
    printf 'subroutine s(x)\ninteger, pointer :: x => null()\nend\n' > "$W/nulldum.f90"
    printf 'function pf(k)\ntype t\nsequence\ninteger :: n\nend type\ntype(t), pointer :: pf\nallocate(pf)\nend\n' > "$W/ptrfn.f90"
    printf "logical, parameter :: l = 'a' < 'b'\nend\n" > "$W/cxchar.f90"
    printf 'type(nope) :: r\nend\n' > "$W/dtnone.f90"
    printf "character(len=4) :: c\nwrite (c, '(A)', advance='no') 'x'\nend\n" > "$W/advint.f90"
    printf "integer :: k\nread (*, '(I3)', size=k) k\nend\n" > "$W/advsize.f90"
    printf "print '(B)', 5\nend\n" > "$W/fmtbw.f90"
    printf 'real, target :: a(5)\nreal, pointer :: p(:)\ninteger :: iv(2)\np => a(iv)\nend\n' > "$W/vecptr.f90"
    printf 'integer :: a(5), iv(2), jv(2)\nprint *, a(iv(jv))\nend\n' > "$W/vecvec.f90"
    printf 'integer :: v(2)\nv = transfer(1.0d0, v)\nprint *, transfer(1.0d0, (/ 0 /))\nend\n' > "$W/trarr.f90"
    printf 'integer :: k\ncall random_number(k)\nend\n' > "$W/rndint.f90"
    printf 'print *, modulo(7, 2.0)\nend\n' > "$W/modmix.f90"
    printf 'integer :: v(2)\ncall date_and_time(values=v, zone=v)\nend\n' > "$W/dtkw.f90"
    printf 'integer, parameter :: v(3) = (/ 1, 2, 3 /)\nv(2) = 5\nend\n' > "$W/parrasg.f90"
    printf 'integer :: v(3) = (/ 1, 2 /)\nend\n' > "$W/initcnt.f90"
    printf 'integer :: k = (/ 1, 2 /)\nend\n' > "$W/initscal.f90"
    printf 'integer, parameter :: v(3) = (/ 1, 2, 3 /)\ninteger, parameter :: k = v\nend\n' > "$W/parrwhole.f90"
    printf 'integer, parameter :: v(3) = (/ 1, 2, 3 /)\ninteger, parameter :: k = v(4)\nend\n' > "$W/parrsub.f90"
    printf 'type t\ninteger :: a, b\nend type\ntype(t) :: x = t(1)\nend\n' > "$W/dtinitfew.f90"
    printf 'integer :: w(2,2) = reshape((/ 1, 2, 3 /), (/ 2, 2 /))\nend\n' > "$W/initresh.f90"
    # ALLOCATABLE components: what is refused
    alt='program p\ntype t\nreal, allocatable :: a(:)\nend type\n'
    printf "${alt}type(t) :: x\nx = t(1.0)\nend\n" > "$W/alcons.f90"
    printf "${alt}type(t) :: x(2), y(2)\nx = y\nend\n" > "$W/alarr.f90"
    printf "${alt}type(t) :: x\nnullify(x%%a)\nend\n" > "$W/alnull.f90"
    printf "${alt}type(t) :: x\nreal, pointer :: q(:)\nq => x%%a\nend\n" > "$W/alptr.f90"
    printf "${alt}type(t) :: x\nprint *, associated(x%%a)\nend\n" > "$W/alassoc.f90"
    printf "${alt}type(t) :: x\nprint *, x\nend\n" > "$W/alio.f90"
    printf "${alt}type u\ntype(t), allocatable :: b(:)\nend type\nend\n" > "$W/alnest.f90"
    printf "program p\ntype t\nreal, allocatable :: a\nend type\nend\n" > "$W/alscal.f90"
    printf 'program p\ntype t\ninteger, pointer :: q\nend type\ntype(t) :: x\nprint *, x\nend\n' > "$W/dtptrio.f90"
    printf 'program p\ntype t\ninteger, pointer :: q\nend type\ntype(t) :: x\nx = t(3)\nend\n' > "$W/dtptrcons.f90"
    printf 'type pt\ncharacter*3 :: c\ninteger :: i\nend type\ntype(pt) :: a(2)\nprint *, a%%c\nend\n' > "$W/dtcdiv.f90"
    printf 'type pt\ninteger :: i\nend type\ntype(pt) :: a(3)\na = eoshift(a, 1)\nend\n' > "$W/dteosh.f90"
    printf 'type pt\ninteger :: i\nend type\ntype(pt) :: b(3)\nb = (/ 1, 2, 3 /)\nend\n' > "$W/dtacon.f90"
    # a RECURSIVE host's local its internal procedure names, first seen in the host's executable text
    printf 'program p\ncall r(2)\nend\nrecursive subroutine r(n)\nimplicit integer (a-z)\nk = n\ncall in\ncontains\nsubroutine in\nprint *, k\nend subroutine\nend\n' > "$W/rechostimp.f90"
    # defined operations: a module with OPERATOR(+) and ASSIGNMENT(=) on PT
    opm='module m\ntype pt\nreal :: x\nend type\ninterface operator(+)\nmodule procedure padd\nend interface\ninterface assignment(=)\nmodule procedure pfromi\nend interface\ncontains\nfunction padd(a, b)\ntype(pt), intent(in) :: a, b\ntype(pt) :: padd\npadd%%x = a%%x + b%%x\nend function\nsubroutine pfromi(a, i)\ntype(pt), intent(out) :: a\ninteger, intent(in) :: i\na%%x = i\nend subroutine\nend module\n'
    printf "${opm}program q\nuse m\ntype(pt) :: a(3), b(3)\na = b + b\nend\n" > "$W/opaex.f90"
    printf "${opm}program q\nuse m\ntype(pt) :: a(3)\na = 1\nend\n" > "$W/opasgarr.f90"
    # Fortran 95's ELEMENTAL: what an array needs of a defined assignment, and where an elemental reference may stand
    printf 'module m\ntype pt\nreal :: x\nend type\ninterface assignment(=)\nmodule procedure pa\nend interface\ncontains\nsubroutine pa(a, b)\ntype(pt), intent(out) :: a(:)\nreal, intent(in) :: b(:)\na%%x = b\nend subroutine\nend module\nprogram q\nuse m\ntype(pt) :: a(3)\nreal :: f(3)\na = f\nend\n' > "$W/asgarrproc.f90"
    printf "program p\nreal :: x(3)\nx = 1.0\nif (cube(x) > 2.0) print *, 'no'\ncontains\nelemental real function cube(a)\nreal, intent(in) :: a\ncube = a * a * a\nend function\nend\n" > "$W/elemarg.f90"
    # LOGICAL is not INTEGER to a generic or to an operator (F90 14.1.2.3)
    glm='module m\ninterface show\nmodule procedure si\nend interface\ninterface operator(.x.)\nmodule procedure xi\nend interface\ncontains\nsubroutine si(k)\ninteger :: k\nend subroutine\ninteger function xi(a, b)\ninteger, intent(in) :: a, b\nxi = a + b\nend function\nend module\n'
    printf "${glm}program q\nuse m\nlogical :: l\ncall show(l)\nend\n" > "$W/genlog.f90"
    printf "${glm}program q\nuse m\ncall show(1 > 0)\nend\n" > "$W/genlogx.f90"
    printf "${glm}program q\nuse m\nlogical :: l\nprint *, l .x. 1\nend\n" > "$W/oplog.f90"
    printf "${opm}program q\nuse m, operator(+) => operator(-)\nend\n" > "$W/opren.f90"
    printf "${opm}program q\nuse m, only: pt, operator(*)\nend\n" > "$W/opnone.f90"
    printf "${opm}program q\nuse m\ntype(pt) :: a\na = 1.5\nend\n" > "$W/opasgty.f90"
    printf 'module m\ntype pt\nreal :: x\nend type\ninterface operator(-)\nmodule procedure ps\nend interface\ncontains\nsubroutine ps(a)\ntype(pt) :: a\nend subroutine\nend module\nprogram q\nuse m\ntype(pt) :: a, b\na = -b\nend\n' > "$W/opsub.f90"
    printf 'module m\ntype pt\nreal :: x\nend type\ninterface assignment(=)\nmodule procedure pf\nend interface\ncontains\ninteger function pf(a, b)\ntype(pt) :: a, b\npf = 0\nend function\nend module\nprogram q\nuse m\ntype(pt) :: a, b\nb = a\nend\n' > "$W/opasgfn.f90"
    printf 'module m\ninterface operator(.x.)\nend interface\nend module\n' > "$W/opempty.f90"
    printf 'module m\ninterface assignment(+)\nend interface\nend module\n' > "$W/opasgplus.f90"
    printf 'program p\ninterface g\nreal function f(x)\nend function\nsubroutine s(i)\nend subroutine\nend interface\nend\n' > "$W/genmix.f90"
    printf 'program p\ninterface g\nmodule procedure f\nend interface\nend\n' > "$W/genmodp.f90"
    printf 'subroutine s(c)\ncharacter*(*) c\ncall t(transfer(1, c))\nend\n' > "$W/trclen.f90"
    printf "character*2 c(2)\nc = 'ab'\nprint *, repeat(c, 2)\nend\n" > "$W/reparr.f90"
    printf "print *, repeat('ab', -1)\nend\n" > "$W/repneg.f90"
    printf 'integer, parameter :: k = selected_real_kind()\nend\n' > "$W/selknone.f90"
    printf 'print *, selected_int_kind(p=3)\nend\n' > "$W/selkp.f90"
    printf 'module m\ncontains\nfunction f()\ncharacter(len=*) :: f\nf = "a"\nend function\nend module\n' > "$W/modclen.f90"
    printf 'program p\ncall ext(k=1)\nend\n' > "$W/kwext.f90"
    printf 'program p\ncall s()\ncontains\nsubroutine s(i, j)\ninteger, optional :: j\nend subroutine\nend\n' > "$W/optmiss.f90"
    printf 'program p\ncall s(1, z=2)\ncontains\nsubroutine s(i, j)\ninteger, optional :: j\nend subroutine\nend\n' > "$W/kwnone.f90"
    printf 'program p\ncall s(i=1, i=2)\ncontains\nsubroutine s(i, j)\ninteger, optional :: j\nend subroutine\nend\n' > "$W/kwtwice.f90"
    printf 'program p\ncall s(j=1, 2)\ncontains\nsubroutine s(i, j)\ninteger, optional :: j\nend subroutine\nend\n' > "$W/kwpos.f90"
    printf 'program p\nk = 1\nprint *, present(k)\nend\n' > "$W/presnot.f90"
    printf 'subroutine s(i)\ninteger, optional :: j\nend\n' > "$W/optlocal.f90"
    printf 'subroutine s\ncall s\nend\n' > "$W/recsub.f90"
    printf 'function f(n) result(f)\nf = n\nend\n' > "$W/resname.f90"
    printf 'implicit none\nimplicit integer (i)\nend\n' > "$W/impmix.f90"
    printf 'i = 1\nselect case (i)\ncase (1:5)\ncase (5)\nend select\nend\n' > "$W/selovl.f90"
    printf "character c\nc = 'a'\nselect case (c)\ncase ('a':'m')\ncase ('k')\nend select\nend\n" > "$W/selcovl.f90"
    printf "i = 1\nselect case (i)\ncase ('a')\nend select\nend\n" > "$W/selty.f90"
    printf 'i = 1\nj = 1\nselect case (i)\ncase (j)\nend select\nend\n' > "$W/selconst.f90"
    printf 'i = 1\nselect case (i)\ncase default\ncase default\nend select\nend\n' > "$W/seldef2.f90"
    printf 'i = 1\nselect case (i)\nj = 1\ncase (1)\nend select\nend\n' > "$W/selstmt.f90"
    printf 'i = 1\ncase (1)\nend\n' > "$W/selstray.f90"
    printf 'i = 1\nend select\nend\n' > "$W/selend.f90"
    printf 'i = 1\nselect case (i)\ncase (1)\nend\n' > "$W/selopen.f90"
    printf 'logical l\nl = .true.\nselect case (l)\ncase (.true.:)\nend select\nend\n' > "$W/sellogr.f90"
    printf 'i = 1\ns: select case (i)\ncase (1)\nend select\nend\n' > "$W/selname.f90"
    printf 'x = 1.0\nselect case (x)\nend select\nend\n' > "$W/selreal.f90"
    printf 'i = 1\ndo\ns: select case (i)\ncase (1)\nexit s\nend select s\nend do\nend\n' > "$W/selexit.f90"
    printf 'real a(4), b(2,2)\na = 1.0\nb = reshape(a, (/2,2/), order=(/2,1,3/))\nend\n' > "$W/rshord.f90"
    printf 'integer k(3), s(3)\nk = 1\ns = 1\nk = cshift(k, s)\nend\n' > "$W/csharr.f90"
    printf 'integer m(2,3), s(2)\nm = 1\ns = 1\nm = cshift(m, s)\nend\n' > "$W/cshshp.f90"
    printf "character*2 c(3)\nc = 'ab'\nc = eoshift(c, 1, 'xyz')\nend\n" > "$W/eolen.f90"
    printf 'integer m(2,2)\nm = 1\ni = maxloc(m)\nend\n' > "$W/mlscal.f90"
    printf 'integer m(2,2)\nm = 1\ni = sum(m, 1)\nend\n' > "$W/sumscal2.f90"
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
             "recfn:line 4: a FUNCTION's name is its result here: a recursive reference needs RESULT" \
             "recnot:line 4: F is not RECURSIVE: it cannot reference itself" \
             "modvredecl:line 5: X is a module's, by USE: it cannot be declared again" \
             "privuse:line 5: X is PRIVATE in module M" \
             "privtwice:line 3: X is said PUBLIC or PRIVATE twice" \
             "privprog:line 1: PUBLIC and PRIVATE are a module's" \
             "ptrtgt:line 3: a pointer's target has the TARGET or POINTER attribute" \
             "ptrrank:line 3: a pointer's target is of its rank" \
             "ptrtype:line 3: a pointer's target is of its type and kind" \
             "ptrdum:line 3: a POINTER dummy argument needs a POINTER actual" \
             "ptrshape:line 1: an ALLOCATABLE or POINTER array's shape is (:), (:,:) ..., its bounds given by ALLOCATE or =>" \
             "dtself:line 3: a component of its own type is a POINTER" \
             "dtcmp:line 7: no OPERATOR(==) here for these operands" \
             "dtadd:line 5: no OPERATOR(+) here for these operands" \
             "dtnocomp:line 5: the type PT has no component Z" \
             "dtasgty:line 5: a value of another type for a structure" \
             "dtcons:line 5: the structure constructor PT needs 2 values" \
             "dtattr:line 3: SAVE is no attribute of a component: POINTER, DIMENSION or ALLOCATABLE" \
             "dfltcom:line 8: a structure of a type with default initialization cannot be in COMMON" \
             "dflteqv:line 9: a structure of a type with default initialization cannot be in an EQUIVALENCE" \
             "dfltout:line 7: an INTENT(OUT) assumed-shape or assumed-size array of a type with default initialization or ALLOCATABLE components is not supported" \
             "nullnoptr:line 1: => NULL() is a POINTER's initial value" \
             "ptrinit:line 1: a POINTER's initial value is => NULL()" \
             "nulldum:line 2: a dummy argument cannot be given an initial value" \
             "ptrfn:line 7: an ALLOCATABLE or POINTER FUNCTION result is not supported" \
             "cxchar:line 1: a CHARACTER or COMPLEX comparison in a constant expression is not supported" \
             "dtnone:line 1: no derived type NOPE here" \
             "advint:line 2: ADVANCE= is for an external file and an explicit format" \
             "advsize:line 2: SIZE= and EOR= are a READ's with ADVANCE=" \
             "fmtbw:line 1: FORMAT: B needs a width, Bw or Bw.m" \
             "vecptr:line 4: a section with a vector subscript is not supported here" \
             "vecvec:line 2: a vector subscript with a vector subscript of its own is not supported" \
             "trarr:line 3: TRANSFER with an array result is supported only as an actual argument" \
             "rndint:line 2: RANDOM_NUMBER's HARVEST is REAL or DOUBLE PRECISION" \
             "modmix:line 1: MODULO's arguments are both INTEGER or both REAL of one kind" \
             "dtkw:line 2: DATE_AND_TIME's ZONE is a CHARACTER scalar" \
             "parrasg:line 2: cannot assign to a PARAMETER" \
             "initcnt:line 1: an initial value with another number of elements than the array" \
             "initscal:line 1: an array value for a scalar" \
             "parrwhole:line 2: a PARAMETER array where a scalar constant is wanted" \
             "parrsub:line 2: a PARAMETER array's subscript is an INTEGER constant within its bounds" \
             "dtinitfew:line 4: too few values in a structure constructor" \
             "initresh:line 1: RESHAPE's SOURCE has another number of elements than its SHAPE" \
             "alcons:line 6: a structure constructor of T, a type with ALLOCATABLE components, is not supported" \
             "alarr:line 6: an array assignment of structures with ALLOCATABLE components is not supported" \
             "alnull:line 6: an ALLOCATABLE component is no POINTER" \
             "alptr:line 7: a pointer to an ALLOCATABLE component is not supported" \
             "alassoc:line 6: an ALLOCATABLE component is no POINTER" \
             "alio:line 6: a structure with a POINTER or ALLOCATABLE component is no I/O item (F90 9.4.2)" \
             "alnest:line 7: an ALLOCATABLE component of a type with ALLOCATABLE components is not supported" \
             "alscal:line 3: an ALLOCATABLE array needs its rank, as (:) or (:,:)" \
             "dtptrio:line 6: a structure with a POINTER or ALLOCATABLE component is no I/O item (F90 9.4.2)" \
             "dtptrcons:line 6: a pointer's target is a variable" \
             "opaex:line 26: a defined operation in an array expression needs an ELEMENTAL procedure" \
             "asgarrproc:line 19: a defined assignment whose procedure takes arrays is not supported" \
             "elemarg:line 4: an ELEMENTAL procedure's array argument is not supported here" \
             "genlog:line 20: no procedure of the generic SHOW takes these arguments" \
             "genlogx:line 19: no procedure of the generic SHOW takes these arguments" \
             "oplog:line 20: no OPERATOR(.X.) here for these operands" \
             "opasgarr:line 26: a value of another type for an array of structures" \
             "opren:line 24: an operator is named in a USE's ONLY list, not renamed" \
             "opnone:line 24: module M has no OPERATOR(*)" \
             "opasgty:line 26: a value of another type for a structure" \
             "opsub:line 16: an operator's procedure is a FUNCTION" \
             "opasgfn:line 17: an ASSIGNMENT(=)'s procedure is a SUBROUTINE" \
             "opempty:line 3: a generic interface with no procedure" \
             "opasgplus:line 2: ASSIGNMENT(=) is the only assignment" \
             "dteosh:line 5: EOSHIFT of structures needs a BOUNDARY" \
             "dtacon:line 5: a value of another type for an array of structures" \
             "rechostimp:line 6: K: a RECURSIVE procedure's local in its internal procedures is not supported yet" \
             "dtcdiv:line 6: a component of an array of structures whose size does not divide the structure's is not supported yet" \
             "genmix:line 7: a generic's procedures are all FUNCTIONs or all SUBROUTINEs" \
             "genmodp:line 3: MODULE PROCEDURE is in a module's generic interface (F90 12.3.2.1)" \
             "trclen:line 3: TRANSFER's CHARACTER MOLD needs a length known here" \
             "reparr:line 3: REPEAT's arguments are scalars" \
             "repneg:line 1: REPEAT's NCOPIES is less than 0" \
             "selknone:line 1: SELECTED_REAL_KIND takes P, R or both" \
             "selkp:line 1: SELECTED_INT_KIND takes one argument, R" \
             "modclen:line 6: a module FUNCTION of assumed CHARACTER length is not supported" \
             "kwext:line 2: keyword arguments to EXT need an explicit interface" \
             "optmiss:line 2: no actual argument for I, which is not OPTIONAL, of S" \
             "kwnone:line 2: S has no dummy argument Z" \
             "kwtwice:line 2: the dummy argument I of S has two actuals" \
             "kwpos:line 2: a positional argument after a keyword argument" \
             "presnot:line 3: PRESENT needs an OPTIONAL dummy argument" \
             "optlocal:line 2: OPTIONAL is a dummy argument's" \
             "recsub:line 2: S is not RECURSIVE: it cannot call itself" \
             "resname:line 1: a FUNCTION's RESULT has a name of its own" \
             "impmix:line 2: IMPLICIT NONE is the only IMPLICIT statement of its unit" \
             "wholearr:line 2: a whole array in an expression is not supported yet" \
             "alloceq:line 4: an ALLOCATABLE array cannot be in an EQUIVALENCE" \
             "allocarg:line 2: an ALLOCATABLE dummy argument is not supported" \
             "allocshape:line 1: an ALLOCATABLE or POINTER array's shape is (:), (:,:) ..., its bounds given by ALLOCATE or =>" \
             "aconshape:line 2: the array constructor has 2 values for 3 elements" \
             "cppdef:line 1: only the preprocessor's conditionals are supported (#if, #ifdef, #ifndef, #elif, #else, #endif)" \
             "cppopen:line 3: #if without #endif" \
             "freeamp:line 1: & in the middle of a line" \
             "freestr:line 2: a character constant runs off the line" \
             "kindq:line 1: kind 16 of this type is not supported" \
             "kindi8:line 1: kind 8 of this type is not supported" \
             "kindlit:line 1: the kind of a constant must be digits or an INTEGER named constant, not K" \
             "kindlit16:line 1: REAL kind 16 is not supported" \
             "ibexec:line 4: an interface body holds declarations only" \
             "ibredecl:line 6: F has an interface body: it is not declared again" \
             "usenone:line 1: no module NOSUCH: neither in this file before the USE, nor a .mod file of it here" \
             "useintr:line 1: USE, INTRINSIC and USE :: are Fortran 2003, not supported" \
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
             "cndecl:line 1: a construct name is on DO, IF (...) THEN, SELECT CASE or WHERE (...) only" \
             "else2:line 3: a second ELSE in one IF construct" \
             "elseelif:line 3: ELSE IF after ELSE" \
             "endifjunk:line 2: unexpected text after END IF" \
             "sumscal:line 2: SUM needs an array" \
             "sumlog:line 3: SUM of a LOGICAL array" \
             "cntnum:line 3: COUNT needs a LOGICAL array" \
             "dotrank:line 3: DOT_PRODUCT needs arrays of rank 1" \
             "sumkw:line 3: the arguments of SUM are not ones it takes" \
             "sizedim:line 3: DIM= of SIZE is outside the array's rank" \
             "wherenest:line 5: the shapes of a WHERE's mask and the mask of a WHERE inside it do not conform" \
             "whereif:line 4: a WHERE construct holds array assignments only" \
             "wherescal:line 3: a WHERE assigns to arrays, not to a scalar or an element" \
             "wheremask:line 6: the shapes of a WHERE's mask and its ELSEWHERE's do not conform" \
             "where2else:line 7: an ELSEWHERE after the WHERE construct's ELSEWHERE with no mask" \
             "endname:line 2: the name on this END is not its unit's (A)" \
             "endkind:line 2: this END names another kind of unit: END SUBROUTINE ends this one" \
             "endnoname:line 2: the name on this END is not its unit's, which has none" \
             "endbdname:line 3: the name on this END is not its unit's, which has none" \
             "endmodname:line 5: the name on this END is not its unit's (M)" \
             "kwfirst:line 2: SQRT's first argument by keyword (X=) is not supported" \
             "reslenref:line 1: the result length of F is not known where it is referenced" \
             "reslenhost:line 9: the length of this FUNCTION's result is one in its host and another here" \
             "reskindhost:line 9: the kind of this FUNCTION's result is one in its host and another here" \
             "reslenunk:line 2: this FUNCTION's CHARACTER length is not a constant known here" \
             "wherestray:line 3: END WHERE without WHERE" \
             "whereopen:WHERE without END WHERE" \
             "wherenoarr:line 2: WHERE needs a LOGICAL array for its mask" \
             "whereshape:line 4: the shapes of a WHERE's mask and its assignment do not conform" \
             "asext:line 3: S has an assumed-shape dummy argument: it needs an explicit interface" \
             "asscal:line 9: an assumed-shape dummy needs an array or a section as its actual" \
             "asrank:line 9: an actual of rank 1 for an assumed-shape dummy of rank 2" \
             "aslocal:line 2: a shape of (:) is an assumed-shape dummy's or an ALLOCATABLE array's" \
             "asasz:line 10: an assumed-size array cannot be the actual of an assumed-shape dummy" \
             "charwhole:line 4: a whole CHARACTER array in an expression is not supported yet" \
             "trimarr:line 3: TRIM's argument is a scalar" \
             "aconlen:line 2: the values of an array constructor differ in length" \
             "shpasg:line 3: the shapes in an array expression do not conform" \
             "shpop:line 3: the shapes in an array expression do not conform" \
             "shpwhere:line 3: the shapes of a WHERE's mask and its assignment do not conform" \
             "afnscal:line 2: an array-valued FUNCTION where a scalar is wanted" \
             "afnext:line 2: F is array-valued: it needs an explicit interface" \
             "afnclen:line 1: an array-valued FUNCTION of assumed CHARACTER length is not supported" \
             "afnent:line 1: ENTRY in an array-valued FUNCTION is not supported" \
             "afngen:line 17: an array-valued FUNCTION through a generic of scalar and array-valued ones is not supported yet" \
             "rshdyn:line 5: RESHAPE's SHAPE must be an array of constant size, 1 to 7" \
             "mmrank:line 4: MATMUL needs a matrix and a matrix, or a matrix and a vector" \
             "trrank:line 3: TRANSPOSE needs an array of rank 2" \
             "sprdim:line 4: DIM= of SPREAD is supported only as a constant" \
             "mrgty:line 2: MERGE's TSOURCE and FSOURCE differ in type" \
             "rddim:line 4: DIM= of a reduction of an array of rank above 1 is supported only as a constant" \
             "aconshp:line 3: the shapes in an array expression do not conform" \
             "selovl:line 4: a value is in two CASEs of one SELECT CASE" \
             "selcovl:line 5: a value is in two CASEs of one SELECT CASE" \
             "selty:line 3: a CASE value has the type of SELECT CASE's expression" \
             "selconst:line 4: a CASE value is a constant" \
             "seldef2:line 4: a second CASE DEFAULT in one SELECT CASE" \
             "selstmt:line 3: only a CASE statement may follow SELECT CASE" \
             "selstray:line 2: CASE without SELECT CASE" \
             "selend:line 2: END SELECT without SELECT CASE" \
             "selopen:SELECT CASE without END SELECT" \
             "sellogr:line 4: a LOGICAL CASE value is no range" \
             "selname:line 4: END SELECT needs the construct name S" \
             "selreal:line 2: SELECT CASE needs an INTEGER, LOGICAL or CHARACTER expression" \
             "selexit:line 5: EXIT names S, a SELECT CASE construct, not a DO (EXIT from it is Fortran 2008)" \
             "rshord:line 3: RESHAPE's ORDER has a value for each dimension of the result" \
             "csharr:line 4: SHIFT is a scalar, or an array of rank one less than the array's" \
             "cshshp:line 4: SHIFT's shape is not the array's without dimension DIM" \
             "eolen:line 3: EOSHIFT's BOUNDARY differs in length from its ARRAY" \
             "mlscal:line 3: MAXLOC without DIM= gives an array, not a scalar" \
             "sumscal2:line 3: SUM with DIM= of an array of rank above 1 gives an array, not a scalar" \
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
