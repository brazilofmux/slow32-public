# Fortran 90/95 in fortran/

2026-11-08.  The user: "Let's do Fortran 90.  We've been positioning for it."
The last two weeks set this up:
- the backend copies resynced;
- ffuzz (f77 against flang, gfortran deciding);
- COMPLEX*16;
- reference LAPACK as the corpus.

Fortran 90/95 is where the Fortran story says "enough", and PL/I may follow
(the case against it is in the 2026-11-08 conversation: F90 brings the new
stress, and the four HIR backend copies should be merged first).

## The compiler grows; the backend does not

fortran/'s f77 grows into a Fortran 90/95 compiler; Fortran 77 stays a
subset of it, so the FCVS (192/192), tests/f77 and ffuzz keep holding it.  It
is one pass with no AST: statements become HIR as they are parsed, and the
inliner re-lexes a callee's text.  Each F90 feature below says how it fits
that, and none needs anything new in the HIR backend (stage08's, shared):
- **Internal procedures (CONTAINS).**  F90 nests them one level and forbids
  passing one as an actual argument (F2008 allowed that).  So host
  association lowers in the front end: an internal procedure gets hidden
  arguments, the addresses of the host's variables it names.  No static
  link, no trampolines.
- **Recursion (RECURSIVE)**: the frame is already per call.  Today's static
  promotions (SAVE, DATA, main's arrays) stay what they are.
- **ALLOCATE, pointers**: runtime calls and addresses.
- **Assumed-shape arrays, sections, pointers to arrays**: a descriptor (base,
  and per dimension a lower bound, extent and stride) built and read in the
  front end.  A call to an explicit-interface procedure passes the
  descriptor; old-style calls pass the base, as now.
- **Array expressions, WHERE, FORALL**: loops the front end generates around
  the element expression, as the implied-DO of I/O already is.  The shape
  comes from the left side or a scan of the statement, before the right
  side is parsed.
- **Derived types**: a layout computed by the front end; components are
  offsets.
- **Modules**: a module's public interface written to a file at compile
  time (`.f77mod`, a text form of the symbol table) and read by USE; the
  code is an ordinary object.  Order of compilation is the user's, as with
  every Fortran compiler.

## Milestones, each held to gfortran and flang before the next

1. **LAPACK's Fortran 90.**  What reference LAPACK, BLAS and LAPACK's own
   test programs use, so the LAPACK suites run under f77.  Today 2,024 of
   2,185 library files and 1,159 of 1,235 testing files compile.  The rest
   need, in fixed form:
   - `::` and attributes (INTENT, PARAMETER, DIMENSION, SAVE, ALLOCATABLE),
     CHARACTER(LEN=n) and CHARACTER(n);
   - `== /= < <= > >=`, double-quoted strings, `!` comments anywhere;
   - EXIT, CYCLE, construct names, RECURSIVE;
   - array sections: as actual arguments, and as the target of a scalar
     assignment;
   - ALLOCATABLE arrays with ALLOCATE and DEALLOCATE;
   - intrinsics: CEILING, HUGE, TINY, EPSILON, EXPONENT, MAXLOC and MINLOC,
     and elemental intrinsics in PARAMETER (DCMPLX(0D0,0D0)).

   Then LAPACK built by f77, its suites run, and held as `run-lapack.sh`
   holds flang: no more over the threshold than gfortran.
2. **Free-form source** (`.f90`), long names, `&` continuation, `;`.
3. **Arrays**: whole-array and section expressions and assignment, strides,
   array constructors, assumed-shape dummies, automatic arrays, WHERE, and
   the array intrinsics (SUM, PRODUCT, MAXVAL, MINVAL, DOT_PRODUCT, MATMUL,
   TRANSPOSE, RESHAPE, SPREAD, SHAPE, SIZE, LBOUND, UBOUND, ANY, ALL, COUNT,
   PACK, UNPACK, CSHIFT, EOSHIFT, MERGE).
4. **Procedures and modules**: modules and USE (ONLY, renames,
   PUBLIC/PRIVATE), explicit interfaces, OPTIONAL and keyword arguments,
   generic interfaces, operator and assignment overloading, internal
   procedures, SELECT CASE, RESULT, KIND and SELECTED_*_KIND.
5. **Derived types and pointers**: TYPE, components, structure constructors,
   POINTER and TARGET, ALLOCATABLE components (as in F95's TR), NULLIFY and
   ASSOCIATED, pointers to arrays.
6. **Fortran 95**: PURE, ELEMENTAL, FORALL, NULL(), default initialization,
   CPU_TIME, and the F95 deletions noted as warnings, not removals.

## Oracles and corpora

- **flang** builds SLOW-32 programs from the same source: the same machine and
  libm, nothing to allow for.  **gfortran** decides when the two differ.
- **ffuzz** grows with each milestone, f77 against flang.
- **gfortran's own test corpus** (the 3,222 execute tests run-flang-corpus.sh
  uses, ~/refs) measures f77's progress, with recorded verdicts like
  flang's.
- **Reference LAPACK** and its suites (milestone 1).
- The **FCVS** and **tests/f77** keep Fortran 77 whole throughout.

## What not to do

- Nothing new in the HIR backend for F90's sake.  A feature that seems to
  need it is lowered differently.
- No F2003 or later (classes, ASSOCIATE, submodules, coarrays) without a
  ruling.
- No silent acceptance: what is not implemented is refused by name, and a
  type or construct that reaches code not written for it stops the unit (the
  COMPLEX*16 guard's discipline).
