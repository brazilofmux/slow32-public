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

   **Status 2026-11-08 (591e8847):** every fixed-form file compiles, all
   3,492 of SRC, BLAS, TESTING and INSTALL.  The suites can't run yet,
   though: 44 files are not fixed form.  31 are free form (`.f90`, `.F90`):
   - the modules LA_CONSTANTS and LA_XISNAN;
   - the newer xLARTG, xLASSQ, xNRM2 and xROTG;
   - the DMD routines and their tests.

   The other 13 are `.F` files that need the preprocessor's conditionals
   (`#if defined(_OPENMP)`, `#ifdef`, `#elif`, `#else`, `#endif`; no
   macros), the suite drivers xCHKEE and xCHKAA among them.  Between them
   they need:
   - milestone 2;
   - the conditionals;
   - a slice of milestone 4: modules with PARAMETERs and procedures, USE
     with ONLY and renames (`wp=>dp`), KIND (`REAL(wp)`, `0.0_wp`), a
     generic interface, and internal procedures.

   That is the order now: milestone 2 and the conditionals, then that
   slice of 4, then the suites.  Linking flang's objects instead is the
   shortcut ruled out.

   **Done 2026-11-08 (b577e857): LAPACK built by f77 passes its suites.**
   `regression/run-lapack.sh --f77` builds reference LAPACK, BLAS and the
   test programs with f77 and runs every suite but DMD.  The result:
   - 90 of 90 suites print what gfortran's build prints;
   - 5,199,980 tests, the same count as the host;
   - one over the threshold, which the host has too;
   - 4.5 minutes on the M5.

   On the way, from milestone 2 and the slice of 4: free form, the
   conditionals, kinds, modules (`.mod` files), USE, generic interfaces,
   internal procedures, automatic arrays, CPU_TIME.

   It found defects, all fixed, each with a gate:
   - COMMON blocks were private to each file: tests/multi/, the new
     gate for programs of several files;
   - automatic arrays were taken silently, on a slot of one element;
   - Infinity and NaN were formatted as numbers;
   - two wrong-code bugs in the HIR backend all four compilers share
     (selfhost ISSUES-116 and 117).  117 is reachable from C, so stage08
     had it too.

   Left out: the DMD routines and their four suites, which USE
   ISO_FORTRAN_ENV (Fortran 2003, REAL64 from F2008).  The user's ruling,
   2026-11-08: "DMD is out for now".

   Open: the opt-in copy-in mode (`F77_COPYIN=1`, off by default and in no
   gate) faults on LAPACK's DTRSYL3.
2. **Free-form source** (`.f90`), long names, `&` continuation, `;`.

   **Done 2026-11-08**, with milestone 1's run: free form, then construct
   names on DO, DO WHILE and block IF (END DO, END IF, ELSE, ELSE IF,
   EXIT and CYCLE carry them, held to gfortran's rules), and DO with no
   loop control.  Checking the names found three things Fortran 77's IF
   blocks took silently: a second ELSE, ELSE IF after ELSE, and text
   after END IF.  EXIT from a named IF is Fortran 2008's, refused.
3. **Arrays**: whole-array and section expressions and assignment, strides,
   array constructors, assumed-shape dummies, automatic arrays, WHERE, and
   the array intrinsics (SUM, PRODUCT, MAXVAL, MINVAL, DOT_PRODUCT, MATMUL,
   TRANSPOSE, RESHAPE, SPREAD, SHAPE, SIZE, LBOUND, UBOUND, ANY, ALL, COUNT,
   PACK, UNPACK, CSHIFT, EOSHIFT, MERGE).

   **Done 2026-11-09.**
   - Reductions of any array expression, with MASK, and with DIM on any
     rank.
   - SIZE, LBOUND, UBOUND and SHAPE.
   - WHERE, the statement and the construct.
   - Sections as actual arguments, copied in and out unless plainly
     contiguous.
   - Assumed-shape dummies, through the descriptor above.  The scan and
     the `.mod` file tell a caller which dummies take one; anything else
     gets the array's storage, packed if needed.
   - The array-valued intrinsics: MATMUL, TRANSPOSE, RESHAPE, SPREAD,
     PACK, UNPACK, CSHIFT, EOSHIFT, MERGE, and MAXLOC and MINLOC without
     DIM.
   - Constructors anywhere an array may be, arrays among their values;
     array expressions as actual arguments and I/O items.
   - CHARACTER arrays in all of the above: assignment (a substring of
     each element too, C(:)(2:3)), expressions, sections as actuals,
     assumed-shape dummies, the intrinsics.
   - RESHAPE's ORDER; an array SHIFT and BOUNDARY.
   - Array-valued functions, internal and module (milestone 4's, done
     here because a result like that had been stored to every element
     as if it were a scalar).  The callee copies its result to the heap
     at each return and fills in the caller's descriptor, hidden
     argument 0; the caller names it as a hidden array, as it does
     MATMUL's.

   How the array-valued ones fit one pass: before a consumer's loops
   (an assignment's right side, a reduction's arguments, a WHERE mask, a
   call's actuals, an I/O list), each array-valued reference in its text
   is evaluated whole into a heap temporary.  The text is then rewritten
   to name a hidden assumed-shape array that holds it (`f77_hoist`), and
   the temporaries are freed after the consumer.  Still nothing new in
   the HIR backend.

   Found on the way, all pre-existing:
   - an array-valued FUNCTION's result was stored to every element of
     the target as a scalar;
   - the shapes in an array expression were never checked: N = N(1:2)
     read past the section (checked now where the extents are
     constants, as gfortran checks them);
   - a generic interface ignored the actuals' rank and called its rank-1
     specific for every array;
   - an array right after a dot operator (.NOT. K) was not seen as an
     operand, and as the target the overlap check missed it:
     L(2:5) = K(1:4) .AND. .NOT. L(1:4) read elements already stored;
   - TRIM of an array was trimmed elementwise;
   - MAXVAL of no INTEGER elements was -HUGE, not -HUGE-1;
   - a whole LOGICAL array in list-directed output printed 1 and 0;
   - a whole CHARACTER array was its first element: D = C assigned C(1)
     to D(1) and stopped (now refused);
   - two ELSEs in one IF, ELSE IF after ELSE, and text after END IF were
     taken silently.

   Still refused by name, for milestone 4: an array-valued function
   through a generic interface, or with ENTRY; CHARACTER and COMPLEX*16
   module functions; TRANSFER of CHARACTER.  Not yet written, F90
   intrinsics outside the array ones: ADJUSTL, ADJUSTR, REPEAT, SCAN,
   VERIFY, ACHAR, IACHAR, INDEX's BACK.
4. **Procedures and modules**: modules and USE (ONLY, renames,
   PUBLIC/PRIVATE), explicit interfaces, OPTIONAL and keyword arguments,
   generic interfaces, operator and assignment overloading, internal
   procedures, SELECT CASE, RESULT, KIND and SELECTED_*_KIND.

   **Status 2026-11-09: done** (operator and assignment overloading
   landed with milestone 5's derived types, adfddbc6):
   - SELECT CASE; RESULT and RECURSIVE;
   - module variables, through a block of the module's own, laid out as
     COMMON's; PUBLIC and PRIVATE;
   - OPTIONAL and keyword arguments, PRESENT;
   - internal procedures using their host's variables: the host's
     locals they name are static, its dummies reached through a table in
     its frame whose address they take as a hidden argument;
   - interface bodies, and generic interfaces of them in any unit;
     keywords and array-valued results through generics;
   - CHARACTER and COMPLEX*16 module functions; SELECTED_REAL_KIND and
     SELECTED_INT_KIND anywhere; TRANSFER with CHARACTER;
   - the CHARACTER intrinsics ADJUSTL, ADJUSTR, REPEAT, SCAN, VERIFY,
     ACHAR, IACHAR, INDEX's BACK.

   Found on the way, pre-existing: an undeclared dummy named as a host's
   PARAMETER took the PARAMETER's value; ELSE and ELSE IF without the
   construct name in a named IF were refused; IMPLICIT in a contained
   procedure did not give its letters back from the host's IMPLICIT
   NONE.  Still refused by name: ENTRY in an array-valued function, a
   generic of scalar and array-valued functions where an array is wanted
   (a RECURSIVE host's locals in its internal procedures came with the
   leftovers, bf5ad581).
5. **Derived types and pointers**: TYPE, components, structure constructors,
   POINTER and TARGET, ALLOCATABLE components (as in F95's TR), NULLIFY and
   ASSOCIATED, pointers to arrays.

   **Status 2026-11-09: done.**  Derived types (a8c334cd), functions of a
   derived type (531c403e), POINTER and TARGET with NULLIFY, ASSOCIATED,
   pointer arrays and pointer dummies (7cf3b5fe), defined operators and
   ASSIGNMENT(=) (adfddbc6) -- INTERFACE OPERATOR(op), .NAME. operators
   binary and unary, through USE ONLY and PUBLIC/PRIVATE -- and
   ALLOCATABLE components (b9893620).  Refused by name: a defined
   operation on array operands (a specific with array dummies; F95's
   ELEMENTAL came with milestone 6), an operator's array-valued function.

   Found on the way, pre-existing: a generic whose specifics differed
   only in a derived type called the first (silent); a module procedure
   could not reach a later sibling or itself through a generic; two
   USEs' generics of one name were not merged; a module re-exported
   neither the procedures nor the generics it had by USE.
6. **Fortran 95**: PURE, ELEMENTAL, FORALL, NULL(), default initialization,
   CPU_TIME, and the F95 deletions noted as warnings, not removals.

   **Status 2026-11-09: done.**  WHERE in full (a WHERE inside a WHERE,
   masked ELSEWHEREs, construct names), the PURE, ELEMENTAL and RECURSIVE
   prefixes, CPU_TIME and the small additions -- KIND on CEILING and
   FLOOR, DIM on MAXLOC and MINLOC, I0 and F0.d, a ! comment in NAMELIST
   input (1a7ac6fb); default initialization of components and `=> NULL()`
   (dfdb0b77); ELEMENTAL procedures, defined operators and defined
   assignment on arrays (db2362dc); FORALL, statement and construct
   (7818d37f); the deleted features as warnings (4cb3db4d).  Automatic
   deallocation of ALLOCATABLE locals at RETURN was there already and has
   its tests now.

   The deletions -- a DO variable or loop control expression that is not
   INTEGER, ASSIGN, the assigned GO TO, an assigned FORMAT, the H edit
   descriptor, PAUSE, a branch to an END IF from outside its IF -- all
   still compile.  Free-form source gets one warning for each; fixed
   form, which is Fortran 77's and the FCVS's, gets none.

   Found on the way, all there before:

   - the compiler crashed in the unit scan on a FUNCTION whose CHARACTER
     length names a constant; an END that names its unit was not checked
     (1a7ac6fb);
   - a SAVEd ALLOCATABLE array of a host was deallocated at RETURN when
     an internal procedure named it; a named constant of the enclosing
     unit was not visible inside a derived type's definition; a whole
     array component of a scalar POINTER structure was read from the
     pointer's slot (dfdb0b77);
   - generic, defined-operator and defined-assignment resolution did not
     tell LOGICAL from INTEGER (db2362dc);
   - an array assignment whose value overlaps its target through a
     POINTER, a TARGET or a structure component stored without a
     temporary (c0640bbe);
   - a FUNCTION or SUBROUTINE spliced into its caller left its names
     there, and the caller's next new name of the same spelling was the
     callee's variable (e4f95346);
   - SELECT CASE inside a WHERE construct was accepted (7818d37f);
   - an I/O list's implied DO with a REAL or DOUBLE PRECISION variable
     counted in integers; a DO or implied DO took a LOGICAL, COMPLEX or
     CHARACTER variable, an array or a PARAMETER, and a constant step of
     zero; an unformatted WRITE of an INTEGER*2 or LOGICAL*1 FUNCTION's
     result wrote a fullword, and of INT(H) of an INTEGER*2 H two bytes;
     a host variable an internal procedure names only against a keyword
     (DO X = ..., PRINT F) was not seen as the host's (e2f6d2fa).

## Fortran 90 and 95: where it stopped (2026-11-09)

All six milestones are done.  After milestone 5 the list of what a
Fortran 90 program could still trip on was worked off:

- initial values of arrays and structures, PARAMETER arrays (c7d811cc);
- the intrinsics that were missing (f97d18a8);
- vector subscripts (2b43a055);
- non-advancing I/O with SIZE= and EOR=, B/O/Z editing, the rest of
  OPEN's and INQUIRE's specifiers, a NAMELIST group in column 1
  (0a658b23);
- arrays of structures in expressions and I/O, implied DOs whose items
  need evaluating first, a constructor in a WHERE, a RECURSIVE host's
  locals in its internal procedures, ASSOCIATED with an array or section
  TARGET, a derived type renamed by USE (bf5ad581);

and then milestone 6, above.

Gates at the pause: tests/run-tests.sh 593/0, FCVS 192/192, ffuzz 2000
of 2000 the same as flang, LAPACK built by f77 printing what gfortran's
build prints in 90 of 90 suites (5,199,980 tests).

What is left is refused by name, none of it accepted and wrong.  The
ones a program is likeliest to meet:

- a defined operation on array operands whose procedure is not
  ELEMENTAL (it takes arrays), and an operator whose function is
  array-valued;
- ALLOCATABLE dummies and function results, POINTER function results
  (the first two are the F95 TR's, the third is Fortran 90's);
- a type with ALLOCATABLE components: no structure constructor, no
  array assignment, no ALLOCATABLE component of such a type in turn; as
  an INTENT(OUT) assumed-shape array, like a type with default
  initialization;
- a component of an array of structures whose size does not divide the
  structure's (a CHARACTER*3 beside an INTEGER);
- a pointer to an ALLOCATABLE component; a vector subscript on a vector
  subscript;
- a local a RECURSIVE host first names in its executable text
  (implicitly typed) and its internal procedures use;
- ENTRY in an array-valued function; a generic of scalar and
  array-valued functions where an array is wanted; array-valued
  functions of assumed CHARACTER length, INTEGER*2 or LOGICAL*1; a
  module function of assumed CHARACTER length;
- TRANSFER to a structure; an intrinsic's first argument by keyword;
- in constant expressions: a component of a structure PARAMETER,
  RESHAPE's PAD and ORDER, CHARACTER and COMPLEX comparisons;
- in FORALL: a WHERE in its body; an assignment that reads what it
  assigns when the target is an assumed-size array, a POINTER or
  ALLOCATABLE component, or a structure with ALLOCATABLE components; a
  pointer assignment that does; an index a statement function of the
  unit names;
- a POINTER as a DO variable;
- kinds: INTEGER and LOGICAL are kind 4 (and the extensions INTEGER*2
  and LOGICAL*1, which have no kind number), REAL 4 and 8, COMPLEX 4
  and 8.

Not diagnosed -- a program that breaks these rules compiles, and does
what it does:

- PURE's constraints, and that a procedure a FORALL references is PURE;
  a store into an INTENT(IN) dummy;
- a FORALL that assigns one element twice, a FORALL stride or a DO step
  that is zero only at run time, a LOGICAL constant as the bound of an
  implied DO among constants;
- a GO TO into the body of a DO, an IF, a WHERE or a FORALL; a DO
  variable redefined while its loop is active.

INTEGER*2 and LOGICAL*1 are storage widths, not kinds: a variable, an
array element or a FUNCTION's result of the type is written at its width
(two bytes or one in an unformatted record, seven columns in a list), and
anything computed from one -- S + S, ABS(S), (/ S, S /) -- is an INTEGER.
gfortran keeps kind 2 where every operand has it.

Processor-dependent and left different from gfortran: the layout of
NAMELIST output, the unit STOP's message goes to.

Where gfortran departs from the standard and f77 does not, the tests pin
f77's output (`<name>.expected`) instead of asking the oracle:

- `x%p(2:6) = a(1:5)` with `x%p => a`, and `e1(2:6) = e2(1:5)` in a
  FORALL with `EQUIVALENCE (e1, e2)`: gfortran stores without a
  temporary;
- a FORALL inside a FORALL whose inner bounds name the outer index, with
  a mask or reading what it stores: gfortran 14 reads a freed
  temporary, and its executable dies or prints garbage;
- a FORALL whose body calls a PURE function that reads the assigned
  array by host or use association: gfortran lets the function see the
  stores;
- a FORALL statement as the last statement of a labelled DO: gfortran
  refuses it.

What would come next if this resumes: the gfortran corpus
(regression/run-flang-corpus.sh's programs, built by f77 with recorded
verdicts) is the measure this plan promised and has not been set up; and
Fortran 2003 is not started and waits for a ruling.

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
