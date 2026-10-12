# ISLISP on SLOW-32

Status: **the verification suite passes, 23 files and 9,585 checks
(2026-10-12), on the host build and on the SLOW-32 build.**  Milestones
1 through 6 landed in one pass of 2026-10-11/12, and the stage08 leg of
milestone 7 with them: the interpreter built by the kit's stage08 cc
prints the same bytes as the clang build over the whole suite.  `islisp.s32x` is
in the kit and `lisp/` is retired (2026-10-12); the images pick it up
on the next builder run.
`islisp/README.md` has the layout and the commands, `islisp/docs/
oracle.md` the conventions taken from the oracle and the places the
text wins over it.

## Why this one

`docs/plans/1987-desk.md` §8 is "Languages, only if they change the
job", and the user's rule for a language since 2026-10-11 has three
tests: is it part of their own history, would they use it, and does it
stress stage08 and the DBT in a way C and COBOL do not. "Standard ISO
Lisp" has been in the queue since the desk was written; it passes the
third test outright and the first two well enough.

What is in the tree today under `lisp/` is a 1,500-line Scheme-flavoured
toy: fixnums only, `display`/`define`/`lambda`, a mark GC, seventeen
tests. It is not a Lisp anyone would write a program in, and it is not
what the queue item meant. ISLISP is ISO/IEC 13816:2007, a small,
object-oriented, industrially minded Lisp-2 with a condition system,
generic functions, dynamic variables, arrays, streams and `format` --
about 134 pages, which is to Common Lisp what COBOL 85 is to COBOL 2023:
the part with widespread agreement, and a size one person can finish.

### What it stresses that the others do not

A Lisp runtime is a different workload from everything stage08 has been
built against: tagged words, a precise mark-sweep collector over a heap
of small objects, deep recursion in the evaluator, `setjmp`/`longjmp`
for every non-local exit (`block`/`return-from`, `catch`/`throw`,
`tagbody`/`go`, `unwind-protect`, condition handlers), closures over
linked environments, bignum arithmetic, and double-precision floats
printed shortest-round-trip. stage08's `setjmp` is a week old (selfhost
ISSUES-75); nothing in the tree has leaned on it yet. The interpreter is
therefore built **twice** -- by clang and by stage08 cc -- and the two
executables must print the same bytes over the whole conformance run,
the SQLite pattern (`sqlite/check-stage08.sh`). That is the standing
stage08 gate this front adds.

## Materials

All references live under `~/refs`, never in a git tree (the standing
rule for FCVS and the NIST SQL suite applies).

- **The standard text.** ISO/IEC 13816:2007 is sold by ISO; its source
  document, *ISLISP Working Draft 23.0* of 2007-03-17, is public domain
  and is what every implementation works from. `~/refs/islisp/
  islisp-2007-03-17-pd-v23.pdf` (134 pages, from islisp.info), with
  `pdftotext -layout` output beside it as `islisp-v23.txt`; the 1997
  draft 20.3 is there too. Cite by section and page (the verification
  files already do: `P.83`).
- **The oracle.** Easy-ISLisp (Kenichi Sasagawa, BSD-2, v5.69 of
  2026-10-09) is an interpreter and C-generating compiler that tracks
  the standard closely. It is Linux-only (`extension.c` wants
  `<linux/fb.h>`), so it runs in `slow32:islisp-oracle`
  (`Dockerfile.islisp-oracle`, Debian + eisl at a pinned tag), driven by
  `islisp/tests/oracle.sh prog.lsp`, exactly as gfortran does for f77.
  The clone is at `~/refs/eisl`.
- **The conformance suite.** eisl's `verify/` directory is a port of the
  ISLisp verification system (the WG's "about 16,000 items"; the
  original site is dead, its domain expired): 44 files, **8,120 `$test`
  and 1,529 `$error` checks plus 466 `$eval` set-ups**, each announced
  with the chapter and the draft's page number. `$test form expected
  [pred]` compares a result (with `equal` by default); `$error form
  <class>` asserts that evaluating `form` signals a condition of
  exactly that class -- so the condition classes of Figure 1 and the
  signalling points of §9 and §29.4 are part of the contract from the
  first day, not a finish. `number.lsp` (3,669 checks) and `object.lsp`
  (3,000) are half the suite between them. A handful of files are
  eisl's own (`hiroi`, `arvyy`, `elixir`, `noble`, `matrix`, the
  `stress*`, `unicode`) and import eisl libraries; they are not in the
  gate.

## Rulings and decisions

Taken 2026-10-11 by me, under the user's standing rules, to be revised
by the user:

1. **A new `islisp/`, beside `lisp/`.** The toy stays untouched until
   `islisp.s32x` is in the kit and does everything it does; then
   `lisp/` is retired in one commit. Nothing is grown from it: the
   value representation, GC, symbol model and evaluator are different
   enough that starting over costs less than converting.
2. **An interpreter in C, in the tree's ordinary universe.** The host C
   compiler builds it for development, clang builds it for SLOW-32,
   and stage08 cc builds it for SLOW-32 again (the gate). Not a
   compiler to HIR: ISLISP's semantics -- dynamic types, generic
   dispatch, conditions, a collector -- want a runtime first, and
   eisl's own compiler shows the shape of a later step (Lisp to C,
   then any of our C compilers). Not a bytecode VM either, for now;
   `lisp/ISSUES.md` wanted one for the toy, but the measurements come
   first.
3. **Representation.** 32-bit tagged words: fixnums in 31 bits (tag
   bit 0), characters as immediates, everything else a pointer to a
   header-carrying heap object (cons, symbol, string, float, bignum,
   vector, array, closure, builtin, generic function, method, class,
   instance, stream, environment). A precise mark-sweep collector with
   a shadow root stack (the toy's one good idea, kept) that doubles as
   the argument stack -- plus, decided on 2026-10-12 after a
   non-deterministic failure in `cond.lsp`, a conservative scan of the
   C stack and registers as the safety net for an unpushed local.
   Objects are `malloc`'d one by one for now; size classes wait for a
   measurement. Lisp-2: a symbol carries separate function,
   global/constant, dynamic and property cells.
4. **Numbers.** §19.3 is explicit: integer arithmetic "shall simulate
   any necessary operations in software in order to assure
   mathematical correctness". So fixnum + bignum from the start
   (magnitude arrays of 32-bit limbs, Knuth D for division; the verify
   suite's `bignum.lsp` and much of `number.lsp` need it), and
   `<float>` is IEEE double, printed shortest-round-trip through the
   runtime's `dtoa`. SLOW-32 has hardware double arithmetic, and the
   transcendental functions go to libm names the DBT intercepts, as
   f77 does.
5. **Characters are bytes.** §6 makes the character set beyond ASCII
   implementation-defined. `unicode.lsp` is eisl's, not the
   standard's, and is out of the gate.
6. **Non-local exits are `setjmp`/`longjmp`** with a dynamic-extent
   chain of frames (block, catch, tagbody, unwind-protect, handler),
   unwound in order. This is the stage08 stress and is chosen partly
   for that.
7. **The reader is hand-written recursive descent**, not Ragel: Lisp's
   lexeme grammar is six lines (§6) and a scanner generator would cost
   more than it saves. The house tastes are for where they pay.
8. **Output conventions follow the oracle where the standard leaves
   them open** (`nil` prints as `NIL`, symbols in upper case, classes
   as `<class name>`), because the gate is a byte diff against it.
   Where eisl and the text disagree, the text wins -- floats print
   shortest-round-trip in the §7 syntax, not eisl's six-digit `%g`;
   `~S` of a string is `"abc"` -- and the disagreement is logged in
   `islisp/docs/oracle.md`, as `cobol/docs/oracles.md` does for
   GnuCOBOL. The suite checks that encode eisl rather than the text are
   listed in `islisp/tests/verify-exceptions.txt` (four lines).

## Gates

- `islisp/tests/run-tests.sh`: (1) self-checking programs under
  `tests/*.lsp` run on the emulator and byte-diffed against the oracle,
  exit 0 required on both (the Fortran shape, including the lesson that
  agreement alone is vacuous); (2) `verify/`: every in-scope file from
  `~/refs/eisl/verify` run with **our** `test` module (the `$test`,
  `$error`, `$eval`, `$ap` macros, some forty lines), the "is bad" lines
  counted, and the count required to be zero wherever the oracle's is;
  (3) the stage08 leg: the same programs through the stage08-built
  interpreter, bytes identical to the clang-built one.
- `tests/run-verify.sh` reports per file like `fortran/tests/
  run-fcvs.sh` reports per program: PASS / FAIL(n) / COMPILE-FAIL /
  RUN-FAULT, with the chapter headers as progress.
- A `bench/` with `tarai` and `fib` from eisl's own bench directory,
  instruction counts under the two builds, as a code-quality fact.

## Milestones

1. **Reader, printer, evaluator core.** Symbols (case folded to the
   neutral case, upper like the oracle's, per §18.1.2; `|...|` and `\`
   escapes),
   integers, floats, strings, characters, `#(` vectors, `#na` arrays,
   quote/quasiquote/unquote/splice, `#'`, comments both kinds. Lexical
   environments, `lambda`/`defun`/`labels`/`flet`/`function`,
   `let`/`let*`/`setq`/`setf` (the standard's places), `if`/`cond`/
   `case`/`case-using`/`and`/`or`/`progn`/`while`/`for`, `defglobal`/
   `defconstant`/`defdynamic`/`dynamic`/`dynamic-let`, `defmacro` with
   quasiquote, the predicates of §13 and the equality of `eq`/`eql`/
   `equal`. Gate target: `list.lsp`, `pred.lsp`, `cond.lsp`,
   `symbol.lsp`, `macro.lsp` clean.
2. **Non-local exits and the condition system.** `block`/`return-from`,
   `catch`/`throw`, `tagbody`/`go`, `unwind-protect`, `with-handler`,
   `signal-condition`, `continue-condition`, `error`/`cerror`,
   `ignore-errors`, the condition classes of Figure 1 with their §29.3
   data, and the error-id. table of §29.4 wired to the right class at
   every signalling point. Gate: `control.lsp`, `block.lsp`,
   `unwind.lsp`, `declare.lsp`, `formeval.lsp`.
3. **Numbers.** Bignums, floats, the whole of §19 (`quotient`,
   `reciprocal`, `isqrt`, `expt`, `gcd`/`lcm`, `float`/`floor`/
   `ceiling`/`round`/`truncate`, the transcendentals, `parse-number`,
   `*pi*`, `*most-positive-float*`), `convert`. Gate: `number.lsp`,
   `bignum.lsp` -- the biggest single step.
4. **Strings, characters, sequences, arrays.** §20, §22-25: `create-
   array`/`aref`/`garef`/`array-dimensions`, vectors, `string-append`/
   `string-index`/`char-index`, `subseq`, `map-into`, `elt`, `length`,
   `reverse`/`nreverse`, `sort`? (not in ISLISP), `mapcar`..`mapcon`,
   `assoc`/`member`, `maphash`? (no). Gate: `string.lsp`, `char.lsp`,
   `seq.lsp`, `array.lsp`, `vector.lsp`.
5. **ILOS.** `defclass` with `:initarg`/`:initform`/`:accessor`/
   `:reader`/`:writer`/`:boundp`, `:abstractp`, class precedence (the
   §15.1.1 rule), `defgeneric`/`defmethod` with `:before`/`:after`/
   `:around`, `call-next-method`/`next-method-p`, `create`/
   `initialize-object`, `class-of`/`instancep`/`subclassp`/`class`,
   built-in classes answering `class-of` and `instancep`. Gate:
   `object.lsp`, `ilos.lsp` (3,011 checks).
6. **Streams, I/O, files, `format`.** §26-28: standard streams, string
   streams, file streams over the MMIO libc (`with-open-*-file`,
   `open-*-file`, `file-position`, `file-length`, `probe-file`),
   `read`/`read-char`/`preview-char`/`read-line`/`read-byte`, `write-
   byte`, `format` with `~A ~S ~D ~B ~O ~X ~C ~% ~& ~T ~R ~G ~~` and
   `format-*`, `print`/`write`? (not in ISLISP). Gate: `io.lsp`,
   `stream.lsp`, `files.lsp`, `misc.lsp`.
7. **The stage08 leg and the kit.** `build-stage08.sh` through the kit
   toolchain; the two builds byte-identical over the verify run; the
   `tests/run-tests.sh` stage08 line; `islisp.s32x` into `~/s32x` and
   the slow32:kit image; `slow32:islisp` image beside `slow32:fortran`
   if wanted.
8. **Retire `lisp/`.** ✅ Done 2026-10-12, with the kit's fourteenth batch.

Each milestone ends with the verify counts in `islisp/README.md` and
in this file's status line, the way `fortran77.md` carries the FCVS
count.

## Out of scope, by ruling

eisl's extensions (its libraries, `import`, threads, FFI, the editor,
Unicode); a compiler (a later plan, modelled on eisl's); an interactive
debugger beyond "print the condition and the form, exit 1" for an
unhandled error, which §9.1 leaves implementation-defined.
