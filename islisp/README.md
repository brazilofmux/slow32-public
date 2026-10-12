# ISLISP for SLOW-32

An interpreter for ISLISP, the ISO Lisp (ISO/IEC 13816:2007), written in
C99 and built three ways: by the host compiler for development, by
clang for SLOW-32, and by the self-hosted stage08 compiler for SLOW-32
again.  The plan and its rulings are in
[`docs/plans/islisp.md`](../docs/plans/islisp.md).

Status (2026-10-12): **the ISLISP verification suite passes, 23 files
and 9,585 checks**, on the host build and on the SLOW-32 build under
the DBT, and the stage08-built interpreter prints the same bytes as the
clang-built one over all of it.  Every chapter of the standard is implemented: the reader and
printer, the evaluator with its special operators, bignums and floats,
strings, characters, vectors and arrays, the sequence functions, the
object system with standard method combination, the condition system,
streams, files and `format`.  See
[`docs/oracle.md`](docs/oracle.md) for the conventions taken from the
reference implementation and the places the text wins over it.

## Layout

    src/isl.h        the object model: tagged words, heap objects, the API
    src/heap.c       objects, the collector, the root stack
    src/symbol.c     the symbol table (neutral case upper)
    src/reader.c     §6-7: the reader
    src/printer.c    §7: the printer, shortest-round-trip floats
    src/number.c     §19: integers of any size, floats, the number functions
    src/eval.c       §12, §14: the evaluator, special and defining forms,
                     non-local exits, dynamic variables
    src/cond.c       §29: conditions, signalling, the error-id table
    src/class.c      §10, §15: classes, instances, generic functions
    src/stream.c     §26-28: streams, files, format
    src/seq.c        §20, §22-25: characters, arrays, vectors, strings, sequences
    src/builtin.c    §13, §21, §18, §30: predicates, lists, symbols, misc
    src/main.c       the driver
    lib/test.lsp     the verification suite's harness macros, our module
    tests/           the gate, the oracle wrapper, two programs

## Building and running

    ./build.sh                 out/islisp-host and out/islisp.s32x (clang)
    ./build.sh host            the host binary only
    ./build.sh stage08         out/islisp-stage08.s32x through the kit (~/s32x)

    ./out/islisp-host prog.lsp                       on the host
    ../tools/dbt/slow32-dbt out/islisp.s32x prog.lsp  on SLOW-32
    ./out/islisp-host                                a read-eval-print loop

`islisp [-l libdir] file...` runs each file's toplevel forms in order;
`-l` (or `ISLISP_LIB`) names the directory `import` searches, default
`lib`.  An unhandled error prints `Error: <class>: ...` on the error
output and ends the run with status 1 (§9.1 leaves this to the
implementation).  `ISLISP_TRACE_TOP=1` prints each toplevel form as it
is read; `ISLISP_LINEBUF=1` line-buffers the output; both are for
finding where a run stops.

## Tests

    tests/run-tests.sh         the gate: the suite on both builds, the
                               stage08 byte-identity leg, the programs
                               against the oracle
    tests/run-verify.sh        the suite alone; --host / --s32x / --stage08,
                               and file names (list number object ...)
    tests/oracle.sh prog.lsp   run a program through the oracle image

The suite is Easy-ISLisp's `verify/` directory, a port of the ISLisp
verification system: it lives in `~/refs/eisl`, never in this tree.
Each file announces its chapters and prints one line per failed check;
a file passes when it runs to completion with no more failure lines
than the oracle's own run has, after `tests/verify-exceptions.txt`,
which names the handful of checks that encode the oracle rather than
the text.  Our `lib/test.lsp` turns an error signalled by a form under
test into a failure line instead of a stop.

The oracle image is built with

    podman build -t slow32:islisp-oracle -f Dockerfile.islisp-oracle .

from the repo root.  Without it, `run-verify.sh` assumes an oracle
count of zero failures per file, and `run-tests.sh` checks the programs'
exit status alone and says so.

## Implementation notes

- Values are tagged words: fixnums in 31 bits (the same range on the
  64-bit host so the builds agree on when a bignum appears), characters
  as immediates, everything else a pointer to a header-carrying object.
- The collector is a precise mark-sweep over an explicit root stack --
  which doubles as the argument stack, so evaluated arguments are
  protected for free -- with a conservative scan of the C stack and the
  registers underneath it.  The scan is the safety net for a C local
  that holds a fresh object across an allocation without having been
  pushed: the class of bug that is otherwise found one wrong answer at
  a time.  Objects are `malloc`'d one by one and threaded on a list for
  the sweep; an address set makes the stack scan precise about what is
  a heap pointer.
- Non-local exits (`block`, `catch`, `tagbody`, `unwind-protect`,
  handlers, continuable signals) are a chain of `setjmp` frames.  Block
  names and tagbody tags are bound lexically to a fresh tag object per
  activation, so `return-from` and `go` find the right frame even when
  a same-named block is active deeper in.  Frames being left while an
  `unwind-protect` cleanup runs are disestablished first, so a `go` or
  `throw` into them from the cleanup is a control error (§14.7.2).
- Method dispatch builds a per-call context and gives each invoked
  method its own copy, bound in the method body's environment; a
  closure made inside a method can therefore `call-next-method` after
  the method has returned, with the right next method (§15.3.4,
  indefinite extent).
- The evaluator is a direct evaluator over the read forms with a
  tail-call loop.  Macros are expanded at every evaluation of a macro
  form.  Nothing has been measured yet; `ISSUES.md` carries the list.
