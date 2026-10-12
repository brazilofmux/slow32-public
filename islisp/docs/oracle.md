# The oracle, and where we part from it

The reference implementation is Easy-ISLisp (Kenichi Sasagawa, BSD-2),
run in the `slow32:islisp-oracle` image (`Dockerfile.islisp-oracle`,
pinned by commit) through `tests/oracle.sh`.  Its `verify/` directory is
the conformance suite; `tests/run-verify.sh` runs it through our
interpreter and requires no more failures than the oracle's own run has
(two, in `misc.lsp`, both the clock functions).

Where the standard (ISLISP Working Draft 23.0, the public-domain source
of ISO/IEC 13816:2007) leaves a choice open, this processor takes the
oracle's, so that a byte diff of program output against it means
something.  Where the oracle departs from the text, the text wins, and
the departure is recorded here.  `tests/verify-exceptions.txt` lists the
individual suite checks that encode the oracle rather than the text.

## Conventions taken from the oracle (the text leaves them open)

- **Neutral alphabetic case is upper.** `foo` and `FOO` read as the same
  symbol, printed `FOO`; `|foo|` is a different symbol (§18.1.2).
- **`nil` prints as `NIL`** (§7 allows `nil` or `()`).
- **No negative zero.** `-0.0` reads as `0.0` and `(* -1 0.0)` is `0.0`
  (§13.3 allows either; the suite expects this).
- **The nil specializer.** `(x nil)` in a parameter profile names the
  class `<null>`.
- **Repeated class options.** `(:abstractp nil) (:abstractp nil)` is
  accepted; a conflict is a program error.
- **`<error>` in the suite means "some error".** Our `test` module's
  `$error` accepts any error class where the suite names `<error>`
  itself, and requires an exact match for any more specific class.
- **Gensyms print as `#:G1`**, uninterned.
- **`condition-continuable` returns the continue string** of a `cerror`
  (§29 says "a string describing the effect of continuing").
- **A handler that returns normally from a continuable condition**
  continues with its value (§29.2 leaves a normal return undefined;
  the suite's `unwind.lsp` re-signals from an inner handler and uses
  what the outer handler continued with).
- **`continue-condition` on a non-continuable condition** signals a
  plain `<error>` (the text names no class).
- **Index errors.** An index that is not a non-negative integer is a
  `<domain-error>`; one past the end is `<program-error>`
  (index-out-of-range).  A `char-index`/`string-index` start position
  must be below the length.
- **Float underflow is an error** (`<floating-point-underflow>`) when a
  non-zero computation lands at zero or in the denormal range, in
  `*`, `quotient`, `reciprocal`, `expt`, `exp` and in reading a literal
  like `1.0e-1000`.
- **`(class <name>)` with an undefined name** is `<undefined-entity>`
  with namespace `class`.
- **Calling a generic function with no applicable method** is
  `<undefined-function>`; so is `call-next-method` outside a method.
- **Only the `<error>` classes among the built-ins can be `create`d**;
  `<serious-condition>` and `<storage-exhausted>` cannot.
- **A huge allocation request** (`create-list 1000000000000`) is
  `<storage-exhausted>`; a negative one is `<domain-error>`.

## Where the text wins over the oracle

- **Floats print shortest-round-trip**, in the §7 syntax: `123456.789`,
  `1000000.0`, `1.5e-7`.  The oracle prints C's `%g` with six digits
  (`123457`, `1e+06.0`, `1.5e-07`), which loses information and is not
  the standard's float syntax.  Two suite checks compare against the
  oracle's printed value and are excepted (`number`, `bignum`).
- **`~S` of a string is `"abc"` and of a character `#\a`** (§7's
  examples).  The oracle writes `\"abc\"` and `#\\a`.
- **`create` is a generic function** (§15.4: "generic function"), an
  instance of `<standard-generic-function>`; the oracle's is an
  ordinary function.  One suite check excepted.
- **`<integer>` is one class.**  The oracle's `<fixnum>`, `<longnum>`,
  `<bignum>` and `<invalid>` are not the standard's; the suite's checks
  naming them are excepted.
- **`(convert 3.7 <integer>)` is an error** (`-(1)` in the §17 table);
  the oracle agrees.  `(convert #\a <string>)` is an error (`-(4)`).
- **`~A` prints without escape characters throughout** (§27.2: "as it
  would with ~S, but without escape characters"), so a string inside a
  list prints bare.  The oracle keeps the quotes of nested strings.
- **A generic function is a function** (§15.2) and can be passed to
  `mapcar`, `apply`, `funcall`; the oracle refuses it in `mapcar`.

## Extensions (compliance clause (d))

Beyond the standard, this processor provides: `import` (loads
`<libdir>/<name>.lsp`; the suite's files begin with `(import "test")`),
`load`, `eval`, `gc`, `exit`, `sleep`, `system`, `finish-output`, the
constants `*positive-infinity*` and `*negative-infinity*`, and the
character names `#\tab`, `#\return`, `#\null`.  Nothing else of the
oracle's library is provided; `unicode.lsp`, `hiroi.lsp`, `arvyy.lsp`,
`elixir.lsp`, `noble.lsp`, `matrix.lsp` and the `stress*.lsp` files are
the oracle's own and are not in the gate.
