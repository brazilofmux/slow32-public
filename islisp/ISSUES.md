# islisp/ -- open items and post-mortems

The in-tree engineering log for the ISLISP interpreter (see
`docs/plans/islisp.md`).  Cite entries as `islisp ISSUES-N`.

## Open

### 1. Performance has not been measured

The evaluator walks the read forms directly and re-expands every macro
form at every evaluation; variable lookup is a linear alist walk per
frame; every object is a `malloc`.  Nothing has been profiled.  The
plan's `bench/` (tarai and fib from the oracle's bench directory, counts
under both SLOW-32 builds) is the next step before any of it is touched.

### 2. `call-next-method` and `next-method-p` are special operators

The text makes them local functions with indefinite extent.  They are
implemented as special operators that find the method context bound
lexically in the environment, which gives the extent (a closure made in
a method keeps its context) but not the function value: `(function
call-next-method)` and `(apply #'call-next-method ...)` do not work.
The suite does not ask for them.

### 3. Binary streams carry bytes only

`open-*-file` accepts an element class of 8, 16, 32 ... bits and
`read-byte`/`write-byte` move one byte.  A 16- or 32-bit element
stream should move that many bytes per element (§27.3).

### 4. The reader keeps whole tokens before classifying them

A token is read with its escapes resolved and case folded, then tried
as a number; a symbol written as `|123|` is therefore the symbol, as it
should be, but a symbol such as `1+` relies on the number parser
rejecting it rather than on the §6 lexeme grammar.  Correct for the
suite; a lexeme-by-lexeme reader would be the proper shape.

### 5. `format` column tracking is per stream, not per line of input

`~&` and `~nT` use the column the stream's own writes left it at.  A
program that writes through two streams to one file, or that reads
from the terminal, confuses it.  §27.2 allows "at least one space" when
the column is unknown, which is what `~T` emits when already past the
column.

### 6. `lisp/` is still in the tree

The Scheme-flavoured toy predates this front.  Plan milestone 8 retires
it once `islisp.s32x` is in the kit and the containers.

## Resolved

(none yet)
