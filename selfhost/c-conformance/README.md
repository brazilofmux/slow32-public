# C11 conformance of stage08 cc

> Status 2026-10-29: stage08 cc claims C11; 320 PASS, 215 DIAG,
> 9 ACCEPTS, 3 OPTIONAL, 0 WRONG, 0 REJECTS (`verdicts.txt`).

How far the self-hosted compiler (`selfhost/stage08`, front end in
`selfhost/src`) is from C11 -- ISO/IEC 9899:2011, chapter 6, the
language -- clause by clause.  The same idea as COBOL's
`cobol/docs/conformance/`: every subclause gets a disposition, backed
by a program that shows it.

The text is read from WG14's N1570, the final committee draft of C11
(C17 is C11 with defect fixes; clause numbers are the standard's).  It
is free to download from open-std.org but remains (c) ISO/IEC, so it is
not in the tree: these pages cite clause and paragraph (`6.7.5p3`) and
paraphrase, never quote.

## The harness

    ./probe.sh                      # every probe
    ./probe.sh probes/6.7.5-*.c     # some
    ./probe.sh -v probes/x.c        # with stage08's error and the diff
    ./probe.sh --check              # every probe against verdicts.txt (the gate)
    ./probe.sh --update             # record this run as verdicts.txt

A probe is a small C program whose first line names the clause it
exercises.  clang (`-std=c11 -pedantic-errors`) decides whether it is
valid C11; both builds run on the reference interpreter and their output
and exit status are compared.  `// expect: reject` marks a program that
breaks a constraint, which clang must also refuse.  `// expect: warnings`
marks a valid program on which clang's default warnings fire: stage08
must warn on the same lines.  `// optional: __STDC_NO_X__` marks a probe
of an optional C11 feature.  The verdicts:

| verdict | meaning |
|---|---|
| PASS | valid; stage08 builds it and prints what clang's build prints |
| REJECTS | valid; stage08 refuses it -- a gap |
| WRONG | valid; both build and the results differ -- wrong code |
| DIAG | invalid; stage08 refuses it, as it must |
| ACCEPTS | invalid; stage08 takes it without a word -- a missing diagnostic |
| OPTIONAL | a probe of an optional C11 feature stage08 declares absent (`// optional: __STDC_NO_X__`) -- a choice, not a gap |
| WARNS | `// expect: warnings`: PASS, but stage08's warnings fall on other lines than clang's |
| ORACLE | the probe and clang disagree about validity: the probe is wrong |
| HANG | a build or a run passed its time limit |

clang is the oracle for validity and behaviour where the two are the
same thing.  Where the standard leaves a choice to the implementation
(sizes, signedness of plain `char`, evaluation order), a probe must not
depend on it, or must say which choice it expects and why.

## The pages

| clauses | page |
|---|---|
| 6.2 Concepts, 6.3 Conversions, 6.4 Lexical elements | [c11-6.2-6.4.md](c11-6.2-6.4.md) |
| 6.5 Expressions, 6.6 Constant expressions | [c11-6.5-6.6.md](c11-6.5-6.6.md) |
| 6.7 Declarations | [c11-6.7.md](c11-6.7.md) |
| 6.8 Statements, 6.9 External definitions | [c11-6.8-6.9.md](c11-6.8-6.9.md) |
| 6.10 Preprocessing directives, 6.11 Future directions | [c11-6.10.md](c11-6.10.md) |

[coverage.md](coverage.md) is the first survey's ranked gap list
(2026-10-28) and the progress log since; today 320 PASS, 215 DIAG,
9 ACCEPTS (rulings), 3 OPTIONAL (complex types), 0 WRONG, 0 REJECTS.

## Scope

Chapter 6 only.  The library (chapter 7) is a separate axis, and the
two C libraries in the tree already have a differential against each
other and the host's (`regression/run-libc-differential.sh`); a header a
probe needs and stage08's `include/` lacks is noted, not surveyed.
stage07 is frozen and out of scope.
