# COBOL 85 specifications

No code lives in `cobol/` yet. These notes are the compiler until there
is one.

| document | job |
|---|---|
| [requirements.md](requirements.md) | product test, dialect, v1 done, deliberately out |
| [architecture.md](architecture.md) | pipeline, IR, which compiler toys earn their keep |
| [dialect.md](dialect.md) | COBOL 85 vs cobc370's 74 vs majesty's GnuCOBOL |
| [framing.md](framing.md) | RDW, line sequential, delete bytes, blocking |
| [indexed.md](indexed.md) | keyed files; dBase machinery vs dBase files |
| [screen.md](screen.md) | SCREEN SECTION as a focus state machine |
| [report-writer.md](report-writer.md) | page engine; majesty's subset; cobc370 lessons |
| [performance.md](performance.md) | where a step's time went (libc, the divider, libcob), the profiling recipe, the levers not taken |
| [lowering.md](lowering.md) | conversion matrix, hot cases, libcob |
| [functions.md](functions.md) | the C bridge, and the corpus rewrite that keeps COBOL 2002 out of the compiler |
| [borrowing.md](borrowing.md) | what to take from cobc370, and what not to |
| [oracles.md](oracles.md) | GnuCOBOL, the 1985 text, CCVS-85, majesty reports |
| [majesty-corpus.md](majesty-corpus.md) | the programs that define done |
| [plan.md](plan.md) | stages, in order |
| [open-questions.md](open-questions.md) | decisions not yet ruled |

Prose style is cobc370's: measured claims, refuse-with-a-message,
the standard as authority and an implementation as oracle.
