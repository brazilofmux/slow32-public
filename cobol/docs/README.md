# COBOL docs

The requirements and rulings `s32-cobc` was built to, and the record
of how it got there. [../README.md](../README.md) is the overview and
the reading order.

| document | job |
|---|---|
| [requirements.md](requirements.md) | product test, dialect, v1 done, deliberately out |
| [architecture.md](architecture.md) | pipeline, IR, which compiler toys earn their keep |
| [standards.md](standards.md) | standards first, dialects second; the `-std` rows; what is out by ruling |
| [behavior-points.md](behavior-points.md) | the `bp()` registry behind `-std`, `-dialect` and `-warn-74` |
| [dialect.md](dialect.md) | COBOL 85 vs cobc370's 74 vs majesty's GnuCOBOL |
| [conformance/](conformance/README.md) | the rule-by-rule pages and the generated coverage.md |
| [refusals.md](refusals.md) | what the compiler refuses, and what the message says |
| [framing.md](framing.md) | RDW, line sequential, delete bytes, blocking |
| [indexed.md](indexed.md) | keyed files; dBase machinery vs dBase files |
| [faults.md](faults.md) | fault injection: the I/O error paths |
| [usage.md](usage.md) | COMP, COMP-n, byte order, floating point |
| [wide.md](wide.md) | thirty-one digits; standard-decimal arithmetic |
| [national.md](national.md) | national data, UTF-16 |
| [boolean.md](boolean.md) | the 2002 BOOLEAN module |
| [typedef.md](typedef.md) | TYPEDEF and TYPE |
| [functions.md](functions.md) | user-defined functions under `-std=2002`, and the C bridge |
| [screen.md](screen.md) | SCREEN SECTION as a focus state machine |
| [adis-observed.md](adis-observed.md) | what Micro Focus's ADIS does, key by key (black-box evidence) |
| [report-writer.md](report-writer.md) | page engine; majesty's subset; cobc370 lessons |
| [esql.md](esql.md) | EXEC SQL on SQLite and PostgreSQL |
| [lowering.md](lowering.md) | conversion matrix, hot cases, libcob |
| [performance.md](performance.md) | where a step's time went (libc, the divider, libcob), the profiling recipe, the levers not taken |
| [borrowing.md](borrowing.md) | what to take from cobc370, and what not to |
| [oracles.md](oracles.md) | GnuCOBOL, gcobol, the historical compilers, and where each is wrong |
| [majesty-corpus.md](majesty-corpus.md) | the programs that defined done |
| [plan.md](plan.md) | Stages 1-63, the 85 compiler, in order |
| [plans/](plans/) | the work queues: standard-queue.md (the road to 2023), locale.md, performance.md, census.md, hir.md, frontend-pass.md, screen-input.md |
| state-2026-09-30.md, state-2026-10-03.md | dated state reports, not kept current |
| [open-questions.md](open-questions.md) | decisions not yet ruled |

Prose style is cobc370's: measured claims, refuse-with-a-message,
the standard as authority and an implementation as oracle.
