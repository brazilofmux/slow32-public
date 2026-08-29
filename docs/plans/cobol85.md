# COBOL 85 on SLOW-32

Status: **specifications only** (2026-08-29). No compiler yet.

The documents live in [`cobol/`](../../cobol/README.md), next to
the language the way `fortran/` holds f77. This note is the desk
catalog entry.

## Why this one

`1987-desk.md` §8: languages only if they change the job. GnuCOBOL
is a compiler story; a COBOL that speaks this machine's files is a
business story. The product test is **retiring GnuCOBOL from
`~/majesty`'s report path** — gl022, gl023, gl030 first — not a
CCVS-85 score and not a port that emits C.

`~/cobc370` is COBOL 74 for MVS 3.8j and is a sibling, not a
parent. The 74/85 differences are subtle and damning; there is no
shared parser.

## Rulings (summary)

Full text in `cobol/docs/`. Short form:

- Host cross-compiler, ordinary universe, SLOW-32 only.
- IR is the symbol table. No SSA/BURG as the program IR. Ragel on
  PICTURE; recursive descent; conversion matrix; parameterized
  runtimes.
- ASCII. LINE SEQUENTIAL and SCREEN SECTION are implementor
  modules in the dialect because majesty writes them.
- Sequential V on disk is IBM RDW (tapemgr's 4-byte prefix), not a
  newline. Newline is LINE SEQUENTIAL. A dBase delete byte is a
  third thing and is optional.
- Indexed I-O reuses dBase *machinery* (slots, btree). File-level
  `.DBF` compatibility is a nice-to-have filter, not the default
  path.

## Stages

See [`cobol/docs/plan.md`](../../cobol/docs/plan.md). Nothing in
`cobol/src/` until Stage 1 opens.
