# cobol — COBOL 85 for SLOW-32

Status: **specifications only.** No compiler yet.

A host cross-compiler in the tree's ordinary universe (like `fortran/`
and `clip/`, not `selfhost/`). It reads COBOL 85 plus the implementor
extensions majesty already writes, and emits SLOW-32 assembler. Success
is retiring GnuCOBOL from `~/majesty`'s report path.

This is not a backend for [`~/cobc370`](../../cobc370/README.md).
cobc370 is COBOL 74 for MVS 3.8j and stays that way. The two compilers
may borrow ideas; they do not share a parser. The 74/85 differences are
subtle and damning.

## Why this one

`docs/plans/1987-desk.md` §8: *"GnuCOBOL is a compiler story; a COBOL
that `WRITE`s `.DBF` is a business story."* The refinement, 2026-08-29:
reuse the **machinery** of DBF/NDX (slots, btree, an honest delete-byte
when we opt in). File-level compatibility with dBase is a nice-to-have,
not an invariant. COBOL can legally describe records dBase cannot store.

The desk-changing job is majesty's general ledger: the same reports,
produced on this machine, without GnuCOBOL.

## Rulings

Settled in the 2026-08-29 design conversation. Defended in the docs
under `docs/`.

1. **Separate compiler.** No copy of `cobc370.c`. No shared front end.
2. **SLOW-32 is the only target.** x86-64 and aarch64 are reached
   through `slow32-dbt`, as with every other language here.
3. **Ordinary universe.** Host cross-compiler. Host `strtod`, host
   oracles, Ragel on the host. Not self-hosted.
4. **Not SSA/BURG.** COBOL is a data-description language with verbs,
   not an Algol. The IR is the symbol table. See
   [docs/architecture.md](docs/architecture.md).
5. **ASCII.** `HIGH-VALUE` is `0xFF`. No EBCDIC on this ISA.
6. **Framing is an FD fact.** Line sequential, RDW-framed V, fixed
   sequential, relative, and indexed are different. Do not conflate a
   newline with a record length. See [docs/framing.md](docs/framing.md).
7. **SCREEN SECTION is in the dialect** even though it is not in the
   1985 text. Majesty already writes it. See [docs/screen.md](docs/screen.md).

## Layout (now)

    README.md       this file
    docs/           requirements, architecture, plans — the whole compiler
                    until there is a compiler

Layout later, when code exists, is sketched in
[docs/architecture.md](docs/architecture.md). It is not created yet.

## Reading order

1. [docs/requirements.md](docs/requirements.md) — product, dialect, done
2. [docs/architecture.md](docs/architecture.md) — shape, toys, IR
3. [docs/plan.md](docs/plan.md) — stages
4. The rest, as the stage needs them
