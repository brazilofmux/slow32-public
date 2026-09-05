# dfsort -- s32sort, a DFSORT-shaped sort/merge for SLOW-32

    tools/dbt/slow32-dbt dfsort/out/s32sort.s32x SORTIN=in.txt [SORTIN=in2.txt ...] SORTOUT=out.txt SYSIN=deck [MAINSIZE=4M]

One C file, `s32sort.c`, on the COBOL runtime's external sort engine
(`cobol/libcob/xsort.h`): records arrive, their fields render once into a
byte string whose unsigned order is the requested order, the engine keeps
as many as MAINSIZE allows (default: half the heap the program was linked
with -- there is no sbrk on SLOW-32), spills sorted runs beside SORTOUT and
merges them k ways. Equal keys keep input order. `build.sh` builds it for
SLOW-32 and for the host from the same source; `tests/run.sh` runs every
deck in `tests/decks/` on both and compares the bytes, then against the
checked-in `.expected`.

## Control statements (SYSIN)

    SORT   FIELDS=(p,l,f,o[,p,l,f,o]...)      MERGE FIELDS=(...)      SORT FIELDS=COPY
    RECORD TYPE={F|L},LENGTH=n
    INCLUDE COND=(p,l,f,op,{p,l,f | C'..' | X'..' | [+-]n}[,{AND|OR},...])
    OMIT    COND=(...)                        op: EQ NE GT GE LT LE; AND binds tighter than OR
    SUM     FIELDS=(p,l,f,...) | FIELDS=NONE
    INREC   FIELDS=(item,...)                 OUTREC FIELDS=(item,...)   item: p,l | nX | C'..' | X'..'
    OPTION  ...                               accepted; EQUALS is always in effect
    *  comment

`p` counts from 1. `o` is `A` or `D`. Formats: `CH` (bytes; `AC`, `AQ`
the same), `ZD` (zoned decimal: overpunched sign, trailing or leading
`-`, EBCDIC zones), `PD` (packed), `BI` (unsigned big-endian binary),
`FI` (signed), `FS`/`CSF` (a signed decimal string).

`RECORD TYPE=F,LENGTH=n` is fixed records; `TYPE=L` is text lines, padded
with blanks to LENGTH for the sort and written back trimmed -- the form
most SLOW-32 files take, and not in DFSORT, whose variable records carry
an RDW. `SUM` collapses records equal on the sort fields into the first,
with the named numeric fields summed (`NONE`: kept as is). `INREC`
reformats before the sort, `OUTREC` after.

Messages and the record counts go to stderr; exit 16 on any error.

## Not there

Column positions in OUTREC (`c:`), `OUTFIL`, `JOINKEYS`, `ICETOOL`,
`ALTSEQ`, symbols, `Y2x` formats, `VLSHRT`. Each is a day's work when a
job needs it; nothing does yet.
