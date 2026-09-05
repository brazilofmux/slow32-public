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
    OUTFIL  FNAMES=(dd,...)[,INCLUDE=(..)|OMIT=(..)|SAVE][,OUTREC=(items)]
            [,STARTREC=n][,ENDREC=n][,SPLIT|SPLITBY=n]
    JOINKEYS F1=dd,FIELDS=(p,l,o,...)[,SORTED][,LENGTH=n]
    JOINKEYS F2=dd,FIELDS=(p,l,o,...)[,SORTED][,LENGTH=n]
    JOIN    UNPAIRED[,F1][,F2][,ONLY]
    REFORMAT FIELDS=(F1:p,l | F2:p,l | ? ...)[,FILL=C'c'|X'hh']
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

**OUTFIL**: any number of statements. After the sort (and SUM and the
main OUTREC), every record goes to each OUTFIL whose INCLUDE/OMIT it
passes and whose STARTREC..ENDREC window (counted over the sorted
records) it falls in; `SAVE` takes the records no other OUTFIL took;
`SPLIT` rotates the FNAMES one record at a time, `SPLITBY=n` n at a time;
an OUTFIL's own OUTREC reformats what it writes. SORTOUT, when given,
still receives everything. The FNAMES are data sets named on the command
line, `NAME=path`.

**JOINKEYS**: F1 and F2 are each sorted on their FIELDS (skipped for a
side marked SORTED), then paired by equal keys -- every F1 record of a key
with every F2 record of it. `JOIN UNPAIRED,F1` adds the F1 records with no
match (F2 fields filled), `UNPAIRED,F2` likewise, `UNPAIRED` both, `ONLY`
drops the paired ones. REFORMAT builds the joined record from F1 and F2
fields and `?`, the indicator `B`, `1` or `2`. The joined records then run
through the main task: `SORT FIELDS=COPY` keeps them in key order, or sort
them, INCLUDE them, SUM them, OUTFIL them. F1/F2 record lengths default to
RECORD LENGTH; `LENGTH=n` on a JOINKEYS statement sets one side's.

Messages and the record counts go to stderr; exit 16 on any error.

## Not there

Column positions in OUTREC (`c:`), OUTFIL `OUTREC` overlays and reports
(HEADER/TRAILER/SECTIONS), JOINKEYS INCLUDE/OMIT (filter in a pass
before), `ICETOOL`, `ALTSEQ`, symbols, `Y2x` formats, `VLSHRT`. Each is
a day's work when a job needs it; nothing does yet.
