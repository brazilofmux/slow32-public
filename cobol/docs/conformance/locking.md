# File sharing and record locking: 9.1.15, 9.1.16, 12.4.5.9, 12.4.5.15, 14.7.9, the LOCK phrases, UNLOCK

Swept 2026-10-07 (docs/plans/standard-queue.md item 39, stage 1). The
2023 text: 9.1.15 file sharing and Table 19, 9.1.16 record locking,
12.4.5.9 LOCK MODE, 12.4.5.15 SHARING, 14.7.9 RETRY, 14.9.27 OPEN's
SHARING phrase, 14.9.30 READ's four phrases, 14.9.35 REWRITE and 14.9.51
WRITE's WITH [NO] LOCK, 14.9.10 DELETE, 14.9.41 START, 14.9.47 UNLOCK.
Optional in 2014 and 2023 (A.4.7); 2002's wording is the same.

**The ruling** (the queue, item 39): the syntax and the statuses first,
with every check made within the run unit; whether the locks ever reach
the host's file system is a later stage. So: *file connectors of one run
unit* are checked against each other -- the sharing modes at OPEN, the
record locks on every operation. Two run units on one file see nothing
of each other, as before.

**The default is the implementor's.** A file control entry with neither a
SHARING nor a LOCK MODE clause, opened without a SHARING phrase, takes
the implementor's sharing mode (12.4.5.15.4 rule 2, 12.4.5.9.4 rule
1b2): here *no sharing checks and no record locks*, so a program written
before this sweep behaves as it did. The checks begin when any connector
of the run unit is open with a sharing mode or a lock mode, and a
connector with neither is then taken as *sharing with all other*,
holding no locks: it is refused where Table 19 refuses it, it reads
another connector's locked record freely (12.4.5.9.4 rule 1).

Tests 2002/locking (indexed, relative and sequential files under every
form below) and 2023/lockdel (DELETE FILE, OPEN RETRY); no oracle:
GnuCOBOL's sharing checks and locks are between processes, none within a
run unit -- every status there is 00. Bad: std2002-lock-multiple-seq,
-lock-multiple-seqaccess, -read-lock-automatic, -rewrite-lock-automatic,
-read-ignoring-with-lock, -read-lock-nolock, -delete-with-lock,
-read-advancing-keyed, sharing-std85, retry-std85.

## File sharing (9.1.15, 12.4.5.15, 14.9.27)

| rule | paraphrase | disposition |
|---|---|---|
| 12.4.5.15, 14.9.27.2 | SHARING WITH ALL OTHER / NO OTHER / READ ONLY, as a clause or an OPEN phrase; the phrase wins (12.4.5.15.4 rule 3) | **implemented**: 2002/locking (both; `OPEN I-O SHARING WITH READ ONLY f2`) |
| 12.4.5.15.4 rule 2 | neither: the implementor's mode | no checks and no locks, above; a LOCK MODE clause alone implies *all other* with locks (the clause has to mean something) |
| 9.1.15, Table 19 | a connector of the run unit is open on the physical file: the new OPEN is refused -- (a) that one is *no other*, (b) this one is, (c) that one *read only* and this I-O or EXTEND, (d) this one *read only* and that I-O or EXTEND, (e) this OUTPUT, or that one OUTPUT | **implemented**: **61**, EC-I-O-FILE-SHARING; 2002/locking tries each. "The physical file" is the assigned name, compared as a string |
| 9.1.13.9 | DELETE FILE of a physical file open through another connector of the run unit | **62**, the file left alone: 2023/lockdel. The connector's own open file is still **41** |
| 14.9.27.3 | the phrase written before the mode (`OPEN SHARING ... I-O f`) | **refused** as before: the format puts the phrase after the mode, before the file names |
| -- | the statement phrases (OPEN SHARING, RETRY, the LOCK phrases) under -std=85 | **refused**: bad/sharing-std85, retry-std85. The two *clauses* under -std=85 are an extension, BP-E33 (behavior-points.md): majesty's 85 programs carry SHARING WITH ALL OTHER, as GnuCOBOL takes it |

## Record locking (9.1.16, 12.4.5.9)

| rule | paraphrase | disposition |
|---|---|---|
| 12.4.5.9.2 | LOCK MODE IS MANUAL / AUTOMATIC [WITH LOCK ON [MULTIPLE] RECORD(S)] | **implemented** |
| 12.4.5.9.3 rule 2 | no MULTIPLE for a sequential file or sequential access | **refused**: bad/std2002-lock-multiple-seq, -seqaccess |
| 12.4.5.9.4 rules 1a, 1b1 | a SHARING clause, or an OPEN SHARING phrase, without a LOCK MODE clause: no locks are set | **implemented**: f6 in 2002/locking reads a locked record |
| 12.4.5.9.4 rule 3 | sharing with no other: LOCK MODE has no effect | **implemented** (nobody else can be open) |
| 12.4.5.9.4 rule 4; 14.9.30.4 rule 11c | AUTOMATIC: every READ locks the record read | **implemented** |
| 12.4.5.9.4 rule 5; 14.9.30.4 rule 11d, 14.9.35.4, 14.9.51.4 rule 11 | MANUAL: a lock only by WITH LOCK, on READ, REWRITE or WRITE | **implemented** |
| 12.4.5.9.4 rule 6; 14.9.30.4 rule 11a; 14.9.51.4 rule 10 | single-record locking: any I-O statement but START releases the connector's lock, before its own | **implemented**: a REWRITE WITH LOCK moves the lock; a plain READ, WRITE, REWRITE or DELETE drops it |
| 12.4.5.9.4 rule 7 | MULTIPLE: the connector keeps every lock until UNLOCK or CLOSE; limits of at least 15 per connector and 255 per run unit | **implemented**: 255 per connector, 1024 per run unit; the operation that would pass a limit is **54** (connector) or **53** (run unit), EC-I-O-RECORD-OPERATION |
| 9.1.16; 14.9.30.4 rules 8-9, 14.9.10.4 rule 6, 14.9.35.4, 14.9.51.4 | a record locked by another connector: READ (keyed, or sequential of that record), REWRITE, DELETE, WRITE of it is **51**, EC-I-O-RECORD-OPERATION; the holder reads its own freely | **implemented**. The record: a relative file's number (the RELATIVE KEY item before a keyed operation, the number read after a sequential one), an indexed record's primary key, a sequential record's position |
| 14.9.41.4 rule 3 | START neither detects, acquires nor releases locks | **implemented** (RETRY is its only phrase) |
| 9.1.16; 14.9.6 | CLOSE releases the connector's locks | **implemented** |
| 14.9.47 | UNLOCK releases them (environment.md for its statuses) | **implemented**; the test's UNLOCK frees a record another connector then reads |

## The phrases (14.9.30, 14.9.35, 14.9.51, 14.7.9)

| rule | paraphrase | disposition |
|---|---|---|
| 14.9.30.2 | READ ... ADVANCING ON LOCK / IGNORING LOCK / WITH LOCK / WITH NO LOCK, any order | **implemented** |
| 14.9.30.4 rule 12 | IGNORING LOCK: the record is read although locked | **implemented** |
| 14.9.30.4 rule 10 | ADVANCING ON LOCK (sequential READ): the next unlocked record | **implemented**: 2002/locking's qb steps over qa's record |
| 14.9.30.4 rule 11b | WITH NO LOCK under multiple-record locking frees the record's own lock alone | **implemented** |
| 14.9.30.3 rule 3 | IGNORING LOCK and a LOCK phrase together | **refused**: bad/std2002-read-ignoring-with-lock; WITH LOCK with WITH NO LOCK: -read-lock-nolock |
| 14.9.30.3 rule 4, 14.9.35.3 rule 4, 14.9.51.3 rule 22 | no LOCK phrase under AUTOMATIC (READ: IGNORING LOCK neither) | **refused**: bad/std2002-read-lock-automatic, -rewrite-lock-automatic |
| 14.9.10.2, 14.9.41.2 | DELETE and START take RETRY alone | **refused**: bad/std2002-delete-with-lock |
| 14.9.30.2 | ADVANCING ON LOCK in format 1 alone (a sequential READ) | **refused** on a keyed READ: bad/std2002-read-advancing-keyed |
| 14.7.9 | RETRY n TIMES / FOR n SECONDS / FOREVER on OPEN, READ, WRITE, REWRITE, DELETE, START; n a literal, item or expression | **implemented** as the text allows an implementor whose locks cannot change between attempts: TIMES retries at once; FOR waits the seconds (at most 60) and reports; FOREVER waits the implementor's maximum, 60 s, and reports -- then **51** or **61** as without the phrase |
| 14.7.9 | RETRY under -std=85 | **refused**: bad/retry-std85 |

## Stage 2 (2026-10-08): one image per physical file

Two connectors of the run unit open on one indexed file share one image
of it -- the key file's tree and its page cache, the data file's slots in
memory, the one stream (libcob `idx_sh`, found by the assigned name;
`cob_idx` is now the connector's cursor alone) -- so a record written,
rewritten or deleted through one is what the other reads, and START
sees it (2002/locking, "one image for the two connectors"). An OPEN
OUTPUT starts an image of its own: the file is new. A relative file was
coherent already (each operation seeks its slot). A **sequential** file's
records appended through one connector reach another's READ when the
writer closes (stdio's buffer): the text leaves the moment to the
implementor, and that is where it is here.

## Left for a later stage

- **Locks across run units**: the host's byte-range locks, or the
  emulator's when it runs several instances (the ruling).
- **APPLY COMMIT**, COMMIT and ROLLBACK (queue item 46: ESQL, not files)
  and the implicit AUTOMATIC WITH LOCK ON MULTIPLE RECORDS they bring.
- The implementor's "other circumstances" of 9.1.16 (a locked block):
  none here, one record is one lock.
