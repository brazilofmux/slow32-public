\ ============================================================
\ SLOW-32 Forth-Hosted Archiver — Stage 3 Spike
\ ============================================================
\
\ Minimal .s32a archive utility focused on list/extract (`t` / `x`).
\
\ Usage:
\   S" runtime/libc_mmio.s32a" AR-T
\   S" runtime/libs32.s32a" AR-LIST
\   S" runtime/libs32.s32a" AR-X
\   S" runtime/libc_mmio.s32a" S" dtoa.s32o" AR-X1
\
\ Runs inside the Forth kernel on the Stage 0 emulator.
\ Requires prelude.fth.
\ ============================================================

HEX
53333241 CONSTANT S32A-MAGIC
DECIMAL

32    CONSTANT HDR-SZ
24    CONSTANT MEM-ENT-SZ
32768 CONSTANT MAX-STRTAB-SZ
26    CONSTANT WO-CREATE

CREATE io-buf 128 ALLOT
CREATE ar-strtab MAX-STRTAB-SZ ALLOT

VARIABLE ar-fh
VARIABLE ar-out-fh
VARIABLE ar-fsize
VARIABLE ar-nmembers
VARIABLE ar-mem-off
VARIABLE ar-nsymbols
VARIABLE ar-sym-off
VARIABLE ar-str-off
VARIABLE ar-str-sz
VARIABLE ar-is-open
VARIABLE ar-x-target-addr
VARIABLE ar-x-target-len
VARIABLE ar-x-found
VARIABLE ar-me-nameoff
VARIABLE ar-me-off
VARIABLE ar-me-size

: RD32 ( addr -- u )
    DUP C@ SWAP 1+
    DUP C@ 8 LSHIFT ROT OR SWAP 1+
    DUP C@ 16 LSHIFT ROT OR SWAP 1+
    C@ 24 LSHIFT OR ;

: STR-LEN0 ( c-addr -- u )
    DUP >R
    BEGIN DUP C@ 0<> WHILE 1+ REPEAT
    R> - ;

: AR-CLOSE ( -- )
    ar-is-open @ IF
        ar-fh @ CLOSE-FILE DROP
        0 ar-is-open !
    THEN ;

: AR-READ-AT ( addr len off -- ok? )
    >R
    R@ S>D ar-fh @ REPOSITION-FILE IF
        R> DROP 2DROP FALSE EXIT
    THEN
    R> DROP
    DUP >R
    ar-fh @ READ-FILE IF
        R> DROP DROP FALSE EXIT
    THEN
    R> = ;

: AR-BOUNDS? ( off len -- ok? )
    + ar-fsize @ <= ;

: STR= ( a1 u1 a2 u2 -- flag ) COMPARE 0= ;

: AR-HEADER-OK? ( -- ok? )
    io-buf HDR-SZ 0 AR-READ-AT 0= IF
        ." Error: cannot read archive header" CR FALSE EXIT
    THEN

    io-buf RD32 S32A-MAGIC <> IF
        ." Error: bad .s32a magic" CR FALSE EXIT
    THEN

    io-buf  8 + RD32 ar-nmembers !
    io-buf 12 + RD32 ar-mem-off !
    io-buf 16 + RD32 ar-nsymbols !
    io-buf 20 + RD32 ar-sym-off !
    io-buf 24 + RD32 ar-str-off !
    io-buf 28 + RD32 ar-str-sz !

    ar-str-sz @ MAX-STRTAB-SZ > IF
        ." Error: archive string table too large" CR FALSE EXIT
    THEN

    ar-mem-off @ ar-nmembers @ MEM-ENT-SZ * AR-BOUNDS? 0= IF
        ." Error: member table out of bounds" CR FALSE EXIT
    THEN

    ar-sym-off @ ar-nsymbols @ 8 * AR-BOUNDS? 0= IF
        ." Error: symbol table out of bounds" CR FALSE EXIT
    THEN

    ar-str-off @ ar-str-sz @ AR-BOUNDS? 0= IF
        ." Error: string table out of bounds" CR FALSE EXIT
    THEN

    TRUE ;

: AR-LOAD-STRTAB ( -- ok? )
    ar-strtab ar-str-sz @ ar-str-off @ AR-READ-AT 0= IF
        ." Error: cannot read archive string table" CR FALSE EXIT
    THEN
    TRUE ;

: AR-STR ( name-off -- c-addr u )
    DUP ar-str-sz @ >= IF DROP S" <bad-name>" EXIT THEN
    ar-strtab + DUP STR-LEN0 ;

: AR-LIST-ONE ( idx -- ok? )
    DUP MEM-ENT-SZ * ar-mem-off @ + >R
    io-buf MEM-ENT-SZ R@ AR-READ-AT 0= IF
        ." Error: cannot read member entry" CR R> DROP FALSE EXIT
    THEN
    R> DROP

    io-buf RD32 >R                 \ name_offset
    io-buf 4 + RD32                \ file offset
    io-buf 8 + RD32                \ size

    OVER OVER + ar-fsize @ > IF
        2DROP R> DROP
        ." Error: member payload out of bounds" CR
        FALSE EXIT
    THEN

    NIP                            \ drop file offset; keep size
    DUP . 32 EMIT                  \ print size + space
    DROP
    R> AR-STR TYPE CR
    TRUE ;

: AR-OPEN ( c-addr u -- ok? )
    AR-CLOSE
    R/O OPEN-FILE IF
        ." Error: cannot open archive" CR FALSE EXIT
    THEN
    ar-fh !
    1 ar-is-open !

    ar-fh @ FILE-SIZE IF
        ." Error: cannot stat archive" CR AR-CLOSE FALSE EXIT
    THEN
    DROP ar-fsize !

    ar-fsize @ HDR-SZ < IF
        ." Error: archive too small" CR AR-CLOSE FALSE EXIT
    THEN

    AR-HEADER-OK? 0= IF AR-CLOSE FALSE EXIT THEN
    AR-LOAD-STRTAB 0= IF AR-CLOSE FALSE EXIT THEN
    TRUE ;

: AR-LIST ( c-addr u -- )
    AR-OPEN 0= IF EXIT THEN
    ." members=" ar-nmembers @ .
    ." symbols=" ar-nsymbols @ . CR
    ar-nmembers @ 0 ?DO
        I AR-LIST-ONE 0= IF LEAVE THEN
    LOOP
    AR-CLOSE ;

: AR-T ( c-addr u -- ) AR-LIST ;

: AR-MATCH-TARGET? ( name-addr name-len -- flag )
    ar-x-target-len @ 0= IF 2DROP TRUE EXIT THEN
    ar-x-target-addr @ ar-x-target-len @ STR= ;

: AR-EXTRACT-ONE ( idx -- ok? )
    DUP MEM-ENT-SZ * ar-mem-off @ + >R
    io-buf MEM-ENT-SZ R@ AR-READ-AT 0= IF
        ." Error: cannot read member entry" CR R> DROP FALSE EXIT
    THEN
    R> DROP

    io-buf RD32 ar-me-nameoff !
    io-buf 4 + RD32 ar-me-off !
    io-buf 8 + RD32 ar-me-size !

    ar-me-off @ ar-me-size @ AR-BOUNDS? 0= IF
        ." Error: member payload out of bounds" CR
        FALSE EXIT
    THEN

    ar-me-nameoff @ AR-STR AR-MATCH-TARGET? 0= IF
        TRUE EXIT
    THEN

    ar-me-nameoff @ AR-STR WO-CREATE OPEN-FILE IF
        DROP ." Error: cannot create output file: "
        ar-me-nameoff @ AR-STR TYPE CR
        FALSE EXIT
    THEN
    ar-out-fh !

    BEGIN ar-me-size @ 0> WHILE
        ar-me-size @ 128 MIN DUP >R
        io-buf R@ ar-me-off @ AR-READ-AT 0= IF
            ." Error: cannot read member payload" CR
            ar-out-fh @ CLOSE-FILE DROP
            R> DROP
            FALSE EXIT
        THEN
        io-buf R@ ar-out-fh @ WRITE-FILE IF
            ." Error: cannot write output file" CR
            ar-out-fh @ CLOSE-FILE DROP
            R> DROP
            FALSE EXIT
        THEN
        ar-me-off @ R@ + ar-me-off !
        ar-me-size @ R> - ar-me-size !
    REPEAT

    ar-out-fh @ CLOSE-FILE DROP
    ." x - " ar-me-nameoff @ AR-STR TYPE CR
    1 ar-x-found !
    TRUE ;

: AR-EXTRACT ( arc-addr arc-len target-addr target-len -- )
    ar-x-target-len !
    ar-x-target-addr !
    0 ar-x-found !

    AR-OPEN 0= IF EXIT THEN
    ar-nmembers @ 0 ?DO
        I AR-EXTRACT-ONE 0= IF LEAVE THEN
    LOOP
    ar-x-target-len @ 0> ar-x-found @ 0= AND IF
        ." Error: member not found: "
        ar-x-target-addr @ ar-x-target-len @ TYPE CR
    THEN
    AR-CLOSE ;

: AR-X ( c-addr u -- ) 0 0 AR-EXTRACT ;
: AR-X1 ( arc-addr arc-len name-addr name-len -- ) AR-EXTRACT ;

0 ar-is-open !

." Stage 3 archiver spike loaded (AR-T / AR-LIST / AR-X)." CR
