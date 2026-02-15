\ ============================================================
\ SLOW-32 Forth-Hosted Archiver — Stage 3 Spike
\ ============================================================
\
\ Minimal .s32a archive utility focused on list/extract/create/replace.
\
\ Usage:
\   S" runtime/libc_mmio.s32a" AR-T
\   S" runtime/libs32.s32a" AR-LIST
\   S" runtime/libs32.s32a" AR-V
\   S" runtime/libs32.s32a" AR-X
\   S" runtime/libs32.s32a" S" debug_char.s32o" AR-P1
\   S" runtime/libc_mmio.s32a" S" dtoa.s32o" AR-X1
\   S" /tmp/new.s32a" AR-C-BEGIN
\   S" runtime/divsi3.s32o" AR-ADD
\   AR-C-END
\   S" /tmp/new.s32a" AR-R-BEGIN
\   S" runtime/builtins.s32o" AR-ADD
\   S" divsi3.s32o" AR-D
\   S" builtins.s32o" AR-M
\   AR-R-END
\
\ Runs inside the Forth kernel on the Stage 0 emulator.
\ Requires prelude.fth.
\ ============================================================

HEX
53333241 CONSTANT S32A-MAGIC
5333324F CONSTANT S32O-MAGIC
DECIMAL

32    CONSTANT HDR-SZ
24    CONSTANT MEM-ENT-SZ
32768 CONSTANT MAX-STRTAB-SZ
26    CONSTANT WO-CREATE
256   CONSTANT MAX-MEMBERS
8192  CONSTANT MAX-SYMS
16384 CONSTANT MNAME-BUF-SZ
65536 CONSTANT SNAME-BUF-SZ
524288 CONSTANT MDATA-BUF-SZ
65536 CONSTANT OUT-STRTAB-SZ

CREATE io-buf 128 ALLOT
CREATE ar-strtab MAX-STRTAB-SZ ALLOT
CREATE mname-buf MNAME-BUF-SZ ALLOT
CREATE sname-buf SNAME-BUF-SZ ALLOT
CREATE mdata-buf MDATA-BUF-SZ ALLOT
CREATE out-strtab OUT-STRTAB-SZ ALLOT

VARIABLE ar-fh
VARIABLE ar-out-fh
VARIABLE ar-tmp-fh
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
VARIABLE m-cnt
VARIABLE mname-ptr
VARIABLE s-cnt
VARIABLE sname-ptr
VARIABLE mdata-sz
VARIABLE out-strsz
VARIABLE build-out-name-len
CREATE build-out-name 256 ALLOT
CREATE m-noff MAX-MEMBERS CELLS ALLOT
CREATE m-nlen MAX-MEMBERS CELLS ALLOT
CREATE m-doff MAX-MEMBERS CELLS ALLOT
CREATE m-size MAX-MEMBERS CELLS ALLOT
CREATE m-out-noff MAX-MEMBERS CELLS ALLOT
CREATE s-noff MAX-SYMS CELLS ALLOT
CREATE s-nlen MAX-SYMS CELLS ALLOT
CREATE s-mid  MAX-SYMS CELLS ALLOT
CREATE s-out-noff MAX-SYMS CELLS ALLOT
VARIABLE m-tmp-nameoff
VARIABLE m-tmp-off
VARIABLE m-tmp-size
VARIABLE m-tmp-pos
VARIABLE m-add-off
VARIABLE m-add-size
VARIABLE t-addr
VARIABLE t-len
VARIABLE t-off
VARIABLE t-size
VARIABLE sym-j
VARIABLE sym-midx

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

: STR-LEN-LIMIT ( addr max -- len )
    0 ?DO
        OVER I + C@ 0= IF DROP I UNLOOP EXIT THEN
    LOOP
    DROP ;

: WR32 ( u addr -- )
    OVER         255 AND OVER     C!
    OVER 8  RSHIFT 255 AND OVER 1+ C!
    OVER 16 RSHIFT 255 AND OVER 2 + C!
    SWAP 24 RSHIFT 255 AND SWAP 3 + C! ;

: WR16 ( u addr -- )
    OVER 255 AND OVER C!
    SWAP 8 RSHIFT 255 AND SWAP 1+ C! ;

: OUT-WR ( addr len -- ok? )
    ar-out-fh @ WRITE-FILE IF DROP FALSE EXIT THEN
    DROP TRUE ;

: OUT-ZEROS ( n -- ok? )
    m-tmp-size !
    BEGIN m-tmp-size @ 0> WHILE
        m-tmp-size @ 128 MIN DUP >R
        io-buf R@ 0 FILL
        io-buf R@ OUT-WR 0= IF R> DROP FALSE EXIT THEN
        m-tmp-size @ R> - m-tmp-size !
    REPEAT
    TRUE ;

: ALIGN4 ( n -- n' ) 3 + -4 AND ;

: BASENAME ( addr len -- baddr blen )
    t-len ! t-addr !
    t-addr @ m-tmp-nameoff !
    t-len @ m-tmp-size !
    t-len @ 0 ?DO
        t-addr @ I + C@ DUP [CHAR] / = SWAP [CHAR] \ = OR IF
            t-addr @ I 1+ + m-tmp-nameoff !
            t-len @ I 1+ - m-tmp-size !
        THEN
    LOOP
    m-tmp-nameoff @ m-tmp-size @ ;

: BUILD-RESET ( -- )
    0 m-cnt ! 0 mname-ptr !
    0 s-cnt ! 0 sname-ptr !
    0 mdata-sz !
    0 out-strsz ! ;

: MEMBER-NAME ( idx -- addr len )
    DUP CELLS m-noff + @ mname-buf + 
    SWAP CELLS m-nlen + @ ;

: MEMBER-FIND ( addr len -- idx|-1 )
    m-cnt @ 0 ?DO
        2DUP I MEMBER-NAME STR= IF 2DROP I UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

: MEMBER-ADD ( addr len dataoff datasz -- idx|-1 )
    t-size ! t-off ! t-len ! t-addr !
    m-cnt @ MAX-MEMBERS >= IF
        ." Error: too many members" CR -1 EXIT
    THEN
    mname-ptr @ t-len @ + MNAME-BUF-SZ > IF
        ." Error: member name buffer full" CR -1 EXIT
    THEN
    m-cnt @ >R
    mname-ptr @ R@ CELLS m-noff + !
    t-len @ R@ CELLS m-nlen + !
    t-addr @ mname-buf mname-ptr @ + t-len @ CMOVE
    R@ CELLS m-nlen + @ mname-ptr +!
    t-off @ R@ CELLS m-doff + !
    t-size @ R@ CELLS m-size + !
    m-cnt @ 1+ m-cnt !
    R> ;

: MEMBER-REMOVE-IDX ( idx -- ok? )
    DUP 0< IF DROP FALSE EXIT THEN
    DUP m-cnt @ >= IF DROP FALSE EXIT THEN
    t-off !
    m-cnt @ 1- t-size !
    t-off @ t-size @ <> IF
        t-size @ CELLS m-noff + @ t-off @ CELLS m-noff + !
        t-size @ CELLS m-nlen + @ t-off @ CELLS m-nlen + !
        t-size @ CELLS m-doff + @ t-off @ CELLS m-doff + !
        t-size @ CELLS m-size + @ t-off @ CELLS m-size + !
        t-size @ CELLS m-out-noff + @ t-off @ CELLS m-out-noff + !
    THEN
    m-cnt @ 1- m-cnt !
    TRUE ;

: MEMBER-MOVE-TO-END ( idx -- ok? )
    DUP 0< IF DROP FALSE EXIT THEN
    DUP m-cnt @ >= IF DROP FALSE EXIT THEN
    DUP m-cnt @ 1- = IF DROP TRUE EXIT THEN
    t-off !
    t-off @ CELLS m-noff + @ m-tmp-nameoff !
    t-off @ CELLS m-nlen + @ m-tmp-size !
    t-off @ CELLS m-doff + @ m-add-off !
    t-off @ CELLS m-size + @ m-add-size !
    t-off @ CELLS m-out-noff + @ m-tmp-off !
    t-off @ 1+ m-tmp-pos !
    BEGIN m-tmp-pos @ m-cnt @ < WHILE
        m-tmp-pos @ CELLS m-noff + @ m-tmp-pos @ 1- CELLS m-noff + !
        m-tmp-pos @ CELLS m-nlen + @ m-tmp-pos @ 1- CELLS m-nlen + !
        m-tmp-pos @ CELLS m-doff + @ m-tmp-pos @ 1- CELLS m-doff + !
        m-tmp-pos @ CELLS m-size + @ m-tmp-pos @ 1- CELLS m-size + !
        m-tmp-pos @ CELLS m-out-noff + @ m-tmp-pos @ 1- CELLS m-out-noff + !
        m-tmp-pos @ 1+ m-tmp-pos !
    REPEAT
    m-cnt @ 1- m-tmp-pos !
    m-tmp-nameoff @ m-tmp-pos @ CELLS m-noff + !
    m-tmp-size @ m-tmp-pos @ CELLS m-nlen + !
    m-add-off @ m-tmp-pos @ CELLS m-doff + !
    m-add-size @ m-tmp-pos @ CELLS m-size + !
    m-tmp-off @ m-tmp-pos @ CELLS m-out-noff + !
    TRUE ;

: DATA-ALLOC ( size -- off ok? )
    mdata-sz @ OVER + MDATA-BUF-SZ > IF DROP 0 FALSE EXIT THEN
    mdata-sz @ SWAP mdata-sz +! TRUE ;

: COPY-INTO-MDATA ( src-off size dest-off -- ok? )
    t-addr ! t-size ! t-off !
    0 m-tmp-off !
    BEGIN m-tmp-off @ t-size @ < WHILE
        t-size @ m-tmp-off @ - 128 MIN DUP >R
        io-buf R@ t-off @ m-tmp-off @ + AR-READ-AT 0= IF
            R> DROP FALSE EXIT
        THEN
        io-buf mdata-buf t-addr @ + m-tmp-off @ + R@ CMOVE
        m-tmp-off @ R@ + m-tmp-off !
        R> DROP
    REPEAT
    TRUE ;

: COPY-FILE-TO-MDATA ( path-addr path-len -- dataoff datasz ok? )
    R/O OPEN-FILE IF
        DROP 0 0 FALSE EXIT
    THEN
    ar-tmp-fh !
    ar-tmp-fh @ FILE-SIZE IF
        DROP ar-tmp-fh @ CLOSE-FILE DROP 0 0 FALSE EXIT
    THEN
    DROP DUP t-size !
    t-size @ DATA-ALLOC IF
        t-off !
    ELSE
        DROP ar-tmp-fh @ CLOSE-FILE DROP 0 0 FALSE EXIT
    THEN
    mdata-buf t-off @ + t-size @ ar-tmp-fh @ READ-FILE IF
        ar-tmp-fh @ CLOSE-FILE DROP 0 0 FALSE EXIT
    THEN
    t-size @ <> IF
        ar-tmp-fh @ CLOSE-FILE DROP 0 0 FALSE EXIT
    THEN
    ar-tmp-fh @ CLOSE-FILE DROP
    t-off @ t-size @ TRUE ;

: OUTSTR-INIT ( -- )
    0 out-strtab C!
    1 out-strsz ! ;

: OUTSTR-ADD ( addr len -- off )
    out-strsz @ OVER + 1+ OUT-STRTAB-SZ > IF
        2DROP 0 EXIT
    THEN
    out-strsz @ >R
    DUP >R
    out-strtab out-strsz @ + SWAP CMOVE
    0 out-strtab out-strsz @ + R> + C!
    out-strsz @ SWAP 1+ + out-strsz !
    R> ;

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

: AR-READ-MEMBER ( idx -- ok? )
    DUP MEM-ENT-SZ * ar-mem-off @ + >R
    io-buf MEM-ENT-SZ R@ AR-READ-AT 0= IF
        ." Error: cannot read member entry" CR R> DROP FALSE EXIT
    THEN
    R> DROP

    io-buf RD32 ar-me-nameoff !
    io-buf 4 + RD32 ar-me-off !
    io-buf 8 + RD32 ar-me-size !

    ar-me-off @ ar-me-size @ + ar-fsize @ > IF
        ." Error: member payload out of bounds" CR
        FALSE EXIT
    THEN
    TRUE ;

: AR-LIST-ONE ( idx -- ok? )
    AR-READ-MEMBER 0= IF FALSE EXIT THEN
    ar-me-size @ . 32 EMIT
    ar-me-nameoff @ AR-STR TYPE CR
    TRUE ;

: AR-LIST-ONE-V ( idx -- ok? )
    DUP AR-READ-MEMBER 0= IF DROP FALSE EXIT THEN
    ." idx=" DUP . 32 EMIT
    ." off=" ar-me-off @ . 32 EMIT
    ." size=" ar-me-size @ . 32 EMIT
    ." name=" ar-me-nameoff @ AR-STR TYPE CR
    DROP TRUE ;

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

: AR-LIST-V ( c-addr u -- )
    AR-OPEN 0= IF EXIT THEN
    ." members=" ar-nmembers @ .
    ." symbols=" ar-nsymbols @ . CR
    ar-nmembers @ 0 ?DO
        I AR-LIST-ONE-V 0= IF LEAVE THEN
    LOOP
    AR-CLOSE ;

: AR-V ( c-addr u -- ) AR-LIST-V ;

: AR-MATCH-TARGET? ( name-addr name-len -- flag )
    ar-x-target-len @ 0= IF 2DROP TRUE EXIT THEN
    ar-x-target-addr @ ar-x-target-len @ STR= ;

: AR-EXTRACT-ONE ( idx -- ok? )
    AR-READ-MEMBER 0= IF FALSE EXIT THEN

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

: AR-PRINT-ONE ( idx -- ok? )
    AR-READ-MEMBER 0= IF FALSE EXIT THEN
    ar-me-nameoff @ AR-STR AR-MATCH-TARGET? 0= IF
        TRUE EXIT
    THEN
    ." p - " ar-me-nameoff @ AR-STR TYPE CR
    BEGIN ar-me-size @ 0> WHILE
        ar-me-size @ 128 MIN DUP >R
        io-buf R@ ar-me-off @ AR-READ-AT 0= IF
            ." Error: cannot read member payload" CR
            R> DROP
            FALSE EXIT
        THEN
        io-buf R@ TYPE
        ar-me-off @ R@ + ar-me-off !
        ar-me-size @ R> - ar-me-size !
    REPEAT
    CR
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

: AR-PRINT ( arc-addr arc-len target-addr target-len -- )
    ar-x-target-len !
    ar-x-target-addr !
    0 ar-x-found !
    AR-OPEN 0= IF EXIT THEN
    ar-nmembers @ 0 ?DO
        I AR-PRINT-ONE 0= IF LEAVE THEN
    LOOP
    ar-x-target-len @ 0> ar-x-found @ 0= AND IF
        ." Error: member not found: "
        ar-x-target-addr @ ar-x-target-len @ TYPE CR
    THEN
    AR-CLOSE ;

: AR-P ( c-addr u -- ) 0 0 AR-PRINT ;
: AR-P1 ( arc-addr arc-len name-addr name-len -- ) AR-PRINT ;

: M-RD32 ( midx off -- u )
    SWAP CELLS m-doff + @ mdata-buf + + RD32 ;

: M-RD16 ( midx off -- u )
    SWAP CELLS m-doff + @ mdata-buf + + DUP C@ SWAP 1+ C@ 8 LSHIFT OR ;

: M-RD8 ( midx off -- u )
    SWAP CELLS m-doff + @ mdata-buf + + C@ ;

: SYM-ADD ( addr len midx -- )
    sym-midx ! t-len ! t-addr !
    s-cnt @ MAX-SYMS >= IF EXIT THEN
    sname-ptr @ t-len @ + SNAME-BUF-SZ > IF EXIT THEN
    s-cnt @ >R
    sname-ptr @ R@ CELLS s-noff + !
    t-len @ R@ CELLS s-nlen + !
    t-addr @ sname-buf sname-ptr @ + t-len @ CMOVE
    sym-midx @ R@ CELLS s-mid + !
    R@ CELLS s-nlen + @ sname-ptr +!
    s-cnt @ 1+ s-cnt !
    R> DROP ;

: BUILD-SYMINDEX ( -- )
    0 s-cnt ! 0 sname-ptr !
    m-cnt @ 0 ?DO
        I sym-midx !
        sym-midx @ 0 M-RD32 S32O-MAGIC <> IF
            ." Error: member is not .s32o" CR LEAVE
        THEN
        sym-midx @ 20 M-RD32 m-tmp-size !      \ nsymbols
        sym-midx @ 24 M-RD32 m-tmp-off !       \ sym_offset
        sym-midx @ 28 M-RD32 m-tmp-nameoff !   \ str_offset
        sym-midx @ 32 M-RD32 m-add-size !      \ str_size

        m-tmp-off @ m-tmp-size @ 16 * + sym-midx @ CELLS m-size + @ > IF
            ." Error: bad symbol table bounds" CR LEAVE
        THEN
        m-tmp-nameoff @ m-add-size @ + sym-midx @ CELLS m-size + @ > IF
            ." Error: bad string table bounds" CR LEAVE
        THEN

        m-tmp-size @ 0 ?DO
            J sym-j !
            sym-j @ 16 * m-tmp-off @ + m-tmp-pos !
            sym-midx @ m-tmp-pos @ 11 + M-RD8 1 = IF          \ GLOBAL
                sym-midx @ m-tmp-pos @ 8 + M-RD16 0<> IF      \ defined
                    sym-midx @ m-tmp-pos @ M-RD32 DUP t-off ! m-add-size @ < IF
                        sym-midx @ CELLS m-doff + @ mdata-buf + m-tmp-nameoff @ + t-off @ +
                        DUP m-add-size @ t-off @ - STR-LEN-LIMIT
                        sym-midx @ SYM-ADD
                    ELSE
                        DROP
                    THEN
                THEN
            THEN
        LOOP
    LOOP ;

: BUILD-OUT-STRTAB ( -- ok? )
    OUTSTR-INIT
    m-cnt @ 0 ?DO
        I MEMBER-NAME OUTSTR-ADD
        DUP 0= IF DROP FALSE EXIT THEN
        I CELLS m-out-noff + !
    LOOP
    s-cnt @ 0 ?DO
        sname-buf I CELLS s-noff + @ + I CELLS s-nlen + @ OUTSTR-ADD
        DUP 0= IF DROP FALSE EXIT THEN
        I CELLS s-out-noff + !
    LOOP
    TRUE ;

: SAVE-OUT-NAME ( addr len -- )
    255 MIN DUP build-out-name-len !
    build-out-name SWAP CMOVE
    build-out-name-len @ 255 < IF
        0 build-out-name build-out-name-len @ + C!
    THEN ;

: WRITE-ARCHIVE ( -- )
    BUILD-SYMINDEX
    BUILD-OUT-STRTAB 0= IF ." Error: output string table full" CR EXIT THEN

    build-out-name build-out-name-len @ WO-CREATE OPEN-FILE IF
        DROP ." Error: cannot create output archive" CR EXIT
    THEN
    ar-out-fh !

    HDR-SZ
    s-cnt @ 8 * +
    m-cnt @ MEM-ENT-SZ * +
    out-strsz @ + ALIGN4 m-tmp-off !      \ data_start

    0 m-tmp-pos !                          \ running member data file offset delta

    io-buf HDR-SZ 0 FILL
    S32A-MAGIC io-buf WR32
    1 io-buf 4 + WR16
    1 io-buf 6 + C!
    m-cnt @ io-buf 8 + WR32
    HDR-SZ s-cnt @ 8 * + io-buf 12 + WR32
    s-cnt @ io-buf 16 + WR32
    HDR-SZ io-buf 20 + WR32
    HDR-SZ s-cnt @ 8 * + m-cnt @ MEM-ENT-SZ * + io-buf 24 + WR32
    out-strsz @ io-buf 28 + WR32
    io-buf HDR-SZ OUT-WR 0= IF ." Error: write header failed" CR EXIT THEN

    s-cnt @ 0 ?DO
        io-buf 8 0 FILL
        I CELLS s-out-noff + @ io-buf WR32
        I CELLS s-mid + @ io-buf 4 + WR32
        io-buf 8 OUT-WR 0= IF ." Error: write sym index failed" CR EXIT THEN
    LOOP

    m-cnt @ 0 ?DO
        io-buf MEM-ENT-SZ 0 FILL
        I CELLS m-out-noff + @ io-buf WR32
        m-tmp-off @ m-tmp-pos @ + io-buf 4 + WR32
        I CELLS m-size + @ io-buf 8 + WR32
        0 io-buf 12 + WR32
        0 io-buf 16 + WR32
        0 io-buf 20 + WR32
        io-buf MEM-ENT-SZ OUT-WR 0= IF ." Error: write member table failed" CR EXIT THEN
        m-tmp-pos @ I CELLS m-size + @ + ALIGN4 m-tmp-pos !
    LOOP

    out-strtab out-strsz @ OUT-WR 0= IF ." Error: write strtab failed" CR EXIT THEN

    HDR-SZ s-cnt @ 8 * + m-cnt @ MEM-ENT-SZ * + out-strsz @ +
    m-tmp-off @ SWAP - DUP 0> IF
        OUT-ZEROS 0= IF ." Error: write align failed" CR EXIT THEN
    ELSE DROP THEN

    m-cnt @ 0 ?DO
        mdata-buf I CELLS m-doff + @ + I CELLS m-size + @ OUT-WR 0= IF
            ." Error: write member payload failed" CR EXIT
        THEN
        I CELLS m-size + @ ALIGN4 I CELLS m-size + @ - DUP 0> IF
            OUT-ZEROS 0= IF ." Error: write payload align failed" CR EXIT THEN
        ELSE DROP THEN
    LOOP

    ar-out-fh @ CLOSE-FILE DROP ;

: LOAD-EXISTING-MEMBERS ( -- ok? )
    m-cnt @ 0<> IF FALSE EXIT THEN
    ar-nmembers @ 0 ?DO
        I MEM-ENT-SZ * ar-mem-off @ + >R
        io-buf MEM-ENT-SZ R@ AR-READ-AT 0= IF R> DROP FALSE EXIT THEN
        R> DROP
        io-buf RD32 m-tmp-nameoff !
        io-buf 4 + RD32 m-tmp-off !
        io-buf 8 + RD32 m-tmp-size !
        m-tmp-off @ m-tmp-size @ AR-BOUNDS? 0= IF FALSE EXIT THEN
        m-tmp-size @ DATA-ALLOC IF
            m-add-off !
        ELSE
            DROP FALSE EXIT
        THEN
        m-tmp-off @ m-tmp-size @ m-add-off @ COPY-INTO-MDATA 0= IF FALSE EXIT THEN
        m-tmp-nameoff @ AR-STR m-add-off @ m-tmp-size @ MEMBER-ADD DROP
    LOOP
    TRUE ;

: AR-C-BEGIN ( out-addr out-len -- )
    BUILD-RESET
    SAVE-OUT-NAME ;

: AR-R-BEGIN ( arc-addr arc-len -- )
    BUILD-RESET
    2DUP SAVE-OUT-NAME
    AR-OPEN IF
        LOAD-EXISTING-MEMBERS DROP
        AR-CLOSE
    THEN ;

: AR-ADD ( path-addr path-len -- )
    2DUP BASENAME t-len ! t-addr !
    COPY-FILE-TO-MDATA IF
        m-add-size ! m-add-off !
    ELSE
        2DROP ." Error: cannot read member file" CR EXIT
    THEN
    t-addr @ t-len @ MEMBER-FIND DUP 0< IF
        DROP
        t-addr @ t-len @ m-add-off @ m-add-size @ MEMBER-ADD DROP
    ELSE
        m-add-off @ OVER CELLS m-doff + !
        m-add-size @ SWAP CELLS m-size + !
    THEN
    ." a - " t-addr @ t-len @ TYPE CR ;

: AR-D ( name-addr name-len -- )
    BASENAME t-len ! t-addr !
    t-addr @ t-len @ MEMBER-FIND DUP 0< IF
        DROP ." d? - " t-addr @ t-len @ TYPE CR EXIT
    THEN
    MEMBER-REMOVE-IDX DROP
    ." d - " t-addr @ t-len @ TYPE CR ;

: AR-M ( name-addr name-len -- )
    BASENAME t-len ! t-addr !
    t-addr @ t-len @ MEMBER-FIND DUP 0< IF
        DROP ." m? - " t-addr @ t-len @ TYPE CR EXIT
    THEN
    MEMBER-MOVE-TO-END DROP
    ." m - " t-addr @ t-len @ TYPE CR ;

: AR-C-END ( -- ) WRITE-ARCHIVE ;
: AR-R-END ( -- ) WRITE-ARCHIVE ;

0 ar-is-open !

." Stage 3 archiver spike loaded (AR-T/AR-V/AR-X/AR-P/AR-C/AR-R/AR-D/AR-M)." CR
