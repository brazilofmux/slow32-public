\ ============================================================
\ SLOW-32 Forth-Hosted Linker — Stage 3 Bootstrap
\ ============================================================
\
\ Links .s32o object files and .s32a archives into a .s32x
\ executable, matching the C linker (s32-ld) output.
\
\ Usage:
\   LINK-INIT
\   S" crt0.s32o" LINK-OBJ
\   S" program.s32o" LINK-OBJ
\   S" libc_mmio.s32a" LINK-ARCHIVE
\   S" libs32.s32a" LINK-ARCHIVE
\   65536 LINK-MMIO
\   S" output.s32x" LINK-EMIT
\
\ Runs inside the Forth kernel on the Stage 0 emulator.
\ Requires prelude.fth.
\ ============================================================

\ === All hex constants defined at top level ===
HEX
5333324F CONSTANT S32O-MAGIC
53333258 CONSTANT S32X-MAGIC
53333241 CONSTANT S32A-MAGIC
FFF      CONSTANT MASK12
7F       CONSTANT MASK7
1F       CONSTANT MASK5
3F       CONSTANT MASK6
FF       CONSTANT MASK8
3FF      CONSTANT MASK10
FFFFF    CONSTANT MASK20
800      CONSTANT HALF12
FFFFFF0  CONSTANT STACK-BASE-DEF
10000000 CONSTANT MEM-SIZE-DEF
100000   CONSTANT HEAP-GAP-DEF
100000   CONSTANT CODE-LIMIT-MIN
80       CONSTANT FLAG-MMIO
01       CONSTANT FLAG-WXORX
0D       CONSTANT SF-XRA
0E       CONSTANT SF-WRA
0C       CONSTANT SF-RA
38       CONSTANT OP-STB
39       CONSTANT OP-STH
3A       CONSTANT OP-STW
32       CONSTANT MACHINE-ID
FE000F80 CONSTANT S-MASK
1FFF07F  CONSTANT B-MASK
FFFFF000 CONSTANT PG-MASK
DECIMAL

\ Decimal constants
65536  CONSTANT STACK-SIZE-DEF    \ 0x10000

\ Section types
1 CONSTANT SEC-CODE
2 CONSTANT SEC-DATA
3 CONSTANT SEC-BSS
4 CONSTANT SEC-RODATA

\ Relocation types
1 CONSTANT REL-32
32 CONSTANT REL-ABS32
2 CONSTANT REL-HI20
3 CONSTANT REL-LO12
4 CONSTANT REL-BRANCH
5 CONSTANT REL-JAL
6 CONSTANT REL-CALL
7 CONSTANT REL-PCREL-HI20
8 CONSTANT REL-PCREL-LO12

\ Symbol binding
0 CONSTANT BIND-LOCAL
1 CONSTANT BIND-GLOBAL
2 CONSTANT BIND-WEAK

\ File open mode for write+create+truncate
26 CONSTANT WO-CREATE

\ === Buffer sizes ===
\ Total ALLOT must fit within 1MB kernel dictionary.
\ Reduced from initial design to avoid overflow.
196608 CONSTANT TEXT-BUFSZ
98304  CONSTANT DATA-BUFSZ
65536  CONSTANT RODATA-BUFSZ
1024   CONSTANT MAX-GSYM
4096   CONSTANT MAX-GREL
32768  CONSTANT GSYM-NBUF-SZ
262144 CONSTANT FILE-BUFSZ
8192   CONSTANT OUT-STRTAB-SZ
512    CONSTANT MAX-FILE-SYM
512    CONSTANT MAX-MEMBERS

\ === Merged section buffers ===
CREATE text-buf  TEXT-BUFSZ  ALLOT
CREATE data-buf  DATA-BUFSZ  ALLOT
CREATE rodata-buf RODATA-BUFSZ ALLOT

VARIABLE text-sz
VARIABLE data-sz
VARIABLE rodata-sz
VARIABLE bss-sz

\ === Global symbol table (parallel arrays) ===
CREATE gsym-noff MAX-GSYM CELLS ALLOT
CREATE gsym-nlen MAX-GSYM CELLS ALLOT
CREATE gsym-val  MAX-GSYM CELLS ALLOT
CREATE gsym-sec  MAX-GSYM CELLS ALLOT
CREATE gsym-bind MAX-GSYM CELLS ALLOT
CREATE gsym-def  MAX-GSYM CELLS ALLOT
VARIABLE gsym-cnt
CREATE gsym-nbuf GSYM-NBUF-SZ ALLOT
VARIABLE gsym-nptr

\ === Global relocation table (parallel arrays) ===
CREATE grel-off MAX-GREL CELLS ALLOT
CREATE grel-sec MAX-GREL CELLS ALLOT
CREATE grel-typ MAX-GREL CELLS ALLOT
CREATE grel-add MAX-GREL CELLS ALLOT
CREATE grel-sym MAX-GREL CELLS ALLOT
VARIABLE grel-cnt

\ === Layout variables ===
VARIABLE text-va
VARIABLE rodata-va
VARIABLE data-va
VARIABLE bss-va
VARIABLE bss-end-va
VARIABLE heap-va
VARIABLE mmio-sz
VARIABLE mmio-va
VARIABLE entry-pt

\ === File I/O buffer ===
CREATE file-buf FILE-BUFSZ ALLOT

\ === Output string table ===
CREATE out-strtab OUT-STRTAB-SZ ALLOT
VARIABLE out-strtab-sz

\ === Write buffer ===
CREATE wb-buf 64 ALLOT

\ === Output filename buffer (saved from transient S") ===
CREATE out-fname 256 ALLOT
VARIABLE out-fname-len

\ === Temp variables ===
VARIABLE tmp-fh
VARIABLE tmp-fsz

\ Per-file section base offsets (indexed by section type 0-7)
CREATE sec-base 8 CELLS ALLOT

\ Per-file symbol index mapping
CREATE sym-map MAX-FILE-SYM CELLS ALLOT

\ Archive buffers
16384  CONSTANT AR-SYMTAB-SZ
12288  CONSTANT AR-MEMTAB-SZ
16384  CONSTANT AR-STRTAB-SZ
CREATE ar-symtab AR-SYMTAB-SZ ALLOT
CREATE ar-memtab AR-MEMTAB-SZ ALLOT
CREATE ar-strtab AR-STRTAB-SZ ALLOT
VARIABLE ar-nmembers
VARIABLE ar-mem-off
VARIABLE ar-nsymbols
VARIABLE ar-sym-off
VARIABLE ar-str-off
VARIABLE ar-str-sz
VARIABLE ar-fh
CREATE mem-loaded MAX-MEMBERS ALLOT
VARIABLE link-error

\ Per-object file parsing state
VARIABLE obj-nsec
VARIABLE obj-nsym
VARIABLE obj-sec-off
VARIABLE obj-sym-off
VARIABLE obj-str-off

\ Section parsing temps
VARIABLE ms-type
VARIABLE ms-size
VARIABLE ms-file-off
VARIABLE ms-base

\ Symbol parsing temps
VARIABLE sy-noff
VARIABLE sy-val
VARIABLE sy-sec
VARIABLE sy-bind
VARIABLE sy-sectype

\ GSYM-UPSERT temp
VARIABLE gs-idx

\ Reloc parsing temps
VARIABLE rl-off
VARIABLE rl-sym
VARIABLE rl-typ
VARIABLE rl-add

\ Reloc application temps
VARIABLE ar-off
VARIABLE ar-sec
VARIABLE ar-rtyp
VARIABLE ar-radd
VARIABLE ar-gsym
VARIABLE ar-val
VARIABLE ar-tgt
VARIABLE ar-pc

\ Output layout variables
VARIABLE out-fh
VARIABLE out-nsec
VARIABLE out-strtab-off
VARIABLE out-data-off
VARIABLE sec-text-foff
VARIABLE sec-data-foff
VARIABLE sec-rodata-foff
VARIABLE str-text-off
VARIABLE str-data-off
VARIABLE str-bss-off
VARIABLE str-rodata-off
VARIABLE file-pos

\ Temp for OSTRTAB-ADD
VARIABLE osa-off
VARIABLE osa-len
VARIABLE sex-a
VARIABLE sex-u
VARIABLE sex-pos
VARIABLE sex-add
VARIABLE sex-i
VARIABLE sex-acc
VARIABLE sex-ch

\ === Helpers ===

: ALIGN4 ( n -- n' ) 3 + -4 AND ;
: ALIGN16 ( n -- n' ) 15 + -16 AND ;
: PAGE-ALIGN ( n -- n' ) 4095 + PG-MASK AND ;

\ Read 32-bit LE from buffer
: RD32 ( addr -- u )
    DUP C@ SWAP 1+
    DUP C@ 8 LSHIFT ROT OR SWAP 1+
    DUP C@ 16 LSHIFT ROT OR SWAP 1+
    C@ 24 LSHIFT OR ;

\ Read 16-bit LE from buffer
: RD16 ( addr -- u )
    DUP C@ SWAP 1+ C@ 8 LSHIFT OR ;

\ Write 32-bit LE to buffer
: WR32 ( u addr -- )
    OVER         255 AND OVER     C!
    OVER 8  RSHIFT 255 AND OVER 1+ C!
    OVER 16 RSHIFT 255 AND OVER 2 + C!
    SWAP 24 RSHIFT 255 AND SWAP 3 + C! ;

\ Write buffer helpers
: WB-INIT  wb-buf 64 0 FILL ;

: WB! ( value offset -- )
    wb-buf +
    OVER         255 AND OVER     C!
    OVER 8  RSHIFT 255 AND OVER 1+ C!
    OVER 16 RSHIFT 255 AND OVER 2 + C!
    SWAP 24 RSHIFT 255 AND SWAP 3 + C! ;

: WB16! ( value offset -- )
    wb-buf +
    OVER 255 AND OVER C!
    SWAP 8 RSHIFT 255 AND SWAP 1+ C! ;

: WB8! ( value offset -- ) wb-buf + C! ;

: WB-WRITE ( n -- ) wb-buf SWAP out-fh @ WRITE-FILE THROW ;

\ String comparison helper
: STR= ( a1 u1 a2 u2 -- flag ) COMPARE 0= ;

: DIGIT? ( c -- n true | false )
    DUP [CHAR] 0 >= OVER [CHAR] 9 <= AND IF
        [CHAR] 0 - TRUE EXIT
    THEN
    DROP FALSE ;

: PARSE-UDEC ( c-addr u -- n true | false )
    2DUP sex-u ! sex-a !
    sex-u @ 0= IF 2DROP FALSE EXIT THEN
    0 sex-acc !
    0 sex-i !
    BEGIN
        sex-i @ sex-u @ <
    WHILE
        sex-a @ sex-i @ + C@ DIGIT? 0= IF 2DROP FALSE EXIT THEN
        sex-acc @ 10 * + sex-acc !
        1 sex-i +!
    REPEAT
    2DROP sex-acc @ TRUE ;

: FIND-CHAR ( c-addr u ch -- pos | -1 )
    2DUP sex-u ! sex-a !
    sex-ch !
    0 sex-i !
    BEGIN
        sex-i @ sex-u @ <
    WHILE
        sex-a @ sex-i @ + C@ sex-ch @ = IF 2DROP sex-i @ EXIT THEN
        1 sex-i +!
    REPEAT
    2DROP -1 ;

\ Split "name+N" or "name-N" into (name, addend).
: SPLIT-SYM-EXPR ( c-addr u -- sym-addr sym-len addend true | false )
    2DUP sex-u ! sex-a !

    2DUP [CHAR] + FIND-CHAR DUP 0> IF
        sex-pos !
        sex-a @ sex-pos @ + 1+ sex-u @ sex-pos @ - 1- PARSE-UDEC IF
            >R 2DROP R>
            sex-a @ sex-pos @ ROT TRUE EXIT
        THEN
        2DROP FALSE EXIT
    THEN
    DROP

    2DUP [CHAR] - FIND-CHAR DUP 0> IF
        sex-pos !
        sex-a @ sex-pos @ + 1+ sex-u @ sex-pos @ - 1- PARSE-UDEC IF
            >R 2DROP R>
            NEGATE sex-a @ sex-pos @ ROT TRUE EXIT
        THEN
        2DROP FALSE EXIT
    THEN
    DROP
    2DROP FALSE ;

\ Resolve "__mmio_base+N" alias directly from computed mmio base.
: MMIO-ALIAS-VA ( c-addr u -- va true | false )
    2DUP [CHAR] + FIND-CHAR DUP 0< OVER 0= OR IF DROP 2DROP FALSE EXIT THEN
    sex-pos !
    OVER sex-pos @ S" __mmio_base" STR= 0= IF 2DROP FALSE EXIT THEN
    sex-pos @ 1+ /STRING PARSE-UDEC IF
        mmio-va @ + TRUE EXIT
    THEN
    FALSE ;

\ Read entire file into file-buf, return size or -1 on error
: READ-FILE-BUF ( c-addr u -- size | -1 )
    R/O OPEN-FILE IF DROP -1 EXIT THEN
    tmp-fh !
    tmp-fh @ FILE-SIZE IF tmp-fh @ CLOSE-FILE DROP -1 EXIT THEN
    DROP  \ high cell of double (zero for small files)
    DUP FILE-BUFSZ > IF
        ." File too large: " DUP . CR
        tmp-fh @ CLOSE-FILE DROP DROP -1 EXIT
    THEN
    DUP tmp-fsz !
    file-buf SWAP tmp-fh @ READ-FILE IF
        tmp-fh @ CLOSE-FILE DROP DROP -1 EXIT
    THEN
    DROP  \ actual bytes read (= requested)
    tmp-fh @ CLOSE-FILE DROP
    tmp-fsz @ ;

\ Get NUL-terminated string from file-buf string table
: OBJ-STR ( name-off -- c-addr u )
    obj-str-off @ + file-buf +
    DUP 0 BEGIN OVER OVER + C@ 0<> WHILE 1+ REPEAT NIP ;

\ Output string table management
: OSTRTAB-INIT  0 out-strtab C! 1 out-strtab-sz ! ;

: OSTRTAB-ADD ( c-addr u -- offset )
    osa-len !
    out-strtab-sz @ osa-off !
    out-strtab osa-off @ + osa-len @ CMOVE
    0 out-strtab osa-off @ + osa-len @ + C!
    osa-len @ 1+ out-strtab-sz +!
    osa-off @ ;

\ === LINK-INIT ===
: LINK-INIT
    0 text-sz !  0 data-sz !  0 rodata-sz !  0 bss-sz !
    0 gsym-cnt !  0 gsym-nptr !
    0 grel-cnt !
    0 text-va !  0 rodata-va !  0 data-va !  0 bss-va !
    0 bss-end-va !  0 heap-va !
    0 mmio-sz !  0 mmio-va !  0 entry-pt !
    0 link-error !
    sec-base 8 CELLS 0 FILL
    OSTRTAB-INIT
    ." Linker initialized" CR ;

\ === Symbol table ===

: GSYM-FIND ( c-addr u -- idx | -1 )
    gsym-cnt @ 0 ?DO
        2DUP
        I CELLS gsym-noff + @ gsym-nbuf +
        I CELLS gsym-nlen + @
        STR= IF 2DROP I UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

\ Add a new symbol to the table
: GSYM-ADD-NEW ( c-addr u sec-type value binding defined -- idx )
    gsym-cnt @ MAX-GSYM >= IF
        ." Error: gsym full" CR
        2DROP 2DROP 2DROP -1 EXIT
    THEN
    gsym-cnt @ >R
    R@ CELLS gsym-def  + !
    R@ CELLS gsym-bind + !
    R@ CELLS gsym-val  + !
    R@ CELLS gsym-sec  + !
    \ Store name in gsym-nbuf
    DUP R@ CELLS gsym-nlen + !
    gsym-nptr @ R@ CELLS gsym-noff + !
    gsym-nptr @ gsym-nbuf + SWAP CMOVE
    R@ CELLS gsym-nlen + @ gsym-nptr +!
    R@ gsym-cnt @ 1+ gsym-cnt !
    R> ;

\ Find-or-add symbol. Update undef -> defined if applicable.
: GSYM-UPSERT ( c-addr u sec-type value binding -- idx )
    >R >R >R    \ R: binding value sec-type
    2DUP GSYM-FIND
    DUP 0< IF
        \ Not found — add new
        DROP
        R> R> R>    \ sec-type value binding
        2 PICK 0= IF 0 ELSE 1 THEN   \ defined = (sec-type != 0)
        GSYM-ADD-NEW
    ELSE
        \ Found — save idx, drop name, retrieve R-stack
        gs-idx ! 2DROP
        R> R> R>    \ ( sec-type value binding )
        2 PICK 0<> gs-idx @ CELLS gsym-def + @ 0= AND IF
            \ New is defined AND existing is undefined — update
            gs-idx @ CELLS gsym-bind + !
            gs-idx @ CELLS gsym-val  + !
            gs-idx @ CELLS gsym-sec  + !
            1 gs-idx @ CELLS gsym-def + !
        ELSE
            2DROP DROP
        THEN
        gs-idx @
    THEN ;

\ Add undefined symbol if not present, return index
: GSYM-ADD-UNDEF ( c-addr u -- idx )
    0 0 BIND-GLOBAL GSYM-UPSERT ;

\ === Section helpers ===

: SEC-BUF ( sec-type -- addr )
    CASE
        SEC-CODE   OF text-buf   ENDOF
        SEC-DATA   OF data-buf   ENDOF
        SEC-RODATA OF rodata-buf ENDOF
        0 SWAP
    ENDCASE ;

: SEC-CAP ( sec-type -- size )
    CASE
        SEC-CODE   OF TEXT-BUFSZ   ENDOF
        SEC-DATA   OF DATA-BUFSZ   ENDOF
        SEC-RODATA OF RODATA-BUFSZ ENDOF
        0 SWAP
    ENDCASE ;

: SEC-SZ-VAR ( sec-type -- var-addr )
    CASE
        SEC-CODE   OF text-sz   ENDOF
        SEC-DATA   OF data-sz   ENDOF
        SEC-BSS    OF bss-sz    ENDOF
        SEC-RODATA OF rodata-sz ENDOF
        0 SWAP
    ENDCASE ;

\ === Parse .s32o header ===
: PARSE-OBJ-HEADER ( -- ok? )
    file-buf RD32 S32O-MAGIC <> IF
        ." Error: bad .s32o magic" CR FALSE EXIT
    THEN
    file-buf 12 + RD32 obj-nsec !
    file-buf 16 + RD32 obj-sec-off !
    file-buf 20 + RD32 obj-nsym !
    file-buf 24 + RD32 obj-sym-off !
    file-buf 28 + RD32 obj-str-off !
    TRUE ;

\ === Merge sections from current .s32o ===
: MERGE-SECTIONS ( -- )
    sec-base 8 CELLS 0 FILL
    obj-nsec @ 0 ?DO
        I 32 * obj-sec-off @ + file-buf + >R
        R@ 4 + RD32 ms-type !
        R@ 12 + RD32 ms-size !
        R@ 16 + RD32 ms-file-off !
        R> DROP

        ms-type @ SEC-CODE = ms-type @ SEC-DATA = OR
        ms-type @ SEC-BSS = OR ms-type @ SEC-RODATA = OR IF
            \ Compute aligned base in merged section
            ms-type @ SEC-SZ-VAR @
            DUP 0> IF ALIGN4 ELSE DROP 0 THEN
            ms-base !

            \ Record base for symbol/reloc adjustment
            ms-base @ ms-type @ CELLS sec-base + !

            \ Copy data (skip BSS)
            ms-type @ SEC-BSS <> IF
                ms-size @ 0> ms-file-off @ 0> AND IF
                    ms-base @ ms-size @ + ms-type @ SEC-CAP > IF
                        ." Error: section buffer overflow type=" ms-type @ .
                        ." need=" ms-base @ ms-size @ + .
                        ." cap=" ms-type @ SEC-CAP . CR
                        1 link-error !
                        LEAVE
                    THEN
                    file-buf ms-file-off @ +
                    ms-type @ SEC-BUF ms-base @ +
                    ms-size @ CMOVE
                THEN
            THEN

            \ Update merged section size
            ms-base @ ms-size @ + ms-type @ SEC-SZ-VAR !
        THEN
    LOOP ;

\ === Merge symbols from current .s32o ===
: MERGE-SYMBOLS ( -- )
    obj-nsym @ 0 ?DO
        I 16 * obj-sym-off @ + file-buf + >R
        R@ RD32 sy-noff !
        R@ 4 + RD32 sy-val !
        R@ 8 + RD16 sy-sec !
        R> 11 + C@ sy-bind !

        sy-noff @ OBJ-STR ( c-addr u )

        sy-sec @ 0= IF
            \ Undefined symbol (locals should not be unresolved across files)
            sy-bind @ BIND-LOCAL = IF
                2DROP -1
            ELSE
                GSYM-ADD-UNDEF
            THEN
        ELSE
            \ Defined: get section type from section table
            sy-sec @ 1- 32 * obj-sec-off @ + file-buf + 4 + RD32 sy-sectype !
            \ Adjust value by section base offset
            sy-sectype @ CELLS sec-base + @ sy-val @ + sy-val !
            \ Locals are file-scoped; do not global-name-upsert (.L* collisions).
            sy-bind @ BIND-LOCAL = IF
                sy-sectype @ sy-val @ sy-bind @ 1 GSYM-ADD-NEW
            ELSE
                \ Add/update in global symbol table
                sy-sectype @ sy-val @ sy-bind @ GSYM-UPSERT
            THEN
        THEN
        I CELLS sym-map + !
    LOOP ;

\ === Merge relocations from current .s32o ===
: MERGE-RELOCS ( -- )
    obj-nsec @ 0 ?DO
        I 32 * obj-sec-off @ + file-buf + >R
        R@ 4 + RD32    \ section type
        R@ 24 + RD32   \ nrelocs
        R> 28 + RD32   \ reloc_offset in file
        ( sec-type nrelocs reloc-off )

        SWAP ( sec-type reloc-off nrelocs )
        0 ?DO
            DUP file-buf + >R
            R@ RD32 rl-off !
            R@ 4 + RD32 rl-sym !
            R@ 8 + RD32 rl-typ !
            R> 12 + RD32 rl-add !

            grel-cnt @ MAX-GREL < IF
                grel-cnt @ >R
                rl-add @ R@ CELLS grel-add + !
                rl-typ @ R@ CELLS grel-typ + !

                \ Map file-local symbol to gsym
                rl-sym @ MAX-FILE-SYM < IF
                    rl-sym @ CELLS sym-map + @
                ELSE -1
                THEN
                R@ CELLS grel-sym + !

                \ Adjust offset by section base
                rl-off @ 2 PICK CELLS sec-base + @ +
                R@ CELLS grel-off + !

                \ Store section type
                OVER R@ CELLS grel-sec + !

                R> DROP
                1 grel-cnt +!
            THEN

            16 +  \ advance reloc-off
        LOOP
        2DROP  \ drop sec-type and reloc-off
    LOOP ;

\ === LINK-OBJ ===
: LINK-OBJ ( c-addr u -- )
    ." Loading: " 2DUP TYPE CR
    READ-FILE-BUF
    DUP 0< IF DROP ." Error reading file" CR EXIT THEN
    DROP
    PARSE-OBJ-HEADER 0= IF EXIT THEN
    sym-map MAX-FILE-SYM CELLS 0 FILL
    MERGE-SECTIONS
    link-error @ IF EXIT THEN
    MERGE-SYMBOLS
    link-error @ IF EXIT THEN
    MERGE-RELOCS
    ."   text=" text-sz @ .
    ." data=" data-sz @ .
    ." bss=" bss-sz @ .
    ." rodata=" rodata-sz @ .
    ." syms=" gsym-cnt @ .
    ." relocs=" grel-cnt @ . CR ;

\ === LINK-ARCHIVE ===

: AR-SYM-NAME ( idx -- c-addr u )
    8 * ar-symtab + RD32
    DUP ar-str-sz @ >= IF DROP ar-strtab 0 EXIT THEN
    ar-strtab +
    DUP 0 BEGIN OVER OVER + C@ 0<> WHILE 1+ REPEAT NIP ;

: AR-SYM-MEMBER ( idx -- member-idx )
    8 * ar-symtab + 4 + RD32 ;

: AR-MEM-OFFSET ( idx -- file-offset )
    24 * ar-memtab + 4 + RD32 ;

: AR-MEM-SIZE ( idx -- size )
    24 * ar-memtab + 8 + RD32 ;

: SYM-UNDEFINED? ( c-addr u -- flag )
    GSYM-FIND DUP 0< IF DROP FALSE EXIT THEN
    CELLS gsym-def + @ 0= ;

: LOAD-AR-MEMBER ( member-idx -- )
    DUP ar-nmembers @ >= IF DROP EXIT THEN
    DUP mem-loaded + C@ IF DROP EXIT THEN
    DUP mem-loaded + 1 SWAP C!
    DUP AR-MEM-SIZE SWAP AR-MEM-OFFSET
    ( size file-offset )
    S>D ar-fh @ REPOSITION-FILE DROP
    ( size )
    DUP FILE-BUFSZ > IF ." Member too large" CR DROP EXIT THEN
    file-buf SWAP ar-fh @ READ-FILE DROP DROP
    PARSE-OBJ-HEADER 0= IF EXIT THEN
    sym-map MAX-FILE-SYM CELLS 0 FILL
    MERGE-SECTIONS
    link-error @ IF EXIT THEN
    MERGE-SYMBOLS
    link-error @ IF EXIT THEN
    MERGE-RELOCS ;

: LINK-ARCHIVE ( c-addr u -- )
    ." Loading archive: " 2DUP TYPE CR
    R/O OPEN-FILE IF DROP ." Cannot open archive" CR EXIT THEN
    ar-fh !

    \ Read header (32 bytes)
    file-buf 32 ar-fh @ READ-FILE DROP DROP
    file-buf RD32 S32A-MAGIC <> IF
        ." Error: bad .s32a magic" CR
        ar-fh @ CLOSE-FILE DROP EXIT
    THEN

    file-buf  8 + RD32 ar-nmembers !
    file-buf 12 + RD32 ar-mem-off !
    file-buf 16 + RD32 ar-nsymbols !
    file-buf 20 + RD32 ar-sym-off !
    file-buf 24 + RD32 ar-str-off !
    file-buf 28 + RD32 ar-str-sz !

    ."   members=" ar-nmembers @ .
    ." symbols=" ar-nsymbols @ . CR

    ar-nmembers @ MAX-MEMBERS > IF
        ." Error: archive member table too large" CR
        1 link-error ! ar-fh @ CLOSE-FILE DROP EXIT
    THEN
    ar-nsymbols @ 8 * AR-SYMTAB-SZ > IF
        ." Error: archive symbol table too large" CR
        1 link-error ! ar-fh @ CLOSE-FILE DROP EXIT
    THEN
    ar-nmembers @ 24 * AR-MEMTAB-SZ > IF
        ." Error: archive member metadata too large" CR
        1 link-error ! ar-fh @ CLOSE-FILE DROP EXIT
    THEN
    ar-str-sz @ AR-STRTAB-SZ > IF
        ." Error: archive string table too large" CR
        1 link-error ! ar-fh @ CLOSE-FILE DROP EXIT
    THEN

    \ Read symbol index
    ar-sym-off @ S>D ar-fh @ REPOSITION-FILE DROP
    ar-symtab ar-nsymbols @ 8 * ar-fh @ READ-FILE DROP DROP

    \ Read member table
    ar-mem-off @ S>D ar-fh @ REPOSITION-FILE DROP
    ar-memtab ar-nmembers @ 24 * ar-fh @ READ-FILE DROP DROP

    \ Read string table
    ar-str-off @ S>D ar-fh @ REPOSITION-FILE DROP
    ar-strtab ar-str-sz @ ar-fh @ READ-FILE DROP DROP

    \ Clear loaded flags
    mem-loaded MAX-MEMBERS 0 FILL

    \ Iterative pull loop
    BEGIN
        FALSE ( added-any )
        ar-nsymbols @ 0 ?DO
            I AR-SYM-MEMBER
            DUP ar-nmembers @ < IF
                DUP mem-loaded + C@ 0= IF
                    I AR-SYM-NAME SYM-UNDEFINED? IF
                        LOAD-AR-MEMBER
                        DROP TRUE
                    ELSE
                        DROP
                    THEN
                ELSE
                    DROP
                THEN
            ELSE
                DROP
            THEN
            link-error @ IF LEAVE THEN
        LOOP
        link-error @ IF DROP FALSE THEN
    0= UNTIL

    ar-fh @ CLOSE-FILE DROP
    ."   After: syms=" gsym-cnt @ .
    ." relocs=" grel-cnt @ . CR ;

\ === LINK-MMIO ===
: LINK-MMIO ( size -- ) mmio-sz ! ;

\ === Section layout ===
: LAYOUT-SECTIONS ( -- )
    0 text-va !
    text-sz @ PAGE-ALIGN CODE-LIMIT-MIN MAX DUP rodata-va !
    rodata-sz @ + PAGE-ALIGN DUP data-va !
    data-sz @ + DUP bss-va !
    bss-sz @ + DUP bss-end-va !
    PAGE-ALIGN heap-va !

    mmio-sz @ 0> IF
        heap-va @ HEAP-GAP-DEF + PAGE-ALIGN mmio-va !
    ELSE
        0 mmio-va !
    THEN ;

\ === Symbol VA computation ===
: SEC-VA ( sec-type -- va )
    CASE
        SEC-CODE   OF text-va   @ ENDOF
        SEC-DATA   OF data-va   @ ENDOF
        SEC-BSS    OF bss-va    @ ENDOF
        SEC-RODATA OF rodata-va @ ENDOF
        0 SWAP
    ENDCASE ;

: GSYM-VA ( idx -- va )
    DUP CELLS gsym-def + @ 2 = IF
        \ Linker-defined: value is absolute
        CELLS gsym-val + @
    ELSE
        DUP CELLS gsym-sec + @ SEC-VA
        SWAP CELLS gsym-val + @ +
    THEN ;

: GSYM-NAME ( idx -- c-addr u )
    DUP CELLS gsym-noff + @ gsym-nbuf +
    SWAP CELLS gsym-nlen + @ ;

\ Resolve symbol VA for relocation. If symbol is undefined but has form
\ "base+N" or "base-N", resolve against base symbol and apply N.
: GSYM-VA-REL ( idx -- va ok? )
    DUP 0< IF DROP 0 FALSE EXIT THEN
    DUP CELLS gsym-def + @ 0<> IF GSYM-VA TRUE EXIT THEN
    GSYM-NAME 2DUP MMIO-ALIAS-VA IF
        >R 2DROP R> TRUE EXIT
    THEN
    2DROP
    GSYM-NAME SPLIT-SYM-EXPR IF
        sex-add !
        2DUP GSYM-FIND DUP 0< IF
            DROP 2DROP 0 FALSE EXIT
        THEN
        >R 2DROP R>
        DUP CELLS gsym-def + @ 0= IF
            DROP 0 FALSE EXIT
        THEN
        GSYM-VA sex-add @ + TRUE EXIT
    THEN
    0 FALSE ;

\ === Inject linker-defined symbols ===
: INJECT-SYM ( c-addr u value -- )
    >R 2DUP GSYM-FIND
    DUP 0< IF
        \ Not yet in table — add as linker-defined
        DROP SEC-CODE R> BIND-GLOBAL 2 GSYM-ADD-NEW DROP
    ELSE
        \ In table — update only if undefined
        DUP CELLS gsym-def + @ 0= IF
            R> OVER CELLS gsym-val + !
            2 OVER CELLS gsym-def + !
            DROP 2DROP
        ELSE
            DROP 2DROP R> DROP
        THEN
    THEN ;

: INJECT-LINKER-SYMBOLS ( -- )
    S" __bss_start"   bss-va @      INJECT-SYM
    S" __bss_end"     bss-end-va @  INJECT-SYM
    S" __bss_start__" bss-va @      INJECT-SYM
    S" __bss_end__"   bss-end-va @  INJECT-SYM
    S" _bss_start"    bss-va @      INJECT-SYM
    S" _bss_end"      bss-end-va @  INJECT-SYM
    S" __mmio_base"   mmio-va @     INJECT-SYM
    S" __mmio_base+4"     mmio-va @ 4 +     INJECT-SYM
    S" __mmio_base+4096"  mmio-va @ 4096 +  INJECT-SYM
    S" __mmio_base+4100"  mmio-va @ 4100 +  INJECT-SYM
    S" __mmio_base+4104"  mmio-va @ 4104 +  INJECT-SYM
    S" __mmio_base+4108"  mmio-va @ 4108 +  INJECT-SYM
    S" __mmio_base+8192"  mmio-va @ 8192 +  INJECT-SYM
    S" __mmio_base+8196"  mmio-va @ 8196 +  INJECT-SYM
    S" __mmio_base+12300" mmio-va @ 12300 + INJECT-SYM
    S" __mmio_base+16384" mmio-va @ 16384 + INJECT-SYM
    S" __mmio_base+16388" mmio-va @ 16388 + INJECT-SYM
    S" __mmio_base+16392" mmio-va @ 16392 + INJECT-SYM
    S" __mmio_base+16396" mmio-va @ 16396 + INJECT-SYM
    S" __mmio_base+16400" mmio-va @ 16400 + INJECT-SYM
    S" __mmio_base+16404" mmio-va @ 16404 + INJECT-SYM
    S" __mmio_base+16408" mmio-va @ 16408 + INJECT-SYM
    S" __mmio_base+16412" mmio-va @ 16412 + INJECT-SYM
    S" __mmio_base+16416" mmio-va @ 16416 + INJECT-SYM
    S" __mmio_base+16420" mmio-va @ 16420 + INJECT-SYM
    S" __mmio_base+16424" mmio-va @ 16424 + INJECT-SYM
    S" __mmio_base+16428" mmio-va @ 16428 + INJECT-SYM
    S" __mmio_base+16432" mmio-va @ 16432 + INJECT-SYM
    S" __mmio_end"    mmio-va @ mmio-sz @ + INJECT-SYM
    S" __heap_start"  heap-va @     INJECT-SYM
    mmio-sz @ 0> IF
        S" __heap_end" mmio-va @    INJECT-SYM
    ELSE
        S" __heap_end" STACK-BASE-DEF STACK-SIZE-DEF - INJECT-SYM
    THEN
    S" __stack_base"   STACK-BASE-DEF INJECT-SYM
    S" __stack_top"    STACK-BASE-DEF INJECT-SYM
    S" __stack_end"    STACK-BASE-DEF STACK-SIZE-DEF - INJECT-SYM
    S" __stack_bottom" STACK-BASE-DEF STACK-SIZE-DEF - INJECT-SYM
    S" _stack"         STACK-BASE-DEF INJECT-SYM
    S" __code_start"   0             INJECT-SYM
    S" __code_end"     text-sz @ PAGE-ALIGN INJECT-SYM
    S" __rodata_start" rodata-va @   INJECT-SYM
    S" __rodata_end"   rodata-va @ rodata-sz @ + PAGE-ALIGN INJECT-SYM
    S" __data_start"   data-va @     INJECT-SYM
    S" __data_end"     bss-va @      INJECT-SYM
    S" __data_load"       data-va @  INJECT-SYM
    S" __data_load_start" data-va @  INJECT-SYM
    S" __etext"  rodata-va @ rodata-sz @ + PAGE-ALIGN INJECT-SYM
    S" _etext"   rodata-va @ rodata-sz @ + PAGE-ALIGN INJECT-SYM
    S" etext"    rodata-va @ rodata-sz @ + PAGE-ALIGN INJECT-SYM
    S" __edata"  bss-va @     INJECT-SYM
    S" _edata"   bss-va @     INJECT-SYM
    S" edata"    bss-va @     INJECT-SYM
    S" __end"    bss-end-va @ INJECT-SYM
    S" _end"     bss-end-va @ INJECT-SYM
    S" end"      bss-end-va @ INJECT-SYM
    S" __init_array_start" 0 INJECT-SYM
    S" __init_array_end"   0 INJECT-SYM ;

\ === Apply relocations ===

: IS-STORE? ( opcode -- flag )
    DUP OP-STB = OVER OP-STH = OR SWAP OP-STW = OR ;

: APPLY-REL ( idx -- )
    >R
    R@ CELLS grel-off + @ ar-off !
    R@ CELLS grel-sec + @ ar-sec !
    R@ CELLS grel-typ + @ ar-rtyp !
    R@ CELLS grel-add + @ ar-radd !
    R@ CELLS grel-sym + @ ar-gsym !
    R> DROP

    \ value = sym_va + addend
    ar-gsym @ GSYM-VA-REL IF
        ar-radd @ + ar-val !
    ELSE
        ." Error: unresolved reloc symbol: "
        ar-gsym @ 0< 0= IF
            ar-gsym @ GSYM-NAME TYPE
        ELSE
            ." <invalid>"
        THEN
        CR
        1 link-error !
        0 ar-val !
    THEN

    \ Target buffer address
    ar-sec @ SEC-BUF ar-off @ + ar-tgt !

    \ PC = section_va + offset
    ar-sec @ SEC-VA ar-off @ + ar-pc !

    ar-rtyp @ CASE
        REL-32 OF
            ar-val @ ar-tgt @ WR32
        ENDOF

        REL-ABS32 OF
            ar-val @ ar-tgt @ WR32
        ENDOF

        REL-HI20 OF
            ar-val @ MASK12 AND  ( lo12 )
            ar-val @ 12 RSHIFT MASK20 AND  ( lo12 hi20 )
            OVER HALF12 AND IF 1+ MASK20 AND THEN
            12 LSHIFT
            ar-tgt @ RD32 MASK12 AND OR
            ar-tgt @ WR32
            DROP
        ENDOF

        REL-LO12 OF
            ar-val @ MASK12 AND  ( imm )
            ar-tgt @ RD32  ( imm insn )
            DUP MASK7 AND  ( imm insn opcode )
            IS-STORE? IF
                S-MASK AND
                OVER MASK5 AND 7 LSHIFT OR
                SWAP 5 RSHIFT MASK7 AND 25 LSHIFT OR
            ELSE
                MASK20 AND
                SWAP 20 LSHIFT OR
            THEN
            ar-tgt @ WR32
        ENDOF

        REL-BRANCH OF
            ar-val @ ar-pc @ - 4 -  ( offset )
            DUP >R
            R@ 12 RSHIFT 1 AND
            R@ 11 RSHIFT 1 AND
            R@ 5 RSHIFT MASK6 AND
            R> 1 RSHIFT 15 AND   \ MASK4 = 0xF = 15
            ar-tgt @ RD32
            B-MASK AND
            SWAP 8 LSHIFT OR
            SWAP 25 LSHIFT OR
            SWAP 7 LSHIFT OR
            SWAP 31 LSHIFT OR
            ar-tgt @ WR32
        ENDOF

        REL-JAL OF
            ar-val @ ar-pc @ -  ( offset )
            DUP >R
            R@ 20 RSHIFT 1 AND
            R@ 12 RSHIFT MASK8 AND
            R@ 11 RSHIFT 1 AND
            R> 1 RSHIFT MASK10 AND
            ar-tgt @ RD32 MASK12 AND
            SWAP 21 LSHIFT OR
            SWAP 20 LSHIFT OR
            SWAP 12 LSHIFT OR
            SWAP 31 LSHIFT OR
            ar-tgt @ WR32
        ENDOF

        REL-CALL OF
            ar-val @ ar-pc @ -
            DUP >R
            R@ 20 RSHIFT 1 AND
            R@ 12 RSHIFT MASK8 AND
            R@ 11 RSHIFT 1 AND
            R> 1 RSHIFT MASK10 AND
            ar-tgt @ RD32 MASK12 AND
            SWAP 21 LSHIFT OR
            SWAP 20 LSHIFT OR
            SWAP 12 LSHIFT OR
            SWAP 31 LSHIFT OR
            ar-tgt @ WR32
        ENDOF

        REL-PCREL-HI20 OF
            ar-val @ ar-pc @ -  ( pc-relative offset )
            DUP MASK12 AND
            SWAP 12 RSHIFT MASK20 AND
            OVER HALF12 AND IF 1+ MASK20 AND THEN
            12 LSHIFT
            ar-tgt @ RD32 MASK12 AND OR
            ar-tgt @ WR32
            DROP
        ENDOF

        REL-PCREL-LO12 OF
            \ Find matching PCREL_HI20
            -1
            grel-cnt @ 0 ?DO
                I CELLS grel-typ + @ REL-PCREL-HI20 = IF
                    I CELLS grel-sym + @ ar-gsym @ = IF
                        I CELLS grel-sec + @ SEC-VA
                        I CELLS grel-off + @ +
                        NIP  \ replace -1 with hi-pc
                        LEAVE
                    THEN
                THEN
            LOOP
            DUP 0< IF
                DROP ." Error: PCREL_LO12 no match" CR
            ELSE
                ar-val @ SWAP -
                MASK12 AND
                ar-tgt @ RD32
                DUP MASK7 AND IS-STORE? IF
                    S-MASK AND
                    OVER MASK5 AND 7 LSHIFT OR
                    SWAP 5 RSHIFT MASK7 AND 25 LSHIFT OR
                ELSE
                    MASK20 AND
                    SWAP 20 LSHIFT OR
                THEN
                ar-tgt @ WR32
            THEN
        ENDOF

        \ Default: skip
        DUP ." Warning: reloc type " . CR
    ENDCASE ;

: APPLY-RELOCATIONS ( -- )
    grel-cnt @ 0 ?DO I APPLY-REL LOOP ;

\ === Find entry point ===
: FIND-ENTRY ( -- )
    S" _start" GSYM-FIND DUP 0< IF
        DROP ." Error: _start not found" CR 0 entry-pt !
    ELSE
        GSYM-VA entry-pt !
    THEN ;

\ === Count output sections ===
: COUNT-SECTIONS ( -- n )
    0
    text-sz @ 0> IF 1+ THEN
    data-sz @ 0> IF 1+ THEN
    bss-sz  @ 0> IF 1+ THEN
    rodata-sz @ 0> IF 1+ THEN ;

\ === Build output string table ===
: BUILD-STRTAB ( -- )
    OSTRTAB-INIT
    text-sz   @ 0> IF S" .text"   OSTRTAB-ADD str-text-off   ! THEN
    data-sz   @ 0> IF S" .data"   OSTRTAB-ADD str-data-off   ! THEN
    bss-sz    @ 0> IF S" .bss"    OSTRTAB-ADD str-bss-off    ! THEN
    rodata-sz @ 0> IF S" .rodata" OSTRTAB-ADD str-rodata-off ! THEN ;

\ === Write helpers ===

: WRITE-ZEROS ( n -- )
    0 ?DO 0 wb-buf C! wb-buf 1 out-fh @ WRITE-FILE DROP LOOP ;

: PAD-BYTES ( alignment -- n )
    file-pos @ OVER MOD
    DUP 0<> IF SWAP OVER - NIP ELSE 2DROP 0 THEN ;

: WRITE-HEADER ( -- )
    WB-INIT
    S32X-MAGIC 0 WB!
    1 4 WB16!              \ version
    1 6 WB8!               \ endian = little
    MACHINE-ID 7 WB8!      \ machine = 0x32
    entry-pt @ 8 WB!       \ entry point
    out-nsec @ 12 WB!      \ nsections
    64 16 WB!              \ sec_offset (right after 64-byte header)
    out-strtab-off @ 20 WB! \ str_offset
    out-strtab-sz @ 24 WB! \ str_size
    \ flags
    FLAG-WXORX mmio-sz @ 0> IF FLAG-MMIO OR THEN
    28 WB!
    text-sz @ PAGE-ALIGN CODE-LIMIT-MIN MAX 32 WB!      \ code_limit
    rodata-va @ rodata-sz @ + PAGE-ALIGN 36 WB!          \ rodata_limit
    bss-end-va @ 40 WB!                                  \ data_limit (= bss end)
    STACK-BASE-DEF 44 WB!                                \ stack_base
    MEM-SIZE-DEF 48 WB!                                  \ mem_size
    heap-va @ 52 WB!                                     \ heap_base
    STACK-BASE-DEF STACK-SIZE-DEF - 56 WB!               \ stack_end
    mmio-va @ 60 WB!                                     \ mmio_base
    wb-buf 64 out-fh @ WRITE-FILE THROW ;

\ Write a section table entry (28 bytes)
: WRITE-SEC-ENTRY ( str-off type vaddr file-off size mem-size flags -- )
    WB-INIT
    24 WB!     \ flags
    20 WB!     \ mem_size
    16 WB!     \ size
    12 WB!     \ file offset
    8 WB!      \ vaddr
    4 WB!      \ type
    0 WB!      \ name string offset
    wb-buf 28 out-fh @ WRITE-FILE THROW ;

\ === LINK-EMIT ===
: LINK-EMIT ( c-addr u -- )
    \ Save filename to permanent buffer (S" is transient)
    DUP out-fname-len !
    out-fname SWAP CMOVE
    ." Writing: " out-fname out-fname-len @ TYPE CR

    LAYOUT-SECTIONS
    INJECT-LINKER-SYMBOLS
    APPLY-RELOCATIONS
    FIND-ENTRY

    COUNT-SECTIONS out-nsec !
    BUILD-STRTAB

    \ File layout:
    \ [0..63]     header (64 bytes)
    \ [64..]      section table (nsec * 28 bytes)
    \ [strtab..]  string table
    \ [pad16]     padding to 16-byte alignment
    \ [data..]    section data: text, data, rodata (BSS has none)

    64 out-nsec @ 28 * + out-strtab-off !
    out-strtab-off @ out-strtab-sz @ + ALIGN16 out-data-off !

    \ Compute file offsets for section data
    out-data-off @
    text-sz @ 0> IF
        DUP sec-text-foff ! text-sz @ + ALIGN4
    ELSE
        0 sec-text-foff !
    THEN
    data-sz @ 0> IF
        DUP sec-data-foff ! data-sz @ + ALIGN4
    ELSE
        0 sec-data-foff !
    THEN
    rodata-sz @ 0> IF
        DUP sec-rodata-foff !
    ELSE
        0 sec-rodata-foff !
    THEN
    DROP

    \ Open output file (use saved filename)
    out-fname out-fname-len @ WO-CREATE OPEN-FILE THROW out-fh !
    0 file-pos !

    \ Write header
    WRITE-HEADER
    64 file-pos !

    \ Write section table
    text-sz @ 0> IF
        str-text-off @
        SEC-CODE text-va @ sec-text-foff @ text-sz @ text-sz @ SF-XRA
        WRITE-SEC-ENTRY
        28 file-pos +!
    THEN
    data-sz @ 0> IF
        str-data-off @
        SEC-DATA data-va @ sec-data-foff @ data-sz @ data-sz @ SF-WRA
        WRITE-SEC-ENTRY
        28 file-pos +!
    THEN
    bss-sz @ 0> IF
        str-bss-off @
        SEC-BSS bss-va @ 0 0 bss-sz @ SF-WRA
        WRITE-SEC-ENTRY
        28 file-pos +!
    THEN
    rodata-sz @ 0> IF
        str-rodata-off @
        SEC-RODATA rodata-va @ sec-rodata-foff @ rodata-sz @ rodata-sz @ SF-RA
        WRITE-SEC-ENTRY
        28 file-pos +!
    THEN

    \ Write string table
    out-strtab out-strtab-sz @ out-fh @ WRITE-FILE THROW
    out-strtab-sz @ file-pos +!

    \ Pad to 16 bytes
    16 PAD-BYTES DUP 0> IF
        WRITE-ZEROS
    ELSE DROP THEN
    out-data-off @ file-pos !

    \ Write section data
    text-sz @ 0> IF
        text-buf text-sz @ out-fh @ WRITE-FILE THROW
        text-sz @ file-pos +!
        4 PAD-BYTES DUP 0> IF WRITE-ZEROS ELSE DROP THEN
        file-pos @ ALIGN4 file-pos !
    THEN
    data-sz @ 0> IF
        data-buf data-sz @ out-fh @ WRITE-FILE THROW
        data-sz @ file-pos +!
        4 PAD-BYTES DUP 0> IF WRITE-ZEROS ELSE DROP THEN
        file-pos @ ALIGN4 file-pos !
    THEN
    rodata-sz @ 0> IF
        rodata-buf rodata-sz @ out-fh @ WRITE-FILE THROW
        rodata-sz @ file-pos +!
    THEN

    out-fh @ CLOSE-FILE DROP

    ." Link complete." CR
    ." entry=" entry-pt @ . CR
    ." text=" text-sz @ . ." data=" data-sz @ .
    ." bss=" bss-sz @ . ." rodata=" rodata-sz @ . CR ;

." Stage 3 linker loaded." CR
