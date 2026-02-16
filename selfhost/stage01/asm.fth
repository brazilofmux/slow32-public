\ ============================================================
\ SLOW-32 Forth-Hosted Assembler — Stage 2 Bootstrap
\ ============================================================
\
\ A two-pass assembler that reads .s source files and produces
\ .s32o object files (linkable with s32-ld).
\
\ Usage:
\   S" input.s" S" output.s32o" ASSEMBLE
\
\ Runs inside the Forth kernel on the Stage 0 emulator.
\ Requires prelude.fth (for /STRING, CASE, R/O, W/O-TRUNC).
\ ============================================================

\ === Hex constants (kernel lacks $ prefix) ===
HEX
FFF      CONSTANT MASK12
1F       CONSTANT MASK5
7F       CONSTANT MASK7
F        CONSTANT MASK4
3F       CONSTANT MASK6
FF       CONSTANT MASK8
3FF      CONSTANT MASK10
FFFFF    CONSTANT MASK20
800      CONSTANT HALF12
80       CONSTANT FLAG-MMIO
53333258 CONSTANT S32X-MAGIC
5333324F CONSTANT S32O-MAGIC
FEFFF0   CONSTANT DEFAULT-STACK
1000000  CONSTANT DEFAULT-MEMSZ
FF0000   CONSTANT DEFAULT-MMIO
D        CONSTANT SF-XRA       \ EXEC|READ|ALLOC
E        CONSTANT SF-WRA       \ WRITE|READ|ALLOC
DECIMAL

\ Relocation types (decimal)
1 CONSTANT REL-32
2 CONSTANT REL-HI20
3 CONSTANT REL-LO12
5 CONSTANT REL-JAL

\ === Configuration ===
262144 CONSTANT INP-SZ      \ 256KB input buffer
131072 CONSTANT CODE-SZ     \ 128KB code output
65536  CONSTANT DATA-SZ     \ 64KB data output
1024   CONSTANT MAX-SYM     \ max symbols
32768  CONSTANT SNAME-SZ    \ symbol name storage

\ === Buffers ===
CREATE inp-buf INP-SZ ALLOT
CREATE code-buf CODE-SZ ALLOT
CREATE data-buf DATA-SZ ALLOT

\ Symbol table (parallel arrays)
CREATE sym-nm   MAX-SYM CELLS ALLOT   \ pointer into sname-buf
CREATE sym-nl   MAX-SYM CELLS ALLOT   \ name length
CREATE sym-sec  MAX-SYM CELLS ALLOT   \ section (0=text,1=data,2=bss)
CREATE sym-off  MAX-SYM CELLS ALLOT   \ offset within section
VARIABLE sym-cnt
CREATE sname-buf SNAME-SZ ALLOT
VARIABLE sname-ptr

\ Relocation table (parallel arrays)
4096 CONSTANT MAX-RELOC
CREATE reloc-off MAX-RELOC CELLS ALLOT   \ offset in section
CREATE reloc-sec MAX-RELOC CELLS ALLOT   \ section (0=text,1=data)
CREATE reloc-typ MAX-RELOC CELLS ALLOT   \ type (REL-HI20, REL-LO12, REL-32)
CREATE reloc-add MAX-RELOC CELLS ALLOT   \ addend
CREATE reloc-sym MAX-RELOC CELLS ALLOT   \ offset into rname-buf
CREATE reloc-sln MAX-RELOC CELLS ALLOT   \ name length
VARIABLE reloc-cnt
65536 CONSTANT RNAME-SZ
CREATE rname-buf RNAME-SZ ALLOT
VARIABLE rname-ptr

\ Global label tracking
256 CONSTANT MAX-GLOBAL
CREATE gname-off MAX-GLOBAL CELLS ALLOT  \ offset into gname-buf
CREATE gname-len MAX-GLOBAL CELLS ALLOT  \ name length
VARIABLE gname-cnt
4096 CONSTANT GNAME-SZ
CREATE gname-buf GNAME-SZ ALLOT
VARIABLE gname-ptr

\ String table for .s32o output
65536 CONSTANT STRTAB-SZ
CREATE strtab STRTAB-SZ ALLOT
VARIABLE strtab-sz

\ Output symbol table
2048 CONSTANT MAX-OUT-SYM
CREATE osym-stridx MAX-OUT-SYM CELLS ALLOT  \ string table index
CREATE osym-value  MAX-OUT-SYM CELLS ALLOT  \ symbol value
CREATE osym-sec    MAX-OUT-SYM CELLS ALLOT  \ output section index (1-based)
CREATE osym-bind   MAX-OUT-SYM CELLS ALLOT  \ binding (0=local, 1=global)
CREATE osym-nmbuf  MAX-OUT-SYM CELLS ALLOT  \ name pointer
CREATE osym-nmlen  MAX-OUT-SYM CELLS ALLOT  \ name length
VARIABLE osym-cnt

\ File handle and layout variables
VARIABLE out-fh
VARIABLE out-nsec
VARIABLE text-out-idx
VARIABLE data-out-idx
VARIABLE bss-out-idx
VARIABLE text-str-idx
VARIABLE data-str-idx
VARIABLE bss-str-idx
VARIABLE text-nreloc
VARIABLE data-nreloc
VARIABLE bss-nreloc
VARIABLE off-sections
VARIABLE off-symbols
VARIABLE off-relocs
VARIABLE off-strtab
VARIABLE off-secdata

\ Temp variables for helpers
VARIABLE imm-sym-addr
VARIABLE imm-sym-len
VARIABLE imm-add
VARIABLE r-off
VARIABLE r-typ
VARIABLE r-add
VARIABLE r-naddr
VARIABLE r-nlen
VARIABLE sa-addr
VARIABLE sa-len
VARIABLE sa-off
VARIABLE bg-addr
VARIABLE bg-len
VARIABLE br-addr
VARIABLE br-len

\ Write buffer for binary output
CREATE wb-buf 64 ALLOT

\ === Assembler State ===
VARIABLE asm-pass       \ 1 or 2
VARIABLE asm-sect       \ current section (0=text,1=data,2=bss)
VARIABLE asm-tsz        \ .text size
VARIABLE asm-dsz        \ .data size
VARIABLE asm-bsz        \ .bss size
VARIABLE asm-errs       \ error count
CREATE out-path 256 ALLOT
VARIABLE out-path-len
VARIABLE asm-lno        \ line number
VARIABLE asm-inp-len    \ input length
VARIABLE asm-inp-pos    \ input scan position

\ Virtual addresses (computed after pass 1)
VARIABLE text-va        \ always 0
VARIABLE data-va        \ after .text, aligned
VARIABLE bss-va         \ after .data, aligned

\ Line parsing
8192 CONSTANT LBUF-SZ
CREATE lbuf LBUF-SZ ALLOT
VARIABLE lbuf-len
VARIABLE lpos           \ current parse position
VARIABLE lbuf-trunc

\ Temp for string emit
VARIABLE str-idx

\ === Helpers ===
: ALIGN4 ( n -- n' ) 3 + -4 AND ;

: ASM-ERR ( addr u -- )
    ." Error line " asm-lno @ . ." : " TYPE CR
    1 asm-errs +! ;

: CUR-OFF ( -- addr )
    asm-sect @ CASE
        0 OF asm-tsz ENDOF
        1 OF asm-dsz ENDOF
        2 OF asm-bsz ENDOF
    ENDCASE ;

: CUR-BUF ( -- addr )
    asm-sect @ CASE
        0 OF code-buf ENDOF
        1 OF data-buf ENDOF
        2 OF 0        ENDOF
    ENDCASE ;

: EMIT-B ( byte -- )
    asm-pass @ 2 = asm-sect @ 2 <> AND IF
        CUR-BUF CUR-OFF @ + C!
    ELSE
        DROP
    THEN
    1 CUR-OFF +! ;

: EMIT-W32 ( u -- )
    DUP         255 AND EMIT-B
    DUP 8  RSHIFT 255 AND EMIT-B
    DUP 16 RSHIFT 255 AND EMIT-B
        24 RSHIFT 255 AND EMIT-B ;

\ === Symbol Table ===
: SYM-ADD ( addr u sect off -- )
    sym-cnt @ MAX-SYM >= IF 2DROP 2DROP S" symbol table full" ASM-ERR EXIT THEN
    sym-cnt @ >R
    R@ CELLS sym-off + !
    R@ CELLS sym-sec + !
    DUP R@ CELLS sym-nl + !
    sname-ptr @ R@ CELLS sym-nm + !
    sname-ptr @ sname-buf + SWAP CMOVE
    R@ CELLS sym-nl + @ sname-ptr +!
    R> 1+ sym-cnt ! ;

: SYM-FIND ( addr u -- idx | -1 )
    sym-cnt @ 0 ?DO
        2DUP
        I CELLS sym-nm + @ sname-buf +
        I CELLS sym-nl + @
        COMPARE 0= IF 2DROP I UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

: SYM-VA ( idx -- vaddr )
    DUP CELLS sym-sec + @ CASE
        0 OF text-va ENDOF
        1 OF data-va ENDOF
        2 OF bss-va  ENDOF
    ENDCASE @
    SWAP CELLS sym-off + @ + ;

\ === .s32o Helper Words ===

\ String table management
: STRTAB-INIT  0 strtab C! 1 strtab-sz ! ;

: STRTAB-ADD ( addr u -- offset )
    sa-len ! sa-addr !
    strtab-sz @ sa-len @ + 1+ STRTAB-SZ > IF
        S" strtab overflow" ASM-ERR
        0 EXIT
    THEN
    strtab-sz @ sa-off !
    sa-addr @ strtab sa-off @ + sa-len @ CMOVE
    0 strtab sa-off @ sa-len @ + + C!
    sa-off @ sa-len @ + 1+ strtab-sz !
    sa-off @ ;

\ Output symbol table management
: OSYM-FIND ( addr u -- index | -1 )
    osym-cnt @ 0 ?DO
        2DUP
        I CELLS osym-nmbuf + @
        I CELLS osym-nmlen + @
        COMPARE 0= IF 2DROP I UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

: OSYM-ADD ( stridx value sec bind c-addr u -- )
    osym-cnt @ MAX-OUT-SYM >= IF
        2DROP 2DROP 2DROP
        S" output symbol table full" ASM-ERR
        EXIT
    THEN
    osym-cnt @ >R
    R@ CELLS osym-nmlen + !
    R@ CELLS osym-nmbuf + !
    R@ CELLS osym-bind + !
    R@ CELLS osym-sec + !
    R@ CELLS osym-value + !
    R@ CELLS osym-stridx + !
    R> 1+ osym-cnt ! ;

\ Record a relocation entry (pass 2 only)
: ADD-RELOC ( offset type addend c-addr u -- )
    asm-pass @ 2 <> IF 2DROP DROP DROP DROP EXIT THEN
    r-nlen ! r-naddr !
    r-add ! r-typ ! r-off !
    reloc-cnt @ MAX-RELOC >= IF S" reloc table full" ASM-ERR EXIT THEN
    rname-ptr @ r-nlen @ + RNAME-SZ > IF
        S" reloc name table full" ASM-ERR EXIT
    THEN
    reloc-cnt @ >R
    r-off @     R@ CELLS reloc-off + !
    asm-sect @  R@ CELLS reloc-sec + !
    r-typ @     R@ CELLS reloc-typ + !
    r-add @     R@ CELLS reloc-add + !
    \ Copy symbol name to rname-buf
    r-naddr @ rname-buf rname-ptr @ + r-nlen @ CMOVE
    rname-ptr @ R@ CELLS reloc-sym + !
    r-nlen @    R@ CELLS reloc-sln + !
    r-nlen @    rname-ptr +!
    R> 1+ reloc-cnt ! ;

\ Map internal section (0/1/2) to 1-based output index
: SEC-TO-OUT-IDX ( sec -- idx )
    CASE
        0 OF text-out-idx @ ENDOF
        1 OF data-out-idx @ ENDOF
        2 OF bss-out-idx @  ENDOF
    ENDCASE ;

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

: WB-WRITE ( count -- ) wb-buf SWAP out-fh @ WRITE-FILE THROW ;

\ Build output symbol table from .global declarations
: BUILD-GLOBALS
    gname-cnt @ 0 ?DO
        I CELLS gname-off + @ gname-buf + bg-addr !
        I CELLS gname-len + @ bg-len !
        bg-addr @ bg-len @ SYM-FIND DUP -1 <> IF
            >R
            bg-addr @ bg-len @ STRTAB-ADD
            R@ CELLS sym-off + @
            R> CELLS sym-sec + @ SEC-TO-OUT-IDX
            1
            bg-addr @ bg-len @
            OSYM-ADD
        ELSE
            DROP
            bg-addr @ bg-len @ STRTAB-ADD
            0 0 1 bg-addr @ bg-len @
            OSYM-ADD
        THEN
    LOOP ;

\ Build output symbols from relocation references
: BUILD-RELOC-SYMS
    reloc-cnt @ 0 ?DO
        I CELLS reloc-sym + @ rname-buf + br-addr !
        I CELLS reloc-sln + @ br-len !
        br-addr @ br-len @ OSYM-FIND -1 = IF
            br-addr @ br-len @ SYM-FIND DUP -1 <> IF
                >R
                br-addr @ br-len @ STRTAB-ADD
                R@ CELLS sym-off + @
                R> CELLS sym-sec + @ SEC-TO-OUT-IDX
                0
                br-addr @ br-len @
                OSYM-ADD
            ELSE
                DROP
                br-addr @ br-len @ STRTAB-ADD
                0 0 1 br-addr @ br-len @
                OSYM-ADD
            THEN
        THEN
    LOOP ;

\ Count per-section relocations
: COUNT-RELOCS
    0 text-nreloc !  0 data-nreloc !  0 bss-nreloc !
    reloc-cnt @ 0 ?DO
        I CELLS reloc-sec + @ CASE
            0 OF 1 text-nreloc +! ENDOF
            1 OF 1 data-nreloc +! ENDOF
            2 OF 1 bss-nreloc  +! ENDOF
        ENDCASE
    LOOP ;

\ Write section table entries
: WRITE-SEC-ENTRIES
    asm-tsz @ 0> IF
        WB-INIT
        text-str-idx @ 0 WB!
        1              4 WB!
        SF-XRA         8 WB!
        asm-tsz @     12 WB!
        off-secdata @ 16 WB!
        4             20 WB!
        text-nreloc @ 24 WB!
        text-nreloc @ 0> IF off-relocs @ ELSE 0 THEN 28 WB!
        32 WB-WRITE
    THEN
    asm-dsz @ 0> IF
        WB-INIT
        data-str-idx @ 0 WB!
        2              4 WB!
        SF-WRA         8 WB!
        asm-dsz @     12 WB!
        off-secdata @ asm-tsz @ + 16 WB!
        4             20 WB!
        data-nreloc @ 24 WB!
        data-nreloc @ 0> IF
            off-relocs @ text-nreloc @ 16 * +
        ELSE 0 THEN 28 WB!
        32 WB-WRITE
    THEN
    asm-bsz @ 0> IF
        WB-INIT
        bss-str-idx @  0 WB!
        3              4 WB!
        SF-WRA         8 WB!
        asm-bsz @     12 WB!
        0             16 WB!
        4             20 WB!
        bss-nreloc @  24 WB!
        0             28 WB!
        32 WB-WRITE
    THEN ;

\ Write symbol table entries
: WRITE-SYM-ENTRIES
    osym-cnt @ 0 ?DO
        WB-INIT
        I CELLS osym-stridx + @ 0 WB!
        I CELLS osym-value + @  4 WB!
        I CELLS osym-sec + @    8 WB16!
        0                      10 WB8!
        I CELLS osym-bind + @  11 WB8!
        0                      12 WB!
        16 WB-WRITE
    LOOP ;

\ Write relocation entries (text first, then data, then bss)
: WRITE-RELOC-ENTRIES
    \ .text relocations
    reloc-cnt @ 0 ?DO
        I CELLS reloc-sec + @ 0 = IF
            WB-INIT
            I CELLS reloc-off + @  0 WB!
            I CELLS reloc-sym + @ rname-buf +
            I CELLS reloc-sln + @
            OSYM-FIND              4 WB!
            I CELLS reloc-typ + @  8 WB!
            I CELLS reloc-add + @ 12 WB!
            16 WB-WRITE
        THEN
    LOOP
    \ .data relocations
    reloc-cnt @ 0 ?DO
        I CELLS reloc-sec + @ 1 = IF
            WB-INIT
            I CELLS reloc-off + @  0 WB!
            I CELLS reloc-sym + @ rname-buf +
            I CELLS reloc-sln + @
            OSYM-FIND              4 WB!
            I CELLS reloc-typ + @  8 WB!
            I CELLS reloc-add + @ 12 WB!
            16 WB-WRITE
        THEN
    LOOP ;

\ === Input / Line Handling ===
: LOAD-INPUT ( c-addr u -- )
    R/O OPEN-FILE THROW >R
    inp-buf INP-SZ R@ READ-FILE THROW
    asm-inp-len !
    R> CLOSE-FILE THROW
    0 asm-inp-pos ! ;

: NEXT-LINE ( -- flag )
    asm-inp-pos @ asm-inp-len @ >= IF FALSE EXIT THEN
    0 lbuf-len !
    0 lbuf-trunc !
    BEGIN
        asm-inp-pos @ asm-inp-len @ >= IF TRUE EXIT THEN
        inp-buf asm-inp-pos @ + C@
        1 asm-inp-pos +!
        DUP 10 = IF DROP TRUE EXIT THEN
        DUP 13 = IF DROP ELSE
            lbuf-len @ LBUF-SZ < IF
                lbuf lbuf-len @ + C!
                1 lbuf-len +!
            ELSE
                DROP
                lbuf-trunc @ 0= IF
                    S" line too long (truncated)" ASM-ERR
                    1 lbuf-trunc !
                THEN
            THEN
        THEN
    AGAIN ;

\ === Tokenizer ===
\ Delimiters: space, tab, comma. '#' starts comment.
: IS-DELIM ( c -- flag )
    DUP 32 = OVER 9 = OR SWAP 44 = OR ;

: SKIP-WS
    BEGIN
        lpos @ lbuf-len @ >= IF EXIT THEN
        lbuf lpos @ + C@
        DUP 35 = IF DROP lbuf-len @ lpos ! EXIT THEN
        IS-DELIM IF 1 lpos +! ELSE EXIT THEN
    AGAIN ;

: GET-TOK ( -- addr u )
    SKIP-WS
    lpos @ lbuf-len @ >= IF lbuf 0 EXIT THEN
    lbuf lpos @ +   \ start address
    0                \ length
    BEGIN
        lpos @ lbuf-len @ < IF
            lbuf lpos @ + C@
            DUP IS-DELIM OVER 35 = OR IF
                DROP FALSE
            ELSE
                DROP 1 lpos +! 1+ TRUE
            THEN
        ELSE
            FALSE
        THEN
    WHILE REPEAT ;

\ === Character Helpers ===
: TO-LOWER ( c -- c )
    DUP [CHAR] A >= OVER [CHAR] Z <= AND IF 32 + THEN ;

: STREQI ( addr1 u1 addr2 u2 -- flag )
    \ Case-insensitive string equality
    ROT OVER <> IF 2DROP DROP FALSE EXIT THEN
    \ Same length. Compare char by char.
    0 ?DO
        OVER I + C@ TO-LOWER
        OVER I + C@ TO-LOWER
        <> IF 2DROP FALSE UNLOOP EXIT THEN
    LOOP
    2DROP TRUE ;

\ === Number Parsing ===
: HEX-DIGIT ( c -- n true | false )
    DUP [CHAR] 0 >= OVER [CHAR] 9 <= AND IF [CHAR] 0 - TRUE EXIT THEN
    DUP [CHAR] A 32 + >= OVER [CHAR] F 32 + <= AND IF [CHAR] A 32 + - 10 + TRUE EXIT THEN
    DUP [CHAR] A >= OVER [CHAR] F <= AND IF [CHAR] A - 10 + TRUE EXIT THEN
    DROP FALSE ;

: PARSE-HEX ( addr u -- n true | false )
    0 SWAP  ( addr 0 u )
    0 ?DO
        OVER I + C@ HEX-DIGIT
        0= IF 2DROP FALSE UNLOOP EXIT THEN
        SWAP 16 * +
    LOOP
    NIP TRUE ;

: DEC-DIGIT ( c -- n true | false )
    DUP [CHAR] 0 >= OVER [CHAR] 9 <= AND IF [CHAR] 0 - TRUE EXIT THEN
    DROP FALSE ;

: PARSE-DEC ( addr u -- n true | false )
    DUP 0= IF 2DROP FALSE EXIT THEN
    0 SWAP  ( addr 0 u )
    0 ?DO
        OVER I + C@ DEC-DIGIT
        0= IF 2DROP FALSE UNLOOP EXIT THEN
        SWAP 10 * +
    LOOP
    NIP TRUE ;

: PARSE-NUM ( addr u -- n true | false )
    DUP 0= IF 2DROP FALSE EXIT THEN
    \ Negative?
    OVER C@ [CHAR] - = IF
        1 /STRING PARSE-NUM IF NEGATE TRUE ELSE FALSE THEN EXIT
    THEN
    \ Hex with 0x prefix?
    DUP 2 >= IF
        OVER C@ [CHAR] 0 = IF
            OVER 1+ C@ DUP [CHAR] X 32 + = SWAP [CHAR] X = OR IF
                2 /STRING PARSE-HEX EXIT
            THEN
        THEN
    THEN
    \ Decimal
    PARSE-DEC ;

\ === Register Parsing ===
: PARSE-REG ( addr u -- regnum true | false )
    DUP 0= IF 2DROP FALSE EXIT THEN
    \ Check aliases (case insensitive)
    2DUP S" zero" STREQI IF 2DROP  0 TRUE EXIT THEN
    2DUP S" sp"   STREQI IF 2DROP 29 TRUE EXIT THEN
    2DUP S" fp"   STREQI IF 2DROP 30 TRUE EXIT THEN
    2DUP S" lr"   STREQI IF 2DROP 31 TRUE EXIT THEN
    \ Parse rN / RN
    OVER C@ TO-LOWER [CHAR] R 32 + <> IF 2DROP FALSE EXIT THEN
    1 /STRING
    PARSE-DEC 0= IF FALSE EXIT THEN
    DUP 31 > IF DROP FALSE EXIT THEN
    TRUE ;

\ === Operand Parsing ===
\ Find character in string, return position or -1
VARIABLE find-ch-val
VARIABLE sx-a
VARIABLE sx-u
VARIABLE sx-pos
: FIND-CH ( addr u ch -- pos | -1 )
    find-ch-val !
    0 ?DO
        DUP I + C@ find-ch-val @ = IF
            DROP I UNLOOP EXIT
        THEN
    LOOP
    DROP -1 ;

\ Parse %hi(sym) or %hi(sym+off)
: IS-HI ( addr u -- sym-addr sym-u true | false )
    DUP 6 < IF 2DROP FALSE EXIT THEN
    OVER 4 S" %hi(" STREQI 0= IF 2DROP FALSE EXIT THEN
    OVER OVER + 1- C@ [CHAR] ) <> IF 2DROP FALSE EXIT THEN
    4 /STRING 1- TRUE ;

: IS-LO ( addr u -- sym-addr sym-u true | false )
    DUP 6 < IF 2DROP FALSE EXIT THEN
    OVER 4 S" %lo(" STREQI 0= IF 2DROP FALSE EXIT THEN
    OVER OVER + 1- C@ [CHAR] ) <> IF 2DROP FALSE EXIT THEN
    4 /STRING 1- TRUE ;

: STRIP-OUTER-PARENS ( addr u -- addr' u' )
    DUP 0> IF
        OVER C@ [CHAR] ( = IF 1 /STRING THEN
    THEN
    DUP 0> IF
        OVER OVER + 1- C@ [CHAR] ) = IF 1- THEN
    THEN ;

\ Split symbol expression "name+N" or "name-N" into base symbol and addend.
\ Returns (sym-addr sym-len addend true) on success, else false.
: SPLIT-SYM-ADDEND ( addr u -- sym-addr sym-len addend true | false )
    2DUP sx-u ! sx-a !

    2DUP [CHAR] + FIND-CH DUP 0> IF
        sx-pos !
        sx-a @ sx-pos @ + 1+ sx-u @ sx-pos @ - 1- PARSE-NUM IF
            2DROP
            sx-a @ sx-pos @ ROT TRUE EXIT
        THEN
        2DROP FALSE EXIT
    THEN
    DROP

    2DUP [CHAR] - FIND-CH DUP 0> IF
        sx-pos !
        sx-a @ sx-pos @ + 1+ sx-u @ sx-pos @ - 1- PARSE-NUM IF
            2DROP
            NEGATE sx-a @ sx-pos @ ROT TRUE EXIT
        THEN
        2DROP FALSE EXIT
    THEN
    DROP
    2DROP FALSE ;

\ Resolve a symbol name to a virtual address
: RESOLVE ( addr u -- vaddr )
    SYM-FIND DUP -1 = IF
        DROP
        asm-pass @ 2 = IF S" undefined symbol" ASM-ERR THEN
        0
    ELSE
        SYM-VA
    THEN ;

\ Parse an immediate operand (number, %hi, %lo, or label)
: PARSE-IMM ( addr u -- value )
    \ Try plain number
    2DUP PARSE-NUM IF NIP NIP EXIT THEN
    \ Try %hi(sym) — emit reloc, return 0
    2DUP IS-HI IF
        2SWAP 2DROP
        imm-sym-len ! imm-sym-addr !
        CUR-OFF @ REL-HI20 0 imm-sym-addr @ imm-sym-len @ ADD-RELOC
        0 EXIT
    THEN
    \ Try %lo(sym) — emit reloc, return 0
    2DUP IS-LO IF
        2SWAP 2DROP
        imm-sym-len ! imm-sym-addr !
        CUR-OFF @ REL-LO12 0 imm-sym-addr @ imm-sym-len @ ADD-RELOC
        0 EXIT
    THEN
    \ Bare symbol (absolute address)
    RESOLVE ;

\ Parse a jump target — result is PC-relative offset (for JAL)
: PARSE-TARGET ( addr u -- offset )
    2DUP PARSE-NUM IF NIP NIP EXIT THEN
    2DUP SYM-FIND DUP -1 = IF
        DROP
        \ Unresolved JAL target: keep placeholder and emit relocation.
        \ Linker resolves final PC-relative offset.
        2DUP
        imm-sym-len ! imm-sym-addr !
        CUR-OFF @ REL-JAL 0 imm-sym-addr @ imm-sym-len @ ADD-RELOC
        2DROP
        0
    ELSE
        SYM-VA
        \ Subtract current PC (text section)
        text-va @ asm-tsz @ + -
    THEN ;

\ Parse a branch target — result is PC+4 relative offset (for BEQ/BNE/BLT etc.)
: PARSE-BTARGET ( addr u -- offset )
    2DUP PARSE-NUM IF NIP NIP EXIT THEN
    RESOLVE
    \ Subtract (current PC + 4)
    text-va @ asm-tsz @ + 4 + - ;

\ === Instruction Encoding ===
\ R-type: op[6:0] | rd[11:7] | rs1[19:15] | rs2[24:20]
: ENC-R ( op rd rs1 rs2 -- word )
    20 LSHIFT >R
    15 LSHIFT >R
     7 LSHIFT OR
    R> OR R> OR ;

\ I-type: op[6:0] | rd[11:7] | rs1[19:15] | imm[31:20]
: ENC-I ( op rd rs1 imm -- word )
    MASK12 AND 20 LSHIFT >R
    15 LSHIFT >R
     7 LSHIFT OR
    R> OR R> OR ;

\ S-type: op[6:0] | imm[4:0]<<7 | rs1[19:15] | rs2[24:20] | imm[11:5]<<25
: ENC-S ( op base val imm -- word )
    >R                 \ save imm; stack: op base val
    20 LSHIFT >R       \ val << 20 saved; stack: op base
    15 LSHIFT OR       \ op | base<<15; stack: result
    R> OR              \ | val<<20
    R>                 \ imm
    DUP MASK5 AND 7 LSHIFT ROT OR SWAP
    5 RSHIFT MASK7 AND 25 LSHIFT OR ;

\ B-type: op[6:0] | imm[11]<<7 | imm[4:1]<<8 | rs1[19:15] | rs2[24:20] | imm[10:5]<<25 | imm[12]<<31
: ENC-B ( op rs1 rs2 imm -- word )
    >R
    20 LSHIFT >R
    15 LSHIFT OR
    R> OR
    R@  11 RSHIFT 1 AND     7 LSHIFT OR
    R@   1 RSHIFT MASK4 AND 8 LSHIFT OR
    R@   5 RSHIFT MASK6 AND 25 LSHIFT OR
    R>  12 RSHIFT 1 AND    31 LSHIFT OR ;

\ U-type: op[6:0] | rd[11:7] | imm[31:12]
: ENC-U ( op rd imm -- word )
    MASK20 AND 12 LSHIFT >R
    7 LSHIFT OR
    R> OR ;

\ J-type: op[6:0] | rd[11:7] | imm[19:12]<<12 | imm[11]<<20 | imm[10:1]<<21 | imm[20]<<31
: ENC-J ( op rd imm -- word )
    >R
    7 LSHIFT OR
    R@  12 RSHIFT MASK8 AND  12 LSHIFT OR
    R@  11 RSHIFT  1 AND    20 LSHIFT OR
    R@   1 RSHIFT MASK10 AND 21 LSHIFT OR
    R>  20 RSHIFT  1 AND    31 LSHIFT OR ;

\ === Instruction Helpers ===
\ Parse operands and emit R-type: mnemonic rd, rs1, rs2
: DO-R ( opcode -- )
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-REG 0= IF DROP S" bad rs1" ASM-ERR R> DROP EXIT THEN >R
    GET-TOK PARSE-REG 0= IF DROP S" bad rs2" ASM-ERR R> R> 2DROP EXIT THEN
    R> R> SWAP ROT
    ENC-R EMIT-W32 ;

\ Parse operands and emit I-type: mnemonic rd, rs1, imm
: DO-I ( opcode -- )
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-REG 0= IF DROP S" bad rs1" ASM-ERR R> DROP EXIT THEN >R
    GET-TOK PARSE-IMM
    R> R> SWAP ROT
    ENC-I EMIT-W32 ;

\ Parse store: stw base, val, offset (3-operand form)
: DO-S3 ( opcode -- )
    GET-TOK PARSE-REG 0= IF DROP S" bad base" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-REG 0= IF DROP S" bad val" ASM-ERR R> DROP EXIT THEN >R
    GET-TOK PARSE-IMM
    R> R> SWAP ROT
    ENC-S EMIT-W32 ;

\ Parse branch: beq rs1, rs2, target
: DO-B ( opcode -- )
    GET-TOK PARSE-REG 0= IF DROP S" bad rs1" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-REG 0= IF DROP S" bad rs2" ASM-ERR R> DROP EXIT THEN >R
    GET-TOK PARSE-BTARGET
    R> R> SWAP ROT
    ENC-B EMIT-W32 ;

\ Reversed branch: bgt rs1,rs2,target = blt rs2,rs1,target (swap regs)
: DO-B-REV ( opcode -- )
    GET-TOK PARSE-REG 0= IF DROP S" bad rs1" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-REG 0= IF DROP S" bad rs2" ASM-ERR R> DROP EXIT THEN >R
    GET-TOK PARSE-BTARGET
    R> R> ROT           \ ( opcode rs2 rs1 imm ) -- note: NOT swapped
    ENC-B EMIT-W32 ;

\ Parse U-type: lui rd, imm
: DO-U ( opcode -- )
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-IMM
    R> SWAP
    ENC-U EMIT-W32 ;

\ Parse J-type: jal rd, target
: DO-J ( opcode -- )
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-TARGET
    R> SWAP
    ENC-J EMIT-W32 ;

\ jalr rd, rs1, imm  (opcode 65 = 0x41)
: DO-JALR
    65
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-REG 0= IF DROP S" bad rs1" ASM-ERR R> DROP EXIT THEN >R
    GET-TOK PARSE-IMM
    R> R> SWAP ROT
    ENC-I EMIT-W32 ;

\ DEBUG rs1 (R-type: opcode=82, rd=0, rs2=0)
: DO-DEBUG
    GET-TOK PARSE-REG 0= IF DROP S" bad rs1" ASM-ERR EXIT THEN
    82 0 ROT 0 ENC-R EMIT-W32 ;

\ HALT (R-type: opcode=127, rd=0, rs1=0, rs2=0)
: DO-HALT  127 0 0 0 ENC-R EMIT-W32 ;

\ YIELD (R-type: opcode=81)
: DO-YIELD  81 0 0 0 ENC-R EMIT-W32 ;

\ === Pseudo-Instructions ===
\ nop = add r0, r0, r0 (opcode 0)
: DO-NOP  0 0 0 0 ENC-R EMIT-W32 ;

\ mv rd, rs = add rd, rs, r0 (opcode 0)
: DO-MV
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-REG 0= IF DROP S" bad rs" ASM-ERR R> DROP EXIT THEN
    0 R> ROT 0 ENC-R EMIT-W32 ;

\ li rd, imm
VARIABLE li-rd
: DO-LI
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN
    li-rd !
    GET-TOK PARSE-IMM
    \ If small enough for ADDI (-2048..2047), use single instruction
    DUP -2048 >= OVER 2047 <= AND IF
        >R 16 li-rd @ 0 R> ENC-I EMIT-W32    \ addi rd, r0, imm
    ELSE
        \ LUI + ADDI (opcode 32 + 16)
        DUP HALF12 + 12 RSHIFT
        >R 32 li-rd @ R> ENC-U EMIT-W32      \ lui rd, upper
        MASK12 AND
        >R 16 li-rd @ DUP R> ENC-I EMIT-W32  \ addi rd, rd, lower
    THEN ;

\ la rd, symbol = lui rd, 0 + addi rd, rd, 0 (with HI20/LO12 relocs)
: DO-LA
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN
    li-rd !
    GET-TOK
    imm-sym-len ! imm-sym-addr !
    \ HI20 reloc at current offset, emit lui rd, 0
    CUR-OFF @ REL-HI20 0 imm-sym-addr @ imm-sym-len @ ADD-RELOC
    32 li-rd @ 0 ENC-U EMIT-W32
    \ LO12 reloc at current offset, emit addi rd, rd, 0
    CUR-OFF @ REL-LO12 0 imm-sym-addr @ imm-sym-len @ ADD-RELOC
    16 li-rd @ DUP 0 ENC-I EMIT-W32 ;

\ j target = jal r0, target (opcode 64)
: DO-J-PSEUDO
    64 0 GET-TOK PARSE-TARGET ENC-J EMIT-W32 ;

\ jal target (shorthand) = jal r31, target
: DO-JAL-SHORT
    64 31 GET-TOK PARSE-TARGET ENC-J EMIT-W32 ;

\ jr rs = jalr r0, rs, 0 (opcode 65)
: DO-JR
    GET-TOK PARSE-REG 0= IF DROP S" bad rs" ASM-ERR EXIT THEN
    65 0 ROT 0 ENC-I EMIT-W32 ;

\ ret = jalr r0, r31, 0
: DO-RET  65 0 31 0 ENC-I EMIT-W32 ;

\ call target = lui r2, 0 + jalr r31, r2, 0 (with HI20/LO12 relocs)
: DO-CALL
    GET-TOK
    imm-sym-len ! imm-sym-addr !
    CUR-OFF @ REL-HI20 0 imm-sym-addr @ imm-sym-len @ ADD-RELOC
    32 2 0 ENC-U EMIT-W32               \ lui r2, 0
    CUR-OFF @ REL-LO12 0 imm-sym-addr @ imm-sym-len @ ADD-RELOC
    65 31 2 0 ENC-I EMIT-W32 ;          \ jalr r31, r2, 0

\ neg rd, rs = sub rd, r0, rs (opcode 1)
: DO-NEG
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-REG 0= IF DROP S" bad rs" ASM-ERR R> DROP EXIT THEN
    \ stack: ( rs ), rstack: ( rd )
    R> SWAP >R         \ ( rd ), rstack: ( rs )
    1 SWAP 0 R>        \ ( 1 rd 0 rs )
    ENC-R EMIT-W32 ;

\ not rd, rs = addi r2, r0, -1 ; xor rd, rs, r2
: DO-NOT
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-REG 0= IF DROP S" bad rs" ASM-ERR R> DROP EXIT THEN
    16 2 0 -1 ENC-I EMIT-W32          \ addi r2, r0, -1
    2 R> ROT 2 ENC-R EMIT-W32 ;       \ xor rd, rs, r2

\ === Directives ===
: DO-TEXT   0 asm-sect ! ;
: DO-DATA   1 asm-sect ! ;
: DO-BSS    2 asm-sect ! ;

: EMIT-WORD-TOK ( addr u -- )
    2DUP PARSE-NUM IF
        NIP NIP EMIT-W32
    ELSE
        \ Symbol reference — record REL-32 reloc, emit placeholder
        imm-sym-len ! imm-sym-addr !
        CUR-OFF @ REL-32 0 imm-sym-addr @ imm-sym-len @ ADD-RELOC
        0 EMIT-W32
    THEN ;

: DO-WORD
    BEGIN
        GET-TOK
        DUP 0= IF 2DROP EXIT THEN
        EMIT-WORD-TOK
    AGAIN ;

: DO-BYTE
    BEGIN
        GET-TOK
        DUP 0= IF 2DROP EXIT THEN
        PARSE-IMM 255 AND EMIT-B
    AGAIN ;

: DO-HALF
    GET-TOK PARSE-IMM
    DUP 255 AND EMIT-B
    8 RSHIFT 255 AND EMIT-B ;

\ Emit a quoted string with escape sequences, using manual counter
\ to properly skip escape characters
: DO-ASCIZ
    lbuf lpos @ + lbuf-len @ lpos @ -
    \ Find opening quote
    2DUP [CHAR] " FIND-CH DUP -1 = IF
        DROP 2DROP S" missing quote" ASM-ERR EXIT
    THEN
    1+ /STRING
    \ Now addr points past opening quote, u is remaining length
    \ Use manual index to handle escape skipping
    0 str-idx !
    BEGIN
        str-idx @ OVER < IF
            OVER str-idx @ + C@
            DUP [CHAR] " = IF
                DROP 0 EMIT-B
                2DROP EXIT
            THEN
            DUP [CHAR] \ = IF
                DROP
                1 str-idx +!
                OVER str-idx @ + C@
                CASE
                    [CHAR] N 32 + OF 10 ENDOF
                    [CHAR] T 32 + OF  9 ENDOF
                    [CHAR] R 32 + OF 13 ENDOF
                    [CHAR] 0      OF  0 ENDOF
                    [CHAR] \      OF 92 ENDOF
                    [CHAR] "      OF 34 ENDOF
                    DUP
                ENDCASE
                EMIT-B
            ELSE
                EMIT-B
            THEN
            1 str-idx +!
            TRUE
        ELSE
            FALSE
        THEN
    WHILE REPEAT
    2DROP
    0 EMIT-B ;

: DO-ASCII
    lbuf lpos @ + lbuf-len @ lpos @ -
    2DUP [CHAR] " FIND-CH DUP -1 = IF
        DROP 2DROP S" missing quote" ASM-ERR EXIT
    THEN
    1+ /STRING
    0 str-idx !
    BEGIN
        str-idx @ OVER < IF
            OVER str-idx @ + C@
            DUP [CHAR] " = IF DROP 2DROP EXIT THEN
            DUP [CHAR] \ = IF
                DROP
                1 str-idx +!
                OVER str-idx @ + C@
                CASE
                    [CHAR] N 32 + OF 10 ENDOF
                    [CHAR] T 32 + OF  9 ENDOF
                    [CHAR] 0      OF  0 ENDOF
                    [CHAR] \      OF 92 ENDOF
                    [CHAR] "      OF 34 ENDOF
                    DUP
                ENDCASE
            THEN
            EMIT-B
            1 str-idx +!
            TRUE
        ELSE
            FALSE
        THEN
    WHILE REPEAT
    2DROP ;

: DO-SPACE
    GET-TOK PARSE-NUM 0= IF S" bad .space size" ASM-ERR EXIT THEN
    0 ?DO 0 EMIT-B LOOP ;

: SAVE-OUT-PATH ( addr u -- )
    DUP 255 > IF DROP 255 THEN
    DUP out-path-len !
    out-path SWAP CMOVE ;

: DO-ALIGN
    GET-TOK PARSE-NUM 0= IF S" bad .align value" ASM-ERR EXIT THEN
    1 SWAP LSHIFT  \ 2^n
    CUR-OFF @ OVER 1- AND  \ misalignment
    DUP 0<> IF
        OVER SWAP - 0 ?DO 0 EMIT-B LOOP
    ELSE
        DROP
    THEN
    DROP ;

: DO-GLOBAL
    asm-pass @ 1 = IF
        GET-TOK
        DUP 0= IF 2DROP EXIT THEN
        gname-cnt @ MAX-GLOBAL >= IF 2DROP S" too many globals" ASM-ERR EXIT THEN
        gname-cnt @ >R
        gname-ptr @ R@ CELLS gname-off + !
        DUP R@ CELLS gname-len + !
        gname-ptr @ gname-buf + SWAP CMOVE
        R@ CELLS gname-len + @ gname-ptr +!
        R> 1+ gname-cnt !
    ELSE
        GET-TOK 2DROP
    THEN ;

: DO-TYPE   GET-TOK 2DROP GET-TOK 2DROP ;
: DO-SIZE   GET-TOK 2DROP GET-TOK 2DROP ;
: DO-FILE   GET-TOK 2DROP ;
: DO-SECTION GET-TOK 2DROP ;

\ === Mnemonic Dispatch ===
\ Opcodes in decimal: add=0 sub=1 xor=2 or=3 and=4 sll=5 srl=6 sra=7
\ slt=8 sltu=9 mul=10 mulh=11 div=12 rem=13 seq=14 sne=15
\ addi=16 ori=17 andi=18 slli=19 srli=20 srai=21 slti=22 sltiu=23
\ sgt=24 sgtu=25 sle=26 sleu=27 sge=28 sgeu=29 xori=30
\ lui=32 divu=44 remu=45
\ ldb=48 ldh=49 ldw=50 ldbu=51 ldhu=52
\ stb=56 sth=57 stw=58
\ jal=64 jalr=65
\ beq=72 bne=73 blt=74 bge=75 bltu=76 bgeu=77
\ yield=80 debug=82 halt=127

: TRY-INSN ( addr u -- handled? )
    \ R-type instructions
    2DUP S" add"   STREQI IF 2DROP  0 DO-R TRUE EXIT THEN
    2DUP S" sub"   STREQI IF 2DROP  1 DO-R TRUE EXIT THEN
    2DUP S" xor"   STREQI IF 2DROP  2 DO-R TRUE EXIT THEN
    2DUP S" or"    STREQI IF 2DROP  3 DO-R TRUE EXIT THEN
    2DUP S" and"   STREQI IF 2DROP  4 DO-R TRUE EXIT THEN
    2DUP S" sll"   STREQI IF 2DROP  5 DO-R TRUE EXIT THEN
    2DUP S" srl"   STREQI IF 2DROP  6 DO-R TRUE EXIT THEN
    2DUP S" sra"   STREQI IF 2DROP  7 DO-R TRUE EXIT THEN
    2DUP S" slt"   STREQI IF 2DROP  8 DO-R TRUE EXIT THEN
    2DUP S" sltu"  STREQI IF 2DROP  9 DO-R TRUE EXIT THEN
    2DUP S" mul"   STREQI IF 2DROP 10 DO-R TRUE EXIT THEN
    2DUP S" mulh"  STREQI IF 2DROP 11 DO-R TRUE EXIT THEN
    2DUP S" div"   STREQI IF 2DROP 12 DO-R TRUE EXIT THEN
    2DUP S" rem"   STREQI IF 2DROP 13 DO-R TRUE EXIT THEN
    2DUP S" seq"   STREQI IF 2DROP 14 DO-R TRUE EXIT THEN
    2DUP S" sne"   STREQI IF 2DROP 15 DO-R TRUE EXIT THEN
    2DUP S" sgt"   STREQI IF 2DROP 24 DO-R TRUE EXIT THEN
    2DUP S" sgtu"  STREQI IF 2DROP 25 DO-R TRUE EXIT THEN
    2DUP S" sle"   STREQI IF 2DROP 26 DO-R TRUE EXIT THEN
    2DUP S" sleu"  STREQI IF 2DROP 27 DO-R TRUE EXIT THEN
    2DUP S" sge"   STREQI IF 2DROP 28 DO-R TRUE EXIT THEN
    2DUP S" sgeu"  STREQI IF 2DROP 29 DO-R TRUE EXIT THEN
    2DUP S" mulhu" STREQI IF 2DROP 31 DO-R TRUE EXIT THEN
    2DUP S" divu"  STREQI IF 2DROP 44 DO-R TRUE EXIT THEN
    2DUP S" remu"  STREQI IF 2DROP 45 DO-R TRUE EXIT THEN

    \ I-type instructions
    2DUP S" addi"  STREQI IF 2DROP 16 DO-I TRUE EXIT THEN
    2DUP S" ori"   STREQI IF 2DROP 17 DO-I TRUE EXIT THEN
    2DUP S" andi"  STREQI IF 2DROP 18 DO-I TRUE EXIT THEN
    2DUP S" slli"  STREQI IF 2DROP 19 DO-I TRUE EXIT THEN
    2DUP S" srli"  STREQI IF 2DROP 20 DO-I TRUE EXIT THEN
    2DUP S" srai"  STREQI IF 2DROP 21 DO-I TRUE EXIT THEN
    2DUP S" xori"  STREQI IF 2DROP 30 DO-I TRUE EXIT THEN
    2DUP S" slti"  STREQI IF 2DROP 22 DO-I TRUE EXIT THEN
    2DUP S" sltiu" STREQI IF 2DROP 23 DO-I TRUE EXIT THEN

    \ Load instructions (I-type)
    2DUP S" ldb"   STREQI IF 2DROP 48 DO-I TRUE EXIT THEN
    2DUP S" ldh"   STREQI IF 2DROP 49 DO-I TRUE EXIT THEN
    2DUP S" ldw"   STREQI IF 2DROP 50 DO-I TRUE EXIT THEN
    2DUP S" ldbu"  STREQI IF 2DROP 51 DO-I TRUE EXIT THEN
    2DUP S" ldhu"  STREQI IF 2DROP 52 DO-I TRUE EXIT THEN
    \ RISC-V style aliases used by stage4 output
    2DUP S" lb"    STREQI IF 2DROP 48 DO-I TRUE EXIT THEN
    2DUP S" lh"    STREQI IF 2DROP 49 DO-I TRUE EXIT THEN
    2DUP S" lw"    STREQI IF 2DROP 50 DO-I TRUE EXIT THEN
    2DUP S" lbu"   STREQI IF 2DROP 51 DO-I TRUE EXIT THEN
    2DUP S" lhu"   STREQI IF 2DROP 52 DO-I TRUE EXIT THEN

    \ Store instructions (S-type)
    2DUP S" stb"   STREQI IF 2DROP 56 DO-S3 TRUE EXIT THEN
    2DUP S" sth"   STREQI IF 2DROP 57 DO-S3 TRUE EXIT THEN
    2DUP S" stw"   STREQI IF 2DROP 58 DO-S3 TRUE EXIT THEN

    \ Branch instructions (B-type)
    2DUP S" beq"   STREQI IF 2DROP 72 DO-B TRUE EXIT THEN
    2DUP S" bne"   STREQI IF 2DROP 73 DO-B TRUE EXIT THEN
    2DUP S" blt"   STREQI IF 2DROP 74 DO-B TRUE EXIT THEN
    2DUP S" bge"   STREQI IF 2DROP 75 DO-B TRUE EXIT THEN
    2DUP S" bltu"  STREQI IF 2DROP 76 DO-B TRUE EXIT THEN
    2DUP S" bgeu"  STREQI IF 2DROP 77 DO-B TRUE EXIT THEN

    \ U-type
    2DUP S" lui"   STREQI IF 2DROP 32 DO-U TRUE EXIT THEN

    \ J-type: jal with peek to distinguish forms
    2DUP S" jal"   STREQI IF 2DROP
        GET-TOK 2DUP PARSE-REG IF
            \ jal rd, target
            >R 2DROP
            GET-TOK PARSE-TARGET
            64 R> ROT ENC-J EMIT-W32
        ELSE
            \ jal target (shorthand for jal r31, target)
            PARSE-TARGET
            64 31 ROT ENC-J EMIT-W32
        THEN
        TRUE EXIT
    THEN

    \ JALR
    2DUP S" jalr"  STREQI IF 2DROP DO-JALR TRUE EXIT THEN

    \ System
    2DUP S" debug" STREQI IF 2DROP DO-DEBUG TRUE EXIT THEN
    2DUP S" halt"  STREQI IF 2DROP DO-HALT  TRUE EXIT THEN
    2DUP S" yield" STREQI IF 2DROP DO-YIELD TRUE EXIT THEN

    \ Pseudo-instructions
    2DUP S" nop"   STREQI IF 2DROP DO-NOP   TRUE EXIT THEN
    2DUP S" mv"    STREQI IF 2DROP DO-MV    TRUE EXIT THEN
    2DUP S" li"    STREQI IF 2DROP DO-LI    TRUE EXIT THEN
    2DUP S" la"    STREQI IF 2DROP DO-LA    TRUE EXIT THEN
    2DUP S" j"     STREQI IF 2DROP DO-J-PSEUDO TRUE EXIT THEN
    2DUP S" jr"    STREQI IF 2DROP DO-JR    TRUE EXIT THEN
    2DUP S" ret"   STREQI IF 2DROP DO-RET   TRUE EXIT THEN
    2DUP S" call"  STREQI IF 2DROP DO-CALL  TRUE EXIT THEN
    2DUP S" neg"   STREQI IF 2DROP DO-NEG   TRUE EXIT THEN
    2DUP S" not"   STREQI IF 2DROP DO-NOT   TRUE EXIT THEN
    \ Reversed branches: bgt=blt(swapped), ble=bge(swapped), etc.
    2DUP S" bgt"   STREQI IF 2DROP 74 DO-B-REV TRUE EXIT THEN
    2DUP S" ble"   STREQI IF 2DROP 75 DO-B-REV TRUE EXIT THEN
    2DUP S" bgtu"  STREQI IF 2DROP 76 DO-B-REV TRUE EXIT THEN
    2DUP S" bleu"  STREQI IF 2DROP 77 DO-B-REV TRUE EXIT THEN

    2DROP FALSE ;

: TRY-DIR ( addr u -- handled? )
    2DUP S" .text"    STREQI IF 2DROP DO-TEXT    TRUE EXIT THEN
    2DUP S" .data"    STREQI IF 2DROP DO-DATA    TRUE EXIT THEN
    2DUP S" .bss"     STREQI IF 2DROP DO-BSS     TRUE EXIT THEN
    2DUP S" .word"    STREQI IF 2DROP DO-WORD    TRUE EXIT THEN
    2DUP S" .byte"    STREQI IF 2DROP DO-BYTE    TRUE EXIT THEN
    2DUP S" .half"    STREQI IF 2DROP DO-HALF    TRUE EXIT THEN
    2DUP S" .short"   STREQI IF 2DROP DO-HALF    TRUE EXIT THEN
    2DUP S" .asciz"   STREQI IF 2DROP DO-ASCIZ   TRUE EXIT THEN
    2DUP S" .string"  STREQI IF 2DROP DO-ASCIZ   TRUE EXIT THEN
    2DUP S" .ascii"   STREQI IF 2DROP DO-ASCII   TRUE EXIT THEN
    2DUP S" .space"   STREQI IF 2DROP DO-SPACE   TRUE EXIT THEN
    2DUP S" .zero"    STREQI IF 2DROP DO-SPACE   TRUE EXIT THEN
    2DUP S" .align"   STREQI IF 2DROP DO-ALIGN   TRUE EXIT THEN
    2DUP S" .p2align" STREQI IF 2DROP DO-ALIGN   TRUE EXIT THEN
    2DUP S" .global"  STREQI IF 2DROP DO-GLOBAL  TRUE EXIT THEN
    2DUP S" .globl"   STREQI IF 2DROP DO-GLOBAL  TRUE EXIT THEN
    2DUP S" .type"    STREQI IF 2DROP DO-TYPE    TRUE EXIT THEN
    2DUP S" .size"    STREQI IF 2DROP DO-SIZE    TRUE EXIT THEN
    2DUP S" .file"    STREQI IF 2DROP DO-FILE    TRUE EXIT THEN
    2DUP S" .section" STREQI IF 2DROP DO-SECTION TRUE EXIT THEN
    2DROP FALSE ;

\ === Line Processing ===
: PROCESS-LINE
    0 lpos !
    GET-TOK
    DUP 0= IF 2DROP EXIT THEN   \ empty line

    \ Check for label (ends with ':')
    2DUP + 1- C@ [CHAR] : = IF
        1-
        asm-pass @ 1 = IF
            2DUP SYM-FIND -1 <> IF
                S" duplicate label" ASM-ERR 2DROP
            ELSE
                asm-sect @ CUR-OFF @ SYM-ADD
            THEN
        ELSE
            2DROP
        THEN
        \ Continue — might have instruction after label
        GET-TOK DUP 0= IF 2DROP EXIT THEN
    THEN

    \ Try as directive
    2DUP TRY-DIR IF 2DROP EXIT THEN

    \ Try as instruction
    2DUP TRY-INSN IF 2DROP EXIT THEN

    \ Unknown
    asm-pass @ 2 = IF
        ." unknown: " TYPE CR
        1 asm-errs +!
    ELSE
        2DROP
    THEN ;

\ === Two-Pass Engine ===
: RUN-PASS ( n -- )
    asm-pass !
    0 asm-tsz !  0 asm-dsz !  0 asm-bsz !
    0 asm-sect !
    0 asm-inp-pos !
    0 asm-lno !
    BEGIN
        NEXT-LINE
    WHILE
        1 asm-lno +!
        asm-lno @ 500 MOD 0= IF
            ." line " asm-lno @ . CR
        THEN
        PROCESS-LINE
    REPEAT ;

\ === .s32o Output ===
CREATE hdr-buf 256 ALLOT

: H! ( value offset -- )
    hdr-buf +
    OVER         255 AND OVER     C!
    OVER 8  RSHIFT 255 AND OVER 1+ C!
    OVER 16 RSHIFT 255 AND OVER 2 + C!
    SWAP 24 RSHIFT 255 AND SWAP 3 + C! ;

: WRITE-S32O ( c-addr u -- )
    \ Open output file
    26 OPEN-FILE THROW out-fh !

    \ Initialize string table
    STRTAB-INIT

    \ Count output sections and assign 1-based indices
    0 out-nsec !
    asm-tsz @ 0> IF out-nsec @ 1+ DUP out-nsec ! text-out-idx ! ELSE 0 text-out-idx ! THEN
    asm-dsz @ 0> IF out-nsec @ 1+ DUP out-nsec ! data-out-idx ! ELSE 0 data-out-idx ! THEN
    asm-bsz @ 0> IF out-nsec @ 1+ DUP out-nsec ! bss-out-idx !  ELSE 0 bss-out-idx !  THEN

    \ Add section names to string table
    asm-tsz @ 0> IF S" .text" STRTAB-ADD text-str-idx ! THEN
    asm-dsz @ 0> IF S" .data" STRTAB-ADD data-str-idx ! THEN
    asm-bsz @ 0> IF S" .bss"  STRTAB-ADD bss-str-idx !  THEN

    \ Build output symbol table
    0 osym-cnt !
    BUILD-GLOBALS
    BUILD-RELOC-SYMS

    \ Count per-section relocations
    COUNT-RELOCS

    \ Calculate file layout offsets
    40                                      off-sections !
    40 out-nsec @ 32 * +                    off-symbols !
    off-symbols @ osym-cnt @ 16 * +         off-relocs !
    off-relocs @ reloc-cnt @ 16 * +         off-strtab !
    off-strtab @ strtab-sz @ + ALIGN4       off-secdata !

    \ Write 40-byte header
    WB-INIT
    S32O-MAGIC      0 WB!
    1               4 WB16!
    1               6 WB8!
    50              7 WB8!        \ machine = 0x32
    0               8 WB!        \ flags
    out-nsec @     12 WB!        \ nsections
    off-sections @ 16 WB!        \ sec_offset
    osym-cnt @     20 WB!        \ nsymbols
    off-symbols @  24 WB!        \ sym_offset
    off-strtab @   28 WB!        \ str_offset
    strtab-sz @    32 WB!        \ str_size
    0              36 WB!        \ checksum
    40 WB-WRITE

    \ Write section table entries
    WRITE-SEC-ENTRIES

    \ Write symbol table entries
    WRITE-SYM-ENTRIES

    \ Write relocation entries
    WRITE-RELOC-ENTRIES

    \ Write string table
    strtab strtab-sz @ out-fh @ WRITE-FILE THROW

    \ Write padding to align to 4
    off-secdata @ off-strtab @ strtab-sz @ + - DUP 0> IF
        wb-buf OVER 0 FILL
        wb-buf SWAP out-fh @ WRITE-FILE THROW
    ELSE DROP THEN

    \ Write section data
    asm-tsz @ 0> IF code-buf asm-tsz @ out-fh @ WRITE-FILE THROW THEN
    asm-dsz @ 0> IF data-buf asm-dsz @ out-fh @ WRITE-FILE THROW THEN

    \ Close file
    out-fh @ CLOSE-FILE THROW ;

\ === Main Entry Point ===
: ASSEMBLE ( input-addr input-u output-addr output-u -- )
    2DUP SAVE-OUT-PATH
    2SWAP
    \ Initialize
    0 sym-cnt !  0 sname-ptr !
    0 asm-errs !
    0 reloc-cnt !  0 rname-ptr !
    0 gname-cnt !  0 gname-ptr !

    \ Load input file
    2DUP LOAD-INPUT
    2DROP

    \ Pass 1: collect labels, compute sizes
    ." Pass 1..." CR
    1 RUN-PASS
    ." Pass 1: text=" asm-tsz @ . ." data=" asm-dsz @ . ." bss=" asm-bsz @ .
    ." syms=" sym-cnt @ . CR

    \ For .s32o, all sections start at offset 0
    0 text-va !  0 data-va !  0 bss-va !

    \ Pass 2: emit code
    ." Pass 2..." CR
    2 RUN-PASS
    ." Pass 2 done. relocs=" reloc-cnt @ . CR

    \ Some parser paths can leave transient values on the data stack for
    \ larger inputs; keep only (output-addr output-u) before final emit.
    DEPTH 2 > IF
        ." warning: stack leak before WRITE-S32O, depth=" DEPTH . CR
        BEGIN DEPTH 2 > WHILE DROP REPEAT
    THEN

    \ Check for errors
    asm-errs @ 0<> IF
        ." FAILED: " asm-errs @ . ." errors" CR
        EXIT
    THEN

    \ Write output
    ." Writing..." CR
    out-path out-path-len @ WRITE-S32O
    ." Done." CR ;
