\ ============================================================
\ SLOW-32 Forth-Hosted C Compiler — Stage 4 Bootstrap
\ ============================================================
\
\ A C compiler that reads C source and emits SLOW-32 assembly
\ text (.s files) for the Stage 2 assembler.
\
\ Usage:
\   S" input.c" S" output.s" COMPILE-FILE
\
\ Architecture: accumulator model (all expressions -> r1)
\   r1 = accumulator, r2 = scratch
\   r3-r10 = args, r29 = SP, r30 = FP, r31 = LR
\
\ Requires prelude.fth loaded first.
\ ============================================================

\ === Constants ===
HEX
FFFFF CONSTANT MASK20
FFF   CONSTANT MASK12
FF    CONSTANT MASK8
F00   CONSTANT BITS-8-11
FFFFFF87 CONSTANT CLR-PTR-MASK   \ clear bits 3-6
FFFF  CONSTANT MASK16
FFFF CONSTANT CLR-ARR-MASK       \ keep bits 0-15, clear array count (bits 16-31)
800   CONSTANT SIGN12            \ sign bit of 12-bit value
FFFFF000 CONSTANT SIGN12-EXT    \ sign extension mask
80    CONSTANT UNSIGNED-BIT      \ bit 7 = unsigned flag
DECIMAL
7 CONSTANT MASK-BYTE             \ mask for base type (bits 0-2)

\ Token types
0  CONSTANT TK-EOF
1  CONSTANT TK-NUM       \ integer literal
2  CONSTANT TK-STR       \ string literal
3  CONSTANT TK-IDENT     \ identifier
4  CONSTANT TK-KW        \ keyword
5  CONSTANT TK-CHAR      \ character constant
6  CONSTANT TK-PUNCT     \ punctuation / operator

\ Keyword IDs (stored in tk-val when tk-type = TK-KW)
0  CONSTANT KW-INT
1  CONSTANT KW-CHAR
2  CONSTANT KW-VOID
3  CONSTANT KW-IF
4  CONSTANT KW-ELSE
5  CONSTANT KW-WHILE
6  CONSTANT KW-FOR
7  CONSTANT KW-DO
8  CONSTANT KW-RETURN
9  CONSTANT KW-BREAK
10 CONSTANT KW-CONTINUE
11 CONSTANT KW-STATIC
12 CONSTANT KW-CONST
13 CONSTANT KW-UNSIGNED
14 CONSTANT KW-SIGNED
15 CONSTANT KW-LONG
16 CONSTANT KW-SHORT
17 CONSTANT KW-STRUCT
18 CONSTANT KW-ENUM
19 CONSTANT KW-TYPEDEF
20 CONSTANT KW-SIZEOF
21 CONSTANT KW-SWITCH
22 CONSTANT KW-CASE
23 CONSTANT KW-DEFAULT
24 CONSTANT KW-EXTERN
25 CONSTANT KW-INLINE
26 CONSTANT KW-BOOL

\ Punctuation IDs (stored in tk-val when tk-type = TK-PUNCT)
\ Single-char punctuation
0  CONSTANT P-LPAREN     \ (
1  CONSTANT P-RPAREN     \ )
2  CONSTANT P-LBRACE     \ {
3  CONSTANT P-RBRACE     \ }
4  CONSTANT P-LBRACK     \ [
5  CONSTANT P-RBRACK     \ ]
6  CONSTANT P-SEMI       \ ;
7  CONSTANT P-COMMA      \ ,
8  CONSTANT P-DOT        \ .
9  CONSTANT P-TILDE      \ ~
10 CONSTANT P-QMARK      \ ?
11 CONSTANT P-COLON      \ :
\ Operators (can be compound)
20 CONSTANT P-PLUS       \ +
21 CONSTANT P-MINUS      \ -
22 CONSTANT P-STAR       \ *
23 CONSTANT P-SLASH      \ /
24 CONSTANT P-PERCENT    \ %
25 CONSTANT P-AMP        \ &
26 CONSTANT P-PIPE       \ |
27 CONSTANT P-CARET      \ ^
28 CONSTANT P-BANG       \ !
29 CONSTANT P-ASSIGN     \ =
30 CONSTANT P-LT         \ <
31 CONSTANT P-GT         \ >
32 CONSTANT P-LSHIFT     \ <<
33 CONSTANT P-RSHIFT     \ >>
34 CONSTANT P-EQ         \ ==
35 CONSTANT P-NE         \ !=
36 CONSTANT P-LE         \ <=
37 CONSTANT P-GE         \ >=
38 CONSTANT P-LAND       \ &&
39 CONSTANT P-LOR        \ ||
40 CONSTANT P-PLUSEQ     \ +=
41 CONSTANT P-MINUSEQ    \ -=
42 CONSTANT P-STAREQ     \ *=
43 CONSTANT P-SLASHEQ    \ /=
44 CONSTANT P-PERCENTEQ  \ %=
45 CONSTANT P-AMPEQ      \ &=
46 CONSTANT P-PIPEEQ     \ |=
47 CONSTANT P-CARETEQ    \ ^=
48 CONSTANT P-LSHIFTEQ   \ <<=
49 CONSTANT P-RSHIFTEQ   \ >>=
50 CONSTANT P-INC        \ ++
51 CONSTANT P-DEC        \ --
52 CONSTANT P-ARROW      \ ->
53 CONSTANT P-ELLIPSIS   \ ...
54 CONSTANT P-HASH       \ #

\ Type base codes (bits 0-7 of type word)
0 CONSTANT TY-VOID
1 CONSTANT TY-CHAR
2 CONSTANT TY-SHORT
3 CONSTANT TY-INT
4 CONSTANT TY-LONG
5 CONSTANT TY-STRUCT     \ index in bits 9-15
6 CONSTANT TY-ENUM

\ Type bit fields
: TY-PTR-SHIFT 3 ;       \ bits 3-6 = pointer depth
: TY-UNSIGNED-BIT 7 ;    \ bit 7 = unsigned
: TY-CONST-BIT 8 ;       \ bit 8 = const
: TY-ARRAY-SHIFT 16 ;    \ bits 16-31 = array count (0=not array)

\ === Buffer Sizes ===
262144 CONSTANT INP-SZ      \ 256KB input buffer
262144 CONSTANT OUT-SZ      \ 256KB output buffer
512    CONSTANT MAX-MACRO    \ max macros
16384  CONSTANT MNAME-SZ    \ macro name storage
49152  CONSTANT MVAL-SZ     \ macro value storage
1024   CONSTANT MAX-GSYM    \ max global symbols
36     CONSTANT GSYM-RSZ    \ bytes per global symbol entry
256    CONSTANT MAX-LSYM    \ max local symbols per function
128    CONSTANT MAX-STRUCT  \ max struct types
2048   CONSTANT MAX-FIELD   \ max total struct fields
16     CONSTANT MAX-INCLUDE \ max include nesting
16384  CONSTANT STR-POOL-SZ \ string literal pool
256    CONSTANT TOK-SZ      \ max token text length
32     CONSTANT MAX-BREAK   \ max nested break targets
32     CONSTANT MAX-CONT    \ max nested continue targets
32     CONSTANT MAX-SWITCH  \ max nested switch depth
128    CONSTANT MAX-TYPEDEF \ max typedefs
256    CONSTANT MAX-ENUM    \ max enum values
64     CONSTANT MAX-MPARAM  \ max macro parameters

\ === Buffers ===
CREATE inp-buf INP-SZ ALLOT
VARIABLE inp-len            \ total input length
VARIABLE inp-pos            \ current read position

CREATE out-buf OUT-SZ ALLOT
VARIABLE out-len            \ current output length

\ Token state
CREATE tok-buf TOK-SZ ALLOT \ current token text
VARIABLE tok-len            \ current token text length
VARIABLE tok-type           \ TK-* type
VARIABLE tok-val            \ numeric value or keyword/punct ID
VARIABLE tok-start          \ inp-pos before this token was lexed

\ Peek token (for lookahead)
CREATE peek-buf TOK-SZ ALLOT
VARIABLE peek-len
VARIABLE peek-type
VARIABLE peek-val
VARIABLE has-peek           \ flag: is peek valid?

\ === Macro Table ===
CREATE mac-name  MAX-MACRO CELLS ALLOT  \ offset into mname-buf
CREATE mac-nlen  MAX-MACRO CELLS ALLOT  \ name length
CREATE mac-val   MAX-MACRO CELLS ALLOT  \ offset into mval-buf
CREATE mac-vlen  MAX-MACRO CELLS ALLOT  \ value length
CREATE mac-npar  MAX-MACRO CELLS ALLOT  \ num params (-1 = object-like)
VARIABLE mac-cnt
CREATE mname-buf MNAME-SZ ALLOT
VARIABLE mname-ptr
CREATE mval-buf  MVAL-SZ ALLOT
VARIABLE mval-ptr

\ Macro parameter names (for function-like macros during expansion)
CREATE mparam-off MAX-MPARAM CELLS ALLOT  \ offset in mname-buf
CREATE mparam-len MAX-MPARAM CELLS ALLOT  \ length

\ Persistent per-macro parameter storage (survives across #define)
CREATE mac-pbase MAX-MACRO CELLS ALLOT   \ index into pinfo arrays
256 CONSTANT MAX-PINFO
CREATE pinfo-off MAX-PINFO CELLS ALLOT   \ offset in pname-store
CREATE pinfo-len MAX-PINFO CELLS ALLOT   \ length
VARIABLE pinfo-cnt
CREATE pname-store 4096 ALLOT
VARIABLE pname-sptr

\ Macro expansion argument buffers
CREATE arg-text 2048 ALLOT     \ concatenated arg text
CREATE arg-off  16 CELLS ALLOT \ offset of each arg
CREATE arg-alen 16 CELLS ALLOT \ length of each arg
VARIABLE arg-cnt
VARIABLE arg-ptr
VARIABLE arg-depth
VARIABLE arg-ch

\ Macro body expansion variables
VARIABLE exp-body
VARIABLE exp-blen
VARIABLE exp-npar
VARIABLE exp-pbase
VARIABLE exp-off
CREATE exp-idbuf 256 ALLOT
VARIABLE exp-idlen

\ Macro expansion stack (for nested expansion)
8 CONSTANT MAX-MEXP
CREATE mexp-buf  MAX-MEXP CELLS ALLOT    \ saved inp-buf pointer (allotted buffer)
CREATE mexp-len  MAX-MEXP CELLS ALLOT    \ saved length
CREATE mexp-pos  MAX-MEXP CELLS ALLOT    \ saved position
VARIABLE mexp-depth

\ === Include Stack ===
CREATE inc-pos   MAX-INCLUDE CELLS ALLOT
CREATE inc-len   MAX-INCLUDE CELLS ALLOT
CREATE inc-line  MAX-INCLUDE CELLS ALLOT
VARIABLE inc-depth
\ Pre-allocated include save buffers (4 levels x 32KB each = 128KB)
\ We limit include depth to 4 to save memory
4 CONSTANT MAX-INC-DEPTH
32768 CONSTANT INC-BUF-SZ
CREATE inc-bufs MAX-INC-DEPTH INC-BUF-SZ * ALLOT

\ === Global Symbol Table ===
\ Symbols: functions and global variables
CREATE gsym-name  MAX-GSYM CELLS ALLOT   \ offset into gsym-nbuf
CREATE gsym-nlen  MAX-GSYM CELLS ALLOT   \ name length
CREATE gsym-type  MAX-GSYM CELLS ALLOT   \ type word
CREATE gsym-kind  MAX-GSYM CELLS ALLOT   \ 0=var, 1=func, 2=prototype
CREATE gsym-off   MAX-GSYM CELLS ALLOT   \ for static locals: mangled name offset
CREATE gsym-flags MAX-GSYM CELLS ALLOT   \ bit 0=static, bit 1=extern
VARIABLE gsym-cnt
32768 CONSTANT GNAME-SZ
CREATE gsym-nbuf GNAME-SZ ALLOT
VARIABLE gsym-nptr

\ === Local Symbol Table (per function) ===
CREATE lsym-name  MAX-LSYM CELLS ALLOT  \ offset into lsym-nbuf
CREATE lsym-nlen  MAX-LSYM CELLS ALLOT  \ name length
CREATE lsym-type  MAX-LSYM CELLS ALLOT  \ type word
CREATE lsym-off   MAX-LSYM CELLS ALLOT  \ FP-relative offset (negative)
CREATE lsym-arg   MAX-LSYM CELLS ALLOT  \ 1 if function argument
VARIABLE lsym-cnt
8192 CONSTANT LNAME-SZ
CREATE lsym-nbuf LNAME-SZ ALLOT
VARIABLE lsym-nptr

\ === Struct Table ===
CREATE st-name   MAX-STRUCT CELLS ALLOT  \ offset into st-nbuf
CREATE st-nlen   MAX-STRUCT CELLS ALLOT  \ name length
CREATE st-size   MAX-STRUCT CELLS ALLOT  \ total size in bytes
CREATE st-fbase  MAX-STRUCT CELLS ALLOT  \ first field index
CREATE st-fcount MAX-STRUCT CELLS ALLOT  \ number of fields
VARIABLE st-cnt
4096 CONSTANT STNAME-SZ
CREATE st-nbuf STNAME-SZ ALLOT
VARIABLE st-nptr

\ Struct fields (flat array, indexed by st-fbase + field_num)
CREATE fld-name  MAX-FIELD CELLS ALLOT
CREATE fld-nlen  MAX-FIELD CELLS ALLOT
CREATE fld-type  MAX-FIELD CELLS ALLOT
CREATE fld-off   MAX-FIELD CELLS ALLOT   \ byte offset within struct
VARIABLE fld-cnt
4096 CONSTANT FNAME-SZ
CREATE fld-nbuf FNAME-SZ ALLOT
VARIABLE fld-nptr

\ === Enum Table ===
CREATE enum-name MAX-ENUM CELLS ALLOT    \ offset into enum-nbuf
CREATE enum-nlen MAX-ENUM CELLS ALLOT    \ name length
CREATE enum-val  MAX-ENUM CELLS ALLOT    \ integer value
VARIABLE enum-cnt
4096 CONSTANT ENAME-SZ
CREATE enum-nbuf ENAME-SZ ALLOT
VARIABLE enum-nptr

\ === Typedef Table ===
CREATE td-name  MAX-TYPEDEF CELLS ALLOT  \ offset into td-nbuf
CREATE td-nlen  MAX-TYPEDEF CELLS ALLOT
CREATE td-type  MAX-TYPEDEF CELLS ALLOT  \ the aliased type
VARIABLE td-cnt
4096 CONSTANT TDNAME-SZ
CREATE td-nbuf TDNAME-SZ ALLOT
VARIABLE td-nptr

\ === String Literal Pool ===
CREATE str-pool STR-POOL-SZ ALLOT
VARIABLE str-pool-ptr
VARIABLE str-count                       \ number of string literals

\ String literal table (offset and length in str-pool)
256 CONSTANT MAX-STRLIT
CREATE strlit-off MAX-STRLIT CELLS ALLOT
CREATE strlit-len MAX-STRLIT CELLS ALLOT

\ === Code Generation State ===
VARIABLE label-cnt          \ next label number
VARIABLE frame-size         \ current function frame size
VARIABLE func-nargs         \ number of args in current function
VARIABLE local-offset       \ next local variable offset (grows negative from FP)
256 CONSTANT FUNC-FRAME-SZ  \ fixed stage4 frame size
VARIABLE in-function        \ are we inside a function?

\ Break/continue label stacks
CREATE break-stk  MAX-BREAK CELLS ALLOT
VARIABLE break-sp
CREATE cont-stk   MAX-CONT CELLS ALLOT
VARIABLE cont-sp

\ Switch state
CREATE switch-end  MAX-SWITCH CELLS ALLOT  \ end label
CREATE switch-expr MAX-SWITCH CELLS ALLOT  \ expression temp label
VARIABLE switch-sp

\ Current function name
CREATE func-name-buf 256 ALLOT
VARIABLE func-name-len

\ === Preprocessor State ===
VARIABLE pp-skip            \ current #if skip depth (0 = not skipping)
VARIABLE pp-nest            \ nesting depth of #if blocks
32 CONSTANT MAX-IFDEF
CREATE pp-stack MAX-IFDEF CELLS ALLOT  \ stack of skip states
CREATE pp-found MAX-IFDEF CELLS ALLOT  \ 1 if branch already matched at this nesting level
VARIABLE pp-sp
VARIABLE cc-line            \ current source line number
VARIABLE cc-errors          \ error count

\ === Include path ===
CREATE inc-path-buf 256 ALLOT
VARIABLE inc-path-len

\ === File I/O ===
VARIABLE out-fh             \ output file handle

\ Temp variables (avoid deep stack manipulation)
VARIABLE tmp-a
VARIABLE tmp-b
VARIABLE tmp-c
VARIABLE tmp-d
VARIABLE tmp-e
VARIABLE tmp-type
VARIABLE tmp-name-off
VARIABLE tmp-name-len
VARIABLE tmp-idx
VARIABLE fld-offset     \ running offset during struct body parsing

\ Statement-level label variables (separate from tmp-* used by expressions)
VARIABLE sl-a   \ label A (loop start, if-else label, etc.)
VARIABLE sl-b   \ label B (loop end, if-end label, etc.)
VARIABLE sl-c   \ label C (continue target, etc.)
VARIABLE sl-d   \ saved input position
VARIABLE sl-e   \ saved line number

CREATE for-inc-buf 1024 ALLOT   \ saved for-loop increment text
VARIABLE for-inc-len            \ length of saved increment text

\ Current expression type tracking
VARIABLE expr-type          \ type of last expression result

\ Flag for lvalue tracking
VARIABLE is-lvalue          \ 1 if last expr result is an lvalue address in r1

\ ============================================================
\ SECTION 2: Utility Words
\ ============================================================

: CC-ERR ( addr u -- )
    ." Error line " cc-line @ . ." : " TYPE CR
    1 cc-errors +! ;

: CC-WARN ( addr u -- )
    ." Warning line " cc-line @ . ." : " TYPE CR ;

\ --- Output buffer management ---
: OUT-INIT  0 out-len ! ;

: OUT-CHAR ( ch -- )
    out-len @ OUT-SZ >= IF DROP S" output buffer full" CC-ERR EXIT THEN
    out-buf out-len @ + C!
    1 out-len +! ;

: OUT-STR ( addr u -- )
    0 ?DO DUP I + C@ OUT-CHAR LOOP DROP ;

: OUT-NL  10 OUT-CHAR ;

: OUT-SPACE  32 OUT-CHAR ;

\ Emit a decimal number to output
: OUT-NUM ( n -- )
    DUP 0< IF 45 OUT-CHAR NEGATE THEN
    DUP 10 >= IF DUP 10 / RECURSE THEN
    10 MOD 48 + OUT-CHAR ;

\ Emit number with sign handling for negatives (two's complement)
: OUT-SNUM ( n -- )
    DUP 0< IF
        45 OUT-CHAR          \ '-'
        NEGATE
        DUP 0< IF            \ INT_MIN special case
            DROP S" 2147483648" OUT-STR EXIT
        THEN
    THEN
    DUP 10 >= IF DUP 10 / RECURSE THEN
    10 MOD 48 + OUT-CHAR ;

\ --- Assembly emission helpers ---
\ These emit assembly text lines to the output buffer

: EMIT-LINE ( addr u -- )  \ emit text + newline
    OUT-STR OUT-NL ;

: EMIT-INDENT  \ emit 4-space indent
    S"     " OUT-STR ;

: EMIT-LABEL ( n -- )  \ emit .L<n>:
    S" .L" OUT-STR OUT-NUM 58 OUT-CHAR OUT-NL ;

: EMIT-LABEL-REF ( n -- )  \ emit .L<n> (no colon, no newline)
    S" .L" OUT-STR OUT-NUM ;

\ Emit: "    <insn> <args>\n"
\ Usage: S" add r1, r2, r1" EMIT-INSN
: EMIT-INSN ( addr u -- )
    EMIT-INDENT OUT-STR OUT-NL ;

\ --- String comparison helpers ---

\ Case-sensitive compare (addr1 u1 addr2 u2 -- flag) true if equal
: STR= ( a1 u1 a2 u2 -- f )
    ROT OVER <> IF 2DROP DROP FALSE EXIT THEN
    \ Stack: ( a1 a2 u ) where u = u1 = u2
    DUP >R SWAP R> COMPARE 0= ;

\ Check if tok-buf matches a string
: TOK= ( addr u -- f )
    tok-buf tok-len @ 2SWAP STR= ;

\ --- Type helpers ---
: TYPE-BASE ( type -- base )     7 AND ;
: TYPE-PTR  ( type -- depth )    3 RSHIFT 15 AND ;
: TYPE-IS-UNSIGNED ( type -- f ) 7 RSHIFT 1 AND ;
: TYPE-ARRAY-COUNT ( type -- n ) 16 RSHIFT ;

: MAKE-TYPE ( base ptr unsigned -- type )
    7 LSHIFT >R
    3 LSHIFT OR
    R> OR ;

: TYPE-ADD-PTR ( type -- type' )
    DUP CLR-PTR-MASK AND   \ clear bits 3-6
    SWAP TYPE-PTR 1+ 15 AND
    3 LSHIFT OR ;

: TYPE-REMOVE-PTR ( type -- type' )
    DUP TYPE-PTR DUP 0= IF DROP EXIT THEN
    1- 15 AND 3 LSHIFT
    SWAP CLR-PTR-MASK AND  \ clear bits 3-6
    OR ;

: TYPE-SET-ARRAY ( type count -- type' )
    DUP 65535 > IF DROP 65535 THEN
    16 LSHIFT
    SWAP CLR-ARR-MASK AND     \ clear bits 16-31 (array count)
    OR ;

: TYPE-IS-PTR ( type -- f )  TYPE-PTR 0> ;
: TYPE-IS-ARRAY ( type -- f ) TYPE-ARRAY-COUNT 0<> ;

\ Robust struct sizing: use cached st-size when set, otherwise derive from fields.
: STRUCT-SIZE ( struct-idx -- n )
    DUP MAX-STRUCT >= IF DROP 4 EXIT THEN
    DUP CELLS st-size + @ DUP 0<> IF NIP EXIT THEN DROP
    DUP CELLS st-fbase + @ tmp-a !
    CELLS st-fcount + @ tmp-b !
    0 tmp-c !
    tmp-b @ 0 ?DO
        tmp-a @ I + DUP CELLS fld-off + @ >R
        CELLS fld-type + @ TYPE-SIZE
        R> +
        DUP tmp-c @ > IF tmp-c ! ELSE DROP THEN
    LOOP
    tmp-c @ 4 ALIGN-UP ;

\ Size of a type in bytes
\ Get element size of a base type (without array/pointer)
: BASE-TYPE-SIZE ( type -- n )
    DUP TYPE-BASE CASE
        TY-VOID   OF DROP 1 ENDOF
        TY-CHAR   OF DROP 1 ENDOF
        TY-SHORT  OF DROP 2 ENDOF
        TY-INT    OF DROP 4 ENDOF
        TY-LONG   OF DROP 4 ENDOF
        TY-STRUCT OF DROP
            \ Extract struct index from bits 9-15 of the ORIGINAL type
            \ But since this is called after stripping array, we need the index
            \ stored alongside the TY-STRUCT base.
            \ Our type encoding puts struct index in bits 9-15 when base=TY-STRUCT
            \ After stripping array count, bits 16-31 are cleared. So we need
            \ to pass the original type. Let me redesign.
            4  \ fallback
        ENDOF
        TY-ENUM   OF DROP 4 ENDOF
        DROP 4
    ENDCASE ;

: TYPE-SIZE ( type -- n )
    DUP TYPE-IS-ARRAY IF
        DUP TYPE-ARRAY-COUNT >R
        CLR-ARR-MASK AND TYPE-SIZE
        R> * EXIT
    THEN
    DUP TYPE-IS-PTR IF DROP 4 EXIT THEN
    DUP TYPE-BASE CASE
        TY-VOID   OF DROP 1 ENDOF
        TY-CHAR   OF DROP 1 ENDOF
        TY-SHORT  OF DROP 2 ENDOF
        TY-INT    OF DROP 4 ENDOF
        TY-LONG   OF DROP 4 ENDOF
        TY-STRUCT OF 9 RSHIFT 127 AND  \ struct index in bits 9-15
                     STRUCT-SIZE ENDOF
        TY-ENUM   OF DROP 4 ENDOF
        DROP 4
    ENDCASE ;

\ Size of pointed-to element (for pointer arithmetic)
: TYPE-DEREF-SIZE ( type -- n )
    DUP TYPE-IS-ARRAY IF
        \ Array: element size = base type size (strip array count)
        CLR-ARR-MASK AND TYPE-SIZE EXIT
    THEN
    TYPE-REMOVE-PTR TYPE-SIZE ;

\ Alignment of a type
: TYPE-ALIGN ( type -- n )
    DUP TYPE-IS-PTR IF DROP 4 EXIT THEN
    TYPE-BASE CASE
        TY-CHAR   OF 1 ENDOF
        TY-SHORT  OF 2 ENDOF
        4 SWAP
    ENDCASE ;

: ALIGN-UP ( n align -- n' ) 1- TUCK + SWAP INVERT AND ;

\ --- Enum table (needed by lexer's CHECK-KEYWORD) ---
: ENUM-FIND ( addr u -- idx | -1 )
    enum-cnt @ 0 ?DO
        2DUP
        I CELLS enum-name + @ enum-nbuf +
        I CELLS enum-nlen + @
        COMPARE 0= IF 2DROP I UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

: ENUM-ADD ( addr u val -- )
    enum-cnt @ MAX-ENUM >= IF DROP 2DROP EXIT THEN
    enum-cnt @ >R
    R@ CELLS enum-val + !
    DUP R@ CELLS enum-nlen + !
    enum-nptr @ R@ CELLS enum-name + !
    enum-nbuf enum-nptr @ + SWAP CMOVE
    R@ CELLS enum-nlen + @ enum-nptr +!
    R> 1+ enum-cnt ! ;

\ ============================================================
\ SECTION 3: Lexer
\ ============================================================

\ Get current character (or -1 for EOF)
: CC-PEEK ( -- ch|-1 )
    inp-pos @ inp-len @ >= IF -1 EXIT THEN
    inp-buf inp-pos @ + C@ ;

: CC-NEXT ( -- ch|-1 )
    CC-PEEK DUP -1 = IF EXIT THEN
    DUP 10 = IF 1 cc-line +! THEN
    1 inp-pos +! ;

: CC-SKIP  \ skip one character
    CC-PEEK -1 <> IF
        CC-PEEK 10 = IF 1 cc-line +! THEN
        1 inp-pos +!
    THEN ;

\ Skip whitespace and comments
: SKIP-WS ( -- )
    BEGIN
        CC-PEEK DUP -1 = IF DROP EXIT THEN
        DUP 32 = OVER 9 = OR OVER 10 = OR OVER 13 = OR
        IF DROP CC-SKIP TRUE
        ELSE
            \ Check for // comment
            DUP 47 = IF   \ '/'
                DROP
                inp-pos @ 1+ inp-len @ < IF
                    inp-buf inp-pos @ 1+ + C@ 47 = IF
                        \ line comment - skip to end of line
                        BEGIN CC-NEXT DUP -1 = OVER 10 = OR IF DROP TRUE ELSE DROP FALSE THEN UNTIL
                        TRUE
                    ELSE
                        inp-buf inp-pos @ 1+ + C@ 42 = IF  \ '/*'
                            CC-SKIP CC-SKIP  \ skip / and *
                            BEGIN
                                CC-PEEK -1 = IF S" unterminated comment" CC-ERR TRUE
                                ELSE
                                    CC-PEEK 42 = IF  \ '*'
                                        CC-SKIP
                                        CC-PEEK 47 = IF CC-SKIP TRUE ELSE FALSE THEN
                                    ELSE CC-SKIP FALSE THEN
                                THEN
                            UNTIL
                            TRUE
                        ELSE FALSE THEN
                    THEN
                ELSE FALSE THEN
            ELSE DROP FALSE THEN
        THEN
    WHILE REPEAT ;

\ Is character a digit?
: IS-DIGIT ( ch -- f ) DUP 48 >= SWAP 57 <= AND ;
: IS-HEXDIG ( ch -- f )
    DUP IS-DIGIT IF DROP TRUE EXIT THEN
    DUP 65 >= OVER 70 <= AND IF DROP TRUE EXIT THEN  \ A-F
    DUP 97 >= SWAP 102 <= AND ;  \ a-f

\ Is character a letter or underscore?
: IS-ALPHA ( ch -- f )
    DUP 95 = IF DROP TRUE EXIT THEN   \ '_'
    DUP 65 >= OVER 90 <= AND IF DROP TRUE EXIT THEN   \ A-Z
    DUP 97 >= SWAP 122 <= AND ;  \ a-z

: IS-ALNUM ( ch -- f ) DUP IS-ALPHA IF DROP TRUE ELSE IS-DIGIT THEN ;

\ Read an identifier into tok-buf
: LEX-IDENT ( -- )
    0 tok-len !
    BEGIN
        CC-PEEK DUP IS-ALNUM
    WHILE
        tok-buf tok-len @ + C!
        1 tok-len +!
        CC-SKIP
    REPEAT DROP ;

\ Check if tok-buf is a keyword, set tok-type/tok-val accordingly
: CHECK-KEYWORD ( -- )
    TK-IDENT tok-type !
    S" int"      TOK= IF TK-KW tok-type ! KW-INT      tok-val ! EXIT THEN
    S" char"     TOK= IF TK-KW tok-type ! KW-CHAR     tok-val ! EXIT THEN
    S" void"     TOK= IF TK-KW tok-type ! KW-VOID     tok-val ! EXIT THEN
    S" if"       TOK= IF TK-KW tok-type ! KW-IF       tok-val ! EXIT THEN
    S" else"     TOK= IF TK-KW tok-type ! KW-ELSE     tok-val ! EXIT THEN
    S" while"    TOK= IF TK-KW tok-type ! KW-WHILE    tok-val ! EXIT THEN
    S" for"      TOK= IF TK-KW tok-type ! KW-FOR      tok-val ! EXIT THEN
    S" do"       TOK= IF TK-KW tok-type ! KW-DO       tok-val ! EXIT THEN
    S" return"   TOK= IF TK-KW tok-type ! KW-RETURN   tok-val ! EXIT THEN
    S" break"    TOK= IF TK-KW tok-type ! KW-BREAK    tok-val ! EXIT THEN
    S" continue" TOK= IF TK-KW tok-type ! KW-CONTINUE tok-val ! EXIT THEN
    S" static"   TOK= IF TK-KW tok-type ! KW-STATIC   tok-val ! EXIT THEN
    S" const"    TOK= IF TK-KW tok-type ! KW-CONST    tok-val ! EXIT THEN
    S" unsigned" TOK= IF TK-KW tok-type ! KW-UNSIGNED  tok-val ! EXIT THEN
    S" signed"   TOK= IF TK-KW tok-type ! KW-SIGNED   tok-val ! EXIT THEN
    S" long"     TOK= IF TK-KW tok-type ! KW-LONG     tok-val ! EXIT THEN
    S" short"    TOK= IF TK-KW tok-type ! KW-SHORT    tok-val ! EXIT THEN
    S" struct"   TOK= IF TK-KW tok-type ! KW-STRUCT   tok-val ! EXIT THEN
    S" enum"     TOK= IF TK-KW tok-type ! KW-ENUM     tok-val ! EXIT THEN
    S" typedef"  TOK= IF TK-KW tok-type ! KW-TYPEDEF  tok-val ! EXIT THEN
    S" sizeof"   TOK= IF TK-KW tok-type ! KW-SIZEOF   tok-val ! EXIT THEN
    S" switch"   TOK= IF TK-KW tok-type ! KW-SWITCH   tok-val ! EXIT THEN
    S" case"     TOK= IF TK-KW tok-type ! KW-CASE     tok-val ! EXIT THEN
    S" default"  TOK= IF TK-KW tok-type ! KW-DEFAULT  tok-val ! EXIT THEN
    S" extern"   TOK= IF TK-KW tok-type ! KW-EXTERN   tok-val ! EXIT THEN
    S" inline"   TOK= IF TK-KW tok-type ! KW-INLINE   tok-val ! EXIT THEN
    S" bool"     TOK= IF TK-KW tok-type ! KW-BOOL     tok-val ! EXIT THEN
    \ check enum values
    tok-buf tok-len @ ENUM-FIND DUP -1 <> IF
        TK-NUM tok-type !
        CELLS enum-val + @ tok-val !
        EXIT
    THEN DROP
    \ check typedefs — handled later in parser
;

\ Read a number (decimal or hex)
: LEX-NUMBER ( -- )
    0 tok-val !
    CC-PEEK 48 = IF   \ starts with '0'
        CC-SKIP
        CC-PEEK DUP 120 = SWAP 88 = OR IF  \ '0x' or '0X'
            CC-SKIP
            BEGIN CC-PEEK DUP IS-HEXDIG WHILE
                tok-val @ 16 *
                SWAP
                DUP IS-DIGIT IF 48 - ELSE
                    DUP 97 >= IF 87 - ELSE 55 - THEN  \ a=10, A=10
                THEN +
                tok-val !
                CC-SKIP
            REPEAT DROP
        ELSE
            \ Octal or just 0
            BEGIN CC-PEEK DUP IS-DIGIT WHILE
                tok-val @ 8 * SWAP 48 - + tok-val !
                CC-SKIP
            REPEAT DROP
        THEN
    ELSE
        BEGIN CC-PEEK DUP IS-DIGIT WHILE
            tok-val @ 10 * SWAP 48 - + tok-val !
            CC-SKIP
        REPEAT DROP
    THEN
    \ Skip optional U/L suffixes
    CC-PEEK DUP 85 = OVER 117 = OR IF DROP CC-SKIP
        CC-PEEK DUP 76 = OVER 108 = OR IF DROP CC-SKIP THEN
    ELSE
        DUP 76 = OVER 108 = OR IF DROP CC-SKIP
            CC-PEEK DUP 85 = OVER 117 = OR IF DROP CC-SKIP THEN
        ELSE DROP THEN
    THEN
    TK-NUM tok-type ! ;

\ Read a string literal into str-pool, return pool offset and length
: LEX-STRING ( -- )
    CC-SKIP  \ skip opening "
    str-pool-ptr @ tmp-a !  \ remember start
    0 tmp-b !  \ length
    BEGIN
        CC-PEEK DUP 34 <> OVER -1 <> AND  \ not " and not EOF
    WHILE
        DUP 92 = IF   \ backslash escape
            DROP CC-SKIP
            CC-PEEK CASE
                110 OF 10 ENDOF   \ \n
                116 OF  9 ENDOF   \ \t
                114 OF 13 ENDOF   \ \r
                 92 OF 92 ENDOF   \ \\
                 34 OF 34 ENDOF   \ \"
                 39 OF 39 ENDOF   \ \'
                 48 OF  0 ENDOF   \ \0
                120 OF             \ \x##
                    CC-SKIP
                    0
                    CC-PEEK DUP IS-HEXDIG IF
                        DUP IS-DIGIT IF 48 - ELSE
                            DUP 97 >= IF 87 - ELSE 55 - THEN
                        THEN
                        CC-SKIP
                        CC-PEEK DUP IS-HEXDIG IF
                            SWAP 16 *
                            DUP IS-DIGIT IF 48 - ELSE
                                DUP 97 >= IF 87 - ELSE 55 - THEN
                            THEN +
                            CC-SKIP
                        ELSE DROP THEN
                    ELSE DROP THEN
                    DUP   \ duplicate for store below
                ENDOF
                DUP   \ unknown escape - keep char as-is
            ENDCASE
            str-pool str-pool-ptr @ + C!
            1 str-pool-ptr +!
            1 tmp-b +!
            CC-SKIP
        ELSE
            str-pool str-pool-ptr @ + C!
            1 str-pool-ptr +!
            1 tmp-b +!
            CC-SKIP
        THEN
    REPEAT DROP
    CC-PEEK 34 = IF CC-SKIP THEN  \ skip closing "
    \ NUL-terminate
    0 str-pool str-pool-ptr @ + C!
    1 str-pool-ptr +!
    1 tmp-b +!
    \ Record in string literal table
    str-count @ MAX-STRLIT < IF
        tmp-a @ str-count @ CELLS strlit-off + !
        tmp-b @  str-count @ CELLS strlit-len + !
        str-count @ tok-val !
        1 str-count +!
    THEN
    TK-STR tok-type !
    \ Also copy string text into tok-buf for convenience
    tmp-b @ 1- tok-len !  \ length without NUL
    str-pool tmp-a @ + tok-buf tmp-b @ 1- CMOVE ;

\ Read a character constant
: LEX-CHAR-CONST ( -- )
    CC-SKIP  \ skip opening '
    CC-PEEK 92 = IF   \ escape
        CC-SKIP
        CC-PEEK CASE
            110 OF 10 ENDOF
            116 OF  9 ENDOF
            114 OF 13 ENDOF
             92 OF 92 ENDOF
             39 OF 39 ENDOF
             48 OF  0 ENDOF
            DUP
        ENDCASE
        tok-val !
        CC-SKIP
    ELSE
        CC-NEXT tok-val !
    THEN
    CC-PEEK 39 = IF CC-SKIP THEN  \ skip closing '
    TK-NUM tok-type ! ;  \ treat as integer

\ Read a punctuation/operator token
: LEX-PUNCT ( -- )
    TK-PUNCT tok-type !
    CC-NEXT CASE
        40 OF P-LPAREN tok-val ! ENDOF    \ (
        41 OF P-RPAREN tok-val ! ENDOF    \ )
        123 OF P-LBRACE tok-val ! ENDOF   \ {
        125 OF P-RBRACE tok-val ! ENDOF   \ }
        91 OF P-LBRACK tok-val ! ENDOF    \ [
        93 OF P-RBRACK tok-val ! ENDOF    \ ]
        59 OF P-SEMI tok-val ! ENDOF      \ ;
        44 OF P-COMMA tok-val ! ENDOF     \ ,
        46 OF                              \ . or ...
            CC-PEEK 46 = IF CC-SKIP CC-PEEK 46 = IF CC-SKIP P-ELLIPSIS tok-val !
            ELSE P-DOT tok-val ! THEN
            ELSE P-DOT tok-val ! THEN
        ENDOF
        126 OF P-TILDE tok-val ! ENDOF    \ ~
        63 OF P-QMARK tok-val ! ENDOF     \ ?
        58 OF P-COLON tok-val ! ENDOF     \ :
        35 OF P-HASH tok-val ! ENDOF      \ #
        43 OF                              \ + ++ +=
            CC-PEEK 43 = IF CC-SKIP P-INC tok-val !
            ELSE CC-PEEK 61 = IF CC-SKIP P-PLUSEQ tok-val !
            ELSE P-PLUS tok-val ! THEN THEN
        ENDOF
        45 OF                              \ - -- -= ->
            CC-PEEK 45 = IF CC-SKIP P-DEC tok-val !
            ELSE CC-PEEK 61 = IF CC-SKIP P-MINUSEQ tok-val !
            ELSE CC-PEEK 62 = IF CC-SKIP P-ARROW tok-val !
            ELSE P-MINUS tok-val ! THEN THEN THEN
        ENDOF
        42 OF                              \ * *=
            CC-PEEK 61 = IF CC-SKIP P-STAREQ tok-val !
            ELSE P-STAR tok-val ! THEN
        ENDOF
        47 OF                              \ / /=
            CC-PEEK 61 = IF CC-SKIP P-SLASHEQ tok-val !
            ELSE P-SLASH tok-val ! THEN
        ENDOF
        37 OF                              \ % %=
            CC-PEEK 61 = IF CC-SKIP P-PERCENTEQ tok-val !
            ELSE P-PERCENT tok-val ! THEN
        ENDOF
        38 OF                              \ & && &=
            CC-PEEK 38 = IF CC-SKIP P-LAND tok-val !
            ELSE CC-PEEK 61 = IF CC-SKIP P-AMPEQ tok-val !
            ELSE P-AMP tok-val ! THEN THEN
        ENDOF
        124 OF                             \ | || |=
            CC-PEEK 124 = IF CC-SKIP P-LOR tok-val !
            ELSE CC-PEEK 61 = IF CC-SKIP P-PIPEEQ tok-val !
            ELSE P-PIPE tok-val ! THEN THEN
        ENDOF
        94 OF                              \ ^ ^=
            CC-PEEK 61 = IF CC-SKIP P-CARETEQ tok-val !
            ELSE P-CARET tok-val ! THEN
        ENDOF
        33 OF                              \ ! !=
            CC-PEEK 61 = IF CC-SKIP P-NE tok-val !
            ELSE P-BANG tok-val ! THEN
        ENDOF
        61 OF                              \ = ==
            CC-PEEK 61 = IF CC-SKIP P-EQ tok-val !
            ELSE P-ASSIGN tok-val ! THEN
        ENDOF
        60 OF                              \ < <= << <<=
            CC-PEEK 60 = IF CC-SKIP
                CC-PEEK 61 = IF CC-SKIP P-LSHIFTEQ tok-val !
                ELSE P-LSHIFT tok-val ! THEN
            ELSE CC-PEEK 61 = IF CC-SKIP P-LE tok-val !
            ELSE P-LT tok-val ! THEN THEN
        ENDOF
        62 OF                              \ > >= >> >>=
            CC-PEEK 62 = IF CC-SKIP
                CC-PEEK 61 = IF CC-SKIP P-RSHIFTEQ tok-val !
                ELSE P-RSHIFT tok-val ! THEN
            ELSE CC-PEEK 61 = IF CC-SKIP P-GE tok-val !
            ELSE P-GT tok-val ! THEN THEN
        ENDOF
        \ Unknown character
        DUP S" unexpected character" CC-ERR
    ENDCASE ;

\ Main tokenizer - read next token
: NEXT-TOKEN ( -- )
    has-peek @ IF
        \ Restore peeked token
        peek-buf tok-buf TOK-SZ CMOVE
        peek-len @ tok-len !
        peek-type @ tok-type !
        peek-val @ tok-val !
        0 has-peek !
        EXIT
    THEN
    SKIP-WS
    CC-PEEK -1 = IF TK-EOF tok-type ! 0 tok-val ! 0 tok-len ! EXIT THEN
    CC-PEEK DUP IS-ALPHA IF
        DROP LEX-IDENT CHECK-KEYWORD EXIT
    THEN
    DUP IS-DIGIT IF
        DROP LEX-NUMBER EXIT
    THEN
    DUP 34 = IF  \ "
        DROP LEX-STRING EXIT
    THEN
    DUP 39 = IF  \ '
        DROP LEX-CHAR-CONST EXIT
    THEN
    DROP LEX-PUNCT ;

\ Save current token for peek
: SAVE-PEEK ( -- )
    tok-buf peek-buf TOK-SZ CMOVE
    tok-len @ peek-len !
    tok-type @ peek-type !
    tok-val @ peek-val ! ;

\ Unget approach: after reading a token, push it back for re-reading
\ After NEXT-TOKEN, call UNGET-TOKEN to push it back
: UNGET-TOKEN ( -- )
    tok-buf peek-buf TOK-SZ CMOVE
    tok-len @ peek-len !
    tok-type @ peek-type !
    tok-val @ peek-val !
    1 has-peek ! ;

\ Expect a specific punctuation, error if not
: EXPECT-PUNCT ( punct-id -- )
    tok-type @ TK-PUNCT <> IF
        S" expected punctuation" CC-ERR DROP EXIT
    THEN
    tok-val @ <> IF
        S" unexpected punctuation" CC-ERR EXIT
    THEN ;

: EXPECT-SEMI ( -- )  \ check current token is ';'
    P-SEMI EXPECT-PUNCT ;

: EXPECT-RPAREN ( -- )
    P-RPAREN EXPECT-PUNCT ;

: EXPECT-RBRACE ( -- )
    P-RBRACE EXPECT-PUNCT ;

: EXPECT-LBRACE ( -- )
    P-LBRACE EXPECT-PUNCT ;

\ Check if current token is a specific punctuation
: TOK-IS-PUNCT ( id -- f )
    tok-type @ TK-PUNCT = IF tok-val @ = ELSE DROP FALSE THEN ;

: TOK-IS-KW ( id -- f )
    tok-type @ TK-KW = IF tok-val @ = ELSE DROP FALSE THEN ;

\ Generate a new unique label number
: NEW-LABEL ( -- n )  label-cnt @ DUP 1+ label-cnt ! ;

\ ============================================================
\ SECTION 4: Symbol Tables
\ ============================================================

\ --- Typedef table ---
: TD-FIND ( addr u -- idx | -1 )
    td-cnt @ 0 ?DO
        2DUP
        I CELLS td-name + @ td-nbuf +
        I CELLS td-nlen + @
        COMPARE 0= IF 2DROP I UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

: TD-ADD ( addr u type -- )
    td-cnt @ MAX-TYPEDEF >= IF DROP 2DROP EXIT THEN
    td-cnt @ >R
    R@ CELLS td-type + !
    DUP R@ CELLS td-nlen + !
    td-nptr @ R@ CELLS td-name + !
    td-nbuf td-nptr @ + SWAP CMOVE
    R@ CELLS td-nlen + @ td-nptr +!
    R> 1+ td-cnt ! ;

\ --- Global symbol table ---
: GSYM-FIND ( addr u -- idx | -1 )
    gsym-cnt @ 0 ?DO
        2DUP
        I CELLS gsym-name + @ gsym-nbuf +
        I CELLS gsym-nlen + @
        COMPARE 0= IF 2DROP I UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

: GSYM-ADD ( addr u type kind flags -- idx )
    gsym-cnt @ MAX-GSYM >= IF 2DROP DROP 2DROP S" global symbol table full" CC-ERR -1 EXIT THEN
    gsym-cnt @ >R
    R@ CELLS gsym-flags + !
    R@ CELLS gsym-kind + !
    R@ CELLS gsym-type + !
    DUP R@ CELLS gsym-nlen + !
    gsym-nptr @ R@ CELLS gsym-name + !
    gsym-nbuf gsym-nptr @ + SWAP CMOVE
    R@ CELLS gsym-nlen + @ gsym-nptr +!
    0 R@ CELLS gsym-off + !
    R> DUP 1+ gsym-cnt ! ;

\ --- Local symbol table ---
: LSYM-FIND ( addr u -- idx | -1 )
    lsym-cnt @ 0 ?DO
        2DUP
        I CELLS lsym-name + @ lsym-nbuf +
        I CELLS lsym-nlen + @
        COMPARE 0= IF 2DROP I UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

: LSYM-ADD ( addr u type fp-offset is-arg -- idx )
    lsym-cnt @ MAX-LSYM >= IF 2DROP DROP 2DROP S" local symbol table full" CC-ERR -1 EXIT THEN
    lsym-cnt @ >R
    R@ CELLS lsym-arg + !
    R@ CELLS lsym-off + !
    R@ CELLS lsym-type + !
    DUP R@ CELLS lsym-nlen + !
    lsym-nptr @ R@ CELLS lsym-name + !
    lsym-nbuf lsym-nptr @ + SWAP CMOVE
    R@ CELLS lsym-nlen + @ lsym-nptr +!
    R> DUP 1+ lsym-cnt ! ;

: LSYM-RESET  0 lsym-cnt ! 0 lsym-nptr ! ;

\ --- Struct table ---
: ST-FIND ( addr u -- idx | -1 )
    st-cnt @ 0 ?DO
        2DUP
        I CELLS st-name + @ st-nbuf +
        I CELLS st-nlen + @
        COMPARE 0= IF 2DROP I UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

: ST-ADD ( addr u -- idx )
    st-cnt @ MAX-STRUCT >= IF 2DROP S" struct table full" CC-ERR -1 EXIT THEN
    st-cnt @ >R
    DUP R@ CELLS st-nlen + !
    st-nptr @ R@ CELLS st-name + !
    st-nbuf st-nptr @ + SWAP CMOVE
    R@ CELLS st-nlen + @ st-nptr +!
    0 R@ CELLS st-size + !
    fld-cnt @ R@ CELLS st-fbase + !
    0 R@ CELLS st-fcount + !
    R> DUP 1+ st-cnt ! ;

\ Find field in struct by name
: FLD-FIND ( struct-idx addr u -- field-idx | -1 )
    tmp-name-len ! tmp-name-off !
    DUP CELLS st-fbase + @ tmp-a !    \ first field
    CELLS st-fcount + @ tmp-b !        \ count
    tmp-b @ 0 ?DO
        tmp-name-off @ tmp-name-len @
        tmp-a @ I + CELLS fld-name + @ fld-nbuf +
        tmp-a @ I + CELLS fld-nlen + @
        COMPARE 0= IF tmp-a @ I + UNLOOP EXIT THEN
    LOOP
    -1 ;

: FLD-ADD ( struct-idx name-addr name-len type offset -- )
    fld-cnt @ MAX-FIELD >= IF 2DROP 2DROP DROP EXIT THEN
    fld-cnt @ >R
    R@ CELLS fld-off + !
    R@ CELLS fld-type + !
    DUP R@ CELLS fld-nlen + !
    fld-nptr @ R@ CELLS fld-name + !
    fld-nbuf fld-nptr @ + SWAP CMOVE
    R@ CELLS fld-nlen + @ fld-nptr +!
    R> 1+ fld-cnt !
    \ Update struct field count
    DUP CELLS st-fcount + @ 1+ SWAP CELLS st-fcount + ! ;

\ --- Macro table ---
: MAC-FIND ( addr u -- idx | -1 )
    mac-cnt @ 0 ?DO
        2DUP
        I CELLS mac-name + @ mname-buf +
        I CELLS mac-nlen + @
        COMPARE 0= IF 2DROP I UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

: MAC-ADD-OBJ ( name-addr name-len val-addr val-len -- )
    mac-cnt @ MAX-MACRO >= IF 2DROP 2DROP EXIT THEN
    mac-cnt @ >R
    \ Store value
    DUP R@ CELLS mac-vlen + !
    mval-ptr @ R@ CELLS mac-val + !
    mval-buf mval-ptr @ + SWAP CMOVE
    R@ CELLS mac-vlen + @ mval-ptr +!
    \ Store name
    DUP R@ CELLS mac-nlen + !
    mname-ptr @ R@ CELLS mac-name + !
    mname-buf mname-ptr @ + SWAP CMOVE
    R@ CELLS mac-nlen + @ mname-ptr +!
    -1 R@ CELLS mac-npar + !  \ -1 = object-like
    R> 1+ mac-cnt ! ;

: MAC-UNDEF ( addr u -- )
    MAC-FIND DUP -1 = IF DROP EXIT THEN
    \ Mark as deleted by setting name length to 0
    0 SWAP CELLS mac-nlen + ! ;

\ --- Lookup any name: local > enum > global > typedef ---
\ Returns: kind (0=local, 1=global, 2=typedef, 3=enum, -1=not found) and index
: LOOKUP-NAME ( addr u -- idx kind )
    2DUP LSYM-FIND DUP -1 <> IF NIP NIP 0 EXIT THEN DROP
    2DUP ENUM-FIND DUP -1 <> IF NIP NIP 3 EXIT THEN DROP
    2DUP GSYM-FIND DUP -1 <> IF NIP NIP 1 EXIT THEN DROP
    2DUP TD-FIND DUP -1 <> IF NIP NIP 2 EXIT THEN DROP
    2DROP -1 -1 ;

\ ============================================================
\ SECTION 5: Code Generation Helpers
\ ============================================================

\ Push r1 to stack (for expression evaluation)
: EMIT-PUSH-R1 ( -- )
    S" addi r29, r29, -4" EMIT-INSN
    S" stw r29, r1, 0" EMIT-INSN ;

\ Pop to r2 from stack
: EMIT-POP-R2 ( -- )
    S" ldw r2, r29, 0" EMIT-INSN
    S" addi r29, r29, 4" EMIT-INSN ;

\ Load immediate into r1
: EMIT-LI-R1 ( n -- )
    DUP -2048 >= OVER 2047 <= AND IF
        EMIT-INDENT S" addi r1, r0, " OUT-STR OUT-SNUM OUT-NL
    ELSE
        DUP 2048 + 12 RSHIFT MASK20 AND
        EMIT-INDENT S" lui r1, " OUT-STR OUT-NUM OUT-NL
        MASK12 AND
        DUP SIGN12 AND IF SIGN12-EXT OR THEN
        EMIT-INDENT S" addi r1, r1, " OUT-STR OUT-SNUM OUT-NL
    THEN ;

\ Load immediate into r2
: EMIT-LI-R2 ( n -- )
    DUP -2048 >= OVER 2047 <= AND IF
        EMIT-INDENT S" addi r2, r0, " OUT-STR OUT-SNUM OUT-NL
    ELSE
        DUP 2048 + 12 RSHIFT MASK20 AND
        EMIT-INDENT S" lui r2, " OUT-STR OUT-NUM OUT-NL
        MASK12 AND
        DUP SIGN12 AND IF SIGN12-EXT OR THEN
        EMIT-INDENT S" addi r2, r2, " OUT-STR OUT-SNUM OUT-NL
    THEN ;

\ Load global variable address into r2
: EMIT-GLOBAL-ADDR ( addr u -- )  \ addr u = symbol name
    EMIT-INDENT S" lui r2, %hi(" OUT-STR 2DUP OUT-STR S" )" OUT-STR OUT-NL
    EMIT-INDENT S" addi r2, r2, %lo(" OUT-STR OUT-STR S" )" OUT-STR OUT-NL ;

\ Load global into r1
: EMIT-LOAD-GLOBAL ( addr u type -- )
    tmp-type !
    2DUP EMIT-GLOBAL-ADDR
    tmp-type @ TYPE-IS-ARRAY IF
        \ Array decays to pointer — address is the value
        S" addi r1, r2, 0" EMIT-INSN
    ELSE
        tmp-type @ TYPE-BASE TY-CHAR = tmp-type @ TYPE-PTR 0= AND IF
            tmp-type @ TYPE-IS-UNSIGNED IF
                S" lbu r1, r2, 0" EMIT-INSN
            ELSE
                S" ldb r1, r2, 0" EMIT-INSN
            THEN
        ELSE tmp-type @ TYPE-BASE TY-SHORT = tmp-type @ TYPE-PTR 0= AND IF
            tmp-type @ TYPE-IS-UNSIGNED IF
                S" lhu r1, r2, 0" EMIT-INSN
            ELSE
                S" ldh r1, r2, 0" EMIT-INSN
            THEN
        ELSE
            S" ldw r1, r2, 0" EMIT-INSN
        THEN THEN
    THEN ;

\ Store r1 to global
: EMIT-STORE-GLOBAL ( addr u type -- )
    tmp-type !
    EMIT-GLOBAL-ADDR
    tmp-type @ TYPE-BASE TY-CHAR = tmp-type @ TYPE-PTR 0= AND IF
        S" stb r2, r1, 0" EMIT-INSN
    ELSE tmp-type @ TYPE-BASE TY-SHORT = tmp-type @ TYPE-PTR 0= AND IF
        S" sth r2, r1, 0" EMIT-INSN
    ELSE
        S" stw r2, r1, 0" EMIT-INSN
    THEN THEN ;

\ Load from FP-relative offset into r1
: EMIT-LOAD-LOCAL ( offset type -- )
    tmp-type !
    tmp-type @ TYPE-IS-ARRAY IF
        \ Array decays to pointer — compute address
        EMIT-INDENT S" addi r1, r30, " OUT-STR OUT-SNUM OUT-NL
    ELSE
        tmp-type @ TYPE-BASE TY-CHAR = tmp-type @ TYPE-PTR 0= AND IF
            tmp-type @ TYPE-IS-UNSIGNED IF
                EMIT-INDENT S" lbu r1, r30, " OUT-STR OUT-SNUM OUT-NL
            ELSE
                EMIT-INDENT S" ldb r1, r30, " OUT-STR OUT-SNUM OUT-NL
            THEN
        ELSE tmp-type @ TYPE-BASE TY-SHORT = tmp-type @ TYPE-PTR 0= AND IF
            tmp-type @ TYPE-IS-UNSIGNED IF
                EMIT-INDENT S" lhu r1, r30, " OUT-STR OUT-SNUM OUT-NL
            ELSE
                EMIT-INDENT S" ldh r1, r30, " OUT-STR OUT-SNUM OUT-NL
            THEN
        ELSE
            EMIT-INDENT S" ldw r1, r30, " OUT-STR OUT-SNUM OUT-NL
        THEN THEN
    THEN ;

\ Store r1 to FP-relative offset
: EMIT-STORE-LOCAL ( offset type -- )
    tmp-type !
    tmp-type @ TYPE-BASE TY-CHAR = tmp-type @ TYPE-PTR 0= AND IF
        EMIT-INDENT S" stb r30, r1, " OUT-STR OUT-SNUM OUT-NL
    ELSE tmp-type @ TYPE-BASE TY-SHORT = tmp-type @ TYPE-PTR 0= AND IF
        EMIT-INDENT S" sth r30, r1, " OUT-STR OUT-SNUM OUT-NL
    ELSE
        EMIT-INDENT S" stw r30, r1, " OUT-STR OUT-SNUM OUT-NL
    THEN THEN ;

\ Load from address in r1 (dereference pointer)
: EMIT-LOAD-DEREF ( type -- )
    DUP TYPE-BASE TY-CHAR = OVER TYPE-PTR 0= AND IF
        TYPE-IS-UNSIGNED IF
            S" lbu r1, r1, 0" EMIT-INSN
        ELSE
            S" ldb r1, r1, 0" EMIT-INSN
        THEN EXIT
    THEN
    DUP TYPE-BASE TY-SHORT = OVER TYPE-PTR 0= AND IF
        TYPE-IS-UNSIGNED IF
            S" lhu r1, r1, 0" EMIT-INSN
        ELSE
            S" ldh r1, r1, 0" EMIT-INSN
        THEN EXIT
    THEN
    DROP S" ldw r1, r1, 0" EMIT-INSN ;

\ Store r1 (value) at address in r2 based on type
: EMIT-STORE-DEREF ( type -- )
    DUP TYPE-BASE TY-CHAR = OVER TYPE-PTR 0= AND IF
        DROP S" stb r2, r1, 0" EMIT-INSN EXIT
    THEN
    DUP TYPE-BASE TY-SHORT = OVER TYPE-PTR 0= AND IF
        DROP S" sth r2, r1, 0" EMIT-INSN EXIT
    THEN
    DROP S" stw r2, r1, 0" EMIT-INSN ;

\ Emit function prologue
: EMIT-PROLOGUE ( frame-size -- )
    DUP EMIT-INDENT S" addi r29, r29, -" OUT-STR OUT-NUM OUT-NL
    DUP 4 - EMIT-INDENT S" stw r29, r31, " OUT-STR OUT-NUM OUT-NL
    DUP 8 - EMIT-INDENT S" stw r29, r30, " OUT-STR OUT-NUM OUT-NL
    EMIT-INDENT S" addi r30, r29, " OUT-STR OUT-NUM OUT-NL ;

\ Emit function epilogue
: EMIT-EPILOGUE ( frame-size -- )
    DUP 4 - EMIT-INDENT S" ldw r31, r29, " OUT-STR OUT-NUM OUT-NL
    DUP 8 - EMIT-INDENT S" ldw r30, r29, " OUT-STR OUT-NUM OUT-NL
    EMIT-INDENT S" addi r29, r29, " OUT-STR OUT-NUM OUT-NL
    S" jalr r0, r31, 0" EMIT-INSN ;

\ Emit conditional branch (branch if r1 == 0 to label)
: EMIT-BEQ-ZERO ( label -- )
    EMIT-INDENT S" beq r1, r0, .L" OUT-STR OUT-NUM OUT-NL ;

\ Emit conditional branch (branch if r1 != 0 to label)
: EMIT-BNE-ZERO ( label -- )
    EMIT-INDENT S" bne r1, r0, .L" OUT-STR OUT-NUM OUT-NL ;

\ Emit unconditional jump to label
: EMIT-JUMP ( label -- )
    EMIT-INDENT S" jal r0, .L" OUT-STR OUT-NUM OUT-NL ;

\ Emit function call
: EMIT-CALL ( addr u -- )  \ addr u = function name
    EMIT-INDENT S" call " OUT-STR OUT-STR OUT-NL ;

\ Emit move r1 to arg register
: EMIT-MOV-ARG ( argnum -- )  \ argnum 0-7 -> r3-r10
    3 + DUP 10 > IF DROP S" too many arguments" CC-ERR EXIT THEN
    EMIT-INDENT S" addi r" OUT-STR OUT-NUM S" , r1, 0" OUT-STR OUT-NL ;

\ Emit move arg register to r1
: EMIT-MOV-FROM-ARG ( argnum -- )
    3 +
    EMIT-INDENT S" addi r1, r" OUT-STR OUT-NUM S" , 0" OUT-STR OUT-NL ;

\ ============================================================
\ SECTION 6: Preprocessor
\ ============================================================

\ Scratch buffer for reading preprocessor lines
CREATE pp-line 1024 ALLOT
VARIABLE pp-line-len

\ Read rest of current line into pp-line
: PP-READ-LINE ( -- )
    0 pp-line-len !
    \ Skip leading whitespace
    BEGIN CC-PEEK DUP 32 = OVER 9 = OR WHILE DROP CC-SKIP REPEAT DROP
    BEGIN
        CC-PEEK DUP 10 <> OVER 13 <> AND OVER -1 <> AND
    WHILE
        pp-line pp-line-len @ + C!
        1 pp-line-len +!
        CC-SKIP
    REPEAT DROP
    \ Strip // comments from pp-line
    0 BEGIN
        DUP pp-line-len @ 1- < IF
            pp-line OVER + C@ 47 = IF     \ '/'
                pp-line OVER + 1+ C@ 47 = IF  \ '//'
                    DUP pp-line-len !
                    DROP pp-line-len @   \ force exit
                ELSE 1+ THEN
            ELSE 1+ THEN
        ELSE DROP pp-line-len @ THEN
        DUP pp-line-len @ >=
    UNTIL DROP
    \ Trim trailing whitespace
    BEGIN
        pp-line-len @ 0> IF
            pp-line pp-line-len @ 1- + C@ DUP 32 = SWAP 9 = OR
        ELSE FALSE THEN
    WHILE -1 pp-line-len +! REPEAT ;

\ Parse identifier from pp-line at position tmp-d into tmp-name area
CREATE pp-ident 256 ALLOT
VARIABLE pp-ident-len

: PP-SKIP-WS ( -- )
    BEGIN
        tmp-d @ pp-line-len @ < IF
            pp-line tmp-d @ + C@ DUP 32 = SWAP 9 = OR
        ELSE FALSE THEN
    WHILE 1 tmp-d +! REPEAT ;

: PP-READ-IDENT ( -- ) \ read ident at tmp-d, store in pp-ident
    0 pp-ident-len !
    BEGIN
        tmp-d @ pp-line-len @ < IF
            pp-line tmp-d @ + C@ DUP IS-ALNUM
        ELSE FALSE THEN
    WHILE
        pp-ident pp-ident-len @ + C!
        1 pp-ident-len +!
        1 tmp-d +!
    REPEAT
    tmp-d @ pp-line-len @ < IF DROP THEN ;

\ Include file support
CREATE inc-fname 256 ALLOT
VARIABLE inc-fname-len

\ Save current input state and load new file
: PP-PUSH-INPUT ( addr u -- flag )  \ returns true on success
    inc-depth @ MAX-INC-DEPTH >= IF
        2DROP S" include nesting too deep" CC-ERR FALSE EXIT
    THEN
    \ Save current state
    inc-depth @ CELLS inc-pos + inp-pos @ SWAP !
    inc-depth @ CELLS inc-len + inp-len @ SWAP !
    inc-depth @ CELLS inc-line + cc-line @ SWAP !
    \ Copy remaining input to pre-allocated save buffer
    inp-len @ inp-pos @ - tmp-a !  \ remaining bytes
    tmp-a @ INC-BUF-SZ > IF
        2DROP S" include file too large to save" CC-ERR FALSE EXIT
    THEN
    inp-buf inp-pos @ +
    inc-bufs inc-depth @ INC-BUF-SZ * +
    tmp-a @ CMOVE
    \ Save remaining length (in inc-len, overwrite with remaining)
    tmp-a @ inc-depth @ CELLS inc-len + !
    1 inc-depth +!
    \ Load new file
    R/O OPEN-FILE IF 2DROP S" cannot open include file" CC-ERR FALSE EXIT THEN
    >R
    inp-buf INP-SZ R@ READ-FILE IF R> CLOSE-FILE DROP DROP S" read error" CC-ERR FALSE EXIT THEN
    inp-len !
    R> CLOSE-FILE DROP
    0 inp-pos !
    1 cc-line !
    TRUE ;

\ Restore previous input state
: PP-POP-INPUT ( -- )
    inc-depth @ 0= IF EXIT THEN
    -1 inc-depth +!
    \ Restore saved input
    inc-depth @ CELLS inc-len + @ tmp-a !  \ saved remaining length
    inc-bufs inc-depth @ INC-BUF-SZ * +
    inp-buf tmp-a @ CMOVE
    tmp-a @ inp-len !
    0 inp-pos !
    inc-depth @ CELLS inc-line + @ cc-line ! ;

\ Resolve include path
\ For "file.h" — try current directory first, then include path
\ For <file.h> — try selfhost/v2/stage04/include/ then runtime/include/
: PP-RESOLVE-INCLUDE ( -- )
    \ tmp-d is already past "include" from PP-DIRECTIVE
    PP-SKIP-WS
    tmp-d @ pp-line-len @ >= IF S" missing include filename" CC-ERR EXIT THEN
    pp-line tmp-d @ + C@ DUP 34 = IF   \ "file.h"
        DROP 1 tmp-d +!
        0 inc-fname-len !
        BEGIN
            tmp-d @ pp-line-len @ < IF
                pp-line tmp-d @ + C@ 34 <>
            ELSE FALSE THEN
        WHILE
            pp-line tmp-d @ + C@ inc-fname inc-fname-len @ + C!
            1 inc-fname-len +! 1 tmp-d +!
        REPEAT
        inc-fname inc-fname-len @ PP-PUSH-INPUT DROP
    ELSE 60 = IF   \ <file.h>
        1 tmp-d +!
        0 inc-fname-len !
        \ Build path: selfhost/v2/stage04/include/<file>
        S" selfhost/v2/stage04/include/" inc-fname SWAP CMOVE
        28 inc-fname-len !
        BEGIN
            tmp-d @ pp-line-len @ < IF
                pp-line tmp-d @ + C@ 62 <>  \ not >
            ELSE FALSE THEN
        WHILE
            pp-line tmp-d @ + C@ inc-fname inc-fname-len @ + C!
            1 inc-fname-len +! 1 tmp-d +!
        REPEAT
        inc-fname inc-fname-len @ PP-PUSH-INPUT IF EXIT THEN
        \ Try runtime/include/
        S" runtime/include/" inc-fname SWAP CMOVE
        \ Recompute with runtime prefix
        0 inc-fname-len !
        S" runtime/include/" inc-fname SWAP DUP inc-fname-len ! CMOVE
        tmp-d @ pp-line-len @ < IF 1 tmp-d +! THEN  \ skip >
        \ Re-extract filename part
        0 tmp-d ! PP-SKIP-WS 1 tmp-d +!  \ skip <
        BEGIN
            tmp-d @ pp-line-len @ < IF
                pp-line tmp-d @ + C@ 62 <>
            ELSE FALSE THEN
        WHILE
            pp-line tmp-d @ + C@ inc-fname inc-fname-len @ + C!
            1 inc-fname-len +! 1 tmp-d +!
        REPEAT
        inc-fname inc-fname-len @ PP-PUSH-INPUT DROP
    ELSE DROP S" bad include syntax" CC-ERR THEN THEN ;

\ Handle #define directive
\ Note: tmp-d is already past "define" from PP-DIRECTIVE
: PP-DEFINE ( -- )
    PP-SKIP-WS
    PP-READ-IDENT
    pp-ident-len @ 0= IF S" missing macro name" CC-ERR EXIT THEN
    \ Check for function-like macro: name immediately followed by (
    tmp-d @ pp-line-len @ < IF
        pp-line tmp-d @ + C@ 40 = IF   \ '(' — function-like macro
            1 tmp-d +!
            \ Parse parameters
            0 tmp-e !  \ param count
            BEGIN
                PP-SKIP-WS
                tmp-d @ pp-line-len @ < IF
                    pp-line tmp-d @ + C@ 41 <>  \ not )
                ELSE FALSE THEN
            WHILE
                pp-line tmp-d @ + C@ 44 = IF 1 tmp-d +! PP-SKIP-WS THEN
                \ Read param name
                pp-line tmp-d @ + tmp-a !  \ param start
                0 tmp-b !
                BEGIN
                    tmp-d @ pp-line-len @ < IF
                        pp-line tmp-d @ + C@ DUP IS-ALNUM
                    ELSE FALSE THEN
                WHILE
                    DROP 1 tmp-d +! 1 tmp-b +!
                REPEAT
                tmp-d @ pp-line-len @ < IF DROP THEN
                \ Store param
                tmp-e @ MAX-MPARAM < IF
                    tmp-a @ inp-buf - mname-ptr @ + tmp-e @ CELLS mparam-off + !
                    \ Actually store name in separate buffer
                    mname-ptr @ tmp-e @ CELLS mparam-off + !
                    tmp-a @ mname-buf mname-ptr @ + tmp-b @ CMOVE
                    tmp-b @ mname-ptr +!
                    tmp-b @ tmp-e @ CELLS mparam-len + !
                THEN
                1 tmp-e +!
                PP-SKIP-WS
                tmp-d @ pp-line-len @ < IF
                    pp-line tmp-d @ + C@ 44 = IF 1 tmp-d +! THEN
                THEN
            REPEAT
            tmp-d @ pp-line-len @ < IF 1 tmp-d +! THEN  \ skip )
            \ Store params persistently for expansion
            pinfo-cnt @ tmp-c !  \ save base index
            tmp-e @ 0 ?DO
                pname-sptr @ pinfo-cnt @ I + CELLS pinfo-off + !
                mname-buf I CELLS mparam-off + @ +
                pname-store pname-sptr @ +
                I CELLS mparam-len + @ CMOVE
                I CELLS mparam-len + @ pinfo-cnt @ I + CELLS pinfo-len + !
                I CELLS mparam-len + @ pname-sptr +!
            LOOP
            tmp-e @ pinfo-cnt +!
            PP-SKIP-WS
            \ Rest of line is macro body
            pp-line tmp-d @ + pp-line-len @ tmp-d @ -
            \ Store as macro with param count
            mac-cnt @ MAX-MACRO >= IF 2DROP EXIT THEN
            mac-cnt @ >R
            DUP R@ CELLS mac-vlen + !
            mval-ptr @ R@ CELLS mac-val + !
            mval-buf mval-ptr @ + SWAP CMOVE
            R@ CELLS mac-vlen + @ mval-ptr +!
            pp-ident-len @ R@ CELLS mac-nlen + !
            mname-ptr @ R@ CELLS mac-name + !
            pp-ident mname-buf mname-ptr @ + pp-ident-len @ CMOVE
            pp-ident-len @ mname-ptr +!
            tmp-e @ R@ CELLS mac-npar + !
            tmp-c @ R@ CELLS mac-pbase + !
            R> 1+ mac-cnt !
            EXIT
        THEN
    THEN
    \ Object-like macro: rest of line is the value
    PP-SKIP-WS
    pp-line tmp-d @ + pp-line-len @ tmp-d @ -
    pp-ident pp-ident-len @ 2SWAP MAC-ADD-OBJ ;

\ Handle #ifdef / #ifndef
: PP-IFDEF ( -- )
    0 tmp-d ! PP-SKIP-WS PP-READ-IDENT
    pp-ident pp-ident-len @ MAC-FIND -1 <> ;

\ ============================================================
\ Preprocessor Expression Evaluator (for #if / #elif)
\ Operates on pp-line at position tmp-d
\ ============================================================

DEFER PP-EVAL-EXPR

: PP-EVAL-PRIMARY ( -- n )
    PP-SKIP-WS
    tmp-d @ pp-line-len @ >= IF 0 EXIT THEN
    pp-line tmp-d @ + C@
    DUP 40 = IF  \ '('
        DROP 1 tmp-d +!
        PP-EVAL-EXPR
        PP-SKIP-WS
        tmp-d @ pp-line-len @ < IF
            pp-line tmp-d @ + C@ 41 = IF 1 tmp-d +! THEN
        THEN EXIT
    THEN
    DUP IS-ALPHA OVER 95 = OR IF
        DROP PP-READ-IDENT
        pp-ident pp-ident-len @ S" defined" STR= IF
            PP-SKIP-WS
            0 ( paren-flag )
            tmp-d @ pp-line-len @ < IF
                pp-line tmp-d @ + C@ 40 = IF DROP 1 1 tmp-d +! THEN
            THEN
            PP-SKIP-WS PP-READ-IDENT
            pp-ident pp-ident-len @ MAC-FIND -1 <> IF 1 ELSE 0 THEN
            SWAP IF  \ had paren — skip )
                PP-SKIP-WS
                tmp-d @ pp-line-len @ < IF
                    pp-line tmp-d @ + C@ 41 = IF 1 tmp-d +! THEN
                THEN
            THEN EXIT
        THEN
        \ Look up identifier as macro — expand to its value
        pp-ident pp-ident-len @ MAC-FIND DUP -1 <> IF
            DUP CELLS mac-vlen + @ 0> IF
                DUP CELLS mac-val + @ mval-buf +
                OVER CELLS mac-vlen + @ ( midx addr len )
                \ Try to parse as decimal number
                0 SWAP 0 ?DO ( midx addr accum )
                    OVER I + C@ DUP IS-DIGIT IF
                        48 - SWAP 10 * + ( midx addr accum' )
                    ELSE DROP LEAVE THEN
                LOOP
                NIP NIP ( n )
            ELSE
                DROP 1 \ defined with empty value → treat as 1
            THEN
        ELSE
            DROP 0 \ truly undefined → 0
        THEN EXIT
    THEN
    DUP IS-DIGIT IF
        DROP
        \ Check for hex: 0x
        pp-line tmp-d @ + C@ 48 =
        tmp-d @ 1+ pp-line-len @ < AND IF
            pp-line tmp-d @ 1+ + C@ DUP 120 = SWAP 88 = OR IF
                \ Hex number
                2 tmp-d +!
                0
                BEGIN
                    tmp-d @ pp-line-len @ < IF
                        pp-line tmp-d @ + C@ DUP IS-DIGIT IF DROP TRUE
                        ELSE DUP 65 >= OVER 70 <= AND IF DROP TRUE
                        ELSE DUP 97 >= SWAP 102 <= AND
                        THEN THEN
                    ELSE FALSE THEN
                WHILE
                    16 *
                    pp-line tmp-d @ + C@ DUP IS-DIGIT IF 48 -
                    ELSE DUP 65 >= OVER 70 <= AND IF 55 -
                    ELSE 87 - THEN THEN
                    +
                    1 tmp-d +!
                REPEAT EXIT
            THEN
        THEN
        \ Decimal number
        0
        BEGIN
            tmp-d @ pp-line-len @ < IF
                pp-line tmp-d @ + C@ IS-DIGIT
            ELSE FALSE THEN
        WHILE
            10 * pp-line tmp-d @ + C@ 48 - +
            1 tmp-d +!
        REPEAT EXIT
    THEN
    DROP 0 ;

: PP-EVAL-UNARY ( -- n )
    PP-SKIP-WS
    tmp-d @ pp-line-len @ < IF
        pp-line tmp-d @ + C@ 33 = IF  \ '!'
            1 tmp-d +!
            PP-EVAL-UNARY 0= IF 1 ELSE 0 THEN EXIT
        THEN
        pp-line tmp-d @ + C@ 126 = IF  \ '~'
            1 tmp-d +!
            PP-EVAL-UNARY INVERT EXIT
        THEN
    THEN
    PP-EVAL-PRIMARY ;

: PP-EVAL-CMP ( -- n )
    PP-EVAL-UNARY
    BEGIN
        PP-SKIP-WS
        tmp-d @ 1+ pp-line-len @ < IF
            pp-line tmp-d @ + C@ 60 =      \ '<'
            pp-line tmp-d @ 1+ + C@ 61 = AND IF  \ '<='
                2 tmp-d +!
                PP-EVAL-UNARY <= IF 1 ELSE 0 THEN TRUE
            ELSE pp-line tmp-d @ + C@ 62 =      \ '>'
                pp-line tmp-d @ 1+ + C@ 61 = AND IF  \ '>='
                    2 tmp-d +!
                    PP-EVAL-UNARY >= IF 1 ELSE 0 THEN TRUE
                ELSE FALSE THEN
            THEN
        ELSE FALSE THEN
    WHILE REPEAT ;

: PP-EVAL-EQ ( -- n )
    PP-EVAL-CMP
    BEGIN
        PP-SKIP-WS
        tmp-d @ 1+ pp-line-len @ < IF
            pp-line tmp-d @ + C@ 61 =       \ '='
            pp-line tmp-d @ 1+ + C@ 61 = AND IF  \ '=='
                2 tmp-d +!
                PP-EVAL-CMP = IF 1 ELSE 0 THEN TRUE
            ELSE pp-line tmp-d @ + C@ 33 =       \ '!'
                pp-line tmp-d @ 1+ + C@ 61 = AND IF  \ '!='
                    2 tmp-d +!
                    PP-EVAL-CMP <> IF 1 ELSE 0 THEN TRUE
                ELSE FALSE THEN
            THEN
        ELSE FALSE THEN
    WHILE REPEAT ;

: PP-EVAL-AND ( -- n )
    PP-EVAL-EQ
    BEGIN
        PP-SKIP-WS
        tmp-d @ 1+ pp-line-len @ < IF
            pp-line tmp-d @ + C@ 38 =       \ '&'
            pp-line tmp-d @ 1+ + C@ 38 = AND  \ '&&'
        ELSE FALSE THEN
    WHILE
        2 tmp-d +!
        PP-EVAL-EQ
        SWAP 0<> SWAP 0<> AND IF 1 ELSE 0 THEN
    REPEAT ;

: PP-EVAL-OR ( -- n )
    PP-EVAL-AND
    BEGIN
        PP-SKIP-WS
        tmp-d @ 1+ pp-line-len @ < IF
            pp-line tmp-d @ + C@ 124 =       \ '|'
            pp-line tmp-d @ 1+ + C@ 124 = AND  \ '||'
        ELSE FALSE THEN
    WHILE
        2 tmp-d +!
        PP-EVAL-AND
        SWAP 0<> SWAP 0<> OR IF 1 ELSE 0 THEN
    REPEAT ;

:NONAME PP-EVAL-OR ; IS PP-EVAL-EXPR

\ Handle preprocessor directive (# already consumed by lexer)
: PP-DIRECTIVE ( -- )
    PP-READ-LINE
    0 tmp-d !
    PP-SKIP-WS
    PP-READ-IDENT
    \ Check directive name
    pp-ident pp-ident-len @ S" include" STR= IF
        pp-skip @ 0= IF PP-RESOLVE-INCLUDE THEN EXIT
    THEN
    pp-ident pp-ident-len @ S" define" STR= IF
        pp-skip @ 0= IF PP-DEFINE THEN EXIT
    THEN
    pp-ident pp-ident-len @ S" undef" STR= IF
        pp-skip @ 0= IF
            PP-SKIP-WS PP-READ-IDENT
            pp-ident pp-ident-len @ MAC-UNDEF
        THEN EXIT
    THEN
    pp-ident pp-ident-len @ S" ifdef" STR= IF
        1 pp-nest +!
        0 pp-nest @ CELLS pp-found + !
        pp-skip @ 0<> IF EXIT THEN
        PP-SKIP-WS PP-READ-IDENT
        pp-ident pp-ident-len @ MAC-FIND -1 = IF
            pp-nest @ pp-skip !
        ELSE
            1 pp-nest @ CELLS pp-found + !
        THEN EXIT
    THEN
    pp-ident pp-ident-len @ S" ifndef" STR= IF
        1 pp-nest +!
        0 pp-nest @ CELLS pp-found + !
        pp-skip @ 0<> IF EXIT THEN
        PP-SKIP-WS PP-READ-IDENT
        pp-ident pp-ident-len @ MAC-FIND -1 <> IF
            pp-nest @ pp-skip !
        ELSE
            1 pp-nest @ CELLS pp-found + !
        THEN EXIT
    THEN
    pp-ident pp-ident-len @ S" if" STR= IF
        1 pp-nest +!
        0 pp-nest @ CELLS pp-found + !
        pp-skip @ 0<> IF EXIT THEN
        PP-EVAL-EXPR 0= IF
            pp-nest @ pp-skip !
        ELSE
            1 pp-nest @ CELLS pp-found + !
        THEN EXIT
    THEN
    pp-ident pp-ident-len @ S" elif" STR= IF
        pp-skip @ 0= IF
            \ Previous branch was visible — mark found and start skipping
            1 pp-nest @ CELLS pp-found + !
            pp-nest @ pp-skip !
        ELSE
            pp-skip @ pp-nest @ = IF
                pp-nest @ CELLS pp-found + @ IF
                    \ Already found match — stay skipped
                ELSE
                    PP-EVAL-EXPR 0<> IF
                        0 pp-skip !
                        1 pp-nest @ CELLS pp-found + !
                    THEN
                THEN
            THEN
        THEN EXIT
    THEN
    pp-ident pp-ident-len @ S" else" STR= IF
        pp-skip @ 0= IF
            pp-nest @ pp-skip !
        ELSE
            pp-skip @ pp-nest @ = IF
                pp-nest @ CELLS pp-found + @ 0= IF
                    0 pp-skip !
                THEN
            THEN
        THEN EXIT
    THEN
    pp-ident pp-ident-len @ S" endif" STR= IF
        pp-skip @ pp-nest @ = IF 0 pp-skip ! THEN
        0 pp-nest @ CELLS pp-found + !
        pp-nest @ 0> IF -1 pp-nest +! THEN
        EXIT
    THEN
    pp-ident pp-ident-len @ S" pragma" STR= IF EXIT THEN  \ ignore
    pp-ident pp-ident-len @ S" error" STR= IF
        pp-skip @ 0= IF S" #error directive" CC-ERR THEN EXIT
    THEN
    pp-ident pp-ident-len @ S" line" STR= IF EXIT THEN  \ ignore
    \ Unknown directive — ignore silently
;

\ Macro-expanded token reading
\ After lexing an identifier, check if it's a macro and expand
CREATE mexp-scratch 4096 ALLOT
VARIABLE mexp-scratch-len

\ Collect function-like macro arguments from input stream
\ Called after '(' has been consumed. Reads until matching ')'.
: COLLECT-MACRO-ARGS ( -- )
    0 arg-cnt ! 0 arg-ptr !
    0 arg-depth !
    arg-ptr @ arg-cnt @ CELLS arg-off + !
    BEGIN
        CC-PEEK DUP -1 <>
    WHILE
        arg-ch !
        arg-ch @ 40 = IF  \ (
            1 arg-depth +!
            arg-ch @ arg-text arg-ptr @ + C! 1 arg-ptr +! CC-SKIP
        ELSE arg-ch @ 41 = IF  \ )
            arg-depth @ 0= IF
                CC-SKIP
                \ Finalize last arg
                arg-ptr @ arg-cnt @ CELLS arg-off + @ -
                arg-cnt @ CELLS arg-alen + !
                1 arg-cnt +! EXIT
            ELSE
                -1 arg-depth +!
                arg-ch @ arg-text arg-ptr @ + C! 1 arg-ptr +! CC-SKIP
            THEN
        ELSE arg-ch @ 44 = arg-depth @ 0= AND IF  \ comma at depth 0
            CC-SKIP
            \ Finalize current arg
            arg-ptr @ arg-cnt @ CELLS arg-off + @ -
            arg-cnt @ CELLS arg-alen + !
            1 arg-cnt +!
            \ Start next arg
            arg-ptr @ arg-cnt @ CELLS arg-off + !
        ELSE
            arg-ch @ arg-text arg-ptr @ + C! 1 arg-ptr +! CC-SKIP
        THEN THEN THEN
    REPEAT DROP ;

\ Match identifier against macro parameters
\ Uses exp-npar and exp-pbase (set before calling)
: MATCH-PARAM ( addr u -- pidx | -1 )
    exp-npar @ 0 ?DO
        2DUP
        exp-pbase @ I + CELLS pinfo-off + @ pname-store +
        exp-pbase @ I + CELLS pinfo-len + @
        COMPARE 0= IF 2DROP I UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

\ Copy argument text to mexp-scratch, trimming leading whitespace
: COPY-ARG ( pidx -- )
    DUP CELLS arg-alen + @ SWAP CELLS arg-off + @ arg-text +
    \ Stack: ( alen arg-addr )
    SWAP
    \ Trim leading whitespace
    BEGIN DUP 0> IF OVER C@ DUP 32 = SWAP 9 = OR ELSE FALSE THEN
    WHILE 1- SWAP 1+ SWAP REPEAT
    \ Stack: ( arg-addr alen )
    DUP >R
    mexp-scratch mexp-scratch-len @ + SWAP CMOVE
    R> mexp-scratch-len +! ;

\ Expand macro body with parameter substitution into mexp-scratch
: EXPAND-MACRO-BODY ( macro-idx -- )
    DUP CELLS mac-val + @ mval-buf + exp-body !
    DUP CELLS mac-vlen + @ exp-blen !
    DUP CELLS mac-npar + @ exp-npar !
    CELLS mac-pbase + @ exp-pbase !
    0 mexp-scratch-len !
    0 exp-off !
    BEGIN exp-off @ exp-blen @ < WHILE
        exp-body @ exp-off @ + C@ DUP IS-ALPHA OVER 95 = OR IF
            DROP
            \ Read identifier from body
            0 exp-idlen !
            BEGIN
                exp-off @ exp-blen @ < IF
                    exp-body @ exp-off @ + C@ DUP IS-ALNUM SWAP 95 = OR
                ELSE FALSE THEN
            WHILE
                exp-body @ exp-off @ + C@ exp-idbuf exp-idlen @ + C!
                1 exp-idlen +! 1 exp-off +!
            REPEAT
            \ Match against params
            exp-idbuf exp-idlen @ MATCH-PARAM
            DUP -1 = IF
                DROP
                \ Not a param — copy identifier to output
                exp-idbuf mexp-scratch mexp-scratch-len @ + exp-idlen @ CMOVE
                exp-idlen @ mexp-scratch-len +!
            ELSE
                \ Param match — copy argument text
                COPY-ARG
            THEN
        ELSE
            mexp-scratch mexp-scratch-len @ + C!
            1 mexp-scratch-len +! 1 exp-off +!
        THEN
    REPEAT ;

\ Inject mexp-scratch content into input buffer for re-lexing
\ Uses overlap-safe MOVE within inp-buf (old code overflowed mexp-scratch)
: INJECT-EXPANSION ( -- )
    32 mexp-scratch mexp-scratch-len @ + C!
    1 mexp-scratch-len +!
    inp-len @ inp-pos @ - tmp-a !      \ remaining bytes after macro
    \ Move remaining input within inp-buf to just after expansion position
    inp-buf inp-pos @ +                 \ src: remaining data in inp-buf
    inp-buf mexp-scratch-len @ +        \ dst: after expansion text
    tmp-a @                             \ count
    MOVE                                \ overlap-safe (memmove)
    \ Copy expansion text from mexp-scratch to start of inp-buf
    mexp-scratch inp-buf mexp-scratch-len @ CMOVE
    \ Update input state
    mexp-scratch-len @ tmp-a @ + inp-len !
    0 inp-pos ! ;

: TRY-EXPAND-MACRO ( -- flag )  \ flag = true if expanded
    tok-type @ TK-IDENT <> IF FALSE EXIT THEN
    tok-buf tok-len @ MAC-FIND DUP -1 = IF DROP FALSE EXIT THEN
    DUP CELLS mac-nlen + @ 0= IF DROP FALSE EXIT THEN  \ deleted
    DUP CELLS mac-npar + @ -1 = IF
        \ Object-like macro: inject expansion text into input
        DUP CELLS mac-vlen + @ 0= IF DROP
            \ Empty macro — just re-lex next token
            TRUE EXIT
        THEN
        DUP CELLS mac-val + @ mval-buf + SWAP CELLS mac-vlen + @
        0 mexp-scratch-len !
        2DUP mexp-scratch SWAP CMOVE
        DUP mexp-scratch-len !
        2DROP
        INJECT-EXPANSION
        TRUE EXIT
    ELSE
        \ Function-like macro — check for (
        DUP >R  \ save macro index
        SKIP-WS
        CC-PEEK 40 <> IF R> DROP FALSE EXIT THEN  \ not (, not a macro call
        CC-SKIP  \ skip (
        COLLECT-MACRO-ARGS
        R@ EXPAND-MACRO-BODY
        INJECT-EXPANSION
        R> DROP
        TRUE EXIT
    THEN ;

\ Enhanced NEXT-TOKEN that handles preprocessor and macros
: CC-TOKEN ( -- )
    inp-pos @ tok-start !   \ remember position before lexing
    BEGIN
        NEXT-TOKEN
        \ Handle preprocessor directives
        tok-type @ TK-PUNCT = tok-val @ P-HASH = AND IF
            \ Check if at start of line (approximately)
            PP-DIRECTIVE
            pp-skip @ 0<> IF
                \ We're in a skipped region, keep consuming
                TRUE
            ELSE TRUE THEN  \ loop to get next real token
        ELSE
            pp-skip @ 0<> IF
                tok-type @ TK-EOF = IF FALSE ELSE TRUE THEN
            ELSE
                \ Try macro expansion
                TRY-EXPAND-MACRO IF TRUE ELSE FALSE THEN
            THEN
        THEN
    WHILE REPEAT
    \ Handle end of include file
    tok-type @ TK-EOF = inc-depth @ 0> AND IF
        PP-POP-INPUT
        CC-TOKEN  \ recurse to get token from parent file
    THEN ;

\ ============================================================
\ SECTION 7: Type Parsing
\ ============================================================

\ Check if current token starts a type specifier
: IS-TYPE-START ( -- f )
    tok-type @ TK-KW = IF
        tok-val @ DUP KW-INT = OVER KW-CHAR = OR OVER KW-VOID = OR
        OVER KW-UNSIGNED = OR OVER KW-SIGNED = OR OVER KW-LONG = OR
        OVER KW-SHORT = OR OVER KW-STRUCT = OR OVER KW-ENUM = OR
        OVER KW-CONST = OR OVER KW-STATIC = OR OVER KW-EXTERN = OR
        OVER KW-INLINE = OR OVER KW-BOOL = OR
        NIP EXIT
    THEN
    tok-type @ TK-IDENT = IF
        tok-buf tok-len @ TD-FIND -1 <> EXIT
    THEN
    FALSE ;

\ Parse base type specifier — returns type word
\ Handles: int, char, void, unsigned int, long, short, struct name, enum name
\ Also handles typedef names
: PARSE-BASE-TYPE ( -- type flags )
    \ flags: bit 0=static, bit 1=extern, bit 2=const, bit 3=inline
    0 tmp-c !  \ flags
    0 tmp-type !  \ base type built up
    0 tmp-a !   \ saw-type flag
    BEGIN
        tok-type @ TK-KW = IF
            tok-val @ CASE
                KW-STATIC   OF 1 tmp-c @ OR tmp-c ! CC-TOKEN TRUE ENDOF
                KW-EXTERN   OF 2 tmp-c @ OR tmp-c ! CC-TOKEN TRUE ENDOF
                KW-CONST    OF 4 tmp-c @ OR tmp-c ! CC-TOKEN TRUE ENDOF
                KW-INLINE   OF 8 tmp-c @ OR tmp-c ! CC-TOKEN TRUE ENDOF
                KW-SIGNED   OF CC-TOKEN TRUE ENDOF
                KW-UNSIGNED OF UNSIGNED-BIT tmp-type @ OR tmp-type ! CC-TOKEN TRUE ENDOF
                KW-INT      OF TY-INT tmp-type @ MASK-BYTE INVERT AND OR tmp-type ! 1 tmp-a ! CC-TOKEN TRUE ENDOF
                KW-CHAR     OF TY-CHAR tmp-type @ MASK-BYTE INVERT AND OR tmp-type ! 1 tmp-a ! CC-TOKEN TRUE ENDOF
                KW-VOID     OF TY-VOID tmp-type @ MASK-BYTE INVERT AND OR tmp-type ! 1 tmp-a ! CC-TOKEN TRUE ENDOF
                KW-LONG     OF TY-LONG tmp-type @ MASK-BYTE INVERT AND OR tmp-type ! 1 tmp-a ! CC-TOKEN TRUE ENDOF
                KW-SHORT    OF TY-SHORT tmp-type @ MASK-BYTE INVERT AND OR tmp-type ! 1 tmp-a ! CC-TOKEN TRUE ENDOF
                KW-BOOL     OF TY-INT tmp-type @ MASK-BYTE INVERT AND OR tmp-type ! 1 tmp-a ! CC-TOKEN TRUE ENDOF
                KW-STRUCT   OF
                    CC-TOKEN  \ named: struct Name ... ; anonymous: struct { ... }
                    tok-type @ TK-IDENT = IF
                        tok-buf tok-len @ ST-FIND DUP -1 = IF
                            DROP
                            \ Forward reference — create empty struct
                            tok-buf tok-len @ ST-ADD
                        THEN
                        CC-TOKEN  \ consume struct name
                    ELSE
                        \ Anonymous struct type (e.g. typedef struct { ... } T;)
                        tok-buf 0 ST-ADD
                    THEN
                    9 LSHIFT TY-STRUCT OR
                    tmp-type @ MASK-BYTE INVERT AND OR tmp-type !
                    1 tmp-a !
                    \ Check for struct definition { ... }
                    tok-type @ TK-PUNCT = tok-val @ P-LBRACE = AND IF
                        \ Body present — tok stays as {, caller will handle
                    THEN
                    TRUE
                ENDOF
                KW-ENUM     OF
                    CC-TOKEN
                    \ enum name or anonymous
                    tok-type @ TK-IDENT = IF
                        CC-TOKEN  \ skip name
                    THEN
                    \ If tok is '{', caller handles enum body directly.
                    TY-INT tmp-type @ MASK-BYTE INVERT AND OR tmp-type !
                    1 tmp-a !
                    TRUE
                ENDOF
                FALSE SWAP
            ENDCASE
        ELSE
            tok-type @ TK-IDENT = tmp-a @ 0= AND IF
                \ Check if it's a typedef name
                tok-buf tok-len @ TD-FIND DUP -1 <> IF
                    CELLS td-type + @ tmp-type !
                    1 tmp-a !
                    CC-TOKEN TRUE
                ELSE DROP FALSE THEN
            ELSE FALSE THEN
        THEN
    WHILE REPEAT
    \ If no explicit type was specified but we got unsigned/signed, default to int
    tmp-a @ 0= tmp-type @ 0<> AND IF
        TY-INT tmp-type @ OR tmp-type !
    THEN
    tmp-type @ tmp-c @ ;

\ Parse pointer decorations: *, const
: PARSE-POINTERS ( type -- type' )
    BEGIN
        tok-type @ TK-PUNCT = tok-val @ P-STAR = AND
    WHILE
        TYPE-ADD-PTR
        CC-TOKEN
        \ Skip const after *
        tok-type @ TK-KW = tok-val @ KW-CONST = AND IF CC-TOKEN THEN
    REPEAT ;

\ Parse declarator: pointers + name + array dimensions
\ Returns: type name-addr name-len
: PARSE-DECLARATOR ( base-type -- type name-addr name-len )
    PARSE-POINTERS
    \ Check for function pointer: (*name)(params...)
    tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF
        CC-TOKEN  \ skip (
        tok-type @ TK-PUNCT = tok-val @ P-STAR = AND IF
            CC-TOKEN  \ skip *
            \ Get name
            tok-type @ TK-IDENT = IF
                tok-buf func-name-buf tok-len @ CMOVE
                tok-len @ tmp-name-off !
                CC-TOKEN  \ skip name
                P-RPAREN EXPECT-PUNCT CC-TOKEN  \ skip )
                \ Skip parameter list (...)
                tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF
                    CC-TOKEN  \ skip (
                    0  \ paren depth
                    BEGIN
                        tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND IF
                            DUP 0= IF DROP CC-TOKEN TRUE ELSE 1- FALSE THEN
                        ELSE
                            tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF 1+ THEN
                            CC-TOKEN FALSE
                        THEN
                    UNTIL
                THEN
                \ Treat as pointer (void*)
                DROP TY-INT TYPE-ADD-PTR
                func-name-buf tmp-name-off @
                EXIT
            ELSE
                \ (*)(params) — abstract function pointer
                P-RPAREN EXPECT-PUNCT CC-TOKEN
                tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF
                    CC-TOKEN
                    0 BEGIN
                        tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND IF
                            DUP 0= IF DROP CC-TOKEN TRUE ELSE 1- FALSE THEN
                        ELSE
                            tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF 1+ THEN
                            CC-TOKEN FALSE
                        THEN
                    UNTIL
                THEN
                DROP TY-INT TYPE-ADD-PTR
                0 0 EXIT
            THEN
        ELSE
            \ Not a function pointer — backtrack
            \ This is a parenthesized declarator, e.g. int (x)
            \ For simplicity, just try to parse name inside parens
            tok-type @ TK-IDENT = IF
                tok-buf func-name-buf tok-len @ CMOVE
                tok-len @ tmp-name-off !
                CC-TOKEN P-RPAREN EXPECT-PUNCT CC-TOKEN
                func-name-buf tmp-name-off @ EXIT
            ELSE
                \ Couldn't parse — return no name
                0 0 EXIT
            THEN
        THEN
    THEN
    \ Get name
    tok-type @ TK-IDENT = IF
        tok-buf tok-len @ tmp-name-off ! tmp-name-len !
        \ Copy name to a temp area (tok-buf will be overwritten)
        tok-buf func-name-buf tok-len @ CMOVE  \ reuse func-name-buf as temp
        CC-TOKEN
        \ Check for array dimensions [N]
        tok-type @ TK-PUNCT = tok-val @ P-LBRACK = AND IF
            CC-TOKEN
            tok-type @ TK-NUM = IF
                tok-val @ TYPE-SET-ARRAY
                CC-TOKEN  \ skip number
            ELSE
                \ [] without size — treat as pointer
                TYPE-ADD-PTR
            THEN
            P-RBRACK EXPECT-PUNCT
            CC-TOKEN
        THEN
        func-name-buf tmp-name-off @
    ELSE
        \ No name (e.g., abstract declarator in cast or parameter)
        0 0
    THEN ;

\ ============================================================
\ SECTION 8: Expression Parser (Precedence Climbing)
\ ============================================================

\ Forward declarations (Forth can't forward-declare, so we use deferred words)
DEFER PARSE-EXPR
DEFER PARSE-ASSIGN-EXPR
DEFER PARSE-STATEMENT
DEFER PARSE-COMPOUND
DEFER PARSE-DECL-OR-STMT

\ --- Primary expression ---
\ Handles: numbers, strings, identifiers, (expr), sizeof

\ Helper: emit address of a variable (for lvalue operations)
\ Sets is-lvalue = 1, expr-type = variable's type
: EMIT-VAR-ADDR-LOCAL ( idx -- )
    DUP CELLS lsym-type + @ expr-type !
    DUP CELLS lsym-type + @ TYPE-IS-ARRAY IF
        \ Array: address is the value itself
        CELLS lsym-off + @
        EMIT-INDENT S" addi r1, r30, " OUT-STR OUT-SNUM OUT-NL
        0 is-lvalue !
    ELSE
        CELLS lsym-off + @
        EMIT-INDENT S" addi r1, r30, " OUT-STR OUT-SNUM OUT-NL
        1 is-lvalue !
    THEN ;

: EMIT-VAR-ADDR-GLOBAL ( idx -- )
    DUP CELLS gsym-type + @ expr-type !
    DUP CELLS gsym-type + @ tmp-type !
    DUP CELLS gsym-name + @ gsym-nbuf + tmp-a !
    CELLS gsym-nlen + @ tmp-b !
    tmp-type @ TYPE-IS-ARRAY IF
        tmp-a @ tmp-b @ EMIT-GLOBAL-ADDR
        S" addi r1, r2, 0" EMIT-INSN
        0 is-lvalue !
    ELSE
        tmp-a @ tmp-b @ EMIT-GLOBAL-ADDR
        S" addi r1, r2, 0" EMIT-INSN
        1 is-lvalue !
    THEN ;

\ Load value from lvalue (if is-lvalue is set, dereference r1)
: LVAL-TO-RVAL ( -- )
    is-lvalue @ 0= IF EXIT THEN
    expr-type @ EMIT-LOAD-DEREF
    0 is-lvalue ! ;

\ Dedicated buffer for function call names — with nesting stack
CREATE call-name 256 ALLOT
VARIABLE call-name-len
CREATE call-stk 576 ALLOT   \ 8 levels × 72 bytes (64 name + 4 len + 4 argc)
VARIABLE call-sp
VARIABLE call-argc
VARIABLE index-base-type
0 call-sp !

: CALL-SAVE ( -- )
    call-sp @ 72 * call-stk +    \ dest slot
    call-name SWAP 64 CMOVE      \ save name (from call-name to slot)
    call-sp @ 72 * call-stk + 64 +
    call-name-len @ SWAP !       \ save length
    call-sp @ 72 * call-stk + 68 +
    call-argc @ SWAP !           \ save arg count
    1 call-sp +! ;

: CALL-RESTORE ( -- )
    -1 call-sp +!
    call-sp @ 72 * call-stk +
    DUP call-name 64 CMOVE       \ CMOVE( from=slot, to=call-name, count=64 )
    DUP 64 + @ call-name-len !
    68 + @ call-argc ! ;

: PARSE-CALL ( -- )
    \ tok-buf has function name, save it
    tok-buf call-name tok-len @ CMOVE
    tok-len @ call-name-len !
    CC-TOKEN  \ skip (
    0 call-argc !
    \ Check for empty args
    tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND 0= IF
        BEGIN
            \ Push previous args to expression stack
            call-argc @ 0> IF EMIT-PUSH-R1 THEN
            \ Save call state before arg evaluation (may nest)
            CALL-SAVE
            PARSE-ASSIGN-EXPR
            LVAL-TO-RVAL
            CALL-RESTORE
            1 call-argc +!
            tok-type @ TK-PUNCT = tok-val @ P-COMMA = AND
        WHILE
            CC-TOKEN  \ skip comma
        REPEAT
    THEN
    P-RPAREN EXPECT-PUNCT
    CC-TOKEN
    \ Assign args to registers
    call-argc @ CASE
        0 OF ENDOF
        1 OF S" addi r3, r1, 0" EMIT-INSN ENDOF
        2 OF S" addi r4, r1, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r3, r2, 0" EMIT-INSN ENDOF
        3 OF S" addi r5, r1, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r4, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r3, r2, 0" EMIT-INSN ENDOF
        4 OF S" addi r6, r1, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r5, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r4, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r3, r2, 0" EMIT-INSN ENDOF
        5 OF S" addi r7, r1, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r6, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r5, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r4, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r3, r2, 0" EMIT-INSN ENDOF
        6 OF S" addi r8, r1, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r7, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r6, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r5, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r4, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r3, r2, 0" EMIT-INSN ENDOF
        7 OF S" addi r9, r1, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r8, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r7, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r6, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r5, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r4, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r3, r2, 0" EMIT-INSN ENDOF
        8 OF S" addi r10, r1, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r9, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r8, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r7, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r6, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r5, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r4, r2, 0" EMIT-INSN
             EMIT-POP-R2 S" addi r3, r2, 0" EMIT-INSN ENDOF
        S" too many arguments (max 8 in registers)" CC-WARN
    ENDCASE
    call-name call-name-len @ EMIT-CALL
    \ Look up return type
    call-name call-name-len @ GSYM-FIND DUP -1 <> IF
        CELLS gsym-type + @ expr-type !
    ELSE DROP TY-INT expr-type ! THEN
    0 is-lvalue ! ;

\ --- sizeof handling ---
: PARSE-SIZEOF ( -- )
    CC-TOKEN  \ skip 'sizeof'
    tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF
        CC-TOKEN  \ skip (
        \ Check if it's a type or expression
        IS-TYPE-START IF
            PARSE-BASE-TYPE DROP  \ discard flags
            PARSE-POINTERS
            \ Handle array
            tok-type @ TK-PUNCT = tok-val @ P-LBRACK = AND IF
                CC-TOKEN
                tok-type @ TK-NUM = IF
                    tok-val @ TYPE-SET-ARRAY
                    CC-TOKEN
                THEN
                tok-type @ TK-PUNCT = tok-val @ P-RBRACK = AND IF CC-TOKEN THEN
            THEN
            TYPE-SIZE
            P-RPAREN EXPECT-PUNCT CC-TOKEN
        ELSE
            \ sizeof(expr) — parse expr, compute type size
            PARSE-ASSIGN-EXPR
            LVAL-TO-RVAL
            expr-type @ TYPE-SIZE
            P-RPAREN EXPECT-PUNCT CC-TOKEN
        THEN
        EMIT-LI-R1
        TY-INT expr-type !
        0 is-lvalue !
    ELSE
        \ sizeof expr (without parens)
        PARSE-ASSIGN-EXPR
        LVAL-TO-RVAL
        expr-type @ TYPE-SIZE
        EMIT-LI-R1
        TY-INT expr-type !
        0 is-lvalue !
    THEN ;

\ Check if current token could be a cast: (type)
: IS-CAST ( -- f )
    tok-type @ TK-PUNCT <> tok-val @ P-LPAREN <> OR IF FALSE EXIT THEN
    \ Save state and peek
    inp-pos @ tmp-a !
    cc-line @ tmp-b !
    CC-TOKEN  \ token after (
    IS-TYPE-START tmp-c !
    \ Restore — we need to unget two tokens effectively
    \ This is tricky. Use the unget mechanism.
    UNGET-TOKEN
    TK-PUNCT tok-type ! P-LPAREN tok-val !  \ restore ( as current
    tmp-a @ inp-pos !
    tmp-b @ cc-line !
    tmp-c @ ;

\ Parse primary expression
: PARSE-PRIMARY ( -- )
    tok-type @ CASE
        TK-NUM OF
            tok-val @ EMIT-LI-R1
            TY-INT expr-type !
            0 is-lvalue !
            CC-TOKEN
        ENDOF
        TK-STR OF
            \ String literal — emit la to string label
            tok-val @ tmp-a !  \ string index
            EMIT-INDENT S" la r1, .Lstr_" OUT-STR tmp-a @ OUT-NUM OUT-NL
            \ Type is char*
            TY-CHAR 1 0 MAKE-TYPE expr-type !
            0 is-lvalue !
            CC-TOKEN
        ENDOF
        TK-IDENT OF
            \ Look up identifier — returns ( idx kind )
            tok-buf tok-len @ LOOKUP-NAME
            CASE
                0 OF  \ local variable
                    EMIT-VAR-ADDR-LOCAL
                ENDOF
                1 OF  \ global symbol
                    DUP CELLS gsym-kind + @ 1 = IF  \ function
                        \ Check if followed by ( — function call
                        DUP CELLS gsym-type + @ expr-type !
                        CC-TOKEN  \ get next token
                        tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF
                            DROP
                            PARSE-CALL EXIT
                        ELSE
                            \ Function name as value (function pointer)
                            UNGET-TOKEN
                            \ Load function address from gsym table
                            DUP CELLS gsym-name + @ gsym-nbuf +
                            SWAP CELLS gsym-nlen + @
                            EMIT-GLOBAL-ADDR
                            S" addi r1, r2, 0" EMIT-INSN
                            0 is-lvalue !
                        THEN
                    ELSE
                        EMIT-VAR-ADDR-GLOBAL
                    THEN
                ENDOF
                3 OF  \ enum value
                    CELLS enum-val + @ EMIT-LI-R1
                    TY-INT expr-type !
                    0 is-lvalue !
                ENDOF
                2 OF  \ typedef — shouldn't appear as expression
                    DROP
                    S" typedef used as expression" CC-ERR
                    0 EMIT-LI-R1
                    TY-INT expr-type !
                    0 is-lvalue !
                ENDOF
                \ -1 = not found
                DROP  \ drop idx (-1)
                \ Assume it's a forward-declared function
                tok-buf call-name tok-len @ CMOVE
                tok-len @ call-name-len !
                \ Add as prototype
                tok-buf tok-len @ TY-INT 2 0 GSYM-ADD DROP
                CC-TOKEN
                tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF
                    PARSE-CALL EXIT
                ELSE
                    UNGET-TOKEN
                    TY-INT expr-type !
                    0 is-lvalue !
                THEN
            ENDCASE
            CC-TOKEN
        ENDOF
        TK-KW OF
            tok-val @ KW-SIZEOF = IF
                PARSE-SIZEOF EXIT
            THEN
            S" unexpected keyword in expression" CC-ERR
            CC-TOKEN
        ENDOF
        TK-PUNCT OF
            tok-val @ P-LPAREN = IF
                CC-TOKEN
                \ Parenthesized expression
                \ Token already consumed — it's the first token of the expr
                PARSE-EXPR
                P-RPAREN EXPECT-PUNCT
                CC-TOKEN
                EXIT
            THEN
            S" unexpected punctuation in expression" CC-ERR
            CC-TOKEN
        ENDOF
        \ Default
        S" unexpected token in expression" CC-ERR
        CC-TOKEN
    ENDCASE ;

\ --- Postfix expression ---
\ Handles: a[i], a.field, a->field, a++, a--, f()
: PARSE-POSTFIX ( -- )
    PARSE-PRIMARY
    BEGIN
        tok-type @ TK-PUNCT = IF
            tok-val @ CASE
                P-LBRACK OF   \ array indexing: a[i]
                    \ Array bases are already addresses; pointer bases must be loaded.
                    expr-type @ TYPE-IS-ARRAY 0= IF
                        LVAL-TO-RVAL
                    THEN
                    EMIT-PUSH-R1  \ save base address
                    expr-type @ index-base-type !
                    CC-TOKEN  \ skip [
                    PARSE-EXPR
                    LVAL-TO-RVAL
                    \ r1 = index, stack top = base address
                    \ Scale index by element size
                    index-base-type @ TYPE-DEREF-SIZE DUP 1 <> IF
                        DUP 4 = IF
                            DROP
                            EMIT-INDENT S" slli r1, r1, 2" OUT-STR OUT-NL
                        ELSE DUP 2 = IF
                            DROP
                            EMIT-INDENT S" slli r1, r1, 1" OUT-STR OUT-NL
                        ELSE
                            EMIT-LI-R2
                            S" mul r1, r1, r2" EMIT-INSN
                        THEN THEN
                    ELSE DROP THEN
                    EMIT-POP-R2
                    S" add r1, r2, r1" EMIT-INSN  \ r1 = base + index*size
                    index-base-type @ DUP TYPE-IS-ARRAY IF
                        CLR-ARR-MASK AND
                    ELSE
                        TYPE-REMOVE-PTR
                    THEN expr-type !
                    1 is-lvalue !
                    P-RBRACK EXPECT-PUNCT CC-TOKEN
                    TRUE
                ENDOF
                P-DOT OF     \ struct member access: s.field
                    \ r1 has address of struct (lvalue)
                    is-lvalue @ IF
                        \ Already an address, good
                    ELSE
                        \ Not an lvalue — this is an error for now
                        S" . requires lvalue" CC-ERR
                    THEN
                    CC-TOKEN  \ skip .
                    \ Get field name
                    expr-type @ TYPE-BASE TY-STRUCT = IF
                        expr-type @ 9 RSHIFT 127 AND  \ struct index
                        tok-buf tok-len @ FLD-FIND DUP -1 <> IF
                            DUP CELLS fld-off + @ tmp-a !   \ field offset
                            CELLS fld-type + @ expr-type !
                            tmp-a @ 0<> IF
                                tmp-a @ EMIT-LI-R2
                                S" add r1, r1, r2" EMIT-INSN
                            THEN
                            1 is-lvalue !
                        ELSE
                            DROP S" unknown struct field" CC-ERR
                        THEN
                    ELSE S" . used on non-struct" CC-ERR THEN
                    CC-TOKEN  \ skip field name
                    TRUE
                ENDOF
                P-ARROW OF   \ pointer member access: p->field
                    LVAL-TO-RVAL  \ load the pointer
                    CC-TOKEN  \ skip ->
                    expr-type @ TYPE-REMOVE-PTR tmp-type !
                    tmp-type @ TYPE-BASE TY-STRUCT = IF
                        tmp-type @ 9 RSHIFT 127 AND  \ struct index
                        tok-buf tok-len @ FLD-FIND DUP -1 <> IF
                            DUP CELLS fld-off + @ tmp-a !
                            CELLS fld-type + @ expr-type !
                            tmp-a @ 0<> IF
                                tmp-a @ EMIT-LI-R2
                                S" add r1, r1, r2" EMIT-INSN
                            THEN
                            1 is-lvalue !
                        ELSE
                            DROP S" unknown struct field" CC-ERR
                        THEN
                    ELSE S" -> used on non-pointer-to-struct" CC-ERR THEN
                    CC-TOKEN
                    TRUE
                ENDOF
                P-LPAREN OF \ indirect function call: fptr(args)
                    LVAL-TO-RVAL   \ r1 = function address
                    EMIT-PUSH-R1  \ save fn addr
                    CC-TOKEN  \ skip (
                    0 call-argc !
                    tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND 0= IF
                        BEGIN
                            call-argc @ 0> IF EMIT-PUSH-R1 THEN
                            CALL-SAVE
                            PARSE-ASSIGN-EXPR
                            LVAL-TO-RVAL
                            CALL-RESTORE
                            1 call-argc +!
                            tok-type @ TK-PUNCT = tok-val @ P-COMMA = AND
                        WHILE
                            CC-TOKEN
                        REPEAT
                    THEN
                    P-RPAREN EXPECT-PUNCT CC-TOKEN
                    \ Assign args to registers (same as PARSE-CALL)
                    call-argc @ CASE
                        0 OF ENDOF
                        1 OF S" addi r3, r1, 0" EMIT-INSN ENDOF
                        2 OF S" addi r4, r1, 0" EMIT-INSN
                             EMIT-POP-R2 S" addi r3, r2, 0" EMIT-INSN ENDOF
                        3 OF S" addi r5, r1, 0" EMIT-INSN
                             EMIT-POP-R2 S" addi r4, r2, 0" EMIT-INSN
                             EMIT-POP-R2 S" addi r3, r2, 0" EMIT-INSN ENDOF
                        4 OF S" addi r6, r1, 0" EMIT-INSN
                             EMIT-POP-R2 S" addi r5, r2, 0" EMIT-INSN
                             EMIT-POP-R2 S" addi r4, r2, 0" EMIT-INSN
                             EMIT-POP-R2 S" addi r3, r2, 0" EMIT-INSN ENDOF
                    ENDCASE
                    \ Pop fn address and call indirectly
                    EMIT-POP-R2  \ r2 = function address
                    S" jalr r31, r2, 0" EMIT-INSN
                    TY-INT expr-type !
                    0 is-lvalue !
                    TRUE
                ENDOF
                P-INC OF    \ postfix x++
                    \ r1 = address of lvalue
                    is-lvalue @ 0= IF S" ++ requires lvalue" CC-ERR THEN
                    expr-type @ tmp-type !
                    \ Strategy: r1=addr. Save addr in r2. Load val. Save old val.
                    \ Increment. Store. Restore old val as result.
                    S" addi r2, r1, 0" EMIT-INSN   \ r2 = addr
                    S" ldw r1, r2, 0" EMIT-INSN    \ r1 = old value
                    EMIT-PUSH-R1                   \ save old value on stack
                    tmp-type @ TYPE-IS-PTR IF
                        tmp-type @ TYPE-DEREF-SIZE
                        EMIT-INDENT S" addi r1, r1, " OUT-STR OUT-SNUM OUT-NL
                    ELSE
                        S" addi r1, r1, 1" EMIT-INSN
                    THEN
                    S" stw r2, r1, 0" EMIT-INSN    \ store new value
                    S" ldw r1, r29, 0" EMIT-INSN   \ r1 = old value (result)
                    S" addi r29, r29, 4" EMIT-INSN  \ pop
                    0 is-lvalue !
                    CC-TOKEN
                    TRUE
                ENDOF
                P-DEC OF    \ postfix x--
                    is-lvalue @ 0= IF S" -- requires lvalue" CC-ERR THEN
                    expr-type @ tmp-type !
                    S" addi r2, r1, 0" EMIT-INSN   \ r2 = addr
                    S" ldw r1, r2, 0" EMIT-INSN    \ r1 = old value
                    EMIT-PUSH-R1                   \ save old value
                    tmp-type @ TYPE-IS-PTR IF
                        tmp-type @ TYPE-DEREF-SIZE NEGATE
                        EMIT-INDENT S" addi r1, r1, " OUT-STR OUT-SNUM OUT-NL
                    ELSE
                        S" addi r1, r1, -1" EMIT-INSN
                    THEN
                    S" stw r2, r1, 0" EMIT-INSN
                    S" ldw r1, r29, 0" EMIT-INSN
                    S" addi r29, r29, 4" EMIT-INSN
                    0 is-lvalue !
                    CC-TOKEN
                    TRUE
                ENDOF
                FALSE SWAP
            ENDCASE
        ELSE FALSE THEN
    WHILE REPEAT ;

\ --- Unary expression ---
\ Handles: -x, !x, ~x, *p, &x, ++x, --x, (cast)
: PARSE-UNARY ( -- )
    \ Cast has unary precedence: (T)e parses as (T)(e), while (T)a[i] must
    \ parse as (T)(a[i]) and not ((T)a)[i].
    tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF
        CC-TOKEN
        IS-TYPE-START IF
            PARSE-BASE-TYPE DROP
            PARSE-POINTERS
            tmp-type !          \ cast target type
            P-RPAREN EXPECT-PUNCT
            CC-TOKEN
            PARSE-UNARY         \ cast-expression
            LVAL-TO-RVAL
            tmp-type @ expr-type !
            0 is-lvalue !
            EXIT
        ELSE
            \ Not a cast: restore only token stream state for "(expr)" path.
            UNGET-TOKEN
            TK-PUNCT tok-type !
            P-LPAREN tok-val !
        THEN
    THEN
    tok-type @ TK-PUNCT = IF
        tok-val @ CASE
            P-MINUS OF    \ unary minus
                CC-TOKEN
                PARSE-UNARY
                LVAL-TO-RVAL
                S" sub r1, r0, r1" EMIT-INSN
                TY-INT expr-type !
                EXIT
            ENDOF
            P-BANG OF     \ logical not
                CC-TOKEN
                PARSE-UNARY
                LVAL-TO-RVAL
                S" seq r1, r1, r0" EMIT-INSN
                TY-INT expr-type !
                0 is-lvalue !
                EXIT
            ENDOF
            P-TILDE OF    \ bitwise not
                CC-TOKEN
                PARSE-UNARY
                LVAL-TO-RVAL
                \ ~x = x XOR -1
                S" addi r2, r0, -1" EMIT-INSN
                S" xor r1, r1, r2" EMIT-INSN
                TY-INT expr-type !
                0 is-lvalue !
                EXIT
            ENDOF
            P-STAR OF     \ dereference
                CC-TOKEN
                PARSE-UNARY
                LVAL-TO-RVAL
                expr-type @ TYPE-REMOVE-PTR expr-type !
                1 is-lvalue !
                EXIT
            ENDOF
            P-AMP OF      \ address-of
                CC-TOKEN
                PARSE-UNARY
                \ Should be an lvalue — r1 already has the address
                is-lvalue @ 0= IF S" & requires lvalue" CC-ERR THEN
                expr-type @ TYPE-ADD-PTR expr-type !
                0 is-lvalue !
                EXIT
            ENDOF
            P-INC OF      \ prefix ++
                CC-TOKEN
                PARSE-UNARY
                is-lvalue @ 0= IF S" prefix ++ requires lvalue" CC-ERR THEN
                expr-type @ tmp-type !
                \ r1 = address
                S" addi r2, r1, 0" EMIT-INSN   \ r2 = addr
                tmp-type @ EMIT-LOAD-DEREF      \ r1 = value
                tmp-type @ TYPE-IS-PTR IF
                    tmp-type @ TYPE-DEREF-SIZE EMIT-LI-R2
                    S" add r1, r1, r2" EMIT-INSN
                    \ Need addr back — problem. Save it first.
                    \ Redo: save addr, load, increment, store, leave new value
                ELSE
                    S" addi r1, r1, 1" EMIT-INSN
                THEN
                S" stw r2, r1, 0" EMIT-INSN     \ store back (r2 still has addr)
                0 is-lvalue !
                EXIT
            ENDOF
            P-DEC OF      \ prefix --
                CC-TOKEN
                PARSE-UNARY
                is-lvalue @ 0= IF S" prefix -- requires lvalue" CC-ERR THEN
                expr-type @ tmp-type !
                S" addi r2, r1, 0" EMIT-INSN
                tmp-type @ EMIT-LOAD-DEREF
                S" addi r1, r1, -1" EMIT-INSN
                S" stw r2, r1, 0" EMIT-INSN
                0 is-lvalue !
                EXIT
            ENDOF
            \ default: not a unary operator, fall through to PARSE-POSTFIX
            \ ENDCASE drops the selector
        ENDCASE
    THEN
    tok-type @ TK-KW = tok-val @ KW-SIZEOF = AND IF
        PARSE-SIZEOF EXIT
    THEN
    PARSE-POSTFIX ;

\ --- Binary expressions with precedence climbing ---

\ Get precedence of current binary operator (0 if not a binary op)
: BINOP-PREC ( -- prec )
    tok-type @ TK-PUNCT <> IF 0 EXIT THEN
    tok-val @ CASE
        P-LOR     OF 2 ENDOF
        P-LAND    OF 3 ENDOF
        P-PIPE    OF 4 ENDOF
        P-CARET   OF 5 ENDOF
        P-AMP     OF 6 ENDOF
        P-EQ      OF 7 ENDOF
        P-NE      OF 7 ENDOF
        P-LT      OF 8 ENDOF
        P-GT      OF 8 ENDOF
        P-LE      OF 8 ENDOF
        P-GE      OF 8 ENDOF
        P-LSHIFT  OF 9 ENDOF
        P-RSHIFT  OF 9 ENDOF
        P-PLUS    OF 10 ENDOF
        P-MINUS   OF 10 ENDOF
        P-STAR    OF 11 ENDOF
        P-SLASH   OF 11 ENDOF
        P-PERCENT OF 11 ENDOF
        0 SWAP
    ENDCASE ;

\ Saved state for binary expression
VARIABLE binop-saved-op
VARIABLE binop-saved-ltype

\ Get precedence of an operator by ID
: OP-PREC ( op-id -- prec )
    CASE
        P-LOR     OF 2 ENDOF
        P-LAND    OF 3 ENDOF
        P-PIPE    OF 4 ENDOF
        P-CARET   OF 5 ENDOF
        P-AMP     OF 6 ENDOF
        P-EQ      OF 7 ENDOF
        P-NE      OF 7 ENDOF
        P-LT      OF 8 ENDOF
        P-GT      OF 8 ENDOF
        P-LE      OF 8 ENDOF
        P-GE      OF 8 ENDOF
        P-LSHIFT  OF 9 ENDOF
        P-RSHIFT  OF 9 ENDOF
        P-PLUS    OF 10 ENDOF
        P-MINUS   OF 10 ENDOF
        P-STAR    OF 11 ENDOF
        P-SLASH   OF 11 ENDOF
        P-PERCENT OF 11 ENDOF
        0 SWAP
    ENDCASE ;

: PARSE-BINARY ( min-prec -- )
    >R  \ save min-prec
    PARSE-UNARY
    BEGIN
        BINOP-PREC DUP R@ >=
        OVER 0> AND
    WHILE
        DROP  \ drop prec from BINOP-PREC (not needed in body)
        \ Current token is a binary operator with sufficient precedence
        LVAL-TO-RVAL
        expr-type @ binop-saved-ltype !
        tok-val @ binop-saved-op !
        EMIT-PUSH-R1
        CC-TOKEN  \ consume operator
        \ Save binop state before recursive call (RECURSE clobbers globals)
        binop-saved-op @ >R
        binop-saved-ltype @ >R
        \ Parse right operand with prec+1 (left-associative)
        binop-saved-op @ OP-PREC 1+
        RECURSE  \ parse right operand
        \ Restore binop state
        R> binop-saved-ltype !
        R> binop-saved-op !
        LVAL-TO-RVAL
        EMIT-POP-R2  \ r2 = left, r1 = right
        \ Generate code for the operation
        binop-saved-op @ CASE
            P-LAND OF
                S" sne r2, r2, r0" EMIT-INSN
                S" sne r1, r1, r0" EMIT-INSN
                S" and r1, r2, r1" EMIT-INSN
            ENDOF
            P-LOR OF
                S" sne r2, r2, r0" EMIT-INSN
                S" sne r1, r1, r0" EMIT-INSN
                S" or r1, r2, r1" EMIT-INSN
            ENDOF
            P-PLUS OF
                binop-saved-ltype @ TYPE-IS-PTR IF
                    binop-saved-ltype @ TYPE-DEREF-SIZE DUP 1 <> IF
                        DUP 4 = IF DROP S" slli r1, r1, 2" EMIT-INSN
                        ELSE DUP 2 = IF DROP S" slli r1, r1, 1" EMIT-INSN
                        ELSE EMIT-PUSH-R1 EMIT-LI-R1 EMIT-POP-R2
                            S" mul r1, r2, r1" EMIT-INSN
                        THEN THEN
                    ELSE DROP THEN
                THEN
                S" add r1, r2, r1" EMIT-INSN
                binop-saved-ltype @ TYPE-IS-PTR IF
                    binop-saved-ltype @ expr-type !
                THEN
            ENDOF
            P-MINUS OF
                binop-saved-ltype @ TYPE-IS-PTR IF
                    expr-type @ TYPE-IS-PTR IF
                        \ ptr - ptr
                        S" sub r1, r2, r1" EMIT-INSN
                        binop-saved-ltype @ TYPE-DEREF-SIZE DUP 1 <> IF
                            EMIT-PUSH-R1 EMIT-LI-R1 EMIT-POP-R2
                            S" div r1, r2, r1" EMIT-INSN
                        ELSE DROP THEN
                        TY-INT expr-type !
                    ELSE
                        \ ptr - int
                        binop-saved-ltype @ TYPE-DEREF-SIZE DUP 1 <> IF
                            DUP 4 = IF DROP S" slli r1, r1, 2" EMIT-INSN
                            ELSE DUP 2 = IF DROP S" slli r1, r1, 1" EMIT-INSN
                            ELSE EMIT-PUSH-R1 EMIT-LI-R1 EMIT-POP-R2
                                S" mul r1, r2, r1" EMIT-INSN
                            THEN THEN
                        ELSE DROP THEN
                        S" sub r1, r2, r1" EMIT-INSN
                        binop-saved-ltype @ expr-type !
                    THEN
                ELSE S" sub r1, r2, r1" EMIT-INSN THEN
            ENDOF
            P-STAR    OF S" mul r1, r2, r1" EMIT-INSN ENDOF
            P-SLASH   OF S" div r1, r2, r1" EMIT-INSN ENDOF
            P-PERCENT OF S" rem r1, r2, r1" EMIT-INSN ENDOF
            P-AMP     OF S" and r1, r2, r1" EMIT-INSN ENDOF
            P-PIPE    OF S" or r1, r2, r1"  EMIT-INSN ENDOF
            P-CARET   OF S" xor r1, r2, r1" EMIT-INSN ENDOF
            P-LSHIFT  OF S" sll r1, r2, r1" EMIT-INSN ENDOF
            P-RSHIFT  OF
                binop-saved-ltype @ TYPE-IS-UNSIGNED IF
                    S" srl r1, r2, r1" EMIT-INSN
                ELSE
                    S" sra r1, r2, r1" EMIT-INSN
                THEN
            ENDOF
            P-EQ      OF S" seq r1, r2, r1" EMIT-INSN ENDOF
            P-NE      OF S" sne r1, r2, r1" EMIT-INSN ENDOF
            P-LT      OF
                binop-saved-ltype @ TYPE-IS-UNSIGNED IF
                    S" sltu r1, r2, r1" EMIT-INSN
                ELSE
                    S" slt r1, r2, r1" EMIT-INSN
                THEN
            ENDOF
            P-GT      OF
                binop-saved-ltype @ TYPE-IS-UNSIGNED IF
                    S" sgtu r1, r2, r1" EMIT-INSN
                ELSE
                    S" sgt r1, r2, r1" EMIT-INSN
                THEN
            ENDOF
            P-LE      OF
                binop-saved-ltype @ TYPE-IS-UNSIGNED IF
                    S" sleu r1, r2, r1" EMIT-INSN
                ELSE
                    S" sle r1, r2, r1" EMIT-INSN
                THEN
            ENDOF
            P-GE      OF
                binop-saved-ltype @ TYPE-IS-UNSIGNED IF
                    S" sgeu r1, r2, r1" EMIT-INSN
                ELSE
                    S" sge r1, r2, r1" EMIT-INSN
                THEN
            ENDOF
            S" unknown binop" CC-ERR
        ENDCASE
        0 is-lvalue !
    REPEAT
    DROP  \ drop the prec from BINOP-PREC
    R> DROP ;  \ drop min-prec

\ --- Ternary expression ---
\ Keep ternary labels in dedicated vars and save/restore across recursion.
VARIABLE tern-false
VARIABLE tern-end

: PARSE-TERNARY ( -- )
    2 PARSE-BINARY
    tok-type @ TK-PUNCT = tok-val @ P-QMARK = AND IF
        tern-false @ >R
        tern-end @ >R
        LVAL-TO-RVAL
        NEW-LABEL tern-false !
        NEW-LABEL tern-end !
        tern-false @ EMIT-BEQ-ZERO  \ branch to false
        CC-TOKEN  \ skip ?
        PARSE-ASSIGN-EXPR
        LVAL-TO-RVAL
        tern-end @ EMIT-JUMP        \ jump to end
        tern-false @ EMIT-LABEL     \ emit false label
        P-COLON EXPECT-PUNCT
        CC-TOKEN
        PARSE-TERNARY
        LVAL-TO-RVAL
        tern-end @ EMIT-LABEL       \ emit end label
        R> tern-end !
        R> tern-false !
    THEN ;

\ --- Assignment expression ---
VARIABLE assign-saved-op
VARIABLE assign-saved-type

: IS-ASSIGN-OP ( -- f )
    tok-type @ TK-PUNCT <> IF FALSE EXIT THEN
    tok-val @ DUP P-ASSIGN = OVER P-PLUSEQ = OR OVER P-MINUSEQ = OR
    OVER P-STAREQ = OR OVER P-SLASHEQ = OR OVER P-PERCENTEQ = OR
    OVER P-AMPEQ = OR OVER P-PIPEEQ = OR OVER P-CARETEQ = OR
    OVER P-LSHIFTEQ = OR SWAP P-RSHIFTEQ = OR ;

: PARSE-ASSIGN ( -- )
    PARSE-TERNARY
    IS-ASSIGN-OP IF
        \ r1 = address of lvalue
        is-lvalue @ 0= IF S" assignment requires lvalue" CC-ERR THEN
        expr-type @ assign-saved-type !
        tok-val @ assign-saved-op !   \ save operator (survives PARSE-ASSIGN-EXPR)
        EMIT-PUSH-R1        \ save lvalue address
        CC-TOKEN             \ skip operator
        PARSE-ASSIGN-EXPR   \ parse right side (right-associative)
        LVAL-TO-RVAL
        \ r1 = right value, stack = lvalue address
        assign-saved-op @ P-ASSIGN = IF
            \ Simple assignment
            EMIT-POP-R2      \ r2 = address
            assign-saved-type @ EMIT-STORE-DEREF
            \ Leave assigned value in r1
        ELSE
            \ Compound assignment: load old value, operate, store
            EMIT-PUSH-R1     \ save right value
            S" ldw r1, r29, 4" EMIT-INSN  \ load lvalue address (below right value)
            assign-saved-type @ EMIT-LOAD-DEREF     \ r1 = old value
            EMIT-POP-R2      \ r2 = right value
            \ r1=old(left), r2=right. Swap to r2=left, r1=right for binop.
            S" addi r3, r1, 0" EMIT-INSN   \ save old in r3
            S" addi r1, r2, 0" EMIT-INSN   \ r1 = right
            S" addi r2, r3, 0" EMIT-INSN   \ r2 = old (left)
            assign-saved-op @ CASE
                P-PLUSEQ    OF S" add r1, r2, r1" EMIT-INSN ENDOF
                P-MINUSEQ   OF S" sub r1, r2, r1" EMIT-INSN ENDOF
                P-STAREQ    OF S" mul r1, r2, r1" EMIT-INSN ENDOF
                P-SLASHEQ   OF S" div r1, r2, r1" EMIT-INSN ENDOF
                P-PERCENTEQ OF S" rem r1, r2, r1" EMIT-INSN ENDOF
                P-AMPEQ     OF S" and r1, r2, r1" EMIT-INSN ENDOF
                P-PIPEEQ    OF S" or r1, r2, r1"  EMIT-INSN ENDOF
                P-CARETEQ   OF S" xor r1, r2, r1" EMIT-INSN ENDOF
                P-LSHIFTEQ  OF S" sll r1, r2, r1" EMIT-INSN ENDOF
                P-RSHIFTEQ  OF S" srl r1, r2, r1" EMIT-INSN ENDOF
            ENDCASE
            \ Store result
            EMIT-POP-R2      \ r2 = address (was below right value on stack)
            assign-saved-type @ EMIT-STORE-DEREF
        THEN
        0 is-lvalue !
        assign-saved-type @ expr-type !
    THEN ;

\ Comma expression (lowest precedence)
: PARSE-COMMA-EXPR ( -- )
    PARSE-ASSIGN
    BEGIN
        tok-type @ TK-PUNCT = tok-val @ P-COMMA = AND
    WHILE
        LVAL-TO-RVAL
        CC-TOKEN
        PARSE-ASSIGN
    REPEAT ;

\ Resolve deferred words
:NONAME PARSE-COMMA-EXPR ; IS PARSE-EXPR
:NONAME PARSE-ASSIGN ; IS PARSE-ASSIGN-EXPR

\ ============================================================
\ SECTION 9: Statement Parser
\ ============================================================

\ Forward-declared above: PARSE-STATEMENT, PARSE-COMPOUND, PARSE-DECL-OR-STMT

\ Return statement epilogue label (set per-function)
VARIABLE ret-label

: PARSE-RETURN-STMT ( -- )
    CC-TOKEN  \ skip 'return'
    tok-type @ TK-PUNCT <> tok-val @ P-SEMI <> OR IF
        PARSE-EXPR
        LVAL-TO-RVAL
    THEN
    \ Pop switch expression values if returning from inside switch(es)
    switch-sp @ 0 ?DO
        S" addi r29, r29, 4" EMIT-INSN
    LOOP
    ret-label @ EMIT-JUMP
    EXPECT-SEMI CC-TOKEN ;

: PARSE-IF-STMT ( -- )
    CC-TOKEN  \ skip 'if'
    sl-a @ >R
    sl-b @ >R
    P-LPAREN EXPECT-PUNCT
    CC-TOKEN
    PARSE-EXPR
    LVAL-TO-RVAL
    EXPECT-RPAREN CC-TOKEN
    NEW-LABEL sl-a !  \ else/end label
    sl-a @ EMIT-BEQ-ZERO
    PARSE-STATEMENT
    \ Check for else
    tok-type @ TK-KW = tok-val @ KW-ELSE = AND IF
        NEW-LABEL sl-b !  \ end label (skip else)
        sl-b @ EMIT-JUMP
        sl-a @ EMIT-LABEL
        CC-TOKEN  \ skip 'else'
        PARSE-STATEMENT
        sl-b @ EMIT-LABEL
    ELSE
        sl-a @ EMIT-LABEL
    THEN
    R> sl-b !
    R> sl-a !
;

: PARSE-WHILE-STMT ( -- )
    CC-TOKEN  \ skip 'while'
    sl-a @ >R
    sl-b @ >R
    NEW-LABEL sl-a !  \ loop start
    NEW-LABEL sl-b !  \ loop end
    \ Push break/continue targets
    sl-b @ break-stk break-sp @ CELLS + !
    1 break-sp +!
    sl-a @ cont-stk cont-sp @ CELLS + !
    1 cont-sp +!
    sl-a @ EMIT-LABEL
    P-LPAREN EXPECT-PUNCT
    CC-TOKEN
    PARSE-EXPR
    LVAL-TO-RVAL
    EXPECT-RPAREN CC-TOKEN
    sl-b @ EMIT-BEQ-ZERO
    PARSE-STATEMENT
    sl-a @ EMIT-JUMP
    sl-b @ EMIT-LABEL
    \ Pop break/continue
    -1 break-sp +!
    -1 cont-sp +!
    R> sl-b !
    R> sl-a !
;

: PARSE-DO-STMT ( -- )
    CC-TOKEN  \ skip 'do'
    sl-a @ >R
    sl-b @ >R
    sl-c @ >R
    NEW-LABEL sl-a !  \ loop start
    NEW-LABEL sl-b !  \ loop end
    NEW-LABEL sl-c !  \ continue target (before condition)
    sl-b @ break-stk break-sp @ CELLS + !
    1 break-sp +!
    sl-c @ cont-stk cont-sp @ CELLS + !
    1 cont-sp +!
    sl-a @ EMIT-LABEL
    PARSE-STATEMENT
    sl-c @ EMIT-LABEL
    \ Expect 'while'
    tok-type @ TK-KW = tok-val @ KW-WHILE = AND IF
        CC-TOKEN
    ELSE S" expected 'while' after 'do'" CC-ERR THEN
    P-LPAREN EXPECT-PUNCT
    CC-TOKEN
    PARSE-EXPR
    LVAL-TO-RVAL
    EXPECT-RPAREN
    CC-TOKEN  \ advance past )
    sl-a @ EMIT-BNE-ZERO
    EXPECT-SEMI CC-TOKEN
    sl-b @ EMIT-LABEL
    -1 break-sp +!
    -1 cont-sp +!
    R> sl-c !
    R> sl-b !
    R> sl-a !
;

: PARSE-FOR-STMT ( -- )
    CC-TOKEN  \ skip 'for'
    sl-a @ >R
    sl-b @ >R
    sl-c @ >R
    sl-d @ >R
    sl-e @ >R
    P-LPAREN EXPECT-PUNCT
    CC-TOKEN
    \ Init expression (or declaration)
    tok-type @ TK-PUNCT = tok-val @ P-SEMI = AND 0= IF
        \ Check if it's a declaration
        IS-TYPE-START IF
            \ Local variable declaration in for-init
            PARSE-BASE-TYPE DROP
            PARSE-DECLARATOR  \ type name-addr name-len
            tmp-name-len ! tmp-name-off !
            \ Allocate local
            DUP TYPE-SIZE tmp-a !
            DUP TYPE-ALIGN tmp-b !
            local-offset @ NEGATE tmp-b @ ALIGN-UP NEGATE local-offset !
            local-offset @ tmp-a @ - local-offset !
            tmp-name-off @ tmp-name-len @
            2 PICK  \ type
            local-offset @
            0  \ not an arg
            LSYM-ADD DROP
            DROP  \ drop type
            \ Check for initializer
            tok-type @ TK-PUNCT = tok-val @ P-ASSIGN = AND IF
                CC-TOKEN
                PARSE-ASSIGN-EXPR
                LVAL-TO-RVAL
                local-offset @ TY-INT EMIT-STORE-LOCAL  \ simplified
            THEN
        ELSE
            PARSE-EXPR
            LVAL-TO-RVAL
        THEN
    THEN
    EXPECT-SEMI
    CC-TOKEN
    NEW-LABEL sl-a !  \ condition
    NEW-LABEL sl-b !  \ end
    NEW-LABEL sl-c !  \ increment (continue target)
    sl-b @ break-stk break-sp @ CELLS + !
    1 break-sp +!
    sl-c @ cont-stk cont-sp @ CELLS + !
    1 cont-sp +!
    sl-a @ EMIT-LABEL
    \ Condition
    tok-type @ TK-PUNCT = tok-val @ P-SEMI = AND 0= IF
        PARSE-EXPR
        LVAL-TO-RVAL
        sl-b @ EMIT-BEQ-ZERO
    THEN
    EXPECT-SEMI
    \ Save increment expression start position (for text capture)
    inp-pos @ sl-d !
    cc-line @ sl-e !
    \ Skip increment expression by scanning tokens
    CC-TOKEN
    0 tmp-idx !  \ paren depth
    BEGIN
        tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND tmp-idx @ 0= AND
        IF FALSE ELSE
            tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF 1 tmp-idx +! THEN
            tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND IF -1 tmp-idx +! THEN
            tok-type @ TK-EOF = IF FALSE ELSE CC-TOKEN TRUE THEN
        THEN
    WHILE REPEAT
    \ Save the increment text (from sl-d to tok-start, before the ")")
    tok-start @ sl-d @ - for-inc-len !
    for-inc-len @ 0> IF
        inp-buf sl-d @ + for-inc-buf for-inc-len @ CMOVE
    THEN
    CC-TOKEN  \ skip )
    \ Parse body
    PARSE-STATEMENT
    \ Now emit increment
    sl-c @ EMIT-LABEL   \ continue label
    for-inc-len @ 0> IF
        \ Inject saved increment text before current token (tok-start).
        \ After loop body parse, tok already holds the next token.
        inp-len @ tok-start @ - tmp-a !             \ remaining bytes incl current token
        for-inc-len @ 3 + tmp-a @ + tmp-b !         \ new total size at insertion point
        \ Move remaining input to make room
        inp-buf tok-start @ +                          \ src
        inp-buf tok-start @ + for-inc-len @ + 3 +     \ dst
        tmp-a @                                        \ count
        MOVE                                           \ overlap-safe
        \ Copy increment text at insertion point
        for-inc-buf inp-buf tok-start @ + for-inc-len @ CMOVE
        \ Add sentinel: space + ) + space
        32 inp-buf tok-start @ for-inc-len @ + + C!
        41 inp-buf tok-start @ for-inc-len @ + 1+ + C!
        32 inp-buf tok-start @ for-inc-len @ + 2 + + C!
        inp-len @ for-inc-len @ + 3 + inp-len !
        tok-start @ inp-pos !   \ re-lex from injected text
        0 has-peek !
        \ Parse injected increment expression
        CC-TOKEN
        tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND 0= IF
            PARSE-EXPR
            LVAL-TO-RVAL
        THEN
        \ Skip sentinel )
        tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND IF
            CC-TOKEN
        THEN
    THEN
    sl-a @ EMIT-JUMP
    sl-b @ EMIT-LABEL
    -1 break-sp +!
    -1 cont-sp +!
    R> sl-e !
    R> sl-d !
    R> sl-c !
    R> sl-b !
    R> sl-a !
;

: PARSE-SWITCH-STMT ( -- )
    CC-TOKEN  \ skip 'switch'
    sl-a @ >R
    sl-b @ >R
    sl-c @ >R
    sl-d @ >R
    sl-e @ >R
    P-LPAREN EXPECT-PUNCT
    CC-TOKEN
    PARSE-EXPR
    LVAL-TO-RVAL
    EXPECT-RPAREN CC-TOKEN
    \ Save switch value in a temp location on stack
    EMIT-PUSH-R1
    1 switch-sp +!
    NEW-LABEL sl-a !  \ end label
    sl-a @ break-stk break-sp @ CELLS + !
    1 break-sp +!
    NEW-LABEL sl-b !  \ default label (may not be used)
    0 sl-c !  \ has-default flag
    0 sl-d !  \ next-case label
    \ Parse switch body — it's a compound statement
    EXPECT-LBRACE
    CC-TOKEN
    BEGIN
        tok-type @ TK-PUNCT = tok-val @ P-RBRACE = AND 0= IF
            tok-type @ TK-EOF = 0=
        ELSE FALSE THEN
    WHILE
        tok-type @ TK-KW = tok-val @ KW-CASE = AND IF
            CC-TOKEN  \ skip 'case'
            \ Get case value (constant expression)
            tok-type @ TK-NUM = IF
                tok-val @ sl-e !
                CC-TOKEN
            ELSE
                \ Try enum or other constant
                tok-type @ TK-IDENT = IF
                    tok-buf tok-len @ ENUM-FIND DUP -1 <> IF
                        CELLS enum-val + @ sl-e !
                        CC-TOKEN
                    ELSE DROP 0 sl-e ! CC-TOKEN THEN
                ELSE 0 sl-e ! CC-TOKEN THEN
            THEN
            P-COLON EXPECT-PUNCT CC-TOKEN
            \ Emit comparison
            sl-d @ 0<> IF sl-d @ EMIT-LABEL THEN  \ previous case's fall-through label
            S" ldw r1, r29, 0" EMIT-INSN  \ load switch value
            sl-e @ EMIT-LI-R2
            NEW-LABEL sl-d !  \ next case label
            S" bne r1, r2, .L" EMIT-INDENT OUT-STR sl-d @ OUT-NUM OUT-NL
        ELSE tok-type @ TK-KW = tok-val @ KW-DEFAULT = AND IF
            CC-TOKEN  \ skip 'default'
            P-COLON EXPECT-PUNCT CC-TOKEN
            sl-d @ 0<> IF sl-d @ EMIT-LABEL THEN
            0 sl-d !
            1 sl-c !
        ELSE
            PARSE-DECL-OR-STMT
        THEN THEN
    REPEAT
    CC-TOKEN  \ skip }
    sl-d @ 0<> IF sl-d @ EMIT-LABEL THEN
    \ End label first, then pop — so break jumps here and pops
    sl-a @ EMIT-LABEL
    S" addi r29, r29, 4" EMIT-INSN  \ pop switch value
    -1 break-sp +!
    -1 switch-sp +!
    R> sl-e !
    R> sl-d !
    R> sl-c !
    R> sl-b !
    R> sl-a !
;

: PARSE-BREAK-STMT ( -- )
    CC-TOKEN  \ skip 'break'
    break-sp @ 0> IF
        break-stk break-sp @ 1- CELLS + @ EMIT-JUMP
    ELSE S" break outside loop/switch" CC-ERR THEN
    EXPECT-SEMI CC-TOKEN ;

: PARSE-CONTINUE-STMT ( -- )
    CC-TOKEN  \ skip 'continue'
    cont-sp @ 0> IF
        cont-stk cont-sp @ 1- CELLS + @ EMIT-JUMP
    ELSE S" continue outside loop" CC-ERR THEN
    EXPECT-SEMI CC-TOKEN ;

\ Parse a single statement
: (PARSE-STATEMENT) ( -- )
    tok-type @ TK-PUNCT = tok-val @ P-LBRACE = AND IF
        PARSE-COMPOUND EXIT
    THEN
    tok-type @ TK-PUNCT = tok-val @ P-SEMI = AND IF
        CC-TOKEN EXIT  \ empty statement
    THEN
    tok-type @ TK-KW = IF
        tok-val @ CASE
            KW-RETURN   OF PARSE-RETURN-STMT EXIT ENDOF
            KW-IF       OF PARSE-IF-STMT EXIT ENDOF
            KW-WHILE    OF PARSE-WHILE-STMT EXIT ENDOF
            KW-FOR      OF PARSE-FOR-STMT EXIT ENDOF
            KW-DO       OF PARSE-DO-STMT EXIT ENDOF
            KW-SWITCH   OF PARSE-SWITCH-STMT EXIT ENDOF
            KW-BREAK    OF PARSE-BREAK-STMT EXIT ENDOF
            KW-CONTINUE OF PARSE-CONTINUE-STMT EXIT ENDOF
        ENDCASE
    THEN
    \ Expression statement
    PARSE-EXPR
    LVAL-TO-RVAL
    EXPECT-SEMI CC-TOKEN ;

:NONAME (PARSE-STATEMENT) ; IS PARSE-STATEMENT

\ ============================================================
\ SECTION 10: Declarations and Compound Statements
\ ============================================================

\ Parse a local variable declaration
: PARSE-LOCAL-DECL ( base-type flags -- )
    DROP  \ ignore flags for locals
    BEGIN
        DUP  \ duplicate base type for each declarator
        PARSE-DECLARATOR  \ -- base-type type name-addr name-len
        DUP 0= IF
            2DROP DROP S" missing variable name" CC-ERR
            BEGIN tok-type @ TK-PUNCT = tok-val @ P-SEMI = AND 0= WHILE CC-TOKEN REPEAT
            CC-TOKEN EXIT
        THEN
        tmp-name-len ! tmp-name-off !  \ save name
        \ Compute size and alignment
        DUP TYPE-IS-ARRAY IF
            DUP TYPE-SIZE tmp-a !
            4 tmp-b !  \ array alignment = 4
        ELSE
            DUP TYPE-SIZE tmp-a !
            DUP TYPE-ALIGN tmp-b !
        THEN
        \ Allocate on stack (negative from FP)
        local-offset @ NEGATE tmp-b @ ALIGN-UP NEGATE local-offset !
        local-offset @ tmp-a @ - local-offset !
        \ Add to local symbol table
        tmp-name-off @ tmp-name-len @
        2 PICK  \ type
        local-offset @
        0  \ not arg
        LSYM-ADD DROP
        DROP  \ drop type
        \ Check for initializer
        tok-type @ TK-PUNCT = tok-val @ P-ASSIGN = AND IF
            CC-TOKEN  \ skip =
            \ Check for array initializer {1, 2, 3}
            tok-type @ TK-PUNCT = tok-val @ P-LBRACE = AND IF
                CC-TOKEN  \ skip {
                local-offset @ tmp-c !  \ base offset
                0 tmp-d !  \ element index
                BEGIN
                    tok-type @ TK-PUNCT = tok-val @ P-RBRACE = AND 0=
                WHILE
                    PARSE-ASSIGN-EXPR
                    LVAL-TO-RVAL
                    tmp-c @ tmp-d @ 4 * + TY-INT EMIT-STORE-LOCAL
                    1 tmp-d +!
                    tok-type @ TK-PUNCT = tok-val @ P-COMMA = AND IF CC-TOKEN THEN
                REPEAT
                CC-TOKEN  \ skip }
            ELSE
                PARSE-ASSIGN-EXPR
                LVAL-TO-RVAL
                \ Find the local we just added
                lsym-cnt @ 1- DUP CELLS lsym-off + @
                SWAP CELLS lsym-type + @
                EMIT-STORE-LOCAL
            THEN
        THEN
        \ Check for more declarators
        tok-type @ TK-PUNCT = tok-val @ P-COMMA = AND IF
            CC-TOKEN  \ skip comma
            TRUE
        ELSE FALSE THEN
    WHILE REPEAT
    DROP  \ drop base type
    EXPECT-SEMI CC-TOKEN ;

\ Parse declaration or statement (for inside compound statements)
: (PARSE-DECL-OR-STMT) ( -- )
    IS-TYPE-START IF
        PARSE-BASE-TYPE  \ -- type flags
        PARSE-LOCAL-DECL
    ELSE
        PARSE-STATEMENT
    THEN ;

:NONAME (PARSE-DECL-OR-STMT) ; IS PARSE-DECL-OR-STMT

\ Parse compound statement (block)
: (PARSE-COMPOUND) ( -- )
    CC-TOKEN  \ skip {
    BEGIN
        tok-type @ TK-PUNCT = tok-val @ P-RBRACE = AND 0= IF
            tok-type @ TK-EOF = 0=
        ELSE FALSE THEN
    WHILE
        \ ." [STMT] type=" tok-type @ . ." val=" tok-val @ . tok-buf tok-len @ TYPE CR
        PARSE-DECL-OR-STMT
    REPEAT
    CC-TOKEN  \ skip }
;

:NONAME (PARSE-COMPOUND) ; IS PARSE-COMPOUND

\ ============================================================
\ SECTION 11: Struct and Enum Definitions
\ ============================================================

\ Parse struct definition body { field; field; ... }
: PARSE-STRUCT-BODY ( struct-idx -- )
    >R
    CC-TOKEN  \ skip {
    0 fld-offset !  \ current offset (dedicated var — tmp-a clobbered by PARSE-BASE-TYPE)
    BEGIN
        tok-type @ TK-PUNCT = tok-val @ P-RBRACE = AND 0=
    WHILE
        PARSE-BASE-TYPE DROP  \ get field type, ignore flags
        BEGIN
            DUP PARSE-DECLARATOR  \ type name-addr name-len
            DUP 0= IF 2DROP DROP EXPECT-SEMI CC-TOKEN ELSE
                tmp-name-len ! tmp-name-off !
                \ Compute field size and alignment
                DUP TYPE-SIZE tmp-b !
                DUP TYPE-ALIGN tmp-c !
                \ Align current offset
                fld-offset @ tmp-c @ ALIGN-UP fld-offset !
                \ Add field
                R@ tmp-name-off @ tmp-name-len @ 3 PICK fld-offset @ FLD-ADD
                DROP  \ drop type
                fld-offset @ tmp-b @ + fld-offset !  \ advance offset
                tok-type @ TK-PUNCT = tok-val @ P-COMMA = AND IF
                    CC-TOKEN TRUE
                ELSE FALSE THEN
            THEN
        WHILE REPEAT
        DROP  \ drop base type
        EXPECT-SEMI CC-TOKEN
    REPEAT
    CC-TOKEN  \ skip }
    \ Set struct size (aligned to 4)
    fld-offset @ 4 ALIGN-UP R@ CELLS st-size + !
    R> DROP ;

\ Parse enum body { NAME = val, NAME, ... }
: PARSE-ENUM-BODY ( -- )
    CC-TOKEN  \ skip {
    0 tmp-a !  \ current value
    BEGIN
        tok-type @ TK-PUNCT = tok-val @ P-RBRACE = AND 0=
    WHILE
        tok-type @ TK-IDENT = IF
            tok-buf tok-len @
            CC-TOKEN
            tok-type @ TK-PUNCT = tok-val @ P-ASSIGN = AND IF
                CC-TOKEN  \ skip =
                \ Parse constant value
                tok-type @ TK-NUM = IF
                    tok-val @ tmp-a !
                    CC-TOKEN
                ELSE
                    tok-type @ TK-PUNCT = tok-val @ P-MINUS = AND IF
                        CC-TOKEN
                        tok-val @ NEGATE tmp-a !
                        CC-TOKEN
                    ELSE
                        \ Try enum name reference
                        tok-buf tok-len @ ENUM-FIND DUP -1 <> IF
                            CELLS enum-val + @ tmp-a !
                        ELSE DROP THEN
                        CC-TOKEN
                    THEN
                THEN
            THEN
            tmp-a @ ENUM-ADD
            1 tmp-a +!
            tok-type @ TK-PUNCT = tok-val @ P-COMMA = AND IF CC-TOKEN THEN
        ELSE
            CC-TOKEN  \ skip unexpected token
        THEN
    REPEAT
    CC-TOKEN ;  \ skip }

\ ============================================================
\ SECTION 12: Top-Level Parsing
\ ============================================================

\ Temp buffers for declarator names
CREATE decl-name 256 ALLOT
VARIABLE decl-name-len

\ Parse function parameter list and add params as locals
: PARSE-PARAMS ( -- nargs )
    0 tmp-e !  \ param count
    CC-TOKEN   \ skip (
    tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND IF
        CC-TOKEN 0 EXIT  \ no params — return 0
    THEN
    \ Check for (void)
    tok-type @ TK-KW = tok-val @ KW-VOID = AND IF
        CC-TOKEN
        tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND IF
            CC-TOKEN 0 EXIT
        THEN
        UNGET-TOKEN
        TK-KW tok-type ! KW-VOID tok-val !
    THEN
    BEGIN
        \ Handle ... (varargs)
        tok-type @ TK-PUNCT = tok-val @ P-ELLIPSIS = AND IF
            CC-TOKEN
            tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND IF
                CC-TOKEN tmp-e @ EXIT
            THEN
        THEN
        PARSE-BASE-TYPE DROP  \ type flags
        PARSE-DECLARATOR      \ type name-addr name-len
        2 PICK tmp-type !     \ save declared param type
        DUP 0= IF
            \ Unnamed parameter
            2DROP
            \ Still count it for register allocation
            tmp-e @ 8 < IF
                S" " 0 tmp-type @ -12 tmp-e @ 4 * - 1 LSYM-ADD DROP
            THEN
            DROP
        ELSE
            tmp-name-len ! tmp-name-off !
            \ Args are saved to frame at fp-12, fp-16, etc.
            tmp-name-off @ tmp-name-len @
            tmp-type @
            -12 tmp-e @ 4 * -  \ FP offset
            1  \ is-arg
            LSYM-ADD DROP
            DROP
        THEN
        1 tmp-e +!
        tok-type @ TK-PUNCT = tok-val @ P-COMMA = AND IF
            CC-TOKEN
        ELSE
            tok-type @ TK-PUNCT = tok-val @ P-RPAREN = AND IF
                CC-TOKEN tmp-e @ EXIT
            ELSE
                S" expected , or ) in parameter list" CC-ERR
                CC-TOKEN tmp-e @ EXIT
            THEN
        THEN
    AGAIN ;

\ Parse function definition
: PARSE-FUNCTION ( type flags name-addr name-len -- )
    \ Save name to decl-name buffer
    decl-name-len !                          \ Stack: ( type flags name-addr )
    decl-name decl-name-len @ CMOVE         \ Stack: ( type flags )
    \ Register as global symbol
    decl-name decl-name-len @ 3 PICK 1 4 PICK GSYM-ADD DROP
    \ Reset local state BEFORE parsing params
    LSYM-RESET
    0 local-offset !
    \ Parse parameters (adds params to local symbol table)
    PARSE-PARAMS func-nargs !
    \ Local offset starts after saved LR, FP, and arg copies
    -8 func-nargs @ 4 * - local-offset !
    \ Check for prototype (semicolon after params)
    tok-type @ TK-PUNCT = tok-val @ P-SEMI = AND IF
        2DROP CC-TOKEN EXIT  \ prototype — just register symbol
    THEN
    \ Function definition — emit label
    decl-name decl-name-len @ OUT-STR 58 OUT-CHAR OUT-NL  \ name:
    \ Check if static (flags bit 0)
    DUP 1 AND 0= IF  \ not static
        EMIT-INDENT S" .global " OUT-STR decl-name decl-name-len @ OUT-STR OUT-NL
    THEN
    2DROP  \ drop type and flags
    \ Set up function state
    0 break-sp ! 0 cont-sp !
    1 in-function !
    NEW-LABEL ret-label !
    \ Local offset is already set by PARSE-PARAMS (after args)
    \ Emit prologue with fixed frame size.
    FUNC-FRAME-SZ EMIT-PROLOGUE
    \ Save args to frame
    func-nargs @ 8 MIN 0 ?DO
        I 3 +
        EMIT-INDENT S" stw r30, r" OUT-STR OUT-NUM S" , " OUT-STR
        -12 I 4 * - OUT-SNUM OUT-NL
    LOOP
    \ Parse body
    EXPECT-LBRACE
    PARSE-COMPOUND
    \ Emit return label and epilogue
    ret-label @ EMIT-LABEL
    FUNC-FRAME-SZ frame-size !
    frame-size @ EMIT-EPILOGUE
    0 in-function !
    OUT-NL ;

\ Parse global variable declaration
: PARSE-GLOBAL-VAR ( type flags -- )
    >R  \ save flags
    BEGIN
        DUP PARSE-DECLARATOR  \ type name-addr name-len
        DUP 0= IF 2DROP DROP R> 2DROP EXPECT-SEMI CC-TOKEN EXIT THEN
        decl-name-len !                          \ Stack: ( type type' name-addr )
        decl-name decl-name-len @ CMOVE         \ Stack: ( type type' )
        \ Register global
        decl-name decl-name-len @ 2 PICK 0 R@ 1 AND GSYM-ADD DROP
        DROP  \ drop type' from PARSE-DECLARATOR
        \ Emit in .data or .bss section
        tok-type @ TK-PUNCT = tok-val @ P-ASSIGN = AND IF
            \ Initialized — .data section
            S" .data" EMIT-LINE
            EMIT-INDENT S" .align 4" OUT-STR OUT-NL
            R@ 1 AND 0= IF
                EMIT-INDENT S" .global " OUT-STR decl-name decl-name-len @ OUT-STR OUT-NL
            THEN
            decl-name decl-name-len @ OUT-STR 58 OUT-CHAR OUT-NL
            CC-TOKEN  \ skip =
            \ Parse initializer
            tok-type @ TK-PUNCT = tok-val @ P-LBRACE = AND IF
                \ Array/struct initializer
                CC-TOKEN  \ skip {
                1 tmp-e !  \ brace depth
                BEGIN
                    tmp-e @ 0>
                WHILE
                    tok-type @ TK-PUNCT = IF
                        tok-val @ P-LBRACE = IF
                            1 tmp-e +!
                            CC-TOKEN
                        ELSE tok-val @ P-RBRACE = IF
                            -1 tmp-e +!
                            CC-TOKEN
                        ELSE tok-val @ P-COMMA = IF
                            CC-TOKEN
                        ELSE
                            \ Unknown punctuation inside initializer
                            EMIT-INDENT S" .word 0" OUT-STR OUT-NL
                            CC-TOKEN
                        THEN THEN THEN
                    ELSE tok-type @ TK-NUM = IF
                        EMIT-INDENT S" .word " OUT-STR tok-val @ OUT-SNUM OUT-NL
                        CC-TOKEN
                    ELSE tok-type @ TK-STR = IF
                        \ String in initializer
                        EMIT-INDENT S" .word .Lstr_" OUT-STR tok-val @ OUT-NUM OUT-NL
                        CC-TOKEN
                    ELSE tok-type @ TK-IDENT = IF
                        \ Identifier — enum value or symbol address
                        EMIT-INDENT S" .word " OUT-STR tok-buf tok-len @ OUT-STR OUT-NL
                        CC-TOKEN
                    ELSE tok-type @ TK-PUNCT = tok-val @ P-MINUS = AND IF
                        CC-TOKEN
                        EMIT-INDENT S" .word " OUT-STR tok-val @ NEGATE OUT-SNUM OUT-NL
                        CC-TOKEN
                    ELSE
                        EMIT-INDENT S" .word 0" OUT-STR OUT-NL
                        CC-TOKEN
                    THEN THEN THEN THEN
                REPEAT
            ELSE tok-type @ TK-NUM = IF
                EMIT-INDENT S" .word " OUT-STR tok-val @ OUT-SNUM OUT-NL
                CC-TOKEN
            ELSE tok-type @ TK-STR = IF
                EMIT-INDENT S" .word .Lstr_" OUT-STR tok-val @ OUT-NUM OUT-NL
                CC-TOKEN
            ELSE
                EMIT-INDENT S" .word 0" OUT-STR OUT-NL
                PARSE-ASSIGN-EXPR  \ runtime init not possible for globals, but consume it
                LVAL-TO-RVAL
            THEN THEN THEN
            S" .text" EMIT-LINE
        ELSE
            \ Uninitialized — .bss section
            S" .bss" EMIT-LINE
            EMIT-INDENT S" .align 4" OUT-STR OUT-NL
            R@ 1 AND 0= IF
                EMIT-INDENT S" .global " OUT-STR decl-name decl-name-len @ OUT-STR OUT-NL
            THEN
            decl-name decl-name-len @ OUT-STR 58 OUT-CHAR OUT-NL
            EMIT-INDENT S" .space " OUT-STR
            DUP TYPE-SIZE OUT-NUM OUT-NL
            S" .text" EMIT-LINE
        THEN
        tok-type @ TK-PUNCT = tok-val @ P-COMMA = AND IF
            CC-TOKEN TRUE
        ELSE FALSE THEN
    WHILE REPEAT
    DROP  \ drop base type
    R> DROP  \ drop flags
    EXPECT-SEMI CC-TOKEN ;

\ Parse typedef
: PARSE-TYPEDEF ( -- )
    CC-TOKEN  \ skip 'typedef'
    PARSE-BASE-TYPE DROP  \ type flags, drop flags
    \ Check for struct definition inline with typedef
    tok-type @ TK-PUNCT = tok-val @ P-LBRACE = AND IF
        DUP TYPE-BASE TY-STRUCT = IF
            \ typedef struct { ... } name;
            \ The struct was created, now parse body
            st-cnt @ 1- PARSE-STRUCT-BODY
        ELSE
            \ typedef enum { ... } name;
            \ Enums are represented as int in type words, so detect by '{'
            PARSE-ENUM-BODY
        THEN
    THEN
    PARSE-DECLARATOR  \ type name-addr name-len
    DUP 0= IF 2DROP S" missing typedef name" CC-ERR DROP EXPECT-SEMI CC-TOKEN EXIT THEN
    \ Add typedef: stack is ( type name-addr name-len )
    \ TD-ADD expects ( addr u type )
    ROT TD-ADD
    EXPECT-SEMI CC-TOKEN ;

\ Parse struct definition at top level
: PARSE-STRUCT-DEF ( -- type )
    CC-TOKEN  \ skip 'struct'
    \ Get struct name
    tok-type @ TK-IDENT = IF
        tok-buf tok-len @ 2DUP ST-FIND DUP -1 = IF
            DROP ST-ADD
        THEN
        tmp-idx !
        CC-TOKEN
    ELSE
        \ Anonymous struct
        S" _anon" ST-ADD tmp-idx !
    THEN
    \ Check for body
    tok-type @ TK-PUNCT = tok-val @ P-LBRACE = AND IF
        tmp-idx @ PARSE-STRUCT-BODY
    THEN
    \ Return type word
    TY-STRUCT tmp-idx @ 9 LSHIFT OR ;

\ Parse top-level declaration
: PARSE-TOP-LEVEL ( -- )
    \ Handle typedef
    tok-type @ TK-KW = tok-val @ KW-TYPEDEF = AND IF
        PARSE-TYPEDEF EXIT
    THEN
    \ Handle standalone struct definition
    tok-type @ TK-KW = tok-val @ KW-STRUCT = AND IF
        \ Peek to see if it's struct { ... } var; or struct name { ... }; or struct name var;
        PARSE-BASE-TYPE  \ type flags
        \ Check if just a definition (no declarator follows)
        tok-type @ TK-PUNCT = tok-val @ P-SEMI = AND IF
            2DROP CC-TOKEN EXIT  \ just struct definition
        THEN
        \ It's a declaration with struct type
        tok-type @ TK-PUNCT = tok-val @ P-LBRACE = AND IF
            \ struct with body + declarator
            OVER TYPE-BASE TY-STRUCT = IF
                OVER 9 RSHIFT 127 AND PARSE-STRUCT-BODY
            THEN
        THEN
        \ Fall through to declarator parsing
        OVER PARSE-DECLARATOR
        DUP 0= IF
            2DROP DROP 2DROP EXPECT-SEMI CC-TOKEN EXIT
        THEN
        \ Check if function definition
        tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF
            PARSE-FUNCTION EXIT
        THEN
        \ Global variable
        UNGET-TOKEN  \ push back the token after name
        DROP 2DROP   \ cleanup
        PARSE-GLOBAL-VAR EXIT
    THEN
    \ Handle enum definition
    tok-type @ TK-KW = tok-val @ KW-ENUM = AND IF
        CC-TOKEN  \ skip 'enum'
        tok-type @ TK-IDENT = IF CC-TOKEN THEN  \ skip name
        tok-type @ TK-PUNCT = tok-val @ P-LBRACE = AND IF
            PARSE-ENUM-BODY
        THEN
        \ Check for variable declaration after enum
        tok-type @ TK-PUNCT = tok-val @ P-SEMI = AND IF
            CC-TOKEN EXIT
        THEN
        \ Could be: enum { ... } var;
        IS-TYPE-START 0= tok-type @ TK-IDENT = AND IF
            TY-INT 0 PARSE-GLOBAL-VAR EXIT
        THEN
        tok-type @ TK-PUNCT = tok-val @ P-SEMI = AND IF CC-TOKEN EXIT THEN
        EXIT
    THEN
    \ Regular declaration (int foo, int *bar, etc.)
    PARSE-BASE-TYPE  \ ( type flags )
    tmp-b !          \ save flags in tmp-b. Stack: ( type )
    \ Check for struct body inline
    DUP TYPE-BASE TY-STRUCT = IF
        tok-type @ TK-PUNCT = tok-val @ P-LBRACE = AND IF
            DUP 9 RSHIFT 127 AND PARSE-STRUCT-BODY
        THEN
    THEN
    DUP PARSE-DECLARATOR  \ ( type type' name-addr name-len )
    DUP 0= IF
        \ No declarator — might be just a type declaration (e.g., struct def)
        2DROP 2DROP
        tok-type @ TK-PUNCT = tok-val @ P-SEMI = AND IF CC-TOKEN THEN
        EXIT
    THEN
    \ Check if this is a function (next token is '(')
    tok-type @ TK-PUNCT = tok-val @ P-LPAREN = AND IF
        \ Stack: ( type type' name-addr name-len )
        \ Save name
        tmp-name-len ! tmp-name-off !
        \ Stack: ( type type' )
        NIP  \ drop base type, keep declared type. Stack: ( type' )
        tmp-b @  \ push flags. Stack: ( type' flags )
        tmp-name-off @ tmp-name-len @  \ Stack: ( type' flags name-addr name-len )
        PARSE-FUNCTION EXIT
    THEN
    \ Global variable — save first name to decl-name, then delegate
    \ Stack: ( type type' name-addr name-len )
    decl-name-len !                          \ save name-len
    decl-name decl-name-len @ CMOVE         \ copy name to decl-name
    NIP   \ keep declared type' (arrays/pointers), drop base type. Stack: ( type )
    \ Register this first variable as global
    decl-name decl-name-len @ 2 PICK 0 tmp-b @ 1 AND GSYM-ADD DROP
    \ Emit .bss or check for initializer
    tok-type @ TK-PUNCT = tok-val @ P-ASSIGN = AND IF
        \ Initialized global
        S" .data" EMIT-LINE
        EMIT-INDENT S" .align 4" OUT-STR OUT-NL
        tmp-b @ 1 AND 0= IF
            EMIT-INDENT S" .global " OUT-STR decl-name decl-name-len @ OUT-STR OUT-NL
        THEN
        decl-name decl-name-len @ OUT-STR 58 OUT-CHAR OUT-NL
        CC-TOKEN  \ skip =
        tok-type @ TK-NUM = IF
            EMIT-INDENT S" .word " OUT-STR tok-val @ OUT-SNUM OUT-NL
            CC-TOKEN
        ELSE tok-type @ TK-STR = IF
            EMIT-INDENT S" .word .Lstr_" OUT-STR tok-val @ OUT-NUM OUT-NL
            CC-TOKEN
        ELSE
            EMIT-INDENT S" .word 0" OUT-STR OUT-NL
            PARSE-ASSIGN-EXPR  \ consume non-constant initializer expression
            LVAL-TO-RVAL
        THEN THEN
        S" .text" EMIT-LINE
    ELSE
        \ Uninitialized global (.bss)
        S" .bss" EMIT-LINE
        EMIT-INDENT S" .align 4" OUT-STR OUT-NL
        tmp-b @ 1 AND 0= IF
            EMIT-INDENT S" .global " OUT-STR decl-name decl-name-len @ OUT-STR OUT-NL
        THEN
        decl-name decl-name-len @ OUT-STR 58 OUT-CHAR OUT-NL
        EMIT-INDENT S" .space " OUT-STR DUP TYPE-SIZE OUT-NUM OUT-NL
        S" .text" EMIT-LINE
    THEN
    \ Check for comma-separated additional declarations
    tok-type @ TK-PUNCT = tok-val @ P-COMMA = AND IF
        CC-TOKEN
        tmp-b @  \ push flags
        PARSE-GLOBAL-VAR
    ELSE
        DROP  \ drop type
        EXPECT-SEMI CC-TOKEN
    THEN ;

\ ============================================================
\ SECTION 13: String Literal Emission
\ ============================================================

\ Emit all string literals as .rodata
: EMIT-STRINGS ( -- )
    str-count @ 0= IF EXIT THEN
    \ Stage 2 assembler currently supports .data/.text, not .rodata.
    S" .data" EMIT-LINE
    str-count @ 0 ?DO
        EMIT-INDENT S" .align 4" OUT-STR OUT-NL
        S" .Lstr_" OUT-STR I OUT-NUM 58 OUT-CHAR OUT-NL
        \ Emit string bytes on one .byte line (Stage 2 now supports comma lists)
        I CELLS strlit-off + @ tmp-a !
        I CELLS strlit-len + @ tmp-b !  \ includes NUL
        EMIT-INDENT S" .byte " OUT-STR
        tmp-b @ 0 ?DO
            I 0<> IF 44 OUT-CHAR 32 OUT-CHAR THEN
            str-pool tmp-a @ + I + C@ OUT-NUM
        LOOP
        OUT-NL
    LOOP
    S" .text" EMIT-LINE ;

\ ============================================================
\ SECTION 14: Main Driver
\ ============================================================

: CC-INIT ( -- )
    0 inp-len ! 0 inp-pos !
    0 out-len !
    0 mac-cnt ! 0 mname-ptr ! 0 mval-ptr !
    0 pinfo-cnt ! 0 pname-sptr !
    0 gsym-cnt ! 0 gsym-nptr !
    0 lsym-cnt ! 0 lsym-nptr !
    0 st-cnt ! 0 st-nptr !
    0 fld-cnt ! 0 fld-nptr !
    0 enum-cnt ! 0 enum-nptr !
    0 td-cnt ! 0 td-nptr !
    0 str-pool-ptr ! 0 str-count !
    0 label-cnt !
    0 frame-size ! 0 func-nargs !
    0 local-offset ! 0 in-function !
    0 break-sp ! 0 cont-sp ! 0 switch-sp !
    0 has-peek !
    0 pp-skip ! 0 pp-nest ! 0 pp-sp !
    1 cc-line ! 0 cc-errors !
    0 inc-depth ! 0 mexp-depth !
    0 is-lvalue !
    TY-INT expr-type !
    \ Pre-define some common macros
    S" NULL" S" ((void *)0)" MAC-ADD-OBJ
    S" __SLOW32__" S" 1" MAC-ADD-OBJ
    S" __BOOTSTRAP_CC__" S" 1" MAC-ADD-OBJ
;

: LOAD-SOURCE ( addr u -- )
    R/O OPEN-FILE IF S" cannot open source file" CC-ERR EXIT THEN
    >R
    inp-buf INP-SZ R@ READ-FILE IF R> CLOSE-FILE DROP S" read error" CC-ERR EXIT THEN
    inp-len !
    R> CLOSE-FILE DROP
    0 inp-pos !
    1 cc-line ! ;

: WRITE-OUTPUT ( addr u -- )
    26 OPEN-FILE IF S" cannot open output file" CC-ERR EXIT THEN
    out-fh !
    out-buf out-len @ out-fh @ WRITE-FILE DROP
    out-fh @ CLOSE-FILE DROP ;

\ Main compilation entry point
: COMPILE-FILE ( src-addr src-u out-addr out-u -- )
    2>R  \ save output filename
    CC-INIT
    ." [CC] Loading source..." CR
    LOAD-SOURCE
    cc-errors @ IF 2R> 2DROP EXIT THEN
    ." [CC] Source loaded: " inp-len @ . ." bytes" CR
    \ Emit assembly header
    S" # Generated by Stage 4 Bootstrap C Compiler" EMIT-LINE
    S" .text" EMIT-LINE
    \ Parse translation unit
    CC-TOKEN  \ get first token
    BEGIN
        tok-type @ TK-EOF <>
    WHILE
        PARSE-TOP-LEVEL
    REPEAT
    \ Emit string literals
    EMIT-STRINGS
    \ Report results
    cc-errors @ 0= IF
        ." Compilation successful." CR
        ." Output: " out-len @ . ." bytes of assembly" CR
        2R> WRITE-OUTPUT
    ELSE
        ." Compilation failed with " cc-errors @ . ." errors" CR
        2R> 2DROP
    THEN ;
