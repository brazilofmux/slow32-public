\ ============================================================
\ SLOW-32 Forth-Hosted Assembler — Stage 2 Bootstrap
\ ============================================================
\
\ A two-pass assembler that reads .s source files and produces
\ .s32x executable binaries.
\
\ Usage:
\   S" input.s" S" output.s32x" ASSEMBLE
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
FEFFF0   CONSTANT DEFAULT-STACK
1000000  CONSTANT DEFAULT-MEMSZ
FF0000   CONSTANT DEFAULT-MMIO
DECIMAL

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

\ === Assembler State ===
VARIABLE asm-pass       \ 1 or 2
VARIABLE asm-sect       \ current section (0=text,1=data,2=bss)
VARIABLE asm-tsz        \ .text size
VARIABLE asm-dsz        \ .data size
VARIABLE asm-bsz        \ .bss size
VARIABLE asm-errs       \ error count
VARIABLE asm-lno        \ line number
VARIABLE asm-inp-len    \ input length
VARIABLE asm-inp-pos    \ input scan position

\ Virtual addresses (computed after pass 1)
VARIABLE text-va        \ always 0
VARIABLE data-va        \ after .text, aligned
VARIABLE bss-va         \ after .data, aligned

\ Line parsing
CREATE lbuf 256 ALLOT
VARIABLE lbuf-len
VARIABLE lpos           \ current parse position

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
    BEGIN
        asm-inp-pos @ asm-inp-len @ >= IF TRUE EXIT THEN
        inp-buf asm-inp-pos @ + C@
        1 asm-inp-pos +!
        DUP 10 = IF DROP TRUE EXIT THEN
        DUP 13 = IF DROP ELSE
            lbuf lbuf-len @ + C!
            1 lbuf-len +!
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
    \ Try %hi(sym)
    2DUP IS-HI IF
        2SWAP 2DROP RESOLVE
        HALF12 + 12 RSHIFT EXIT
    THEN
    \ Try %lo(sym)
    2DUP IS-LO IF
        2SWAP 2DROP RESOLVE
        MASK12 AND EXIT
    THEN
    \ Bare symbol (absolute address)
    RESOLVE ;

\ Parse a jump target — result is PC-relative offset (for JAL)
: PARSE-TARGET ( addr u -- offset )
    2DUP PARSE-NUM IF NIP NIP EXIT THEN
    RESOLVE
    \ Subtract current PC (text section)
    text-va @ asm-tsz @ + - ;

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
    >R >R
    20 LSHIFT >R       \ val << 20
    15 LSHIFT OR       \ base << 15 | opcode
    R> OR              \ | val
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

\ YIELD (R-type: opcode=80)
: DO-YIELD  80 0 0 0 ENC-R EMIT-W32 ;

\ === Pseudo-Instructions ===
\ nop = add r0, r0, r0 (opcode 0)
: DO-NOP  0 0 0 0 ENC-R EMIT-W32 ;

\ mv rd, rs = addi rd, rs, 0 (opcode 16)
: DO-MV
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN >R
    GET-TOK PARSE-REG 0= IF DROP S" bad rs" ASM-ERR R> DROP EXIT THEN
    16 R> ROT 0 ENC-I EMIT-W32 ;

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

\ la rd, symbol = lui rd, %hi(sym) + addi rd, rd, %lo(sym)
: DO-LA
    GET-TOK PARSE-REG 0= IF DROP S" bad rd" ASM-ERR EXIT THEN
    li-rd !
    GET-TOK RESOLVE
    DUP HALF12 + 12 RSHIFT
    >R 32 li-rd @ R> ENC-U EMIT-W32          \ lui rd, upper
    MASK12 AND
    >R 16 li-rd @ DUP R> ENC-I EMIT-W32 ;    \ addi rd, rd, lower

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

\ call target = lui r2, %hi(target) + jalr r31, r2, %lo(target)
: DO-CALL
    GET-TOK RESOLVE
    DUP HALF12 + 12 RSHIFT
    >R 32 2 R> ENC-U EMIT-W32           \ lui r2, upper
    MASK12 AND
    >R 65 31 2 R> ENC-I EMIT-W32 ;      \ jalr r31, r2, lower

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

: DO-WORD   GET-TOK PARSE-IMM EMIT-W32 ;

: DO-BYTE   GET-TOK PARSE-IMM 255 AND EMIT-B ;

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
    GET-TOK 2DROP ;  \ just consume the token for now

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
            DROP PARSE-TARGET
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
        PROCESS-LINE
    REPEAT ;

\ === .s32x Output ===
CREATE hdr-buf 256 ALLOT

: H! ( value offset -- )
    hdr-buf +
    OVER         255 AND OVER     C!
    OVER 8  RSHIFT 255 AND OVER 1+ C!
    OVER 16 RSHIFT 255 AND OVER 2 + C!
    SWAP 24 RSHIFT 255 AND SWAP 3 + C! ;

: WRITE-S32X ( c-addr u -- )
    \ Compute virtual addresses
    0 text-va !
    asm-tsz @ ALIGN4 data-va !
    data-va @ asm-dsz @ ALIGN4 + bss-va !

    \ Open output file
    26 OPEN-FILE THROW >R          \ 26 = write+create+trunc

    \ Build header (256 bytes, zero-filled)
    hdr-buf 256 0 FILL
    S32X-MAGIC     0 H!       \ magic "S32X"
    1         4 hdr-buf + C!   \ version = 1 (uint16 LE low byte)
    0         5 hdr-buf + C!   \ version high byte = 0
    1         6 hdr-buf + C!   \ endian = little
    50        7 hdr-buf + C!   \ machine = 0x32 = SLOW-32
    0              8 H!        \ entry = 0
    3             12 H!        \ nsections
    64            16 H!        \ sec_offset
    0             20 H!        \ str_offset
    0             24 H!        \ str_size
    FLAG-MMIO     28 H!        \ flags = MMIO
    asm-tsz @     32 H!        \ code_limit
    0             36 H!        \ rodata_limit
    data-va @ asm-dsz @ + 40 H!  \ data_limit
    DEFAULT-STACK 44 H!        \ stack_base
    DEFAULT-MEMSZ 48 H!        \ mem_size = 16MB
    bss-va @ asm-bsz @ + ALIGN4 52 H!  \ heap_base
    0             56 H!        \ stack_end
    DEFAULT-MMIO  60 H!        \ mmio_base

    \ Section 0: .text (at offset 64, 28 bytes each)
    0             64 H!        \ name_offset
    1             68 H!        \ type = CODE
    0             72 H!        \ vaddr = 0
    148           76 H!        \ file offset = 64 + 3*28 = 148
    asm-tsz @     80 H!        \ size
    asm-tsz @     84 H!        \ mem_size
    1             88 H!        \ flags = EXEC

    \ Section 1: .data (at offset 92)
    0             92 H!        \ name_offset
    2             96 H!        \ type = DATA
    data-va @    100 H!        \ vaddr
    148 asm-tsz @ + 104 H!     \ file offset
    asm-dsz @    108 H!        \ size
    asm-dsz @    112 H!        \ mem_size
    2            116 H!        \ flags = WRITE

    \ Section 2: .bss (at offset 120)
    0            120 H!        \ name_offset
    3            124 H!        \ type = BSS
    bss-va @     128 H!        \ vaddr
    0            132 H!        \ file offset = 0
    0            136 H!        \ size = 0
    asm-bsz @    140 H!        \ mem_size
    2            144 H!        \ flags = WRITE

    \ Write header + section table (148 bytes)
    hdr-buf 148 R@ WRITE-FILE THROW

    \ Write .text data
    code-buf asm-tsz @ R@ WRITE-FILE THROW

    \ Write .data data
    data-buf asm-dsz @ R@ WRITE-FILE THROW

    R> CLOSE-FILE THROW ;

\ === Main Entry Point ===
: ASSEMBLE ( input-addr input-u output-addr output-u -- )
    2SWAP
    \ Initialize
    0 sym-cnt !  0 sname-ptr !
    0 asm-errs !

    \ Load input file
    2DUP LOAD-INPUT
    2DROP

    \ Pass 1: collect labels, compute sizes
    ." Pass 1..." CR
    1 RUN-PASS
    ." Pass 1: text=" asm-tsz @ . ." data=" asm-dsz @ . ." bss=" asm-bsz @ .
    ." syms=" sym-cnt @ . CR

    \ Compute virtual addresses for pass 2 symbol resolution
    0 text-va !
    asm-tsz @ ALIGN4 data-va !
    data-va @ asm-dsz @ ALIGN4 + bss-va !

    \ Pass 2: emit code
    ." Pass 2..." CR
    2 RUN-PASS
    ." Pass 2 done." CR

    \ Check for errors
    asm-errs @ 0<> IF
        ." FAILED: " asm-errs @ . ." errors" CR
        2DROP EXIT
    THEN

    \ Write output
    ." Writing..." CR
    WRITE-S32X
    ." Done." CR ;
