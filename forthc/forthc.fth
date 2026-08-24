\ forthc.fth — the native Forth compiler, milestone M1.
\ docs/plans/forthc.md is the charter; this file is the front half.
\
\ Runs on the DTC kernel like asm.fth/cc.fth do, reads a closed-world
\ Forth source file, and emits SLOW-32 assembly text: subroutine-
\ threaded colon words, inlined primitives, real branches later (M2).
\ The output assembles with any of the three assemblers and links
\ standalone (its own _start, stacks, and dot routine — no libc).
\
\ Register model matches the kernel: DSP=r28 (TOS at [r28], grows
\ down), RSP=r27. Colon words save/restore r31 on the r-stack so
\ nested calls work. (Yes, leaf words could skip that — that is an
\ M3-gated optimization per the charter: measure, then shave.)
\
\ Pipeline (see compile.sh):
\   cat prelude.fth forthc.fth - <<< 'S" prog.fth" S" prog.s" FORTHC BYE'
\     | slow32-fast kernel.s32x
\   slow32asm prog.s prog.s32o && s32-ld -o prog.s32x prog.s32o

DECIMAL

\ --- buffers ---------------------------------------------------------
131072 CONSTANT INP-SZ
262144 CONSTANT OUT-SZ
CREATE inp-buf INP-SZ ALLOT
CREATE out-buf OUT-SZ ALLOT
VARIABLE inp-len   VARIABLE ipos
VARIABLE out-len
VARIABLE fc-errors
VARIABLE have-main

: FC-ERR ( addr u -- ) ." forthc: " TYPE CR 1 fc-errors +! ABORT ;

\ --- output text (cc.fth's idiom) ------------------------------------
: OUT-CHAR ( ch -- )
    out-len @ OUT-SZ >= IF DROP S" output buffer full" FC-ERR EXIT THEN
    out-buf out-len @ + C!  1 out-len +! ;
: OUT-STR ( addr u -- ) 0 ?DO DUP I + C@ OUT-CHAR LOOP DROP ;
: OUT-NL  10 OUT-CHAR ;
: OUT-SNUM ( n -- )
    DUP 0< IF 45 OUT-CHAR NEGATE
        DUP 0< IF DROP S" 2147483648" OUT-STR EXIT THEN THEN
    DUP 10 >= IF DUP 10 / RECURSE THEN
    10 MOD 48 + OUT-CHAR ;
: RLINE ( addr u -- )  OUT-STR OUT-NL ;              \ flush-left line
: ALINE ( addr u -- )  S"     " OUT-STR RLINE ;      \ indented line

\ --- source loading (cc.fth's idiom) ---------------------------------
: LOAD-SOURCE ( addr u -- )
    R/O OPEN-FILE IF S" cannot open source file" FC-ERR EXIT THEN >R
    R@ FILE-SIZE IF R> CLOSE-FILE DROP S" cannot stat source" FC-ERR EXIT THEN
    DROP
    DUP INP-SZ > IF DROP R> CLOSE-FILE DROP S" source too large" FC-ERR EXIT THEN
    inp-buf OVER R@ READ-FILE IF R> CLOSE-FILE DROP S" read error" FC-ERR EXIT THEN
    DUP inp-len !
    SWAP <> IF R> CLOSE-FILE DROP S" short read" FC-ERR EXIT THEN
    R> CLOSE-FILE DROP
    0 ipos ! ;

: WRITE-OUTPUT ( addr u -- )
    26 OPEN-FILE IF S" cannot open output file" FC-ERR EXIT THEN >R
    out-buf out-len @ R@ WRITE-FILE IF
        R> CLOSE-FILE DROP S" write error" FC-ERR EXIT THEN
    R> CLOSE-FILE DROP ;

\ --- tokenizer -------------------------------------------------------
CREATE tok-buf 64 ALLOT   VARIABLE tok-len

: SRC-CH ( -- ch ) inp-buf ipos @ + C@ ;
: SRC-END? ( -- f ) ipos @ inp-len @ >= ;
: UPC ( ch -- ch' ) DUP 97 123 WITHIN IF 32 - THEN ;

: SKIP-WS   BEGIN SRC-END? IF EXIT THEN SRC-CH 32 > IF EXIT THEN
            1 ipos +! AGAIN ;
: SKIP-EOL  BEGIN SRC-END? IF EXIT THEN SRC-CH 10 = IF 1 ipos +! EXIT THEN
            1 ipos +! AGAIN ;
: SKIP-PAREN BEGIN SRC-END? IF EXIT THEN SRC-CH 41 = IF 1 ipos +! EXIT THEN
            1 ipos +! AGAIN ;

: RAW-TOK ( -- addr u )   \ collect one uppercased whitespace-free token
    0 tok-len !
    BEGIN
        SRC-END? IF tok-buf tok-len @ EXIT THEN
        SRC-CH 33 < IF tok-buf tok-len @ EXIT THEN
        tok-len @ 63 >= IF S" token too long" FC-ERR THEN
        SRC-CH UPC tok-buf tok-len @ + C!  1 tok-len +!  1 ipos +!
    AGAIN ;

: NEXT-TOK ( -- addr u )  \ u=0 at end of source; comments skipped
    BEGIN
        SKIP-WS RAW-TOK
        DUP 0= IF EXIT THEN
        2DUP S" \" COMPARE 0= IF 2DROP SKIP-EOL ELSE
        2DUP S" (" COMPARE 0= IF 2DROP SKIP-PAREN ELSE
        EXIT THEN THEN
    AGAIN ;

\ --- number parsing (decimal, optional leading minus) ----------------
VARIABLE num-acc  VARIABLE num-neg  VARIABLE num-ok
: TOK-NUM ( addr u -- n true | false )
    0 num-acc !  0 num-neg !  TRUE num-ok !
    DUP 0= IF 2DROP FALSE EXIT THEN
    OVER C@ 45 = IF
        DUP 1 = IF 2DROP FALSE EXIT THEN
        1 num-neg !  1 - SWAP 1 + SWAP THEN
    0 ?DO
        DUP I + C@ DUP 48 58 WITHIN 0= IF
            DROP FALSE num-ok ! LEAVE THEN
        48 - num-acc @ 10 * + num-acc !
    LOOP DROP
    num-ok @ IF num-acc @ num-neg @ IF NEGATE THEN TRUE
    ELSE FALSE THEN ;

\ --- word table: [id:cell][len:byte][name...] ------------------------
CREATE wtab 8192 ALLOT   VARIABLE wt-len   VARIABLE nwords
0 wt-len !  0 nwords !

: W-ADD ( addr u -- id )
    nwords @ >R
    wt-len @ 6 + OVER + 8192 > IF S" too many words" FC-ERR THEN
    R@ wtab wt-len @ + !        4 wt-len +!
    DUP wtab wt-len @ + C!      1 wt-len +!
    ( addr u ) DUP >R wtab wt-len @ + SWAP CMOVE R> wt-len +!
    1 nwords +!  R> ;

VARIABLE fw-cur
: FIND-WORD ( addr u -- id true | false )
    0 fw-cur !
    BEGIN fw-cur @ wt-len @ < WHILE
        wtab fw-cur @ + 4 + C@ >R                  ( a u ) ( R: len )
        2DUP  wtab fw-cur @ + 5 +  R@  COMPARE 0= IF
            R> DROP
            wtab fw-cur @ + @  NIP NIP TRUE EXIT THEN
        R> 5 + fw-cur +!
    REPEAT 2DROP FALSE ;

\ --- emission --------------------------------------------------------
: EMIT-LABEL ( id -- )  S" FW" OUT-STR OUT-SNUM 58 OUT-CHAR OUT-NL ;
: EMIT-CALL  ( id -- )  S"     jal r31, FW" OUT-STR OUT-SNUM OUT-NL ;

\ --- local labels + control-flow stack (M2) --------------------------
VARIABLE nlabels
: NEWLBL ( -- n ) nlabels @ 1 nlabels +! ;
: OUT-LBL  ( n -- ) S" FL" OUT-STR OUT-SNUM ;
: EMIT-LBL ( n -- ) OUT-LBL 58 OUT-CHAR OUT-NL ;
: EMIT-JMP ( n -- ) S"     jal r0, " OUT-STR OUT-LBL OUT-NL ;
: EMIT-BEQZ ( n -- )  \ pop TOS, branch to FLn if it is zero
    S" ldw r1, r28, 0" ALINE  S" addi r28, r28, 4" ALINE
    S"     beq r1, r0, " OUT-STR OUT-LBL OUT-NL ;

\ Entries are [a][b][c][type], 16 bytes each.
1 CONSTANT CT-IF   2 CONSTANT CT-BEGIN
3 CONSTANT CT-WHILE  4 CONSTANT CT-DO
CREATE ctl-stk 512 ALLOT   VARIABLE ctl-sp  0 ctl-sp !

: CPUSH ( a b c type -- )
    ctl-sp @ 496 > IF S" control stack overflow" FC-ERR THEN
    ctl-stk ctl-sp @ + >R
    R@ 12 + !  R@ 8 + !  R@ 4 + !  R> !
    16 ctl-sp +! ;

: CPOP ( type -- a b c )
    ctl-sp @ 0= IF S" unbalanced control structure" FC-ERR THEN
    -16 ctl-sp +!
    ctl-stk ctl-sp @ + >R
    R@ 12 + @ <> IF S" mismatched control structure" FC-ERR THEN
    R@ @  R@ 4 + @  R> 8 + @ ;

VARIABLE cur-id     \ current definition's word id (RECURSE)
VARIABLE cur-exit   \ current definition's exit label (EXIT and ;)

\ --- structure words -------------------------------------------------
: C-IF    NEWLBL DUP EMIT-BEQZ 0 0 CT-IF CPUSH ;
: C-ELSE  CT-IF CPOP 2DROP NEWLBL DUP EMIT-JMP SWAP EMIT-LBL
          0 0 CT-IF CPUSH ;
: C-THEN  CT-IF CPOP 2DROP EMIT-LBL ;
: C-BEGIN NEWLBL DUP EMIT-LBL 0 0 CT-BEGIN CPUSH ;
: C-AGAIN CT-BEGIN CPOP 2DROP EMIT-JMP ;
: C-UNTIL CT-BEGIN CPOP 2DROP EMIT-BEQZ ;
: C-WHILE CT-BEGIN CPOP 2DROP NEWLBL DUP EMIT-BEQZ
          0 0 CT-WHILE CPUSH  0 0 CT-BEGIN CPUSH ;
: C-REPEAT CT-BEGIN CPOP 2DROP EMIT-JMP
           CT-WHILE CPOP 2DROP EMIT-LBL ;

\ DO frame: a=start-label b=leave-label c=skip-label.
\ rstack layout inside a loop matches the kernel: [r27]=index,
\ [r27+4]=limit; J's outer index sits at [r27+8].
: EMIT-DO-PUSH
    S" ldw r1, r28, 0" ALINE      \ index (top of data stack)
    S" ldw r2, r28, 4" ALINE      \ limit
    S" addi r28, r28, 8" ALINE
    S" addi r27, r27, -8" ALINE
    S" stw r27, r1, 0" ALINE
    S" stw r27, r2, 4" ALINE ;

: C-DO
    EMIT-DO-PUSH
    NEWLBL NEWLBL NEWLBL          ( start leave skip )
    2 PICK EMIT-LBL
    CT-DO CPUSH ;

: C-?DO
    S" ldw r1, r28, 0" ALINE
    S" ldw r2, r28, 4" ALINE
    S" addi r28, r28, 8" ALINE
    NEWLBL NEWLBL NEWLBL          ( start leave skip )
    DUP S"     beq r1, r2, " OUT-STR OUT-LBL OUT-NL
    S" addi r27, r27, -8" ALINE
    S" stw r27, r1, 0" ALINE
    S" stw r27, r2, 4" ALINE
    2 PICK EMIT-LBL
    CT-DO CPUSH ;

\ The kernel's boundary-cross test, inlined. Fallthrough exits and
\ pops the loop frame; LEAVE jumps straight to that pop.
: EMIT-LOOP-TAIL ( start leave skip -- )
    S" sub r3, r1, r2" ALINE
    S" sub r5, r4, r2" ALINE
    S" xor r6, r3, r5" ALINE
    S" slt r6, r6, r0" ALINE
    OVER S"     bne r6, r0, " OUT-STR OUT-LBL OUT-NL
    S" stw r27, r4, 0" ALINE
    ROT EMIT-JMP                  ( leave skip )
    SWAP EMIT-LBL                 ( skip )
    S" addi r27, r27, 8" ALINE
    EMIT-LBL ;

: C-LOOP
    CT-DO CPOP
    S" ldw r1, r27, 0" ALINE
    S" ldw r2, r27, 4" ALINE
    S" addi r4, r1, 1" ALINE
    EMIT-LOOP-TAIL ;

: C-+LOOP
    CT-DO CPOP
    S" ldw r7, r28, 0" ALINE      \ increment from data stack
    S" addi r28, r28, 4" ALINE
    S" ldw r1, r27, 0" ALINE
    S" ldw r2, r27, 4" ALINE
    S" add r4, r1, r7" ALINE
    EMIT-LOOP-TAIL ;

: FIND-DO ( -- leave-lbl )  \ innermost DO frame, without popping it
    ctl-sp @
    BEGIN DUP 0> WHILE
        16 -
        DUP ctl-stk + 12 + @ CT-DO = IF
            ctl-stk + 4 + @ EXIT THEN
    REPEAT
    DROP 0 S" LEAVE outside DO" FC-ERR ;
: C-LEAVE FIND-DO EMIT-JMP ;

: P-I   S" ldw r1, r27, 0" ALINE
        S" addi r28, r28, -4" ALINE  S" stw r28, r1, 0" ALINE ;
: P-J   S" ldw r1, r27, 8" ALINE
        S" addi r28, r28, -4" ALINE  S" stw r28, r1, 0" ALINE ;
: P->R  S" ldw r1, r28, 0" ALINE  S" addi r28, r28, 4" ALINE
        S" addi r27, r27, -4" ALINE  S" stw r27, r1, 0" ALINE ;
: P-R>  S" ldw r1, r27, 0" ALINE  S" addi r27, r27, 4" ALINE
        S" addi r28, r28, -4" ALINE  S" stw r28, r1, 0" ALINE ;
: P-R@  S" ldw r1, r27, 0" ALINE
        S" addi r28, r28, -4" ALINE  S" stw r28, r1, 0" ALINE ;
: P-ZCMP ( op-a op-u -- )   \ TOS := TOS <op> 0  (kernel flags: 0/1)
    S" ldw r1, r28, 0" ALINE
    S"     " OUT-STR OUT-STR S"  r1, r1, r0" OUT-STR OUT-NL
    S" stw r28, r1, 0" ALINE ;
: P-INC ( n -- )            \ TOS += n, in place
    S" ldw r1, r28, 0" ALINE
    S"     addi r1, r1, " OUT-STR OUT-SNUM OUT-NL
    S" stw r28, r1, 0" ALINE ;

: EMIT-LIT ( n -- )
    S" addi r28, r28, -4" ALINE
    DUP -2048 2048 WITHIN IF
        S"     addi r1, r0, " OUT-STR OUT-SNUM OUT-NL
    ELSE
        DUP 4095 AND DUP 2048 >= IF 4096 - THEN    ( n lo )
        2DUP - S"     lui r1, " OUT-STR 12 RSHIFT 1048575 AND OUT-SNUM OUT-NL
        DUP 0= IF DROP ELSE
            S"     addi r1, r1, " OUT-STR OUT-SNUM OUT-NL THEN
        DROP                                       \ the original n
    THEN
    S" stw r28, r1, 0" ALINE ;

: EMIT-PROLOGUE
    S" addi r27, r27, -4" ALINE
    S" stw r27, r31, 0" ALINE ;
: EMIT-EPILOGUE
    S" ldw r31, r27, 0" ALINE
    S" addi r27, r27, 4" ALINE
    S" jalr r0, r31, 0" ALINE
    S" " RLINE ;

\ Primitive templates: the kernel's bodies, minus `jal r0, next`.
: P-DUP   S" ldw r1, r28, 0" ALINE
          S" addi r28, r28, -4" ALINE
          S" stw r28, r1, 0" ALINE ;
: P-DROP  S" addi r28, r28, 4" ALINE ;
: P-SWAP  S" ldw r1, r28, 0" ALINE  S" ldw r2, r28, 4" ALINE
          S" stw r28, r2, 0" ALINE  S" stw r28, r1, 4" ALINE ;
: P-OVER  S" ldw r1, r28, 4" ALINE
          S" addi r28, r28, -4" ALINE
          S" stw r28, r1, 0" ALINE ;
: P-BINOP ( op-addr op-u -- )   \ TOS := second <op> top
    S" ldw r1, r28, 0" ALINE
    S" ldw r2, r28, 4" ALINE
    S" addi r28, r28, 4" ALINE
    S"     " OUT-STR OUT-STR S"  r2, r2, r1" OUT-STR OUT-NL
    S" stw r28, r2, 0" ALINE ;
: P-FETCH S" ldw r1, r28, 0" ALINE  S" ldw r1, r1, 0" ALINE
          S" stw r28, r1, 0" ALINE ;
: P-STORE S" ldw r1, r28, 0" ALINE  S" ldw r2, r28, 4" ALINE
          S" stw r1, r2, 0" ALINE   S" addi r28, r28, 8" ALINE ;
: P-CFETCH S" ldw r1, r28, 0" ALINE S" ldbu r1, r1, 0" ALINE
          S" stw r28, r1, 0" ALINE ;
: P-CSTORE S" ldw r1, r28, 0" ALINE S" ldw r2, r28, 4" ALINE
          S" stb r1, r2, 0" ALINE   S" addi r28, r28, 8" ALINE ;
: P-EMIT  S" ldw r1, r28, 0" ALINE  S" addi r28, r28, 4" ALINE
          S" debug r1" ALINE ;
: P-CR    S" addi r1, r0, 10" ALINE S" debug r1" ALINE ;
: P-DOT   S" jal r31, FDOT" ALINE ;

\ --- the compiler ----------------------------------------------------
: COMPILE-TOK ( addr u -- )
    2DUP S" DUP"  COMPARE 0= IF 2DROP P-DUP  EXIT THEN
    2DUP S" DROP" COMPARE 0= IF 2DROP P-DROP EXIT THEN
    2DUP S" SWAP" COMPARE 0= IF 2DROP P-SWAP EXIT THEN
    2DUP S" OVER" COMPARE 0= IF 2DROP P-OVER EXIT THEN
    2DUP S" +"    COMPARE 0= IF 2DROP S" add" P-BINOP EXIT THEN
    2DUP S" -"    COMPARE 0= IF 2DROP S" sub" P-BINOP EXIT THEN
    2DUP S" *"    COMPARE 0= IF 2DROP S" mul" P-BINOP EXIT THEN
    2DUP S" @"    COMPARE 0= IF 2DROP P-FETCH  EXIT THEN
    2DUP S" !"    COMPARE 0= IF 2DROP P-STORE  EXIT THEN
    2DUP S" C@"   COMPARE 0= IF 2DROP P-CFETCH EXIT THEN
    2DUP S" C!"   COMPARE 0= IF 2DROP P-CSTORE EXIT THEN
    2DUP S" EMIT" COMPARE 0= IF 2DROP P-EMIT EXIT THEN
    2DUP S" CR"   COMPARE 0= IF 2DROP P-CR   EXIT THEN
    2DUP S" ."    COMPARE 0= IF 2DROP P-DOT  EXIT THEN
    2DUP S" 1+"   COMPARE 0= IF 2DROP 1 P-INC  EXIT THEN
    2DUP S" 1-"   COMPARE 0= IF 2DROP -1 P-INC EXIT THEN
    2DUP S" ="    COMPARE 0= IF 2DROP S" seq"  P-BINOP EXIT THEN
    2DUP S" <>"   COMPARE 0= IF 2DROP S" sne"  P-BINOP EXIT THEN
    2DUP S" <"    COMPARE 0= IF 2DROP S" slt"  P-BINOP EXIT THEN
    2DUP S" >"    COMPARE 0= IF 2DROP S" sgt"  P-BINOP EXIT THEN
    2DUP S" <="   COMPARE 0= IF 2DROP S" sle"  P-BINOP EXIT THEN
    2DUP S" >="   COMPARE 0= IF 2DROP S" sge"  P-BINOP EXIT THEN
    2DUP S" U<"   COMPARE 0= IF 2DROP S" sltu" P-BINOP EXIT THEN
    2DUP S" AND"  COMPARE 0= IF 2DROP S" and"  P-BINOP EXIT THEN
    2DUP S" OR"   COMPARE 0= IF 2DROP S" or"   P-BINOP EXIT THEN
    2DUP S" XOR"  COMPARE 0= IF 2DROP S" xor"  P-BINOP EXIT THEN
    2DUP S" 0="   COMPARE 0= IF 2DROP S" seq" P-ZCMP EXIT THEN
    2DUP S" 0<"   COMPARE 0= IF 2DROP S" slt" P-ZCMP EXIT THEN
    2DUP S" 0<>"  COMPARE 0= IF 2DROP S" sne" P-ZCMP EXIT THEN
    2DUP S" IF"     COMPARE 0= IF 2DROP C-IF     EXIT THEN
    2DUP S" ELSE"   COMPARE 0= IF 2DROP C-ELSE   EXIT THEN
    2DUP S" THEN"   COMPARE 0= IF 2DROP C-THEN   EXIT THEN
    2DUP S" BEGIN"  COMPARE 0= IF 2DROP C-BEGIN  EXIT THEN
    2DUP S" AGAIN"  COMPARE 0= IF 2DROP C-AGAIN  EXIT THEN
    2DUP S" UNTIL"  COMPARE 0= IF 2DROP C-UNTIL  EXIT THEN
    2DUP S" WHILE"  COMPARE 0= IF 2DROP C-WHILE  EXIT THEN
    2DUP S" REPEAT" COMPARE 0= IF 2DROP C-REPEAT EXIT THEN
    2DUP S" DO"     COMPARE 0= IF 2DROP C-DO     EXIT THEN
    2DUP S" ?DO"    COMPARE 0= IF 2DROP C-?DO    EXIT THEN
    2DUP S" LOOP"   COMPARE 0= IF 2DROP C-LOOP   EXIT THEN
    2DUP S" +LOOP"  COMPARE 0= IF 2DROP C-+LOOP  EXIT THEN
    2DUP S" LEAVE"  COMPARE 0= IF 2DROP C-LEAVE  EXIT THEN
    2DUP S" I"      COMPARE 0= IF 2DROP P-I  EXIT THEN
    2DUP S" J"      COMPARE 0= IF 2DROP P-J  EXIT THEN
    2DUP S" >R"     COMPARE 0= IF 2DROP P->R EXIT THEN
    2DUP S" R>"     COMPARE 0= IF 2DROP P-R> EXIT THEN
    2DUP S" R@"     COMPARE 0= IF 2DROP P-R@ EXIT THEN
    2DUP S" EXIT"    COMPARE 0= IF 2DROP cur-exit @ EMIT-JMP EXIT THEN
    2DUP S" RECURSE" COMPARE 0= IF 2DROP cur-id @ EMIT-CALL EXIT THEN
    2DUP TOK-NUM IF NIP NIP EMIT-LIT EXIT THEN
    2DUP FIND-WORD IF NIP NIP EMIT-CALL EXIT THEN
    ." forthc: unknown word: " TYPE CR 1 fc-errors +! ABORT ;

: COMPILE-DEF
    NEXT-TOK DUP 0= IF S" name expected after :" FC-ERR THEN
    2DUP TOK-NUM IF DROP S" word name is a number" FC-ERR THEN
    2DUP FIND-WORD IF DROP S" duplicate definition" FC-ERR THEN
    2DUP W-ADD DUP cur-id ! >R
    NEWLBL cur-exit !
    S" # : " OUT-STR 2DUP OUT-STR OUT-NL
    R@ EMIT-LABEL
    S" MAIN" COMPARE 0= IF S" FMAIN:" RLINE TRUE have-main ! THEN
    R> DROP
    EMIT-PROLOGUE
    ctl-sp @ >R
    BEGIN
        NEXT-TOK DUP 0= IF S" missing ;" FC-ERR THEN
        2DUP S" ;" COMPARE 0= IF
            2DROP
            ctl-sp @ R> <> IF S" unbalanced structure at ;" FC-ERR THEN
            cur-exit @ EMIT-LBL
            EMIT-EPILOGUE EXIT THEN
        COMPILE-TOK
    AGAIN ;

: EMIT-PREAMBLE
    S" # generated by forthc (M1)" RLINE
    S" .text" RLINE
    S" .global _start" RLINE
    S" _start:" RLINE
    S" lui r28, %hi(fdstack_top)" ALINE
    S" addi r28, r28, %lo(fdstack_top)" ALINE
    S" lui r27, %hi(frstack_top)" ALINE
    S" addi r27, r27, %lo(frstack_top)" ALINE
    S" jal r31, FMAIN" ALINE
    S" addi r1, r0, 0" ALINE
    S" halt" ALINE
    S" " RLINE
    S" # . : signed decimal + trailing space, via debug" RLINE
    S" FDOT:" RLINE
    S" ldw r1, r28, 0" ALINE
    S" addi r28, r28, 4" ALINE
    S" addi r5, r0, 10" ALINE
    S" bge r1, r0, FDOT_pos" ALINE
    S" addi r2, r0, 45" ALINE
    S" debug r2" ALINE
    S" sub r1, r0, r1" ALINE
    S" FDOT_pos:" RLINE
    S" addi r3, r0, 0" ALINE
    S" FDOT_split:" RLINE
    S" rem r4, r1, r5" ALINE
    S" div r1, r1, r5" ALINE
    S" addi r28, r28, -4" ALINE
    S" stw r28, r4, 0" ALINE
    S" addi r3, r3, 1" ALINE
    S" bne r1, r0, FDOT_split" ALINE
    S" FDOT_out:" RLINE
    S" ldw r4, r28, 0" ALINE
    S" addi r28, r28, 4" ALINE
    S" addi r4, r4, 48" ALINE
    S" debug r4" ALINE
    S" addi r3, r3, -1" ALINE
    S" bne r3, r0, FDOT_out" ALINE
    S" addi r4, r0, 32" ALINE
    S" debug r4" ALINE
    S" jalr r0, r31, 0" ALINE
    S" " RLINE ;

: EMIT-POSTAMBLE
    S" .bss" RLINE
    S" fdstack: .space 16384" RLINE
    S" fdstack_top:" RLINE
    S" frstack: .space 16384" RLINE
    S" frstack_top:" RLINE
    S" fpad: .space 4" RLINE ;

: FORTHC ( src-addr src-u out-addr out-u -- )
    2>R
    0 fc-errors !  FALSE have-main !  0 out-len !
    0 wt-len !  0 nwords !  0 nlabels !  0 ctl-sp !
    LOAD-SOURCE
    EMIT-PREAMBLE
    BEGIN
        NEXT-TOK DUP 0<> WHILE
        2DUP S" :" COMPARE 0= IF 2DROP COMPILE-DEF ELSE
            ." forthc: only definitions at top level, got: " TYPE CR
            1 fc-errors +! ABORT THEN
    REPEAT 2DROP
    have-main @ 0= IF S" no MAIN defined" FC-ERR THEN
    EMIT-POSTAMBLE
    2R> WRITE-OUTPUT
    ." forthc: " nwords @ . ." words, " out-len @ . ." bytes of assembly" CR ;
