' builtins s32-bc computes itself or types, held to the interpreter
A% = -7: B& = -100000: X = -2.5: Y# = 3.75#
PRINT INT(X); INT(Y#); INT(A%); FIX(X); FIX(-Y#); FIX(B&); INT(2.5); FIX(-0.5)
PRINT ABS(A%); ABS(B&); ABS(X); ABS(-Y#); ABS(0); ABS(-0)
PRINT SGN(A%); SGN(B&); SGN(X); SGN(Y#); SGN(0); SGN(0#)
PRINT CINT(X); CINT(3.5); CINT(-Y#); CINT(A%); CLNG(Y# * 1000000); CLNG(B&); CLNG(2.5)
PRINT CDBL(X); CDBL(A%); CSNG(Y# / 3); CSNG(A%); CSNG(1D+300); CDBL(1 / 3)
RANDOMIZE 1
FOR I = 1 TO 5: PRINT RND; RND(1); INT(RND(1) * 6) + 1: NEXT
PRINT RND(0); RND(-3); RND
PRINT LEN("abc") + 1; ASC("A") * 2; INSTR("hello", "l") + 0.5; SQR(2) * 2; SIN(1) + COS(1)
S$ = LEFT$("abcdef", 3) + MID$("abcdef", 4, 2) + RIGHT$("xyz", 1): PRINT S$; LEN(S$)
D = 0: FOR I = 1 TO 200: D = D + INT(RND * 10): NEXT: PRINT D
