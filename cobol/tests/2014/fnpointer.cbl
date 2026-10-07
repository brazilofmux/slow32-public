*> Function pointers (COBOL 2014; 2023 8.5.2.7, 13.18.60 USAGE FUNCTION-
*> POINTER TO prototype, 8.4.3.12 ADDRESS OF FUNCTION, 14.9.39 format 8,
*> 8.4.3.2 the invocation): a pointer restricted to a prototype takes
*> NULL, ADDRESS OF FUNCTION of a prototype with the same signature, of
*> an identifier holding a function's externalized name (the function
*> registry at run time; a name not there is NULL), or another such
*> pointer; it is invoked as pointer(arguments), with or without the
*> word FUNCTION, through the prototype's signature; compared with NULL
*> and with another; passed to a program; INITIALIZE makes it NULL.
*> No oracle: GnuCOBOL 4 has no function pointers.
*> docs/conformance/usage.md
identification division.
function-id. binop is prototype.
data division.
linkage section.
01 a pic s9(5).
01 b pic s9(5).
01 r pic s9(7).
procedure division using a b returning r.
end function binop.

identification division.
function-id. plus.
data division.
linkage section.
01 a pic s9(5).
01 b pic s9(5).
01 r pic s9(7).
procedure division using a b returning r.
    compute r = a + b
    goback.
end function plus.

identification division.
function-id. times as "fn-times".
data division.
linkage section.
01 a pic s9(5).
01 b pic s9(5).
01 r pic s9(7).
procedure division using a b returning r.
    compute r = a * b
    goback.
end function times.

identification division.
function-id. neg.
data division.
linkage section.
01 a pic s9(5).
01 r pic s9(7).
procedure division using a returning r.
    compute r = - a
    goback.
end function neg.

identification division.
function-id. greet.
data division.
linkage section.
01 who pic x(5).
01 r pic x(12).
procedure division using who returning r.
    string "hello " who delimited by size into r
    goback.
end function greet.

identification division.
program-id. fnpointer.
environment division.
configuration section.
repository.
    function binop
    function plus
    function times as "fn-times"
    function neg
    function greet.
data division.
working-storage section.
01 op usage function-pointer to binop.
01 op2 usage function-pointer to binop.
01 np usage function-pointer to neg.
01 g usage function-pointer to greet.
01 name pic x(10).
01 x pic s9(5) value 6.
01 y pic s9(5) value 7.
01 r pic s9(7).
procedure division.
    if op = null display "op starts NULL" end-if
    set op to address of function plus
    display "plus: " op(x, y) " " function op(3, 4)
    set op to address of function times
    compute r = op(x, y) + 1 display "times + 1: " r
    move "fn-times" to name
    set op to address of function name
    display "by name: " op(2, 3)
    move "plus" to name
    set op2 to address of function name
    set op to op2
    display "op2: " op(10, 20) " " op2(1, 1)
    if op = op2 display "equal pointers" end-if
    if op not = null display "not null" end-if
    if op = address of function plus display "and plus" end-if
    set np to address of function neg
    display np(x) " " op(np(2), 1) " " function neg(np(3))
    set g to address of function greet
    display "[" g("world") "]"
    call "fnpointer-sub" using op np
    move "nobody" to name
    set op to address of function name
    if op = null display "unknown name is NULL" end-if
    initialize op2
    if op2 = null display "initialized to NULL" end-if
    initialize op2 replacing function-pointer by op
    if op2 = op display "replaced" end-if
    stop run.
identification division.
program-id. fnpointer-sub.
environment division.
configuration section.
repository.
    function binop
    function neg.
data division.
linkage section.
01 p usage function-pointer to binop.
01 q usage function-pointer to neg.
procedure division using p q.
    display "sub: " p(20, 22) " " q(1)
    goback.
end program fnpointer-sub.
end program fnpointer.
