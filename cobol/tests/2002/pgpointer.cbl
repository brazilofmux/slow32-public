identification division.
program-id. greet is prototype.
*> Program pointers (standard-queue item 9): USAGE PROGRAM-POINTER, plain
*> and restricted TO a prototype (13.18.60); ADDRESS OF PROGRAM by a
*> literal, an item holding the name and a prototype-name (8.4.3.13),
*> NULL for a program that is not here; SET format 9, with NULL and
*> pointer to pointer; CALL through the pointer, the restricted one's
*> arguments converted by the prototype (14.8.2.3.3 rule 2); a NULL
*> pointer's CALL taking ON EXCEPTION; pointers compared (8.8.4.2.4);
*> INITIALIZE setting them to NULL; a contained program's address in
*> scope.  No oracle: GnuCOBOL 4 has no ADDRESS OF PROGRAM.
data division.
linkage section.
01  n        pic 9(3).
procedure division using n.
end program greet.

identification division.
program-id. pgpointer.
environment division.
configuration section.
repository.
    program greet.
data division.
working-storage section.
01  p1       usage program-pointer.
01  p2       usage program-pointer.
01  pg       usage program-pointer to greet.
01  nm       pic x(10) value "shout".
01  v        pic 9(5)v99 value 42.5.
01  w        pic 9(3) value 7.
01  gp       usage program-pointer.
procedure division.
    set p1 to address of program "shout"
    set p2 to address of program nm
    if p1 = p2 display "same program" else display "different" end-if
    if p1 not = null display "p1 set" end-if
    set p2 to address of program "nowhere"
    if p2 = null display "nowhere is NULL" end-if
    set pg to address of program greet
    call pg using by content v
    call pg using w + 1
    call p1 using w
    set p2 to p1
    call p2 using w
    set p1 to address of program "inner"
    call p1 using w
    set p1 to null
    call p1 using w
        on exception display "p1 is NULL: exception"
    end-call
    set gp to p2
    initialize gp
    if gp = null display "gp initialized to NULL" end-if
    stop run.

identification division.
program-id. inner.
data division.
linkage section.
01  k        pic 9(3).
procedure division using k.
    display "inner " k
    goback.
end program inner.
end program pgpointer.

identification division.
program-id. greet.
data division.
linkage section.
01  n        pic 9(3).
procedure division using n.
    display "greet " n
    goback.
end program greet.

identification division.
program-id. shout.
data division.
linkage section.
01  n        pic 9(3).
procedure division using n.
    display "shout " n
    goback.
end program shout.
