*> The IDENTIFICATION DIVISION header is optional from 2002 on (2002
*> 11.1.1): a program may begin at PROGRAM-ID, a contained one too.
*> X-COBOL has 47 such programs (ISSUES 120).
program-id. noidhdr.
data division.
working-storage section.
01 greeting pic x(12) value "no header".
procedure division.
    display greeting
    call "inner"
    stop run.

program-id. inner.
procedure division.
    display "contained, no header either"
    goback.
end program inner.
end program noidhdr.
