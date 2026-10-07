identification division.
program-id. p.
*> CALL identifier: the identifier is an alphanumeric item holding the
*> name, or a program-pointer (2023 14.9.4); a data-pointer is neither.
data division.
working-storage section.
01  dp       usage pointer.
procedure division.
    call dp
    stop run.
end program p.
