identification division.
program-id. p.
*> A data-pointer receiver takes a data-pointer value; ADDRESS OF
*> PROGRAM is a program-pointer's (2023 14.9.39.3 rules 17, 21).
data division.
working-storage section.
01  dp       usage pointer.
procedure division.
    set dp to address of program "x"
    stop run.
end program p.
