identification division.
program-id. csvsub.
*> Called from C (tests/c/calleesaved.c): a dynamic CALL, whose code
*> uses r12 and r13.
data division.
working-storage section.
01  target   pic x(8) value "csvnoop".
procedure division.
    call target
    goback.
end program csvsub.

identification division.
program-id. csvnoop.
procedure division.
    goback.
end program csvnoop.
