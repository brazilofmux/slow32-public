identification division.
program-id. p.
*> CALL literal AS NESTED names a program contained in, or common to,
*> this one (2023 14.9.4.3 rule 15).
procedure division.
    call "elsewhere" as nested
    stop run.
end program p.
