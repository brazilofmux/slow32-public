identification division.
program-id. performcall.
*> A CALL inside a performed paragraph, to a program that performs a
*> paragraph of its own.  Paragraphs are numbered from 1 in each
*> program, so M2 and S2 had the same id, and the called program's
*> PERFORM found the caller's frame, dropped it as abandoned, and the
*> caller fell through from M2 into M3.  Each activation now keeps its
*> PERFORM frames apart (cob_perform_enter/leave).
procedure division.
m1.
    perform m2
    display "after perform"
    stop run.
m2.
    call "performsub"
    display "back in m2".
m3.
    display "WRONG: fell into m3".
end program performcall.
identification division.
program-id. performsub.
procedure division.
s1.
    perform s2
    exit program.
s2.
    display "in performsub s2".
end program performsub.
