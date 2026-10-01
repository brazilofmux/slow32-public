*> The scope of program-names (2023 8.4.6.3; X3.23-1985 X-6): a
*> contained program without COMMON is named only by the program that
*> directly contains it; a COMMON one also by every program inside that
*> container, but not by itself or its own contained programs.  Out of
*> scope, the name means an outermost program of that name, and there is
*> none: ON EXCEPTION.  CALL identifier and CANCEL follow the same rules.
*> Every program was callable from anywhere before (ISSUES 120).
*> No oracle: GnuCOBOL 4.0-early-dev takes ON EXCEPTION after CALLs that
*> succeeded, and lets deep call box -- out of scope, and active -- and
*> dies with SIGSEGV (docs/oracles.md).
identification division.
program-id. progscope.
data division.
working-storage section.
01 pname pic x(8).
procedure division.
    call "plain" on exception display "outer: plain not found" end-call
    call "shared" on exception display "outer: shared not found" end-call
    call "box" on exception display "outer: box not found" end-call
    call "deep" on exception display "outer: deep not in scope" end-call
    move "plain" to pname
    call pname on exception display "outer: plain by name not found" end-call
    stop run.

identification division.
program-id. plain.
procedure division.
    display "plain runs"
    call "shared" on exception display "plain: shared not found" end-call
    call "deep" on exception display "plain: deep not in scope" end-call
    exit program.
end program plain.

identification division.
program-id. shared is common.
data division.
working-storage section.
01 pname pic x(8) value "plain".
procedure division.
    display "shared runs"
    call "plain" on exception display "shared: plain not in scope" end-call
    call pname on exception display "shared: plain by name not in scope" end-call
    exit program.
end program shared.

identification division.
program-id. box.
procedure division.
    display "box runs"
    call "deep" on exception display "box: deep not found" end-call
    exit program.

identification division.
program-id. deep.
procedure division.
    display "deep runs"
    call "shared" on exception display "deep: shared not found" end-call
    call "box" on exception display "deep: box not in scope" end-call
    exit program.
end program deep.
end program box.
end program progscope.
