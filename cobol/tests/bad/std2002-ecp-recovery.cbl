identification division.
program-id. ecprec.
*> After an error inside an exception-checking PERFORM's WHEN phrase and
*> inside an inline PERFORM, the compiler goes on with the state as it was
*> before the failed sentence (cobol ISSUES-94 E10): the RAISE after the
*> first is not taken for one in a WHEN phrase, and the stray EXIT
*> PERFORM at the end is refused as outside any PERFORM. Exactly these
*> three errors.
procedure division.
m1.
    perform
        continue
    when exception ec-user-a
        move nosuch to nothing
    end-perform.
m2.
    raise exception ec-user-a.
    perform 2 times
        move nosuch2 to nothing
    end-perform.
    exit perform.
    stop run.
