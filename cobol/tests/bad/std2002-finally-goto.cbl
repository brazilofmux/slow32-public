identification division.
program-id. fgo.
*> No statement in a FINALLY phrase transfers control out of the PERFORM
*> (2023 14.9.28.4 rule 16).
procedure division.
m1.
    perform
        continue
    when exception ec-user-a
        continue
    finally
        go to m2
    end-perform.
m2.
    stop run.
