identification division.
program-id. excyc.
*> EXIT PERFORM CYCLE is not in an exception-checking PERFORM (2023 14.9.14.3 rule 8).
procedure division.
    perform
        exit perform cycle
    when exception ec-user-a
        continue
    end-perform
    stop run.
