identification division.
program-id. ecpr.
*> RAISE belongs to imperative-statement-1 alone (2023 14.9.29.3 rule 4).

procedure division.
    perform
        continue
    when exception ec-user-a
        raise exception ec-user-b
    end-perform
    stop run.
