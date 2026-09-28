identification division.
program-id. raiselv.
*> RAISE names a level-3 condition, not a group (2023 14.9.29.3 rule 1).
procedure division.
    raise exception ec-size
    stop run.
