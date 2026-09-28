identification division.
program-id. ecunk.
*> An exception-name must be one of Table 13's, or EC-USER-suffix.
procedure division.
    raise exception ec-sizes-overflow
    stop run.
