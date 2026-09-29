identification division.
program-id. exr.
*> EXIT PROGRAM RAISING (2023 14.9.14 format 2) propagates an exception
*> to the caller: not implemented yet, refused by name.
procedure division.
    exit program raising exception ec-size-overflow
    stop run.
