*> -warn-extensions: RETURN-CODE, an IBM and Micro Focus special
*> register, is BP-E1 (docs/behavior-points.md, class E); silent without
*> the switch.
identification division.
program-id. extrc.
procedure division.
    move 3 to return-code
    stop run.
