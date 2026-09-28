identification division.
program-id. fnrmnum.
*> Only an alphanumeric function's result can be reference-modified
*> (X3.23a-1989; cobol ISSUES-54).
procedure division.
    display function integer(3.5)(1:1)
    stop run.
