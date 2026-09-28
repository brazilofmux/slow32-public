identification division.
program-id. natstr.
*> STRING into a national receiver takes national sources only (2023
*> 14.9.43.3 rule 1): "b" is an alphanumeric literal.
data division.
working-storage section.
01  r pic n(4).
procedure division.
    string n"a" "b" delimited by size into r
    stop run.
