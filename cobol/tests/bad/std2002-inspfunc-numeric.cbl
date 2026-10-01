identification division.
program-id. ifn.
*> INSPECT's subject is alphanumeric or national (2023 14.9.22.3 rule 1).
data division.
working-storage section.
01 n pic 99.
procedure division.
    inspect function length("abc") tallying n for all "3"
    stop run.
