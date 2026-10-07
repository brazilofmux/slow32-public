identification division.
program-id. p-std2014-zero-search.
*> The value compared in a SEARCH ALL WHEN is not a zero-length literal
*> (2023 14.9.37.3 rule 13).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.
01 t.
   05 e pic x(3) occurs 3 ascending key e indexed by i value "b".
procedure division.
    search all e when e(i) = "" display "found" end-search
    stop run.
