identification division.
program-id. p-std2002-resume-outside.
*> RESUME only in a declarative or a WHEN phrase (14.9.33.3 rule 1).
data division.
working-storage section.
01 t pic x(5) value "abcde".
01 n pic 9 value 9.
procedure division.

main section.
    >>turn ec-bound-ref-mod checking on
    move t(n:1) to t.
    resume at next statement.
    stop run.
