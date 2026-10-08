identification division.
program-id. p-std2002-resume-decl-proc.
*> RESUME AT names a nondeclarative procedure (14.9.33.3 rule 3).
data division.
working-storage section.
01 t pic x(5) value "abcde".
01 n pic 9 value 9.
procedure division.
declaratives.
b-sec section.
    use after exception condition ec-bound-ref-mod.
b-para.
    resume at b-para.
end declaratives.
main section.
    >>turn ec-bound-ref-mod checking on
    move t(n:1) to t.
    stop run.
