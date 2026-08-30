identification division.
program-id. copyrep.
*> COPY ... REPLACING: a word by a word, a literal by a literal, and
*> ==pseudo-text== by ==pseudo-text==, matched token for token against
*> the copied text (the Library module of COBOL 85).
data division.
working-storage section.
copy repl replacing proof-rec by first-rec.
copy repl replacing proof-rec by second-rec
                    tst-fld-1 by sf-1
                    'abcd' by 'wxyz'.
copy repl replacing ==proof-rec== by ==third-rec==
                    ==05  tst-fld-1        pic 9(5).== by ==05  tf-1 pic 9(3) value 7. 05 tf-1b pic xx value 'zz'.==.
procedure division.
main.
    move 12345 to tst-fld-1 of first-rec.
    display '[' first-rec ']'.
    move 6 to sf-1 of second-rec.
    display '[' second-rec ']'.
    display '[' third-rec ']' ' ' tf-1 ' ' tf-1b.
    stop run.
