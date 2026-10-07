identification division.
program-id. p-para-ambiguous.
*> A paragraph-name declared in two sections, referenced from outside
*> both: qualification is required unless the section containing the
*> reference contains the paragraph (2023 8.4.2.2 rule 6).
procedure division.
    go to p1.
s1 section.
p1.
    display "a".
s2 section.
p1.
    display "b"
    goback.
