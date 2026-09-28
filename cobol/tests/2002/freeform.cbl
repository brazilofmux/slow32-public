identification division. *> an inline comment after a header
program-id. freeform.
*> Free-form reference format (COBOL 2002 6.4; cobol ISSUES-51): comment
*> lines and inline comments (the floating indicator *>), literals
*> continued with the floating indicator "- or '- (6.2.3, 6.4.2), a
*> comment line between the parts of a literal, a doubled quote on one
*> line, and >>SOURCE FORMAT switching to fixed form and back (7.3).
data division.
working-storage section.
    *> an indented comment line
01  s1       pic x(50) value "the first part of a literal, "-
    "and its continuation".
01  s2       pic x(30) value 'single quotes '-
      *> a comment line between the parts
    'work too'.
01  s3       pic x(40) value "a doubled "" quote, "-
    "then "-
    "three lines".
procedure division.
main.
    display s1              *> inline, after an operand
    display s2
    display s3
>>SOURCE FORMAT IS FIXED
      * a fixed-form comment line (column 7)
           DISPLAY "FIXED FORM AGAIN" *> AN INLINE COMMENT
           DISPLAY "FIXED "-
      *> A FLOATING COMMENT LINE IN FIXED FORM
               "CONTINUED"
       >>SOURCE FORMAT FREE
    display "and free once more"
    stop run.
end program freeform.
