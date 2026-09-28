identification division.
program-id. natstring.
*> STRING and UNSTRING of national data (2023 14.9.43, 14.9.48; cobol
*> ISSUES-69): every operand national, a figurative constant one
*> national character; POINTER, COUNT IN and TALLYING IN count
*> characters, and a delimiter matches only at a character boundary --
*> N"AB" (00 41 00 42) holds the bytes 41 00, but not NX"4100".
*> No oracle (docs/national.md).
data division.
working-storage section.
01  r        pic n(12).
01  p        pic 99.
01  src      pic n(12) value n"東京,大阪,,名古屋".
01  a        pic n(4).
01  b        pic n(4).
01  c        pic n(4).
01  d        pic n(4).
01  dl       pic n(1).
01  ca       pic 99.
01  cb       pic 99.
01  t        pic 99.
01  ab       pic n(2) value n"AB".
procedure division.
main.
    move spaces to r
    string n"日本" delimited by size
           n"語です。" delimited by n"で"
           space delimited by size
           n"ok" delimited by size
        into r
    end-string
    display "string: [" r "]"
    move 3 to p
    move all n"-" to r
    string n"abc" n"xyz" delimited by size into r with pointer p
    display "pointer: [" r "] " p
    move 11 to p
    string n"長すぎる" delimited by size into r with pointer p
        on overflow display "overflow at " p
    end-string
    move 0 to t
    unstring src delimited by n"," or all space
        into a delimiter in dl count in ca
             b count in cb
             c
             d
        tallying in t
    end-unstring
    display "unstring: [" a "] [" dl "] " ca " [" b "] " cb " [" c "] [" d "] " t
    move 4 to p
    unstring src delimited by n"," into a with pointer p
    display "from 4: [" a "] " p
    move spaces to a
    unstring ab delimited by nx"4100" into a count in ca
    display "misaligned: [" a "] " ca
    stop run.
