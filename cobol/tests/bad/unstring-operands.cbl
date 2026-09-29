       identification division.
       program-id. unsop.
      * UNSTRING's operand rules (X3.23-1985 UNSTRING syntax rules 1-3
      * and 5; 2023 14.9.48.3 rules 1, 2, 4 and 6): the sending item,
      * the delimiters and DELIMITER IN items are alphanumeric; a delimiter
      * literal is nonnumeric; a receiver is alphabetic, alphanumeric or
      * numeric without P, usage display; the POINTER can hold one more
      * than the sending item's length.
       data division.
       working-storage section.
       01 s pic x(20).
       01 sn pic 9(5).
       01 d9 pic 9.
       01 r pic x(4).
       01 re pic zz9.
       01 rp pic 9pp.
       01 p1 pic 9.
       procedure division.
           unstring sn delimited by "," into r.
           unstring s delimited by d9 into r.
           unstring s delimited by 5 into r.
           unstring s delimited by "," into re.
           unstring s delimited by "," into rp.
           unstring s delimited by "," into r delimiter in d9.
           unstring s delimited by "," into r with pointer p1.
           stop run.
