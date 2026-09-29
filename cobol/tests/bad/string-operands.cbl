       identification division.
       program-id. strop.
      * STRING's operand rules (X3.23-1985 STRING syntax rules 1, 2, 5
      * and 6; 2023 14.9.43.3 rules 1, 2, 7 and 8): no ALL figurative
      * constant, nonnumeric literals, DISPLAY items, a numeric sending
      * item an integer without P, and a POINTER that can hold one more
      * than the receiver's length.
       data division.
       working-storage section.
       01 d pic x(20).
       01 a pic x(3) value "abc".
       01 k pic 9(4) comp.
       01 v pic 9v9.
       01 p1 pic 9.
       procedure division.
           string all "ab" delimited by size into d.
           string 12 delimited by size into d.
           string k delimited by size into d.
           string v delimited by size into d.
           string a delimited by size into d with pointer p1.
           stop run.
