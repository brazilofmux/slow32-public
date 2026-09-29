       identification division.
       program-id. exna.
      * A simple EXIT is a sentence by itself, the only one in its
      * paragraph (X3.23-1985 EXIT syntax rules 1-2; 2023 14.9.14.3
      * rule 1).
       procedure division.
       m1.
           display "x".
           exit.
       m2.
           stop run.
