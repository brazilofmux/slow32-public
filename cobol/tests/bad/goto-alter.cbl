       identification division.
       program-id. gotalt.
      * GO TO and ALTER (X3.23-1985 GO TO syntax rule 2, ALTER syntax
      * rule 1): an unconditional GO TO ends its sequence; a paragraph
      * that ALTER names holds one sentence, a GO TO without DEPENDING.
       data division.
       working-storage section.
       01 a pic 9 value 1.
       procedure division.
       p0.
           go to p9 display "never".
       p1.
           display "x".
       p2.
           go to p9 p8 depending on a.
       p3.
           alter p1 to proceed to p8.
           alter p2 to proceed to p8.
       p8.
           continue.
       p9.
           stop run.
