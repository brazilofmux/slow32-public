       identification division.
       program-id. ptn.
      * PERFORM ... TIMES takes an integer (X3.23-1985 PERFORM syntax
      * rule 4; 2023 14.9.28.3 rule 2).
       data division.
       working-storage section.
       01 n pic 9v9 value 2.5.
       procedure division.
           perform n times display "x" end-perform.
           stop run.
