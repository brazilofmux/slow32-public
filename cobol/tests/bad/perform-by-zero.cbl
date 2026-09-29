       identification division.
       program-id. pbz.
      * The BY literal of PERFORM VARYING is not zero (X3.23-1985
      * PERFORM syntax rule 9; 2023 14.9.28.3 rule 6).
       data division.
       working-storage section.
       01 i pic 9.
       procedure division.
           perform varying i from 1 by 0 until i > 3
               display i
           end-perform.
           stop run.
