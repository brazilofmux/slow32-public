       identification division.
       program-id. pia.
      * In COBOL 85 an in-line PERFORM VARYING takes no AFTER phrase
      * (X3.23-1985 PERFORM syntax rule 2); 2023 allows it.
       data division.
       working-storage section.
       01 i pic 9.
       01 j pic 9.
       procedure division.
           perform varying i from 1 by 1 until i > 2
                   after j from 1 by 1 until j > 2
               display i j
           end-perform.
           stop run.
