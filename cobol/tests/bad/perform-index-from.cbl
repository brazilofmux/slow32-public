       identification division.
       program-id. pif.
      * VARYING an index-name: the FROM identifier is an integer item
      * and a FROM literal a positive integer (X3.23-1985 PERFORM syntax
      * rule 7; 2023 14.9.28.3 rule 4).
       data division.
       working-storage section.
       01 t.
          05 e pic x occurs 5 indexed by ix.
       procedure division.
           perform varying ix from 0 by 1 until ix > 3
               display e(ix)
           end-perform.
           stop run.
