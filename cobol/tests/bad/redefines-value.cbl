       identification division.
       program-id. redefv.
      * No VALUE clause in a REDEFINES entry or below it, but at level 88
      * (X3.23-1985 REDEFINES syntax rule 9; 2023 13.18.44.3 rule 9).
       data division.
       working-storage section.
       01 g.
          05 k pic x(4).
          05 l redefines k.
             10 l1 pic x(4) value "ab".
                88 l1-ok value "ok".
       procedure division.
           stop run.
