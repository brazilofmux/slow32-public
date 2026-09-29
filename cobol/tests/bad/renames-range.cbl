       identification division.
       program-id. renrange.
      * The THRU item begins no earlier than data-name-2 and ends after
      * it; so it is not inside data-name-2 (X3.23-1985 RENAMES syntax
      * rule 8; 2023 13.18.45.3 rule 11).
       data division.
       working-storage section.
       01 g.
          05 a pic x(2).
          05 b.
             10 b1 pic x.
             10 b2 pic x.
          05 c pic x(2).
       66 r renames b thru b1.
       procedure division.
           stop run.
