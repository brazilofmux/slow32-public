       identification division.
       program-id. renlevel.
      * A RENAMES entry names items at levels 02-49, not its record
      * (X3.23-1985 RENAMES syntax rule 4; 2023 13.18.45.3 rule 5).
       data division.
       working-storage section.
       01 g.
          05 a pic x(2).
          05 b.
             10 b1 pic x.
             10 b2 pic x.
          05 c pic x(2).
       66 r renames g.
       procedure division.
           stop run.
