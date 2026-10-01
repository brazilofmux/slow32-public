       identification division.
       program-id. nopic.
      * Every elementary item has a PICTURE clause, but an index data
      * item and a RENAMES subject (X3.23-1985 VI-21, data description
      * entry syntax rule 3; 2023 13.16.3 rule 8).
       data division.
       working-storage section.
       01 g.
          05 a.
       procedure division.
           stop run.
