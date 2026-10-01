       identification division.
       program-id. grppic.
      * PICTURE is for an elementary item (X3.23-1985 VI-21, data
      * description entry general rule 1; 2023 13.16.3 rule 11).
       data division.
       working-storage section.
       01 g pic x(2).
          05 a pic x.
          05 b pic x.
       procedure division.
           stop run.
