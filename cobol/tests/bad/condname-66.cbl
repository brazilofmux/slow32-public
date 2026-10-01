       identification division.
       program-id. cn66.
      * A condition-name is not associated with a level 66 entry, nor
      * with a group holding items of a usage other than DISPLAY, or
      * JUSTIFIED or SYNCHRONIZED ones (X3.23-1985 VI-21, data
      * description entry general rule 2b and 2c; 2023 13.16.3 rule 24b
      * to d).  Accepted before the DATA DIVISION sweep (2026-09-30).
       data division.
       working-storage section.
       01 r.
          05 r1 pic x.
          05 r2 pic x.
       66 rr renames r1 thru r2.
          88 c value "ab".
       procedure division.
           stop run.
