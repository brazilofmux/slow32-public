       identification division.
       program-id. pfi.
      * FROM an index-name: the VARYING item is an integer item
      * (X3.23-1985 PERFORM syntax rule 8; 2023 14.9.28.3 rule 5).
       data division.
       working-storage section.
       01 t.
          05 e pic x occurs 5 indexed by ix.
       01 d pic 9v9.
       procedure division.
           set ix to 1.
           perform varying d from ix by 1 until d > 3
               display d
           end-perform.
           stop run.
