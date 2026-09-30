       identification division.
       program-id. c5bad.
      * COMP-5 takes a PICTURE of 9s or of Xs (Micro Focus): an edited
      * one is refused, the message no longer citing 13.18.60.3 rule 3,
      * which is about BINARY, COMP and PACKED-DECIMAL.
       data division.
       working-storage section.
       01 i pic x9 comp-5.
       procedure division.
           stop run.
