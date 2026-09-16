       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPYUPPER.
      * COPY of a copybook kept under an uppercase name with no
      * extension (tests/copy/SUPPER), named as written by a bare word the
      * tokenizer lowercases: the lookup tries the name upper-cased
      * too, so ~/open's SCONFIG is found on Linux as on macOS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SUPPER.
       COPY SUPPER REPLACING ==UPPER-ITEM== BY ==SECOND-ITEM==
                             ==UPPER-TEXT== BY ==SECOND-TEXT==
                             ==UPPER-NUM==  BY ==SECOND-NUM==.
       PROCEDURE DIVISION.
           DISPLAY UPPER-TEXT " " UPPER-NUM.
           MOVE "again" TO SECOND-TEXT.
           ADD 1 TO SECOND-NUM.
           DISPLAY SECOND-TEXT " " SECOND-NUM.
           STOP RUN.
