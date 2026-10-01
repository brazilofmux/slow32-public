       identification division.
       program-id. secord.
      * The DATA DIVISION's sections come in their order: FILE,
      * WORKING-STORAGE, LINKAGE, (COMMUNICATION,) REPORT (X3.23-1985
      * IV-34; 2023 13.2.1).  Accepted before the DATA DIVISION sweep.
       environment division.
       input-output section.
       file-control.
           select f1 assign to "x.dat".
       data division.
       working-storage section.
       01 a pic x.
       file section.
       fd f1.
       01 r1 pic x(10).
       procedure division.
           stop run.
