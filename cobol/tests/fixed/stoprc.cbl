       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOPRC.
      * STOP RUN identifier: the process exit status (GitHub #35). The
      * Open Systems suite ends every program through STOP RUN JCL-CODE
      * and its menu scripts branch on the status. Not in the 1985 text
      * (RM/COBOL; RETURNING is 2002): default dialect.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JCL-CODE PIC 999 VALUE 3.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "LEAVING WITH " JCL-CODE.
           STOP RUN JCL-CODE.
