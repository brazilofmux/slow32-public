       identification division.
       program-id. p-debugging-mode.
      * WITH DEBUGGING MODE was removed from COBOL 2014 (E.2 item 19).
       environment division.
       configuration section.
       source-computer. x with debugging mode.
       procedure division.
           display "normal"
           stop run.
