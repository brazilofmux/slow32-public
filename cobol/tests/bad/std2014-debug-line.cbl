       identification division.
       program-id. p-debug-line.
      * Debugging lines and WITH DEBUGGING MODE were removed from COBOL 2014
      * (E.2 item 19): the D indicator is refused under -std=2014.
       procedure division.
      d    display "debug line"
           display "normal"
           stop run.
