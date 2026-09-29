       identification division.
       program-id. exitprog.
      * EXIT PROGRAM (X3.23-1985 EXIT PROGRAM general rules 1-2; 2023
      * 14.9.14.4 rules 2-3): in the run unit's first program, which no
      * calling program controls, it continues with the next statement;
      * in a called program it returns to the caller.  And a simple EXIT
      * paragraph, the only sentence in its paragraph, does nothing
      * (general rule 1).
       procedure division.
       main-para.
           display "main: before EXIT PROGRAM".
           exit program.
           display "main: after EXIT PROGRAM (continues)".
           call "inner".
           display "main: back from inner".
           perform p1 thru p1-exit.
           display "main: after the PERFORM".
           stop run.
       p1.
           display "p1".
       p1-exit.
           exit.
       identification division.
       program-id. inner.
       procedure division.
       ip.
           display "inner: before EXIT PROGRAM".
           exit program.
           display "inner: not reached".
       end program inner.
       end program exitprog.
