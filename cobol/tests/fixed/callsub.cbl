       identification division.
       program-id. callsub.
      * CALL ... USING between COBOL programs: BY REFERENCE addresses,
      * a group handed across, a subprogram that keeps state, nested
      * CALLs, GOBACK and EXIT PROGRAM.
       data division.
       working-storage section.
       01  a       pic s9(4) comp value 17.
       01  b       pic s9(4) comp value 25.
       01  sum-r   pic s9(6) comp.
       01  person.
           05  p-name  pic x(10) value 'ada'.
           05  p-age   pic 99 value 36.
       01  txt     pic x(12) value 'hello'.
       01  cnt     pic 99 value 0.
       procedure division.
           call 'addup' using a b sum-r.
           display 'sum=' sum-r.
           move 100 to a.
           call 'addup' using a b sum-r.
           display 'sum=' sum-r.
           call 'counter' using cnt.
           call 'counter' using cnt.
           call 'counter' using cnt.
           display 'cnt=' cnt.
           call 'greet' using person txt.
           display '[' txt '] ' person.
           call 'twoup' using a sum-r.
           display 'twoup=' sum-r.
           call 'early' using a.
           display 'a=' a.
           stop run.
       end program callsub.
