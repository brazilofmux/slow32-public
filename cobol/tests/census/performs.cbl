       identification division.
       program-id. performs.
       data division.
       working-storage section.
       77 i pic 9(4) comp.
       77 eof pic x.
       procedure division.
       main.
           perform init
           perform body varying i from 1 by 1 until i > 3
           perform a thru a-exit
           perform b thru b-exit
           go to finish.
       init.
           move 0 to i.
       body.
           if i = 2 go to skip-it end-if
           display i.
       skip-it.
           exit.
       a.
           display "a"
           if i > 1 go to a-exit end-if
           display "a2".
       a-exit.
           exit.
       b.
           display "b"
           if i > 9 go to finish end-if.
       b-exit.
           exit.
       finish.
           stop run.
       orphan.
           display "never".
