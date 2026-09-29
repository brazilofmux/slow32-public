       identification division.
       program-id. rwnested.
      * A REPORT SECTION in a contained program (the report section may
      * be specified in any program definition, 2023 13.8.3 rule 1;
      * cobol ISSUES-94, a Stage A gap).  Each program keeps its own
      * reports: the contained program's no longer overwrites its
      * container's, which generates again after the CALL.  Both print
      * files are read back.
       environment division.
       input-output section.
       file-control.
           select outf assign to "tmp/rwn-outer.prn"
               organization is line sequential.
           select bf assign to "tmp/rwn-outer.prn"
               organization is line sequential.
           select innerf assign to "tmp/rwn-inner.prn"
               organization is line sequential.
       data division.
       file section.
       fd  outf report is outer-r.
       fd  bf.
       01  bl pic x(40).
       fd  innerf.
       01  cl pic x(40).
       working-storage section.
       01  n pic 9 value 0.
       01  eof pic x.
       report section.
       rd  outer-r.
       01  od type detail line plus 1.
           05 column 1 pic x(12) value "outer line".
           05 column 13 pic 9 source n.
       procedure division.
           open output outf.
           initiate outer-r.
           move 1 to n.
           generate od.
           call "inner".
           move 2 to n.
           generate od.
           terminate outer-r.
           close outf.
           display "=== outer".
           move "n" to eof.
           open input bf.
           perform until eof = "y"
               read bf at end move "y" to eof
                   not at end display bl
               end-read
           end-perform.
           close bf.
           display "=== inner".
           move "n" to eof.
           open input innerf.
           perform until eof = "y"
               read innerf at end move "y" to eof
                   not at end display cl
               end-read
           end-perform.
           close innerf.
           stop run.
       identification division.
       program-id. inner.
       environment division.
       input-output section.
       file-control.
           select qf assign to "tmp/rwn-inner.prn"
               organization is line sequential.
       data division.
       file section.
       fd  qf report is inner-r.
       working-storage section.
       01  k pic 9 value 7.
       report section.
       rd  inner-r.
       01  idt type detail line plus 1.
           05 column 3 pic x(10) value "inner line".
           05 column 14 pic 9 source k.
       procedure division.
           open output qf.
           initiate inner-r.
           generate idt.
           terminate inner-r.
           close qf.
           exit program.
       end program inner.
       end program rwnested.
