       identification division.
       program-id. relkeyrec.
       environment division.
       input-output section.
       file-control.
           select r-file assign to 'tmp/r.dat'
               organization is relative
               access mode is random
               relative key is r-num.
       data division.
       file section.
       fd  r-file.
       01  r-record.
           05  r-num     pic 9(4).
           05  r-text    pic x(16).
       procedure division.
           open output r-file.
           stop run.
