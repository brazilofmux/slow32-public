       identification division.
       program-id. relopen.
       environment division.
       input-output section.
       file-control.
           select r-file assign to 'tmp/r.dat'
               organization is relative
               access mode is sequential.
       data division.
       file section.
       fd  r-file.
       01  r-record  pic x(20).
       procedure division.
           open output r-file.
           stop run.
