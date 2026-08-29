       identification division.
       program-id. idxopen.
       environment division.
       input-output section.
       file-control.
           select k-file assign to 'tmp/k.dat'
               organization is indexed
               access mode is random
               record key is k-id.
       data division.
       file section.
       fd  k-file.
       01  k-record.
           05  k-id   pic 9(5).
           05  k-text pic x(10).
       procedure division.
           open output k-file.
           stop run.
