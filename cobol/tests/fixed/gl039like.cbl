       identification division.
       program-id. gl039like.
      * gl039's shape on the fixture: line sequential in, indexed out by
      * desc-id, then the index read back at random.  GOBACK is not a
      * COBOL 85 word (IBM's; majesty uses it): default dialect.
       environment division.
       input-output section.
       file-control.
           select input-file
               assign to 'data/descriptions.txt'
               organization is line sequential
               access is sequential.
           select output-file
               assign to 'tmp/descriptions.dat'
               organization is indexed
               access mode is random
               record key is o-desc-id
               file status is file-status.
       data division.
       file section.
       fd  input-file
           block contains 2000 records.
       01  input-record.
           05 i-desc-id      pic 9(10).
           05 i-description  pic x(40).
       fd  output-file
           block contains 2000 records.
       01  output-record.
           05 o-desc-id      pic 9(10).
           05 o-description  pic x(40).
       working-storage section.
       01 file-status pic xx.
       01 ws-eof pic x value 'N'.
       01 n pic 9(4) comp value 0.
       01 i pic 9(4) comp.
       procedure division.
       main-procedure.
           open input input-file
           open output output-file
           perform read-write-records until ws-eof = 'Y'
           close input-file
           close output-file
           display n ' written, status ' file-status
           open input output-file
           perform varying i from 3 by 24 until i > 75
               move i to o-desc-id
               read output-file
                   invalid key display 'no ' o-desc-id
                   not invalid key display o-desc-id ' ' o-description
               end-read
           end-perform
           move 4 to o-desc-id
           read output-file
               invalid key display 'no ' o-desc-id ': ' file-status
           end-read
           close output-file
           goback.
       read-write-records.
           read input-file
               at end
                   move 'Y' to ws-eof
               not at end
                   move input-record to output-record
                   write output-record
                       invalid key
                           display "Error writing record: " o-desc-id
                   end-write
                   add 1 to n
           end-read.
       end program gl039like.
