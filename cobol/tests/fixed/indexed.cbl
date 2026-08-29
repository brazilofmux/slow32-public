       identification division.
       program-id. indexed.
      * INDEXED on the default path: random WRITE out of key order,
      * duplicate keys, READ by key hit and miss, REWRITE, DELETE, START
      * and READ NEXT, then the file reopened and read in key order.
       environment division.
       input-output section.
       file-control.
           select k-file assign to 'tmp/keyed.dat'
               organization is indexed
               access mode is dynamic
               record key is k-id
               file status is k-status.
           select r-file assign to 'tmp/keyed.dat'
               organization is indexed
               access mode is random
               record key is r-id
               file status is r-status.
       data division.
       file section.
       fd  k-file.
       01  k-record.
           05  k-id    pic 9(5).
           05  k-text  pic x(12).
       fd  r-file.
       01  r-record.
           05  r-id    pic 9(5).
           05  r-text  pic x(12).
       working-storage section.
       01  k-status    pic xx.
       01  r-status    pic xx.
       01  eof         pic x value 'N'.
       01  n           pic 99 comp value 0.
       procedure division.
           open output k-file.
           display 'open output: ' k-status.
           move 300 to k-id. move 'three hundred' to k-text.
           write k-record invalid key display 'x' end-write.
           move 100 to k-id. move 'one hundred' to k-text.
           write k-record invalid key display 'x' end-write.
           move 200 to k-id. move 'two hundred' to k-text.
           write k-record invalid key display 'x' end-write.
           move 100 to k-id. move 'again' to k-text.
           write k-record
               invalid key display 'duplicate 100: ' k-status
               not invalid key display 'x'
           end-write.
           move 50 to k-id. move 'fifty' to k-text.
           write k-record.
           display 'write 50: ' k-status.
           close k-file.
           display 'close: ' k-status.
      * random access through a second SELECT of the same file
           open i-o r-file.
           display 'open i-o: ' r-status.
           move 200 to r-id.
           read r-file
               invalid key display 'miss 200'
               not invalid key display 'hit ' r-id ' [' r-text ']'
           end-read.
           move 250 to r-id.
           read r-file
               invalid key display 'miss 250: ' r-status
               not invalid key display 'x'
           end-read.
           move 200 to r-id. move 'TWO HUNDRED' to r-text.
           rewrite r-record invalid key display 'x' end-rewrite.
           display 'rewrite: ' r-status.
           move 999 to r-id.
           rewrite r-record
               invalid key display 'rewrite 999: ' r-status
           end-rewrite.
           move 300 to r-id.
           delete r-file record invalid key display 'x' end-delete.
           display 'delete 300: ' r-status.
           read r-file
               invalid key display 'read 300 after delete: ' r-status
           end-read.
           delete r-file record
               invalid key display 'delete 300 again: ' r-status
           end-delete.
           close r-file.
      * START and READ NEXT in key order
           open input k-file.
           move 100 to k-id.
           start k-file key is greater than k-id
               invalid key display 'x'
           end-start.
           display 'start > 100: ' k-status.
           perform until eof = 'Y'
               read k-file next record
                   at end move 'Y' to eof
                   not at end display 'next ' k-id ' [' k-text ']'
               end-read
           end-perform.
           display 'at end: ' k-status.
           move 400 to k-id.
           start k-file key is >= k-id
               invalid key display 'start >= 400: ' k-status
           end-start.
           move 'N' to eof.
           move 0 to n.
           start k-file key is not less than k-id.
           move 0 to k-id.
           start k-file key >= k-id.
           perform until eof = 'Y'
               read k-file next
                   at end move 'Y' to eof
                   not at end add 1 to n
               end-read
           end-perform.
           display n ' records in key order'.
           close k-file.
           stop run.
