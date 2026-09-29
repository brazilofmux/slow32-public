       identification division.
       program-id. iorul.
      * The I-O statements' rules (X3.23-1985 sequential OPEN rule 3,
      * relative and indexed OPEN rule 1, the OPEN and CLOSE formats,
      * READ rule 1, sequential WRITE rules 7-8, relative REWRITE rule
      * 3, DELETE rule 1): EXTEND for sequential access without LINAGE,
      * NO REWIND and REEL for sequential files, INTO not the record
      * area, END-OF-PAGE only with LINAGE and never with ADVANCING PAGE,
      * no ADVANCING on an indexed file, no INVALID KEY in sequential
      * access.
       environment division.
       input-output section.
       file-control.
           select sq assign to "sq.dat".
           select pr assign to "pr.txt".
           select rl assign to "rl.dat" organization relative
               relative key n.
           select ix assign to "ix.dat" organization indexed
               record key ik.
       data division.
       file section.
       fd sq.
       01 sqr pic x(10).
       fd pr linage is 20 lines.
       01 prr pic x(10).
       fd rl.
       01 rlr pic x(10).
       fd ix.
       01 ixr.
          05 ik pic x(4).
          05 ix2 pic x(6).
       working-storage section.
       01 n pic 9(4).
       procedure division.
           open extend pr.
           open i-o rl with no rewind.
           close ix reel.
           read sq into sqr.
           write sqr at end-of-page continue end-write.
           write prr after advancing page at end-of-page continue
               end-write.
           write ixr after advancing 1 line.
           rewrite rlr invalid key continue end-rewrite.
           delete rl invalid key continue end-delete.
           stop run.
