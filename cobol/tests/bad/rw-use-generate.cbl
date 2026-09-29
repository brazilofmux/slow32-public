       identification division.
       program-id. rwug.
      * No GENERATE, INITIATE or TERMINATE in a USE BEFORE REPORTING
      * procedure (X3.23-1985 USE rules; 2023 14.9.49.3 rule 10).
       environment division.
       input-output section.
       file-control.
           select outf assign to "x.prn".
       data division.
       file section.
       fd  outf report is r.
       report section.
       rd  r.
       01  d type detail line plus 1.
           05 column 1 pic x(4) value "line".
       procedure division.
       declaratives.
       ud section.
           use before reporting d.
       udp.
           generate d.
       end declaratives.
       m section.
       m1.
           stop run.
