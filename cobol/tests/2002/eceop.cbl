identification division.
program-id. eceop.
*> EC-I-O-EOP, EC-I-O-EOP-OVERFLOW (2023 14.9.51.4 rule 27a) and
*> EC-I-O-LINAGE (13.18.34.4 rule 6; cobol queue item 4).  A LINAGE file
*> of 4 lines with FOOTING 3: the WRITE that reaches the footing area is
*> EC-I-O-EOP, the one past the page EC-I-O-EOP-OVERFLOW, both nonfatal
*> and raised whether or not END-OF-PAGE is written; a second file whose
*> LINAGE items say a page of 0 lines is EC-I-O-LINAGE at its WRITE,
*> fatal, nothing written and LINAGE-COUNTER 0.  No oracle (ecraise).
environment division.
input-output section.
file-control.
    select p assign to "tmp/eceop-p.txt" organization sequential.
    select q assign to "tmp/eceop-q.txt" organization sequential.
data division.
file section.
fd p linage is 4 lines with footing at 3.
01 prec pic x(8).
fd q linage is lines-q lines with footing at foot-q.
01 qrec pic x(8).
working-storage section.
01 lines-q pic 99 value 0.
01 foot-q pic 99 value 1.
01 i pic 9.
procedure division.
declaratives.
d1 section. use after exception condition ec-i-o-eop.
p1. display "  nonfatal: " function trim(function exception-status) " counter=" linage-counter of p.
d2 section. use after exception condition ec-i-o-eop-overflow.
p2. display "  nonfatal: " function trim(function exception-status) " counter=" linage-counter of p.
d3 section. use after exception condition ec-i-o-linage.
p3. display "  fatal: " function trim(function exception-status) " counter=" linage-counter of q.
end declaratives.
main section.
m1.
>>TURN EC-I-O-EOP EC-I-O-EOP-OVERFLOW EC-I-O-LINAGE CHECKING ON
    open output p.
    perform varying i from 1 by 1 until i > 6
        move i to prec
        write prec after advancing 1 line
            at end-of-page display "  phrase: eop after line " i
        end-write
        display "wrote " i " counter=" linage-counter of p
    end-perform.
    close p.
    open output q.
    display "q open, counter=" linage-counter of q.
    move "x" to qrec.
    write qrec.
    display "not reached".
    stop run.
