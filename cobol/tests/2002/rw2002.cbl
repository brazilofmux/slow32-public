*> COBOL 2002's Report Writer additions (docs/plans/standard-queue.md item
*> 37): COLUMN RIGHT, CENTER and PLUS (13.18.14.4 rules 6-8), several
*> column numbers in one clause, PRESENT WHEN on an item, a line and a
*> group (13.18.41), OCCURS on an item (horizontal, STEP, DEPENDING ON) and
*> on a line (vertical), VARYING stepping the SOURCE's subscript (13.18.64).
*> The report is read back and shown. No oracle: GnuCOBOL 4 has no VARYING
*> in a report, and generates C that does not compile for the rest.
identification division.
program-id. rw2002.
environment division.
input-output section.
file-control.
    select prt assign to "rw2002.prn" organization line sequential.
    select chk assign to "rw2002.prn" organization line sequential.
data division.
file section.
fd prt report is rep.
fd chk.
01 chk-line pic x(70).
working-storage section.
01 rec.
   05 r-name pic x(8).
   05 r-qty pic 9(3).
   05 r-amt pic 9(4).
   05 r-flag pic x.
01 tbl.
   05 t-val pic 9(2) occurs 5 times.
01 ndeps pic 9 value 3.
report section.
rd rep controls are final page limit 20 lines heading 1 first detail 3.
01 type ph.
   05 line 1.
      10 column 1 value "Name".
      10 column right 20 value "Qty".
      10 column center 30 value "Amt".
      10 column plus 3 value "+3".
   05 line plus 1 column 1 value "---".
01 det type de.
   05 line plus 1.
      10 column 1 pic x(8) source r-name.
      10 column right 20 pic zz9 source r-qty.
      10 column center 30 pic z(4) source r-amt.
      10 column plus 2 value "*" present when r-flag = "Y".
      10 column 40 50 60 pic 99 source t-val (ix) varying ix from 1 by 2.
   05 line plus 1 present when r-flag = "Y".
      10 column 5 value "flagged".
   05 line plus 1 occurs 2 times varying iy from 4.
      10 column 5 value "occ".
      10 column plus 1 pic 99 source t-val (iy).
      10 column plus 1 pic 9 source iy.
   05 line plus 1.
      10 column 1 pic 99 source t-val (iz) occurs 1 to 5 times depending on ndeps step 4 varying iz from 1 by 1.
01 fl type cf final present when r-qty > 0.
   05 line plus 2 column 1 value "end".
procedure division.
    move 11 to t-val (1). move 22 to t-val (2). move 33 to t-val (3). move 44 to t-val (4). move 55 to t-val (5).
    open output prt.
    initiate rep.
    move "alpha" to r-name. move 5 to r-qty. move 1234 to r-amt. move "Y" to r-flag.
    generate det.
    move "beta" to r-name. move 12 to r-qty. move 7 to r-amt. move "N" to r-flag. move 5 to ndeps.
    generate det.
    terminate rep.
    close prt.
    open input chk.
    perform until exit
        read chk at end exit perform end-read
        if chk-line not = spaces display "[" function trim(chk-line trailing) "]" end-if
    end-perform.
    close chk.
    stop run.
