identification division.
program-id. printer.
*> A print file is a line printer (cobol ISSUES-46): the cursor sits on
*> the line last printed; AFTER n moves it and prints, BEFORE n prints
*> and moves it; printing on a line with ink is an overprint, written
*> as a carriage return.  The file is read back one byte at a time and
*> shown with $ for a newline, <CR> and <FF>.  L3 overprints L2, and BP
*> overprints PG, as on the printer: AFTER leaves the cursor on the line
*> it printed and BEFORE prints where the cursor is.  (SQ101M writes a
*> line of spaces first so a BEFORE has a blank line to land on.)
*> GnuCOBOL's own layout is in printer.oracle-expected (a documented
*> divergence, section C): it starts the file a line lower, appends an
*> overprint to the line instead of returning the carriage, and does
*> not advance at all for a WRITE with no ADVANCING phrase.
environment division.
input-output section.
file-control.
    select prt assign to 'tmp/printer.prt'.
    select raw assign to 'tmp/printer.prt'
        organization sequential.
data division.
file section.
fd  prt.
01  pl              pic x(8).
fd  raw.
01  rb              pic x.
working-storage section.
01  n               pic 9 value 0.
01  eof             pic x value 'n'.
01  buf             pic x(70) value spaces.
01  p               pic 99 value 1.
procedure division.
main.
    open output prt
    move 'L1' to pl  write pl after advancing 1 line
    move 'L2' to pl  write pl
    move 'L3' to pl  write pl before advancing 2 lines
    move 'L4' to pl  write pl after advancing 1 line
    move '  OVER' to pl  write pl after advancing 0 lines
    move 'B0' to pl  write pl after 1
    move '  OVB' to pl  write pl before 1
    move 'DZ' to pl  write pl after n
    move '  DZ2' to pl  write pl after advancing n lines
    move 'PG' to pl  write pl after advancing page
    move 'BP' to pl  write pl before advancing page
    move 'TOP' to pl  write pl after 1
    close prt
    open input raw
    perform until eof = 'y'
        read raw
            at end move 'y' to eof
            not at end perform show
        end-read
    end-perform
    close raw
    if p > 1 display buf(1:p - 1) end-if
    stop run.
show.
    evaluate rb
        when x'0a'
            move '$' to buf(p:1)
            display buf(1:p)
            move spaces to buf
            move 1 to p
        when x'0d'
            move '<CR>' to buf(p:4)
            add 4 to p
        when x'0c'
            move '<FF>' to buf(p:4)
            add 4 to p
        when other
            move rb to buf(p:1)
            add 1 to p
    end-evaluate.
