identification division.
program-id. ecturnfile.
*> >>TURN for one file (2023 7.3.25 rules 4, 6, 8; cobol ISSUES-87).
*> Each file reads one record and then reaches its end with no AT END
*> phrase: EC-I-O-AT-END, when checking is on for that file.  First on
*> for LEFT only; then on for every file and off for LEFT; then WITH
*> LOCATION for RIGHT alone.  A TURN with no file-name sets every file
*> and clears the per-file settings.  TURN holds for the statements that
*> follow it in the source (rule 6), so each phase's READs are written
*> after its directives.
*> No oracle (ecraise).
environment division.
input-output section.
file-control.
    select left-f  assign to "tmp/etf-left.dat"  organization sequential file status is sl.
    select right-f assign to "tmp/etf-right.dat" organization sequential file status is sr.
data division.
file section.
fd  left-f.
01  lrec     pic x(4).
fd  right-f.
01  rrec     pic x(4).
working-storage section.
01  sl       pic xx.
01  sr       pic xx.
01  phase    pic 9.
01  ef       pic x(10).
procedure division.
declaratives.
at-end section.
    use after exception condition ec-i-o-at-end.
a1.
    move function exception-file to ef
    display "  phase " phase ": " function exception-status(1:13)
            " on " ef(3:8) " [" function exception-location "]".
end declaratives.
main section.
m1.
    open output left-f right-f
    write lrec from "L001"  write rrec from "R001"
    close left-f right-f
    move 1 to phase
>>TURN EC-I-O-AT-END left-f CHECKING ON
    open input left-f right-f
    read left-f  read left-f  read right-f  read right-f
    close left-f right-f
    move 2 to phase
>>TURN EC-I-O-AT-END CHECKING ON
>>TURN EC-I-O-AT-END left-f CHECKING OFF
    open input left-f right-f
    read left-f  read left-f  read right-f  read right-f
    close left-f right-f
    move 3 to phase
>>TURN EC-I-O-AT-END CHECKING OFF
>>TURN EC-I-O-AT-END right-f CHECKING ON WITH LOCATION
    open input left-f right-f
    read left-f  read left-f  read right-f  read right-f
    close left-f right-f
    display "done: " sl " " sr
    stop run.
