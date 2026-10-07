identification division.
program-id. fdnorec.
*> An FD with no record description entry (2023 13.4.5.3 rule 3;
*> standard-queue item 11): its RECORD clause sizes the area, WRITE
*> FILE ... FROM and REWRITE FILE ... FROM (14.9.51, 14.9.35 format 2)
*> write it, READ ... INTO reads it.  No oracle: GnuCOBOL 4 wants a
*> record description.
environment division.
input-output section.
file-control.
    select f assign to "fdnorec.dat" organization is line sequential.
    select r assign to "fdnorec.rel" organization is relative access is random relative key is rk.
data division.
file section.
fd  f record contains 10 characters.
fd  r record contains 6 characters.
working-storage section.
01  rk          pic 9(3).
01  out-rec     pic x(10).
01  in-rec      pic x(10).
01  r-rec       pic x(6).
01  eof         pic 9 value 0.
procedure division.
    open output f
    move "first" to out-rec
    write file f from out-rec
    write file f from "second"
    close f
    open input f
    perform until eof = 1
        read f into in-rec
            at end move 1 to eof
            not at end display "[" in-rec "]"
        end-read
    end-perform
    close f
    open output r
    move 1 to rk write file r from "one   "
    move 2 to rk write file r from "two   "
    close r
    open i-o r
    move 2 to rk read r into r-rec display "rel 2: " r-rec
    rewrite file r from "deux  "
    move 2 to rk read r into r-rec display "rel 2: " r-rec
    close r
    stop run.
end program fdnorec.
