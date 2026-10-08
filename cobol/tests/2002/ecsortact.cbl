>>TURN EC-SORT-MERGE-ACTIVE CHECKING ON
identification division.
program-id. ecsortact.
*> EC-SORT-MERGE-ACTIVE (2023 14.9.40.4 rule 2): a SORT begun while another
*> is under way.  Within one program the text forbids it outright
*> (14.9.40.3 rule 3, refused at compile time); through a CALL from the
*> first SORT's INPUT PROCEDURE it is the run-time condition, fatal: its
*> declarative, then the run ends.  No oracle (ecraise).
environment division.
input-output section.
file-control.
    select s1 assign to "tmp/ecsortact1.srt".
data division.
file section.
sd s1.
01 r1 pic x(4).
procedure division.
declaratives.
d1 section. use after exception condition ec-sort-merge-active.
p1. display "  fatal: " function trim(function exception-status).
end declaratives.
main section.
m1.
    sort s1 on ascending key r1 input procedure is feed output procedure is drain.
    display "not reached".
    stop run.
feed.
    display "  input procedure: calling a program that sorts".
    call "sortsub".
    display "  not reached".
drain.
    return s1 at end display "  s1 drained" end-return.
identification division.
program-id. sortsub.
environment division.
input-output section.
file-control.
    select s2 assign to "tmp/ecsortact2.srt".
data division.
file section.
sd s2.
01 r2 pic x(4).
procedure division.
declaratives.
d2 section. use after exception condition ec-sort-merge-active.
p2. display "  fatal in sortsub: " function trim(function exception-status).
end declaratives.
main section.
m2.
    display "  sortsub: sorting s2".
    sort s2 on ascending key r2 input procedure is feed2 output procedure is drain2.
    display "  not reached".
    goback.
feed2.
    move "b" to r2. release r2.
drain2.
    return s2 at end display "  s2 drained" end-return.
end program sortsub.
end program ecsortact.
