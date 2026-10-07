*> The sort and merge conditions (2023 Table 13): EC-FLOW-RELEASE and
*> EC-FLOW-RETURN (RELEASE and RETURN outside their SORT, fatal: each
*> ends the run after its declarative, so each is the last thing a
*> program does); EC-SORT-MERGE-RETURN (a RETURN after the at end
*> condition, fatal); EC-SORT-MERGE-SEQUENCE (a MERGE USING file out
*> of order, fatal); EC-SORT-MERGE-FILE-OPEN (a USING file open, fatal).
*> Each is shown by a contained program CALLed in turn, the run ending
*> at the first fatal one; the one wanted is chosen by the argument so
*> that every condition can be seen (ecsort.args: return).
*> No oracle: GnuCOBOL 4 does not implement exception declaratives.
identification division.
program-id. ecsort.
data division.
working-storage section.
01  which pic x(8).
procedure division.
    accept which from command-line
    display "case: " which
    evaluate which
      when "return" call "ec-return"
      when "release" call "ec-release"
      when "atend" call "ec-atend"
      when "seq" call "ec-seq"
      when "open" call "ec-open"
    end-evaluate
    display "not reached"
    stop run.
identification division.
program-id. ec-return.
environment division.
input-output section.
file-control.
    select sf assign to "ecsort.tmp".
data division.
file section.
sd  sf.
01  sr pic x(4).
procedure division.
declaratives.
d1 section.
    use after exception condition ec-flow-return.
    display "declarative: " function exception-status.
end declaratives.
main section.
>>TURN EC-FLOW-RETURN CHECKING ON
    return sf at end display "at end" end-return
    display "after RETURN (undefined)".
end program ec-return.
identification division.
program-id. ec-release.
environment division.
input-output section.
file-control.
    select sf assign to "ecsort.tmp".
data division.
file section.
sd  sf.
01  sr pic x(4).
procedure division.
declaratives.
d1 section.
    use after exception condition ec-flow-release.
    display "declarative: " function exception-status.
end declaratives.
main section.
>>TURN EC-FLOW-RELEASE CHECKING ON
    release sr
    display "after RELEASE (undefined)".
end program ec-release.
identification division.
program-id. ec-atend.
environment division.
input-output section.
file-control.
    select sf assign to "ecsort.tmp".
data division.
file section.
sd  sf.
01  sr pic x(4).
procedure division.
declaratives.
d1 section.
    use after exception condition ec-sort-merge-return.
    display "declarative: " function exception-status.
end declaratives.
main section.
>>TURN EC-SORT-MERGE-RETURN CHECKING ON
    sort sf ascending sr input procedure put-one output procedure get-all.
put-one.
    move "only" to sr release sr.
get-all.
    return sf at end display "at end (not: one record)" end-return
    return sf at end display "at end" end-return
    return sf at end display "a third RETURN (not reached)" end-return.
end program ec-atend.
identification division.
program-id. ec-seq.
environment division.
input-output section.
file-control.
    select sf assign to "ecsort.tmp".
    select f1 assign to "ecsort1.dat" organization line sequential.
    select f2 assign to "ecsort2.dat" organization line sequential.
    select fo assign to "ecsort3.dat" organization line sequential.
data division.
file section.
sd  sf.
01  sr pic x(4).
fd  f1.
01  r1 pic x(4).
fd  f2.
01  r2 pic x(4).
fd  fo.
01  ro pic x(4).
procedure division.
declaratives.
d1 section.
    use after exception condition ec-sort-merge-sequence.
    display "declarative: " function exception-status.
end declaratives.
main section.
    open output f1 move "b" to r1 write r1 move "a" to r1 write r1 close f1
    open output f2 move "c" to r2 write r2 close f2
>>TURN EC-SORT-MERGE-SEQUENCE CHECKING ON
    merge sf ascending sr using f1 f2 giving fo
    display "after MERGE (undefined)".
end program ec-seq.
identification division.
program-id. ec-open.
environment division.
input-output section.
file-control.
    select sf assign to "ecsort.tmp".
    select f1 assign to "ecsort1.dat" organization line sequential.
    select fo assign to "ecsort3.dat" organization line sequential.
data division.
file section.
sd  sf.
01  sr pic x(4).
fd  f1.
01  r1 pic x(4).
fd  fo.
01  ro pic x(4).
procedure division.
declaratives.
d1 section.
    use after exception condition ec-sort-merge-file-open.
    display "declarative: " function exception-status.
end declaratives.
main section.
    open output f1 move "b" to r1 write r1 close f1
    open input f1
>>TURN EC-SORT-MERGE-FILE-OPEN CHECKING ON
    sort sf ascending sr using f1 giving fo
    display "after SORT (undefined)".
end program ec-open.
end program ecsort.
