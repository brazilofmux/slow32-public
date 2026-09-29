identification division.
program-id. turnf.
*> >>TURN with a file-name takes an EC-I-O exception-name only (2023
*> 7.3.25.3 rule 4).
environment division.
input-output section.
file-control.
    select infile assign to "x.dat".
data division.
file section.
fd  infile.
01  r pic x.
procedure division.
>>TURN EC-SIZE infile CHECKING ON
    stop run.
