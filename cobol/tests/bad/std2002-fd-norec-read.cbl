identification division.
program-id. p.
*> A file with no record description entry is read INTO (2023 13.4.5.3 rule 3c).
environment division.
input-output section.
file-control.
    select f assign to "x.dat".
data division.
file section.
fd  f record contains 10 characters.
procedure division.
    open input f
    read f at end continue end-read
    stop run.
end program p.
