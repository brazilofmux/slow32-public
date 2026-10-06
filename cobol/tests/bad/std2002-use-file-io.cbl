identification division.
program-id. usefio.
*> FILE in USE AFTER EXCEPTION CONDITION follows an exception-name
*> beginning EC-I-O (2023 14.9.49.3 rule 13).
environment division.
input-output section.
file-control.
    select f assign to "x.dat".
data division.
file section.
fd  f.
01  r pic x.
procedure division.
declaratives.
d1 section.
    use after exception condition ec-size file f.
    display "x".
end declaratives.
main section.
    stop run.
