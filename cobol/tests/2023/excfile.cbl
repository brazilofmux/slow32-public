*> FUNCTION EXCEPTION-FILE (file-name) (2023 15.28.4 rule 2; standard-queue
*> item 33): the connector's last I-O status and its name as the SELECT
*> clause wrote it, two spaces while it has never been accessed; the form
*> without an argument as before.  No oracle: GnuCOBOL 4 takes no argument.
identification division.
program-id. excfile.
environment division.
input-output section.
file-control.
    select InFile assign to "nosuch-file.dat" organization line sequential file status fs.
    select Other assign to "other.dat" organization line sequential.
data division.
file section.
fd InFile.
01 in-rec pic x(10).
fd Other.
01 o-rec pic x(10).
working-storage section.
01 fs pic xx.
procedure division.
    display "[" function exception-file(InFile) "]".
    display "[" function exception-file(Other) "]".
    open input InFile.
    display "[" function exception-file(InFile) "] [" function exception-file "]".
    display "[" function exception-file(Other) "]".
    open output Other.
    close Other.
    display "[" function exception-file(Other) "]".
    display "[" function exception-file-n(Other) "]".
    stop run.
