identification division.
program-id. p-sd-no-record.
*> An SD without a record description entry (2023 13.4.6.3 rule 2).
environment division.
input-output section.
file-control.
    select wk assign to "wk".
data division.
file section.
sd wk.
procedure division.
    goback.
