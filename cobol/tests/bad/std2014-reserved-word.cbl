identification division.
program-id. p-reserved-word.
*> The words COBOL 2014 reserved (E.2 item 24) cannot name a data item.
data division.
working-storage section.
01 nearest-to-zero pic x.
procedure division.
    display "x"
    stop run.
