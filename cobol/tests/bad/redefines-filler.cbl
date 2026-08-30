identification division.
program-id. redeffiller.
data division.
working-storage section.
01  names.
    05  filler pic x(12) value 'GST PST HST '.
    05  nm redefines filler pic x(4) occurs 3 times.
procedure division.
main.
    display nm(1).
    stop run.
