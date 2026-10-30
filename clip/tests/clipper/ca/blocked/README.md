# Drivers that wait on a known wall

`ca-frmrun.prg` runs CA's REPORT FORM runner (SYS/FRMRUN.PRG, FRMBACK.PRG)
over `ca-frmrun.frm`, a form our interpreter's CREATE REPORT wrote.  Harbour
produces `ca-frmrun.expected` from them (the heading, the column headings,
three detail lines, the total; then the summary form).  Our runtime cannot
yet: FRMBACK reads the whole 1,990-byte form with one FREAD into one string,
and a string value here holds 255 characters and ends at the first NUL.
When strings grow, move the three files up a level and the harness copies
the form beside the driver (a driver's data files are named after it).
