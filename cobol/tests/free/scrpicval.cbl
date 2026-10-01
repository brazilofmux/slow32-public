*> A screen entry with PICTURE and VALUE (2002 13.15.2 rule 7, general
*> rule 3: the PICTURE "may be omitted" for an alphanumeric literal, so it
*> may be written): the literal in a field of the picture's size -- padded
*> with spaces, or cut on the right with a warning at compile time.
*> abrignoli_COBSOFT draws its menus so (pic x(122) value "...").  It was
*> "a VALUE slot takes no PICTURE" (ISSUES 120).
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. scrpicval.
data division.
working-storage section.
screen section.
01 menu.
   05 blank screen.
   05 line 2 col 3 pic x(12) value "OPTION (  )".
   05 line 2 col 15 value "|".
   05 line 3 col 3 pic x(4) value "TRUNCATED".
   05 line 3 col 7 value "|".
procedure division.
    display menu
    display " " at line 5 col 1
    stop run.
