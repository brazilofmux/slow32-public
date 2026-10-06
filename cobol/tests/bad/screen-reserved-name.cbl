identification division.
program-id. rsvname.
*> A screen-name is a user-defined word, and no reserved word is one.
*> MOVE (and GLOBAL, taken for a name) was accepted.
data division.
working-storage section.
01 a pic x(3).
screen section.
01 move.
   05 line 1 col 1 pic x(3) using a.
procedure division.
    goback.
