identification division.
program-id. p-address-strong-elem.
*> ADDRESS OF an elementary item inside a strongly-typed group (2023
*> 8.4.3.11 rule 2).
data division.
working-storage section.
01 p usage pointer.
01 sg typedef strong.
   05 sv pic x(3).
01 st type sg.
procedure division.
    set p to address of sv of st
    goback.
