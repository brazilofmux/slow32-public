identification division.
program-id. p-std2014-dynl-address-of.
*> ADDRESS OF a dynamic-length item (2023 8.4.3.11.3 rule 6).
data division.
working-storage section.
01 s pic x dynamic length.
01 p usage pointer.
procedure division.
    set p to address of s
    goback.
