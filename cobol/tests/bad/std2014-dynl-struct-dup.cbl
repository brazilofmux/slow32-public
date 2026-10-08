identification division.
program-id. p-std2014-dynl-struct-dup.
*> A DYNAMIC LENGTH STRUCTURE declared twice (2023 12.3.7).
environment division.
configuration section.
special-names.
    dynamic length structure sp is short prefixed
    dynamic length structure sp is delimited.
data division.
working-storage section.
01 s pic x dynamic length sp.
procedure division.
    display s
    goback.
