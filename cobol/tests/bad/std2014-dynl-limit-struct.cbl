identification division.
program-id. p-std2014-dynl-limit-struct.
*> LIMIT above what the SHORT PREFIXED structure allows, 65535 (2023 13.18.19.3 rule 4).
environment division.
configuration section.
special-names.
    dynamic length structure sp is short prefixed.
data division.
working-storage section.
01 s pic x dynamic length sp limit 70000.
procedure division.
    display s
    goback.
