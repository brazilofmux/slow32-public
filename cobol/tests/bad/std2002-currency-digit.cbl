identification division.
program-id. p.
*> The currency string has no digit and none of + - , . * (2023 12.3.7 rule 23).
environment division.
configuration section.
special-names.
    currency sign is "US$1" with picture symbol "$".
procedure division.
    stop run.
end program p.
