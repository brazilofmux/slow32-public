identification division.
program-id. p.
*> One currency symbol per source unit here (2023 12.3.7 rule 21 allows
*> several, each with its own string): the second is refused.
environment division.
configuration section.
special-names.
    currency sign is "EUR" with picture symbol "#"
    currency sign is "GBP" with picture symbol "@".
procedure division.
    stop run.
end program p.
