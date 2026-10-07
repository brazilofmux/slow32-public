identification division.
program-id. p-proto-after-def.
*> A program prototype after a program definition: prototypes come first in
*> the compilation group (2023 10.6.2 rule 1).
procedure division.
    goback.
end program p-proto-after-def.
identification division.
program-id. sub1 is prototype.
procedure division.
end program sub1.
