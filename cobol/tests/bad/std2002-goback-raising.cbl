identification division.
program-id. gbr.
*> GOBACK RAISING EXCEPTION of an EC-USER name the header's RAISING
*> phrase does not list (2023 14.9.18.3 rule 2).
procedure division raising ec-user-b.
    goback raising exception ec-user-a.
