identification division.
program-id. ecturn.
*> >>TURN's scope (COBOL 2002 7.3.25; cobol ISSUES-53): EC-ALL turns on
*> every condition but EC-I-O-WARNING, which only its own name turns on
*> (rule 4); a later TURN ... OFF of one name leaves the rest on; WITH
*> LOCATION decides whether EXCEPTION-STATEMENT is filled (15.32).  The
*> most specific USE applies: EC-USER-A's own section, then EC-USER's for
*> the other user names, then EC-ALL's.  A RAISE in a performed paragraph
*> returns there after its declarative.  No oracle (see ecraise).
data division.
working-storage section.
01  stmt     pic x(63).
procedure division.
declaratives.
own-a section.
    use after exception condition ec-user-a.
oa.
    display "  EC-USER-A's own: " function exception-status.
user-group section.
    use after exception condition ec-user.
ug.
    display "  the EC-USER group's: " function exception-status.
all-others section.
    use after ec ec-all.
ao.
    move function exception-statement to stmt
    display "  EC-ALL's: " function exception-status " [" stmt(1:5) "]".
end declaratives.
main section.
m1.
>>TURN EC-ALL CHECKING ON
    display "raise ec-user-a"
    raise exception ec-user-a
    display "raise ec-user-b"
    raise exception ec-user-b
    display "raise ec-i-o-warning (not turned on by EC-ALL)"
    raise exception ec-i-o-warning
>>TURN EC-I-O-WARNING CHECKING ON WITH LOCATION
    display "raise ec-i-o-warning (turned on by name)"
    raise exception ec-i-o-warning
>>TURN EC-USER-A CHECKING OFF
    display "raise ec-user-a (turned off)"
    raise exception ec-user-a
    display "raise ec-user-b (still on)"
    raise exception ec-user-b
    perform p2
    display "back from p2"
    stop run.
p2.
    display "in p2"
    raise exception ec-user-c
    display "p2 goes on".
end program ecturn.
