identification division.
program-id. ecturnstmt.
*> A TURN directive inside a statement applies to every statement after
*> it in the source, whether or not inside that statement (2023 7.3.25.4
*> rule 5): here one in an IF's THEN branch turns checking on for the
*> RAISE after it and for the one in the ELSE branch, not for the RAISE
*> before it. The IF runs twice, each branch once.
*> No oracle: GnuCOBOL 4 does not implement exception declaratives.
data division.
working-storage section.
01  flag pic x.
procedure division.
declaratives.
ua section.
    use after exception condition ec-user-a.
ua1.
    display "  USE EC-USER-A".
end declaratives.
main section.
m1.
    move "y" to flag
    perform twice
    move "n" to flag
    perform twice
    stop run.
twice.
    if flag = "y"
        display "then: raise before the TURN"
        raise exception ec-user-a
>>TURN EC-USER-A CHECKING ON
        display "then: raise after the TURN"
        raise exception ec-user-a
    else
        display "else: raise, after the TURN in the source"
        raise exception ec-user-a
    end-if.
