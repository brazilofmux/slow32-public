identification division.
program-id. ecflowuse.
*> A RAISE inside a USE declarative that would select that same
*> declarative, still active, is EC-FLOW-USE (2023 14.9.49.4 rule 2), a
*> fatal condition: the run ends at the second raise instead of
*> performing the declarative again and losing its return (cobol
*> ISSUES-94 E14).
*> No oracle: GnuCOBOL 4 does not implement exception declaratives.
data division.
working-storage section.
01 n pic 9 value 0.
>>TURN EC-ALL CHECKING ON
procedure division.
declaratives.
ua section.
    use after exception condition ec-user-a.
u1.
    add 1 to n
    display "  enter activation " n
    if n < 3
        raise exception ec-user-a
    end-if
    display "  leave, n=" n.
end declaratives.
main section.
m1.
    raise exception ec-user-a
    display "main after (not expected), n=" n
    stop run.
