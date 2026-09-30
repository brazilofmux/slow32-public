*> The template for the EC-ARGUMENT-FUNCTION sites (run-tests.sh, gate 6):
*> @STMT@ is replaced by each line of argfn.txt in turn.  An argument, or
*> the returned value, outside a function's rules sets the condition
*> (2023 15.3); checked, the declarative says so.  The condition is fatal,
*> so each site is a program of its own; a "not raised" line beside each
*> kind shows the check does not fire on a correct argument.
identification division.
program-id. argsite.
data division.
working-storage section.
01 m1   pic s9 value -1.
01 z0   pic 9 value 0.
01 p2   pic 9 value 2.
01 p7   pic 9 value 7.
01 p34  pic 99 value 34.
01 p66  pic 99 value 66.
01 p300 pic 999 value 300.
01 gd   pic 9(8) value 20230131.
01 bad  pic 9(8) value 20231301.
01 badd pic 9(7) value 2023400.
01 junk pic x(4) value "ab1c".
01 r    pic s9(9)v9(6).
01 x    pic x(10).
procedure division.
declaratives.
dx section.
    use after exception condition ec-argument-function.
d1.
    display "RAISED".
end declaratives.
main section.
m1.
>>TURN EC-ARGUMENT-FUNCTION CHECKING ON
    @STMT@
    display "not raised"
    stop run.
