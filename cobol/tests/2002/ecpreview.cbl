identification division.
program-id. ecpreview.
*> The exception-checking PERFORM after the Stage B review (cobol
*> ISSUES-94), one case each, nonfatal conditions.
*> E4: WHEN EXCEPTION EC-USER turns on the EC-USER names met later too,
*> so a raise of a name first seen inside imperative-statement-1 reaches
*> the WHEN.
*> E5: after END-PERFORM, what the implicit TURN enabled is off again
*> (2023 14.9.28 rule 22): a new EC-USER name raised later is not
*> checked, and no USE runs.
*> E6: WITH LOCATION goes into the implicit TURN only, for names not
*> already enabled (rule 14): a name turned on before keeps its setting,
*> so the raise after the PERFORM still reports no location.
*> E8: inside imperative-statement-1 a WHEN takes the condition and the
*> file's USE AFTER ERROR procedure is ignored (rule 17).
*> E11: WHEN phrases match in USE rule 3c-3g's order (14.9.49.4): the
*> group with the file before the name without one.
*> E7: a name enabled for all files but turned off for one is not enabled
*> for that one, so the implicit TURN turns it on (rule 14).
*> No oracle: GnuCOBOL 4 has no exception-checking PERFORM.
environment division.
input-output section.
file-control.
    select f2 assign to "tmp/ecp2.dat" organization line sequential file status fs.
    select f3 assign to "tmp/ecp3.dat" organization line sequential file status fs.
    select f1 assign to "tmp/ecp1.dat" organization line sequential file status fs.
data division.
file section.
fd  f1.
01  r1 pic x(10).
fd  f2.
01  r2 pic x(10).
fd  f3.
01  r3 pic x(10).
working-storage section.
01  fs pic xx.
procedure division.
declaratives.
ua section.
    use after exception condition ec-user.
ua1.
    display "  USE EC-USER: " function exception-status.
ub section.
    use after exception condition ec-user-a.
ub1.
    display "  USE EC-USER-A, location [" function exception-location "]".
fe section.
    use after error procedure on f2.
fe1.
    display "  USE AFTER ERROR on f2 (not expected)".
end declaratives.
main section.
e4.
    display "E4: WHEN EC-USER, a name first seen inside"
    perform
        raise exception ec-user-new
        display "E4: resumed after the raise"
    when exception ec-user
        display "  WHEN EC-USER: " function exception-status
    end-perform.
e5.
    display "E5: WHEN EC-ALL, then a new user name after END-PERFORM"
    perform
        continue
    when exception ec-all
        display "  WHEN EC-ALL (not expected)"
    end-perform
    raise exception ec-user-later
    display "E5: no USE above".
e6.
    display "E6: a name on before PERFORM WITH LOCATION keeps no location"
>>TURN EC-USER-A CHECKING ON
    perform with location
        continue
    when exception ec-user-a
        continue
    end-perform
    raise exception ec-user-a.
e8.
    display "E8: WHEN takes AT END; USE AFTER ERROR ignored"
    open output f2 close f2
    open input f2
    perform
        read f2
        display "E8: after read, fs=" fs
    when exception ec-i-o-at-end
        display "  WHEN EC-I-O-AT-END"
    end-perform
    close f2.
e11.
    display "E11: WHEN EC-I-O FILE f3 before WHEN EC-I-O-AT-END"
    open output f3 close f3
    open input f3
    perform
        read f3
    when exception ec-i-o-at-end
        display "  WHEN EC-I-O-AT-END (not expected)"
    when exception ec-i-o file f3
        display "  WHEN EC-I-O FILE f3"
    end-perform
    close f3.
e7.
    display "E7: on for all files, off for f1: the implicit TURN for f1"
    open output f1 close f1
    open input f1
>>TURN EC-I-O-AT-END CHECKING ON
>>TURN EC-I-O-AT-END f1 CHECKING OFF
    perform
        read f1
        display "E7: after read, fs=" fs
    when exception ec-i-o-at-end
        display "  WHEN EC-I-O-AT-END"
    end-perform
    close f1
    stop run.
