*> Exception propagation (2023 14.9.18 and 14.9.14 RAISING, 14.2 the
*> header's RAISING phrase, 7.3.21 PROPAGATE).  A condition handed back
*> is raised in the caller after the CALL, if the caller's checking for
*> it is on, and goes where the caller's own would: a declarative, a
*> WHEN of an exception-checking PERFORM, or (fatal, with neither) the
*> end of the run -- so the nonfatal cases run first, and the fatal one
*> is chosen by the argument (ecraising.args: last) and is the last
*> thing the run does; the others were run by hand.
*>   sub1   GOBACK RAISING EXCEPTION EC-RANGE-SEARCH-NO-MATCH (nonfatal):
*>          the caller's USE takes it, the run goes on;
*>   sub2   EXIT PROGRAM RAISING EXCEPTION EC-USER-OOPS, listed in its
*>          header's RAISING; the caller's USE for EC-USER takes it;
*>   sub6   raises a name the caller has no checking on: nothing;
*>   last   sub3: a declarative's GOBACK RAISING LAST EXCEPTION hands on
*>          the EC-SIZE-ZERO-DIVIDE it ran for; the caller's WHEN takes
*>          it, and being fatal it ends the run after the WHEN;
*>   nspec  sub4: its declarative's GOBACK RAISING LAST hands on
*>          EC-USER-NOT-LISTED, which its header's RAISING does not list
*>          (a name written in the GOBACK itself is refused at compile
*>          time: bad/std2002-goback-raising): the caller receives
*>          EC-RAISING-NOT-SPECIFIED instead (14.9.18.4 rule 1b3a);
*>   prop   sub5, under >>PROPAGATE ON (2002/lib/ecpropagate.cbl): a divide
*>          by zero nothing in sub5 handles comes to the caller as if by
*>          GOBACK RAISING LAST.
*> No oracle: GnuCOBOL 4 has no exception declaratives.
identification division.
program-id. ecraising.
data division.
working-storage section.
01  which pic x(8).
procedure division.
declaratives.
d-nomatch section.
    use after exception condition ec-range-search-no-match.
    display "caller's USE: " function exception-status.
d-user section.
    use after exception condition ec-user.
    display "caller's USE for EC-USER: " function exception-status.
end declaratives.
main section.
>>TURN EC-SIZE EC-USER EC-RAISING EC-RANGE-SEARCH-NO-MATCH CHECKING ON
    accept which from command-line
    call "sub1"
    display "after sub1"
    call "sub2"
    display "after sub2"
    call "sub6"
    display "after sub6: nothing"
    display "fatal case: " which
    evaluate which
      when "last"
        perform
            call "sub3"
        when exception ec-size
            display "caller's WHEN: " function exception-status
        end-perform
      when "nspec"
        perform
            call "sub4"
        when exception ec-raising-not-specified
            display "caller's WHEN: " function exception-status
        end-perform
      when "prop"
        perform
            call "sub5"
        when exception ec-size-zero-divide
            display "caller's WHEN (propagated): " function exception-status
        end-perform
    end-evaluate
    display "not reached"
    stop run.
identification division.
program-id. sub1.
procedure division.
    goback raising exception ec-range-search-no-match.
end program sub1.
identification division.
program-id. sub2.
procedure division raising ec-user-oops.
    display "in sub2"
    exit program raising exception ec-user-oops.
end program sub2.
identification division.
program-id. sub3.
data division.
working-storage section.
01  a pic 9 value 1.
01  z pic 9 value 0.
procedure division.
declaratives.
d1 section.
    use after exception condition ec-size-zero-divide.
    display "sub3's declarative, handing it on"
    goback raising last exception.
end declaratives.
main section.
>>TURN EC-SIZE-ZERO-DIVIDE CHECKING ON
    divide a by z giving a
    display "after the divide (not reached)".
end program sub3.
identification division.
program-id. sub4.
procedure division raising ec-user-other.
declaratives.
d1 section.
    use after exception condition ec-user-not-listed.
    goback raising last exception.
end declaratives.
main section.
>>TURN EC-USER-NOT-LISTED CHECKING ON
    raise exception ec-user-not-listed
    display "after the RAISE (not reached)".
end program sub4.
end program ecraising.
