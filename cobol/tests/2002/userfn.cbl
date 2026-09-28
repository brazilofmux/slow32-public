identification division.
function-id. twice.
*> User-defined functions (COBOL 2002; cobol ISSUES-50), defined ahead
*> of the program that invokes them, as REPOSITORY requires (2023 12.3.8
*> rule 10).  TWICE: a LINKAGE result.  FACT: recursive, through a
*> LOCAL-STORAGE item.  PAD: an alphanumeric result, built with a
*> reference modification.  HALF-OF: calls TWICE's neighbour through its
*> own REPOSITORY.  The program invokes them bare, with FUNCTION, in
*> conditions, loops, WHEN, ADD/SUBTRACT ... GIVING, with literal and
*> expression arguments.  An expression argument is passed BY CONTENT
*> into a copy described like the parameter (14.8.2.3.3 rule 2a), so
*> twice(a + 4) is 50 and twice(-7) is -14; GnuCOBOL 4 gets 0 and 1400
*> (userfn.oracle-expected).
data division.
linkage section.
01  x        pic s9(4).
01  r        pic s9(5).
procedure division using x returning r.
    compute r = x * 2
    goback.
end function twice.

identification division.
function-id. fact.
data division.
local-storage section.
01  m        pic s9(4).
linkage section.
01  n        pic s9(4).
01  res      pic 9(9).
procedure division using n returning res.
    if n <= 1
        move 1 to res
    else
        compute m = n - 1
        compute res = n * fact(m)
    end-if
    goback.
end function fact.

identification division.
function-id. pad.
data division.
linkage section.
01  s        pic x(3).
01  out      pic x(8).
procedure division using s returning out.
    move all "." to out
    move s to out(3:3)
    goback.
end function pad.

identification division.
function-id. half-of.
environment division.
configuration section.
repository.
    function twice.
data division.
linkage section.
01  v        pic s9(4).
01  h        pic s9(5).
procedure division using v returning h.
    compute h = twice(v) / 4
    goback.
end function half-of.

identification division.
program-id. userfn.
environment division.
configuration section.
repository.
    function twice
    function fact
    function pad
    function half-of
    function all intrinsic.
data division.
working-storage section.
01  a        pic s9(4) value 21.
01  b        pic s9(5).
01  k        pic s9(4) value 0.
01  t        pic 9(3).
01  big      pic 9(3).
procedure division.
main.
    move twice(a) to b
    display "twice(21) = " b
    display "function twice(a + 4) = " function twice(a + 4)
    display "twice(-7) = " twice(-7)
    if twice(a) = 42 display "condition sees 42" end-if
    perform until twice(k) > 6
        add 1 to k
    end-perform
    display "k = " k
    move 10 to k
    display "fact(10) = " fact(k)
    display "pad: [" pad("abc") "]"
    display "half-of(21) = " half-of(a)
    evaluate true
        when twice(a) < 40 display "when: small"
        when twice(a) = 42 display "when: forty-two"
        when other display "when: other"
    end-evaluate
    add twice(a) to 8 giving b
    display "add giving: " b
    subtract 2 from twice(a) giving b
    display "subtract giving: " b
    move max(3 7 5) to big
    display "max = " big
    move length(pad("xyz")) to t
    display "length = " t
    stop run.
end program userfn.
