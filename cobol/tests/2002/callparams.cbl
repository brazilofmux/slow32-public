*> The CALL parameter family (2023 14.2, 14.9.4): ten arguments, the
*> ninth and tenth passed on the stack; BY VALUE from a literal and from
*> binary items described as the parameters are (14.8.2.3.3 rule 1: the
*> same length); OMITTED for
*> an OPTIONAL parameter, a trailing one not passed at all, and IS
*> [NOT] OMITTED; a RECURSIVE program taking its argument BY VALUE, each
*> activation with its own copy.  docs/conformance/call.md
identification division.
program-id. callparams.
data division.
working-storage section.
01 w1 pic x(4) value "aaaa".
01 w2 pic x(4) value "bbbb".
01 w9 pic x(4) value "iiii".
01 w10 pic x(4) value "jjjj".
01 bl binary-long value 1234.
01 bs binary-short value 77.
01 fx pic 9(9) comp value 0.
01 k pic 99.
procedure division.
    call "ten" using w1 w2 by value 3 bl bs by reference omitted w1 w2 w9 w10
    display "caller w10 [" w10 "]"
    call "opt" using w1
    call "opt" using w1 w2
    call "opt" using omitted w2
    call "fact" using by value 6 by reference fx
    display "fact(6) = " fx
    stop run.
end program callparams.

identification division.
program-id. ten.
data division.
linkage section.
01 p1 pic x(4).
01 p2 pic x(4).
01 v3 binary-long.
01 v4 binary-long.
01 v5 binary-short.
01 p6 pic x(4).
01 p7 pic x(4).
01 p8 pic x(4).
01 p9 pic x(4).
01 p10 pic x(4).
procedure division using p1 p2 by value v3 v4 v5 by reference optional p6 p7 p8 p9 p10.
    display "ten: " p1 " " p2 " " v3 " " v4 " " v5
    if p6 is omitted display "p6 omitted" end-if
    display "p7-p10: " p7 " " p8 " " p9 " " p10
    move "JJJJ" to p10
    goback.
end program ten.

identification division.
program-id. opt.
data division.
linkage section.
01 a pic x(4).
01 b pic x(4).
procedure division using optional a optional b.
    if a is omitted display "opt: a omitted" else display "opt: a " a end-if
    if b is not omitted display "opt: b " b else display "opt: b omitted" end-if
    goback.
end program opt.

identification division.
program-id. fact recursive.
data division.
working-storage section.
01 r pic 9(9) comp.
linkage section.
01 n binary-long.
01 res pic 9(9) comp.
procedure division using by value n by reference res.
    if n <= 1
        move 1 to res
    else
        compute r = n - 1
        call "fact" using by value r by reference res
        compute res = n * res
    end-if
    goback.
end program fact.
