*> kedit -- numeric editing and de-editing: a signed COMP-3 value moved
*> to three edited pictures (zero suppression, floating currency with CR,
*> check protection with a trailing sign) and one of them back to numeric.
identification division.
program-id. kedit.
data division.
working-storage section.
01  n        pic 9(9) comp value 2000000.
01  i        pic 9(9) comp.
01  v        pic s9(7)v99 comp-3.
01  e1       pic -z,zzz,zz9.99.
01  e2       pic $$$,$$$,$$9.99cr.
01  e3       pic ***,***,**9.99-.
01  w        pic s9(7)v99.
01  tot      pic s9(15)v99 comp-3 value 0.
01  cnt      pic 9(9) comp value 0.
procedure division.
    perform varying i from 1 by 1 until i > n
        compute v = i * 0.37 - 50000
        move v to e1 e2 e3
        move e1 to w
        add w to tot
        if e2(15:2) = "CR" add 1 to cnt end-if
        if e3(1:1) = "*" add 1 to cnt end-if
    end-perform
    display "kedit " tot " " cnt " [" e1 "][" e2 "][" e3 "]"
    stop run.
