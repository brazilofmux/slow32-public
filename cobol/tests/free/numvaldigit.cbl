*> A number read a digit at a time: v = v * 10 + NUMVAL(one character).
*> By its picture v * 10 passes 18 digits and NUMVAL's value could be
*> anything, so the statement was the wide stack's; it is computed in
*> 64 bits with the product and the character tested (checked
*> arithmetic, docs/performance.md), and must store what the stack
*> stored -- here up to the eighteenth digit, into binary, packed and
*> DISPLAY items, from a part of an item and from a one-character item.
identification division.
program-id. numvaldigit.
data division.
working-storage section.
01  t            pic x(20) value "98765432109876543210".
01  c            pic x.
01  p            pic 9(4) comp.
01  n            pic 9(4) comp.
01  v            pic s9(18) comp.
01  vp           pic s9(18) packed-decimal.
01  vd           pic 9(12).
01  small        pic 9(3).
procedure division.
    perform varying n from 1 by 1 until n > 18
        move 0 to v
        perform varying p from 1 by 1 until p > n
            compute v = v * 10 + function numval(t(p:1))
        end-perform
        display n " " v
    end-perform
    move 0 to vp vd small
    perform varying p from 3 by 1 until p > 20
        move t(p:1) to c
        compute vp = vp * 10 + function numval(c)
        compute vd = vd * 10 + function numval(t(p:1))
        compute small = small * 10 + function numval(c)
    end-perform
    display vp " " vd " " small
    compute v = 0 - function numval(t(3:1)) * 1000000007
    display v
    stop run.
