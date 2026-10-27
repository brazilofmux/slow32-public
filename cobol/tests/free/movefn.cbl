*> MOVE with an intrinsic function as the sender, inside a loop the
*> lowering takes: the function's value is formed as COMPUTE forms it, so
*> the MOVE is a node and the loop around it one island (a MOVE FUNCTION
*> MIN(...) TO k left as text kept a whole paragraph of majesty's csv2fw in
*> the text).  The receivers truncate as any numeric MOVE does: high-order
*> digits lost, a sign dropped by an unsigned receiver, the fraction cut.
identification division.
program-id. movefn.
data division.
working-storage section.
01  i         pic 9(4) comp.
01  lim       pic 9(4) comp value 37.
01  small     pic 9(4) comp.
01  big       pic 9(4) comp.
01  signed-v  pic s9(4) comp.
01  unsigned  pic 9(4).
01  two-dig   pic 99.
01  dec       pic 9(3)v9.
01  total     pic 9(9) comp value 0.
01  shown     pic z(8)9.
procedure division.
main-procedure.
    perform varying i from 1 by 1 until i > 60
        move function min(i, lim) to small
        move function max(i * 3, 100) to big
        move function mod(i * 7, 11) to signed-v
        move function rem(signed-v - 9, 7) to unsigned
        move function max(i * 5, 0) to two-dig
        move function min(i / 4, 12.5) to dec
        compute total = total + small + big + signed-v + unsigned + two-dig + dec * 10
        if i = 1 or i = 37 or i = 60
            display i ' ' small ' ' big ' ' signed-v ' ' unsigned ' ' two-dig ' ' dec
        end-if
    end-perform
    move total to shown
    display 'total' shown
    stop run.
