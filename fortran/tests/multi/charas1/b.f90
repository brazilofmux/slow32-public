program chasu
   use chasm
   implicit none
   character(len=5) :: w(4), g(3,2)
   w = (/ 'ab   ', 'cde  ', 'f    ', 'ghijk' /)
   g = 'xy'
   print *, tally(w), tally(w(2:4)), tally(w(:)(1:2)), tally(g), tally(g(2:3,:)(1:3))
   call stamp(w(1:4:3), '*')
   print *, w
   call stamp(g(2,:), '+')
   print *, g
end program
