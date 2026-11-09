program ashuse
   use ashmod
   implicit none
   real :: a(4,5), r(6)
   double precision :: d(3)
   integer :: i
   a = 0.0
   call fill(a(2:4, 1:5:2), 0.5)
   print '(5F7.1)', (a(i, :), i = 1, 4)
   r = (/ 1.0, -2.0, 3.0, -4.0, 5.0, -6.0 /)
   d = (/ -1.5d0, 2.5d0, -3.5d0 /)
   print '(3F8.2)', norm1(r), norm1(r(2:6:2)), real(norm1(d))
end program
