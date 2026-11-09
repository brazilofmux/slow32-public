! WHERE (Fortran 90): the statement and the construct with ELSEWHERE; the
! mask evaluated once before any assignment (it names what the first
! assignment changes); the right side evaluated only where the mask holds
! (SQRT of negatives never taken); sections, rank 2, scalars, overlap.
program f90where1
   implicit none
   integer :: i, j
   real :: a(6), b(6), m(3,4), w(3,4)
   integer :: k(5)
   double precision :: d(4)
   a = (/ 4.0, -1.0, 9.0, -16.0, 0.25, 2.0 /)
   b = 0.0
   where (a > 0.0) b = sqrt(a)
   print '(6F8.3)', b
   where (a < 0.0)
      b = -a
      a = 0.0
   elsewhere
      b = b * 10.0
   end where
   print '(6F8.3)', a
   print '(6F8.3)', b
   ! the mask names A, which the first assignment changes
   a = (/ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 /)
   where (a > 2.5)
      a = a - 2.0
      b = a
   end where
   print '(6F8.3)', a
   print '(6F8.3)', b
   do j = 1, 4
      do i = 1, 3
         m(i, j) = real(i * j) - 4.0
      end do
   end do
   w = 1.0
   where (m /= 0.0) w = 1.0 / m
   print '(4F8.3)', ((w(i, j), j = 1, 4), i = 1, 3)
   where (m(2:3, 2:4) > 0.0)
      w(1:2, 1:3) = 7.0
   elsewhere
      w(1:2, 1:3) = -m(2:3, 2:4)
   end where
   print '(4F8.3)', ((w(i, j), j = 1, 4), i = 1, 3)
   k = (/ 5, 3, 8, 1, 9 /)
   where (k > 4) k = k(5:1:-1)
   print *, k
   d = (/ 1d0, -2d0, 3d0, -4d0 /)
   where (d < 0d0) d = d * d
   print *, d
   where (k == 1) k = 100
   print *, k
end program
