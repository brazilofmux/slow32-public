program afuse
   use afmod
   implicit none
   integer :: k(5)
   real :: a(2,3), b(2,3)
   integer :: i, j
   k = evens(5)
   print *, k
   print *, sum(evens(4)), evens(3) + 1
   do j = 1, 3
      do i = 1, 2
         a(i, j) = real(i + 10 * j)
      end do
   end do
   b = scaled(a, 0.5)
   print '(6F6.1)', b
   print '(3F6.1)', transpose(scaled(a(:, 2:3), 2.0))
end program
