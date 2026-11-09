! PACK UNPACK CSHIFT EOSHIFT; reductions with DIM of arrays of rank 2 and
! 3; MAXLOC and MINLOC without DIM; array expressions as actual arguments.
module avm3
contains
   real function total(x)
      real :: x(:)
      total = sum(x)
   end function
end module

subroutine olds(v, n)
   integer n
   real v(n)
   print '(A,6F7.2)', ' olds', v
end subroutine

program f90avi3
   use avm3
   implicit none
   integer :: i, j, kk, k(6), m(3,4), n3(2,3,2), l2(2)
   real :: a(6), b(2,3), z
   logical :: msk(3,4)
   k = (/ 5, -2, 7, 0, -9, 3 /)
   print *, pack(k, k > 0)
   print *, pack(k, k < 0, (/ 10, 20, 30, 40 /))
   print *, size(pack(k, .true.)), size(pack(k, .false.))
   do j = 1, 4
      do i = 1, 3
         m(i, j) = mod(7 * i + 3 * j, 11) - 4
      end do
   end do
   msk = m > 0
   print *, unpack((/ 100, 200, 300, 400, 500, 600, 700, 800, 900 /), msk, 0)
   print *, unpack((/ 100, 200, 300, 400, 500, 600, 700, 800, 900 /), msk, m)
   print *, cshift(k, 2), cshift(k, -1)
   print *, eoshift(k, 2), eoshift(k, -3, 99)
   print *, cshift(m, 1, 2)
   print *, eoshift(m, -1, dim = 1)
   print *, sum(m, dim = 1), sum(m, 2)
   print *, maxval(m, 1), minval(m, dim = 2), product(m(1:2, :), 1)
   print *, count(msk, 1), any(m > 4, 2), all(m > -4, dim = 1)
   print *, maxloc(m, 1), minloc(m, dim = 2)
   print *, sum(m, 1, msk), maxval(m, 2, m < 3)
   print *, maxloc(m), minloc(m), maxloc(k), maxloc(m, mask = m < 0)
   l2 = minloc(m(2:3, :))
   print *, l2
   do kk = 1, 2
      do j = 1, 3
         do i = 1, 2
            n3(i, j, kk) = i * 100 + j * 10 + kk
         end do
      end do
   end do
   print *, sum(n3, 3), sum(n3, dim = 2), maxloc(n3)
   a = (/ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 /)
   b = reshape(a, (/ 2, 3 /))
   call olds(a * 2.0 - 1.0, 6)
   call olds(sqrt(a), 6)
   z = total(a + 0.5) + total(b(1, :) * b(2, :))
   print '(F8.2)', z
   print '(F8.2)', total(sum(b, 1)), total(pack(a, a > 2.5))
end program
