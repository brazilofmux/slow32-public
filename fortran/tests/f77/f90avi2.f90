! Array-valued intrinsics and constructors as actual arguments, in WHERE
! masks, in I/O lists, in a DO WHILE condition (evaluated afresh each
! time), and MERGE, elemental.
module avm
contains
   subroutine show(x)
      real :: x(:,:)
      integer :: i
      print '(A,2I3)', ' shape', size(x, 1), size(x, 2)
      do i = 1, size(x, 1)
         print '(4F7.2)', x(i, :)
      end do
   end subroutine
   real function tr(x)
      real :: x(:,:)
      integer :: i
      tr = 0.0
      do i = 1, min(size(x, 1), size(x, 2))
         tr = tr + x(i, i)
      end do
   end function
   integer function isum(k)
      integer :: k(:)
      isum = sum(k)
   end function
end module

subroutine olds(v, n)
   integer n
   real v(n)
   print '(A,4F7.2)', ' olds', v
end subroutine

program f90avi2
   use avm
   implicit none
   real :: a(2,3), b(2,2)
   integer :: i, j, k(4), it
   integer*2 :: h(3)
   do j = 1, 3
      do i = 1, 2
         a(i, j) = real(i * j) - 2.5
      end do
   end do
   call show(transpose(a))
   call show(matmul(a, transpose(a)))
   print '(F8.2)', tr(matmul(transpose(a), a)), tr(reshape((/ 1.0, 2.0, 3.0, 4.0 /), (/ 2, 2 /)))
   print *, isum((/ 1, 2, 3 /)), isum(shape(a)), isum((/ (i * i, i = 1, 4) /))
   call olds((/ 1.0, 2.5, -1.0, 0.5 /), 4)
   call olds(reshape(transpose(a(:, 1:2)), (/ 4 /)), 4)
   b = 0.0
   where (matmul(a, transpose(a)) > 5.0) b = 1.0
   print '(2F5.1)', b
   print '(3F7.2)', a * 2.0
   print *, int(abs(a(1, :))), shape(a), lbound(a)
   h = (/ 4, -5, 6 /)
   print *, (int(h(i)), i = 1, 3)
   k = merge((/ 1, 2, 3, 4 /), (/ -1, -2, -3, -4 /), (/ .true., .false., .false., .true. /))
   print *, k
   a = merge(a, -a, a > 0.0)
   print '(3F7.2)', a
   print *, merge(1.5d0, 2.5d0, k(2) > 0), merge(.true., .false., .false.)
   k = (/ 1, 2, 3, 4 /)
   it = 0
   do while (any(spread(k, 1, 2) < 10))
      k = k * 2
      it = it + 1
   end do
   print *, it, k
   if (all(transpose(reshape((/ 1, 2, 3, 4 /), (/ 2, 2 /))) == reshape((/ 1, 3, 2, 4 /), (/ 2, 2 /)))) print *, 'transposed'
end program
