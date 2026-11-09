! Assumed-shape dummies (Fortran 90): A(:), M(:,:), lower bounds of the
! declaration's; whole arrays, sections (strided, reversed, a row), an
! ALLOCATABLE, an assumed-shape array passed on whole or in sections, and
! passed to an old-style procedure (packed when its strides make no run).
module ashape
contains
   subroutine show(a)
      real :: a(:)
      integer :: i
      print '(A,I3,8F7.2)', ' show', size(a), (a(i), i = 1, size(a))
   end subroutine
   subroutine scale(a, f)
      real, intent(inout) :: a(:)
      real, intent(in) :: f
      a = a * f
   end subroutine
   real function total(m)
      real, dimension(:,:) :: m
      total = sum(m)
   end function
   subroutine mshow(m)
      real :: m(0:, :)
      integer :: i, j
      print *, 'mshape', lbound(m, 1), ubound(m, 1), lbound(m, 2), ubound(m, 2), size(m)
      do i = 0, ubound(m, 1)
         print '(8F7.2)', (m(i, j), j = 1, size(m, 2))
      end do
   end subroutine
   subroutine pass(a)
      real :: a(:)
      call show(a)
      call show(a(size(a):1:-1))
      call scale(a(1:size(a):2), -1.0)
      call oldsum(a, size(a))
   end subroutine
   subroutine zset(z)
      complex*16 :: z(:)
      z = z + (1d0, -1d0)
   end subroutine
   integer function icount(k)
      integer :: k(:,:)
      icount = count(k > 2)
      k(1, :) = 0
   end function
end module

subroutine oldsum(x, n)
   integer n
   real x(n)
   print '(A,F8.2)', ' oldsum', sum(x)
   x(1) = 99.0
end subroutine

program f90ash1
   use ashape
   implicit none
   integer :: i, j
   real :: v(8), m(3,4)
   real, allocatable :: w(:)
   complex*16 :: z(3)
   integer :: k(2,3)
   v = (/ (real(i), i = 1, 8) /)
   do j = 1, 4
      do i = 1, 3
         m(i, j) = real(i) + 0.1 * real(j)
      end do
   end do
   call show(v)
   call show(v(2:7:2))
   call show(m(2, :))
   call show(m(:, 3))
   call scale(v(8:1:-3), 10.0)
   call show(v)
   print '(A,F8.2)', ' total', total(m)
   print '(A,F8.2)', ' total', total(m(1:3:2, 2:4))
   call mshow(m)
   call mshow(m(2:3, 1:4:3))
   allocate (w(5))
   w = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)
   call scale(w, 2.0)
   call show(w)
   call pass(v)
   call show(v)
   call pass(m(3, :))
   print '(4F7.2)', ((m(i, j), j = 1, 4), i = 1, 3)
   z = (/ (1d0, 1d0), (2d0, 2d0), (3d0, 3d0) /)
   call zset(z(1:3:2))
   print *, z
   do j = 1, 3
      do i = 1, 2
         k(i, j) = i + 2 * (j - 1)
      end do
   end do
   i = icount(k)
   print *, i, k
end program
