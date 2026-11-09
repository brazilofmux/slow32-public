! Sections as actual arguments (Fortran 90) to procedures with explicit-
! shape dummies: strided and non-leading sections through a temporary,
! copied in and back out; contiguous ones by address; subscripts evaluated
! once, before the call, though the call changes what they were made of.
program f90secact1
   implicit none
   integer :: i, j, n
   real :: a(8), m(4,5)
   integer*2 :: h(6)
   complex*16 :: z(4)
   double precision, allocatable :: d(:,:)
   a = (/ (real(i), i = 1, 8) /)
   do j = 1, 5
      do i = 1, 4
         m(i, j) = real(10 * i + j)
      end do
   end do
   call neg(a(1:7:3), 3)
   print '(8F6.1)', a
   call neg(m(2, :), 5)
   call neg(m(1:4:2, 2:3), 4)
   call neg(m(:, 4), 4)
   call neg(m(:, 5:5), 4)
   print '(5F7.1)', ((m(i, j), j = 1, 5), i = 1, 4)
   n = 2
   call bump(a(n:8:n), 4, n)
   print '(8F6.1)', a
   print *, 'n', n
   call neg(a(5:4), 0)
   h = (/ 1, 2, 3, 4, 5, 6 /)
   call hdbl(h(6:1:-2), 3)
   print *, (int(h(i)), i = 1, 6)
   z = (/ (1d0, 1d0), (2d0, -1d0), (3d0, 0.5d0), (4d0, 2d0) /)
   call zconj(z(4:1:-3), 2)
   print *, z
   allocate (d(3, 3))
   d = 1d0
   call dset(d(2, 1:3), 3)
   call dset(d(:, 2), 3)
   print '(3F6.1)', ((d(i, j), j = 1, 3), i = 1, 3)
   print *, total(m(3, 1:5:2), 3), total(m(:, 1), 4)
contains
   subroutine neg(x, k)
      integer k
      real x(k)
      x = -x
   end subroutine
   subroutine bump(x, k, nn)
      integer k, nn
      real x(k)
      nn = 99
      x = x + 100.0
   end subroutine
   subroutine hdbl(x, k)
      integer k
      integer*2 x(k)
      x = x * 2
   end subroutine
   subroutine zconj(x, k)
      integer k
      complex*16 x(k)
      x = conjg(x)
   end subroutine
   subroutine dset(x, k)
      integer k
      double precision x(k)
      x = x + dble(k)
   end subroutine
   real function total(x, k)
      integer k
      real x(k)
      total = sum(x)
      x(1) = 0.0
   end function
end program
