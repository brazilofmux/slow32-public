! Overlap in two dimensions, under WHERE, through dummies, with no
! elements at all, and in CHARACTER components and pointers.
module m6
  implicit none
  type g
    real :: m(3, 3)
    character(3) :: c(4)
  end type
contains
  subroutine sh(a, p, n)
    integer, target :: a(:)
    integer, pointer :: p(:)
    integer :: n
    a(2:n) = p(1:n-1)
  end subroutine
  subroutine sw(x)
    type(g) :: x
    where (x%m > 4.0) x%m = transpose(x%m) - x%m(3:1:-1, :)
  end subroutine
end module
program f90overlap5
  use m6
  implicit none
  type(g) :: x
  type(g), pointer :: xp
  type(g), target :: xt
  integer, target :: a(6)
  integer, pointer :: p(:)
  real, target :: r(3, 3)
  real, pointer :: rp(:, :)
  character(3), target :: ct(4)
  character(3), pointer :: cp(:)
  integer :: i, j, n
  a = (/ (i, i = 1, 6) /)
  p => a
  n = 1
  call sh(a, p, n)
  print *, a
  n = 6
  call sh(a, p, n)
  print *, a
  n = 0
  p(1:n) = a(6:7-n:-1)
  print *, a
  do j = 1, 3
    do i = 1, 3
      x%m(i, j) = i + 10 * j
      r(i, j) = i + 10 * j
    end do
  end do
  x%m = transpose(x%m)
  print '(9f5.0)', x%m
  x%m(1:2, :) = x%m(2:3, :)
  print '(9f5.0)', x%m
  x%m(:, 2:3) = x%m(:, 1:2) + x%m(:, 3:2:-1)
  print '(9f5.0)', x%m
  call sw(x)
  print '(9f5.0)', x%m
  rp => r
  rp(2:3, 2:3) = r(1:2, 1:2)
  print '(9f5.0)', r
  r(:, 1) = rp(1, :)
  print '(9f5.0)', r
  where (r > 12.0) r = rp(3:1:-1, 3:1:-1)
  print '(9f5.0)', r
  x%c = (/ 'abc', 'def', 'ghi', 'jkl' /)
  x%c(2:4) = x%c(1:3)
  print *, x%c
  x%c = x%c(4:1:-1)
  print *, x%c
  ct = (/ 'abc', 'def', 'ghi', 'jkl' /)
  cp => ct
  cp(2:4) = ct(1:3)
  print *, ct
  ct(1:3) = cp(2:4)(2:3) // cp(4:2:-1)(1:1)
  print *, ct
  xt = x
  xp => xt
  xp%c(2:4) = xt%c(1:3)
  print *, xt%c
  xt%m(1, :) = xp%m(:, 1)
  print '(9f5.0)', xt%m
  xt%m = xp%m(1, 1)
  print '(9f5.0)', xt%m
end program
