! FORALL on module, SAVEd and COMMON arrays; array-valued functions in
! the body and the mask, 3000 turns of them (their results are freed).
module f95forall5m
  implicit none
  type t
    integer :: v(3)
    real :: w
  end type
  type(t), save :: gs
  integer :: garr(5) = (/ 1, 2, 3, 4, 5 /)
contains
  pure function twice(x) result(r)
    integer, intent(in) :: x(:)
    integer :: r(size(x))
    r = 2 * x
  end function
end module
program f95forall5
  use f95forall5m
  implicit none
  type(t) :: ls
  type(t), save :: ss
  integer :: i, j, m(3, 3), a(3), big(1000), it
  common /blk/ cc
  integer :: cc(4)
  gs%v = (/ 1, 2, 3 /)
  forall (i = 1:3) gs%v(i) = gs%v(4 - i) + 10
  print *, gs%v
  ss%v = (/ 4, 5, 6 /)
  forall (i = 1:3) ss%v(i) = ss%v(4 - i) + 10
  print *, ss%v
  ls%v = (/ 7, 8, 9 /)
  forall (i = 1:3) ls%v(i) = ls%v(4 - i) + 10
  print *, ls%v
  forall (i = 1:5) garr(i) = garr(6 - i)
  print *, garr
  cc = (/ 1, 2, 3, 4 /)
  forall (i = 1:4) cc(i) = cc(5 - i)
  print *, cc
  ! hoists in the body
  m = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), (/ 3, 3 /))
  forall (i = 1:3) a(i) = sum(m(i, :))
  print *, a
  forall (i = 1:3) a(i) = sum(twice(m(:, i))) + a(4 - i)
  print *, a
  forall (i = 1:3) m(i, :) = twice(m(:, i))
  print *, m
  forall (i = 1:3) m(:, i) = (/ i, a(i), maxval(m(i, :)) /)
  print *, m
  forall (i = 1:3, sum(m(:, i)) > 60) a(i) = -1
  print *, a
  ! leaks
  big = 1
  do it = 1, 3000
    forall (i = 1:3, sum(big) > 0) m(i, :) = twice(m(:, i)) / 2
    forall (i = 1:1000) big(i) = big(1001 - i)
  end do
  print *, m
end program
