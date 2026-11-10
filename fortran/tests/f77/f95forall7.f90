! FORALL on arrays whose lower bounds are not 1, an assumed-size array
! (nothing read that is stored), CHARACTER(*) arrays, a vector subscript
! on the left, COMPLEX, DOUBLE PRECISION, a constructor's implied DO in
! the body, lists that come up empty, strides in nested lists.
module f95forall7m
  implicit none
  integer :: mv(-2:2) = (/ 1, 2, 3, 4, 5 /)
  double precision :: dm(2, 0:2)
contains
  subroutine asz(a, n)
    integer :: n, a(*), i
    forall (i = 1:n) a(i) = 2 * i
  end subroutine
  subroutine chs(c)
    character(len=*) :: c(:)
    integer :: i
    forall (i = 1:size(c)) c(i) = c(size(c) + 1 - i)
    forall (i = 1:size(c)) c(i)(1:1) = c(i)(len(c):len(c))
  end subroutine
  subroutine lb(a)
    integer :: a(0:, -1:), i, j
    forall (i = 0:ubound(a, 1), j = -1:ubound(a, 2)) a(i, j) = a(ubound(a, 1) - i, j) + 1000 * j
  end subroutine
end module
program f95forall7
  use f95forall7m
  implicit none
  integer :: i, j, k, a(0:5), b(6), idx(6), m(3, 3), e1(6), e2(6), n
  complex :: z(3)
  double precision :: d(4)
  character(len=5) :: cc(3)
  integer, allocatable :: al(:, :)
  forall (i = -2:2) mv(i) = mv(-i) * 10
  print *, mv
  forall (i = 1:2, j = 0:2) dm(i, j) = i + j / 4.0d0
  forall (i = 1:2, j = 0:2) dm(i, j) = dm(3 - i, 2 - j)
  print '(6f6.2)', dm
  forall (i = 0:5) a(i) = i
  forall (i = 0:5) a(i) = a(5 - i)
  print *, a
  call asz(b, 6)
  print *, b
  cc = (/ 'alpha', 'bravo', 'charl' /)
  call chs(cc)
  print *, cc
  allocate (al(0:2, -1:1))
  forall (i = 0:2, j = -1:1) al(i, j) = 10 * i + j
  call lb(al)
  print *, al
  idx = (/ 2, 3, 4, 5, 6, 1 /)
  forall (i = 1:6) b(idx(i)) = b(i)
  print *, b
  z = (/ (1.0, 2.0), (3.0, 4.0), (5.0, 6.0) /)
  forall (i = 1:3) z(i) = conjg(z(4 - i))
  print '(6f6.1)', z
  d = (/ 1.0d0, 2.0d0, 3.0d0, 4.0d0 /)
  forall (i = 1:4, d(i) > 1.5d0) d(i) = d(i - 1) / 2
  print '(4f8.4)', d
  ! an implied DO in a constructor in the body
  forall (i = 1:3) m(i, :) = (/ (j * i, j = 1, 3) /)
  print *, m
  forall (i = 1:3) m(:, i) = (/ (m(k, 4 - i), k = 3, 1, -1) /)
  print *, m
  k = 5
  b(1:3) = (/ (k, k = 1, 3) /)
  print *, b(1:3), k
  ! empty lists
  n = 0
  forall (i = 1:n)
    forall (j = 1:3) m(i, j) = -1
  end forall
  forall (i = 1:3)
    forall (j = 1:3, m(i, j) > 100000) m(i, j) = -1
    forall (j = 1:n) m(i, j) = -1
  end forall
  forall (i = 1:3, .false.) m(i, i) = -1
  print *, m
  ! strides in nested lists
  m = 0
  forall (i = 3:1:-2)
    forall (j = 1:i:2) m(i, j) = 10 * i + j
  end forall
  print *, m
end program
