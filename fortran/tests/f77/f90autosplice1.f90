! An automatic array in a routine small enough to splice into its caller,
! in each way of declaring one: its bounds in a DIMENSION attribute, a
! CHARACTER array, an array of structures.  The locals declared before
! it are what an array with too little room would be stored over.
program p
  call foo(5)
  call bar(6)
  call baz(7)
  call qux(3)
end program
subroutine foo(n)
  integer :: before(4)
  integer, dimension(n) :: a
  integer :: i
  before = 7
  do i = 1, n; a(i) = i * i; end do
  print *, 'foo', sum(a), size(a), before
end subroutine
subroutine bar(n)
  character(len=4) :: before(2)
  character(len=3), dimension(n) :: c
  integer :: i
  before = 'keep'
  do i = 1, n; c(i) = 'ab' // achar(48 + i); end do
  print *, 'bar ', c(1), c(n), size(c), before
end subroutine
subroutine baz(n)
  integer before(4)
  character*4 d(n)
  integer i
  before(1) = 1
  before(2) = 2
  before(3) = 3
  before(4) = 4
  do i = 1, n
    d(i) = 'wxyz'
  end do
  print *, 'baz ', d(n), size(d), before
end subroutine
subroutine qux(n)
  type t
    integer :: k
    real :: r
  end type
  real :: before(4)
  type(t) :: s(n)
  integer :: i
  before = 2.5
  do i = 1, n; s(i)%k = i; s(i)%r = 0.5 * i; end do
  print *, 'qux', s(n)%k, s(n)%r, size(s), before
end subroutine
