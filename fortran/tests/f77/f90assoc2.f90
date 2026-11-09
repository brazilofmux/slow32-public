! ASSOCIATED with component arrays and their sections, ALLOCATABLE and
! assumed-shape targets, TARGET= by keyword.
module m4
  implicit none
  type t
    integer :: v(6)
  end type
contains
  subroutine chk(x, p)
    integer, target :: x(:)
    integer, pointer :: p(:)
    print *, associated(p, x), associated(p, x(2:)), associated(p, x(1:size(x):1)), associated(p, x(::2))
  end subroutine
end module
program p4b
  use m4
  implicit none
  integer, target, allocatable :: a(:)
  integer, target :: c(0:9), m(4,4)
  type(t), target :: s
  integer, pointer :: p(:), q(:)
  integer :: i
  allocate (a(8))
  a = (/ (i, i = 1, 8) /)
  p => a
  print *, associated(p, a), associated(p, a(1:8)), associated(p, a(1:7)), associated(p, a(:))
  p => a(3:)
  print *, associated(p, a), associated(p, a(3:8)), associated(p, a(3:))
  call chk(a, p)
  p => a
  call chk(a, p)
  call chk(a(1:8:2), p)
  p => a(1:8:2)
  call chk(a(1:8:2), p)
  call chk(a, p)
  p => c(0:8:2)
  q => p(2:3)
  print *, associated(q, c(2:4:2)), associated(q, c(2:5:2)), associated(q, c(2:3)), associated(q, p(2:3)), associated(p, c)
  p => s%v(2:5)
  print *, associated(p, s%v(2:5)), associated(p, s%v), associated(p, s%v(2:4))
  p => s%v
  print *, associated(p, s%v(1:6)), associated(p, s%v), associated(p, target=s%v(2:6))
  p => m(2, :)
  print *, associated(p, m(2, :)), associated(p, m(2, 1:4)), associated(p, m(:, 2)), associated(p, m(3, :))
  p => m(:, 3)
  print *, associated(p, m(:, 3)), associated(p, m(1:4, 3)), associated(p, m(2:4, 3))
  nullify (p)
  print *, associated(p, a), associated(p, a(1:3))
end program
