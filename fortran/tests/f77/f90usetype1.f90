! USE with a derived type renamed (USE M, POINT => PT): declarations,
! constructors and a module function's result by the local name.
module m3
  implicit none
  type pt
    integer :: x, y
  end type pt
  integer :: cnt = 3
contains
  function mk(i) result(p)
    integer, intent(in) :: i
    type(pt) :: p
    p%x = i; p%y = -i
  end function mk
end module m3
program p3
  use m3, point => pt, n => cnt
  implicit none
  type(point) :: a
  a = mk(n)
  print *, a%x, a%y
  a = point(4, 5)
  print *, a
end program p3
