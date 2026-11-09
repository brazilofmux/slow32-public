! Sections of a structure's array and pointer components as the values of
! an array constructor, the constructor naming its target among them.
program f90aconcomp1
  implicit none
  type t
    integer :: a(6)
    integer, pointer :: p(:)
  end type
  type(t) :: x
  integer, target :: a(6)
  integer :: b(5), i
  a = (/ (i, i = 1, 6) /)
  x%a = a
  x%p => a
  b = (/ x%a(1:5) /)
  print *, b
  b = (/ x%p(2:6) /)
  print *, b
  b = (/ x%a(1:2), x%p(4:6) /) + 1
  print *, b
  print *, (/ x%a(3:4), x%p(1:2) /)
  x%a(2:6) = (/ x%a(1:5) /)
  print *, x%a
  a(2:6) = (/ a(1:5) /)
  print *, a
end program
