! A pointer component of a structure against its target by name, either
! side of an array assignment: the value is evaluated before any element
! is stored (F90 7.5.1.5), so every line is 1 1 2 3 4 5.  Pinned by
! f90overlap6.expected rather than gfortran, which (14.2 and 16.2) stores
! as it goes in the first two and prints 1 1 1 1 1 1.
program f90overlap6
  implicit none
  type t
    integer, pointer :: p(:)
  end type
  type(t) :: x
  integer, target :: a(6)
  integer, pointer :: q(:)
  integer :: i
  a = (/ (i, i = 1, 6) /)
  x%p => a
  x%p(2:6) = a(1:5)
  print *, a
  a = (/ (i, i = 1, 6) /)
  a(2:6) = x%p(1:5)
  print *, a
  a = (/ (i, i = 1, 6) /)
  q => a
  q(2:6) = a(1:5)
  print *, a
  a = (/ (i, i = 1, 6) /)
  a(2:6) = q(1:5)
  print *, a
end program
