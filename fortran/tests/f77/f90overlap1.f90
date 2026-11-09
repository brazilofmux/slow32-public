! An array assignment's value is evaluated whole before any of it is
! stored (F90 7.5.1.5).  Here it reaches its target through a POINTER or
! a TARGET, by another name: a temporary holds it.
program f90overlap1
  implicit none
  integer, target :: a(6)
  integer, pointer :: p(:)
  integer :: i
  a = (/ (i, i = 1, 6) /)
  p => a
  a(2:6) = p(1:5)
  print *, a
  a = (/ (i, i = 1, 6) /)
  p(2:6) = a(1:5)
  print *, a
  a = (/ (i, i = 1, 6) /)
  p => a(1:5)
  a(2:6) = p + 10
  print *, a
  call sub(a, a)
contains
  subroutine sub(x, y)
    integer, target :: x(:)
    integer, target :: y(:)
    integer, pointer :: q(:)
    q => x
    x = (/ (i, i = 1, 6) /)
    q(2:6) = x(1:5)
    print *, y
  end subroutine
end program
