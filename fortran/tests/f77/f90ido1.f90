! An implied DO whose items need evaluating first -- a constructor, an
! array-valued reference -- once for each value of its variable.
program s2
  implicit none
  type pt
    integer :: x
    real :: w(2)
  end type pt
  type(pt) :: a(3), b
  integer :: i
  real :: z(2)
  i = 3
  b = pt(i, (/ 0.5*i, 2.0*i /))
  print *, b
  a = (/ (pt(i, (/ 0.5*i, 2.0*i /)), i = 1, 3) /)
  print *, a
  a = (/ pt(1, (/ 1.0, 2.0 /)), pt(2, (/ 3.0, 4.0 /)), pt(3, (/ 5.0, 6.0 /)) /)
  print *, a
  z = (/ 8.0, 9.0 /)
  a = (/ (pt(i, z), i = 1, 3) /)
  print *, a
  a = (/ (pt(i, z + i), i = 1, 3) /)
  print *, a
end program s2
