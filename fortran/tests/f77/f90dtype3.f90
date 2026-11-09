! Components of arrays of structures (Fortran 90 6.1.2): A%X and A(2:4)%Y
! as arrays -- assigned from and to, reduced, in WHERE, as an actual --
! through a descriptor whose strides are the structure's size over the
! component's.
program f90dtype3
  implicit none
  type pt
    real :: x, y
    integer :: tag
  end type
  type(pt) :: a(5)
  real :: xs(5)
  integer :: i
  do i = 1, 5
    a(i) = pt(real(i), real(10 * i), i)
  end do
  xs = a%x
  print *, xs
  print *, a%y, sum(a%x), maxval(a(2:4)%y)
  a%x = a%x * 2
  a(1:5:2)%tag = -1
  print *, a%x, a%tag
  where (a%y > 25.0) a%y = 0.0
  print *, a%y
  call show(a%tag, 5)
  print *, count(a%tag > 0), a(3:1:-1)%x
contains
  subroutine show(k, n)
    integer :: n, k(n)
    print *, 'show', k
  end subroutine
end program
