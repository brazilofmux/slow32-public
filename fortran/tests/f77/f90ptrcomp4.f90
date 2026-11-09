program q3
  implicit none
  type u
    integer :: n
    real :: w(3)
  end type
  type(u), pointer :: pu
  type(u), target :: tu
  real :: x(3)
  allocate(pu)
  pu%n = 1
  pu%w = (/ 1.0, 2.0, 3.0 /)
  print *, pu%w(1), pu%w(2), pu%w(3)
  x = pu%w
  print *, x
  print *, sum(pu%w)
  print *, pu%w
  print *, pu%w + 1.0
  tu%w = (/ 4.0, 5.0, 6.0 /)
  pu => tu
  print *, pu%w
  write(*,*) pu%w(1:3)
  call s(pu%w)
contains
  subroutine s(a)
    real :: a(3)
    print *, a
  end subroutine
end program
