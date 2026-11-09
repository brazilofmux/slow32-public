program q8
  implicit none
  type in
    integer :: k = 2
    real :: r(2) = (/ 4.0, 5.0 /)
  end type
  type out
    type(in), allocatable :: v(:)
    type(in), pointer :: pv(:) => null()
    type(in), pointer :: ps => null()
    type(in) :: one
    integer :: m = 1
  end type
  type(out) :: x, y
  type(out), allocatable :: ax(:)
  integer :: i, st
  print *, allocated(x%v), associated(x%pv), associated(x%ps), x%one, x%m
  allocate(x%v(2), x%pv(3), x%ps)
  print *, x%v(2)%k, x%v(1)%r, x%pv(3)%k, x%pv(2)%r, x%ps%k, x%ps%r
  x%v(1)%k = 9
  allocate(x%v(2), stat=st)
  print *, st /= 0, x%v(1)%k
  y = x
  x%v(1)%r(1) = 99.0
  print *, y%v(1)%r, y%v(1)%k, associated(y%ps, x%ps)
  allocate(ax(2))
  print *, allocated(ax(2)%v), associated(ax(1)%pv), ax(2)%one%r, ax(1)%m
  allocate(ax(2)%v(1))
  print *, ax(2)%v(1)
  do i = 1, 300
    call work
  end do
  deallocate(x%v, x%pv, x%ps)
  print *, allocated(x%v), allocated(y%v)
contains
  subroutine work
    type(out) :: l
    type(out), allocatable :: la(:)
    allocate(l%v(20000))
    allocate(la(2))
    allocate(la(2)%v(20000))
    if (l%v(20000)%k /= 2 .or. la(2)%v(777)%r(2) /= 5.0) print *, 'bad'
  end subroutine
end program
