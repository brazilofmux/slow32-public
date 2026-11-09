module m
  type t
     integer :: n
     real :: a(6)
     real, allocatable :: v(:)
  end type t
end module m
subroutine s(k)
  use m
  integer :: k, i
  integer :: guard1(4)
  type(t), allocatable :: z(:)
  integer :: guard2(4)
  type(t), pointer :: p
  guard1 = 11; guard2 = 22
  print *, allocated(z)
  allocate(z(k))
  do i = 1, k
     print *, i, allocated(z(i)%v)
  end do
  allocate(z(2)%v(5))
  z(2)%v = 2.5
  z(2)%n = 9
  print *, z(2)%n, z(2)%v, guard1, guard2
  allocate(p)
  print *, allocated(p%v)
  allocate(p%v(2)); p%v = 1.0
  print *, p%v
  deallocate(p%v)
  deallocate(p)
end subroutine s
program pp
  call s(3)
  call s(4)
end program pp
