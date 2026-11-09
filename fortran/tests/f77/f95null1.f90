program a3
  implicit none
  type t
    integer :: n = 7
    real :: w(2) = (/ 1.5, 2.5 /)
    character(3) :: c = 'abc'
    integer, pointer :: p => null()
    integer :: plain
  end type
  type(t) :: x, y(2)
  integer, pointer :: q => null(), r(:) => null()
  type(t), allocatable :: z(:)
  print *, x%n, x%w, x%c, associated(x%p)
  print *, y(2)%n, y(1)%w, y(2)%c
  print *, associated(q), associated(r)
  allocate (z(2))
  print *, z(2)%n, z(1)%c
  allocate (q)
  q = 4
  q => null()
  print *, associated(q)
  call s
contains
  subroutine s
    type(t) :: l
    print *, l%n, l%c
    l%n = 9
  end subroutine
end program
