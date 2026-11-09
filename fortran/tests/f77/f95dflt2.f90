module m9
  type t
    sequence
    integer :: n = 5
    integer :: v(2) = (/ 1, 2 /)
  end type
  type h
    integer :: b = 31
    real :: r = 1.0
    character(4) :: c = 'ab'
    logical :: l = .not. .false.
    integer :: e = 2**3 + len('abc')
    double precision :: d = 1d0 / 4
    complex :: z = (1.0, 2.0)
  end type
end module
integer function tny(k)
  use m9
  type(t) :: l
  tny = l%n + k + l%v(2)
  l%n = l%n + 1
  l%v(2) = 100
end function
subroutine o(x)
  use m9
  type(t), intent(out) :: x
end subroutine
program q9
  use m9
  integer :: tny, i
  type(t) :: x
  type(t), pointer :: p
  type(h) :: hh
  type(t) :: sv = t(8, (/ 3, 4 /))
  type(t), save :: s2
  type(t), parameter :: c = t(1, 2)
  do i = 1, 3
    print *, tny(i)
    x%n = i; x%v = i
    call o(x)
    print *, x
  end do
  allocate(p)
  print *, p%n, p%v
  print *, hh
  print *, sv, s2, c
end program
