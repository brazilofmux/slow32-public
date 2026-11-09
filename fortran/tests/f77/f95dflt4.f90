module m6
  implicit none
  type t
    integer :: n = 5
    real, allocatable :: a(:)
  end type
contains
  subroutine big(x)
    type(t), intent(inout) :: x
    if (allocated(x%a)) deallocate(x%a)
    allocate(x%a(5000))
    x%a = 1.0
  end subroutine
  subroutine out1(x)
    type(t), intent(out) :: x
  end subroutine
  subroutine outn(x, n)
    integer :: n
    type(t), intent(out) :: x(n)
  end subroutine
  function mk() result(r)
    type(t) :: r
    call big(r)
  end function
  subroutine loc(k, s)
    integer :: k
    real :: s
    type(t) :: l, la(k), lb(2)
    type(t), allocatable :: al(:)
    type(t), pointer :: pa(:)
    call big(l); call big(la(k)); call big(lb(1))
    allocate(al(k))
    call big(al(1)); call big(al(k))
    allocate(pa(2))
    call big(pa(2))
    lb(2) = pa(2)
    s = s + l%a(1) + la(k)%a(2) + lb(1)%a(3) + al(1)%a(4) + al(k)%a(5) + pa(2)%a(6) + lb(2)%a(7)
    deallocate(pa)
    if (mod(k, 2) == 0) deallocate(al)
  end subroutine
end module
program q6
  use m6
  implicit none
  type(t) :: x, y(3), z
  integer :: i
  real :: s
  s = 0
  do i = 1, 400
    call loc(2 + mod(i, 2), s)
    call big(x)
    call out1(x)
    call big(y(1)); call big(y(3))
    call outn(y, 3)
    z = mk()
    s = s + z%a(9)
  end do
  print *, s
end program
