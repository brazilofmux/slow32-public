module m
  implicit none
  integer, parameter :: dp = kind(1d0)
  type inner
    integer :: a = 3
    real(dp) :: d = 2.5d0
  end type
  type t
    integer :: n = 7
    real :: w(3) = (/ 1.5, 2.5, 3.5 /)
    character(5) :: c = 'abc'
    logical :: l = .true.
    complex :: z = (1.0, -2.0)
    integer :: h = 12
    type(inner) :: in
    type(inner) :: arr(2)
    real :: fill = 9.0
    integer :: bc(2) = 4
    integer, pointer :: p => null()
    integer :: plain
  end type
  type u
    integer :: n = 7
    real :: w(3) = (/ 1.5, 2.5, 3.5 /)
    character(5) :: c = 'abc'
    type(inner) :: in = inner(4, 4d0)
    integer, pointer :: p => null()
  end type
  type(t) :: mv
  type(t) :: ma(2)
  type(t), save :: ms
contains
  subroutine show(x, tag)
    type(t), intent(in) :: x
    character(*), intent(in) :: tag
    print *, tag, x%n, x%w, x%c, x%l, x%z, x%h
    print *, x%in%a, x%in%d, x%arr(2)%a, x%arr(1)%d, x%fill, x%bc, associated(x%p)
  end subroutine
  subroutine reset(x)
    type(t), intent(out) :: x
  end subroutine
  subroutine reseta(x, n)
    integer, intent(in) :: n
    type(t), intent(out) :: x(n)
  end subroutine
  subroutine reset2(x)
    type(t), intent(out) :: x(2)
  end subroutine
  subroutine reseto(k, x)
    integer :: k
    type(t), intent(out), optional :: x
    print *, 'reseto', k, present(x)
  end subroutine
  function make() result(r)
    type(t) :: r
    r%plain = 1
  end function
  subroutine twice(k)
    integer :: k
    type(t) :: l
    type(t), save :: sv
    type(t) :: la(2)
    call show(l, 'l ')
    call show(la(2), 'la')
    print *, 'sv', sv%n
    l%n = 100 + k
    l%w = 0
    la(2)%c = 'zz'
    la(2)%in%a = k
    sv%n = sv%n + 1
  end subroutine
  subroutine autoarr(n)
    integer :: n
    type(t) :: au(n)
    call show(au(n), 'au')
    au(n)%n = 0
  end subroutine
end module
program q1
  use m
  implicit none
  type(t) :: x, y(2)
  type(u) :: e = u(1, (/ 9.0, 8.0, 7.0 /), 'q', inner(8, 1d0), null())
  type(u) :: e2
  type(inner) :: i1, i2(3)
  call show(x, 'x ')
  call show(y(2), 'y ')
  call show(mv, 'mv')
  call show(ma(2), 'ma')
  call show(ms, 'ms')
  print *, e%n, e%w, e%c, e%in, associated(e%p)
  print *, e2%n, e2%w, e2%c, e2%in, associated(e2%p)
  print *, i1, i2
  x%n = 1; x%c = 'changed'; x%in%d = 0
  call reset(x)
  call show(x, 'x ')
  y(1)%n = 5; y(2)%n = 6
  call reseta(y, 2)
  call show(y(1), 'y1')
  call show(y(2), 'y2')
  y(1)%n = 5; y(2)%n = 6
  call reset2(y)
  call show(y(1), 'y1')
  call show(y(2), 'y2')
  call reseto(1)
  x%n = 44
  call reseto(2, x)
  print *, x%n
  x = make()
  call show(x, 'mk')
  call twice(1)
  call twice(2)
  call autoarr(2)
  call autoarr(3)
end program
