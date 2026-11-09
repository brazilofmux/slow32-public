module money
  implicit none
  private
  type, public :: cents
    integer :: c
  end type
  public :: operator(+), operator(*), assignment(=), operator(.pct.), operator(==)
  interface operator(+)
    module procedure add
  end interface
  interface operator(*)
    module procedure times
  end interface
  interface operator(.pct.)
    module procedure pct
  end interface
  interface operator(==)
    module procedure same
  end interface
  interface assignment(=)
    module procedure from_str, to_str
  end interface
contains
  function add(a, b)
    type(cents), intent(in) :: a, b
    type(cents) :: add
    add%c = a%c + b%c
  end function
  function times(n, a)
    integer, intent(in) :: n
    type(cents), intent(in) :: a
    type(cents) :: times
    times%c = n * a%c
  end function
  function pct(a, p)
    type(cents), intent(in) :: a
    integer, intent(in) :: p
    type(cents) :: pct
    pct%c = (a%c * p + 50) / 100
  end function
  logical function same(a, b)
    type(cents), intent(in) :: a, b
    same = a%c == b%c
  end function
  subroutine from_str(a, s)
    type(cents), intent(out) :: a
    character(len=*), intent(in) :: s
    integer :: d, k
    a%c = 0
    do k = 1, len(s)
      d = index('0123456789', s(k:k)) - 1
      if (d >= 0) a%c = a%c * 10 + d
    end do
  end subroutine
  subroutine to_str(s, a)
    character(len=*), intent(out) :: s
    type(cents), intent(in) :: a
    write (s, '(I0,A,I2.2)') a%c / 100, '.', mod(a%c, 100)
  end subroutine
end module
