module ma
  implicit none
  interface dbl
    module procedure dbi, dbr
  end interface
  interface operator(.x.)
    module procedure xop, xlg
  end interface
  type v2
    real :: x, y
  end type
  interface operator(+)
    module procedure vadd
  end interface
  interface operator(.dot.)
    module procedure vdot
  end interface
  interface assignment(=)
    module procedure v_from_r, r_from_v, v_from_l
  end interface
  interface kind_of
    module procedure kind_i, kind_l
  end interface
contains
  elemental function vadd(a, b) result(r)
    type(v2), intent(in) :: a, b
    type(v2) :: r
    r%x = a%x + b%x
    r%y = a%y + b%y
  end function
  elemental real function vdot(a, b)
    type(v2), intent(in) :: a, b
    vdot = a%x * b%x + a%y * b%y
  end function
  elemental subroutine v_from_r(a, r)
    type(v2), intent(out) :: a
    real, intent(in) :: r
    a%x = r
    a%y = -r
  end subroutine
  elemental subroutine r_from_v(r, a)
    real, intent(out) :: r
    type(v2), intent(in) :: a
    r = a%x + a%y
  end subroutine
  elemental subroutine v_from_l(a, l)
    type(v2), intent(out) :: a
    logical, intent(in) :: l
    a%x = 0.0
    if (l) a%x = 1.0
    a%y = a%x
  end subroutine
  elemental integer function kind_i(i)
    integer, intent(in) :: i
    kind_i = 100 + i
  end function
  elemental integer function kind_l(l)
    logical, intent(in) :: l
    kind_l = 200
    if (l) kind_l = 201
  end function
  elemental logical function xlg(a, b)
    logical, intent(in) :: a, b
    xlg = a .neqv. b
  end function
  elemental function cube(x) result(r)
    real, intent(in) :: x
    real :: r
    r = x * x * x
  end function
  elemental integer function dbi(i)
    integer, intent(in) :: i
    dbi = 2 * i
  end function
  elemental real function dbr(r)
    real, intent(in) :: r
    dbr = 2.0 * r
  end function
  elemental subroutine swap(a, b)
    integer, intent(inout) :: a, b
    integer :: t
    t = a; a = b; b = t
  end subroutine
  pure function plain(x) result(r)
    real, intent(in) :: x(:)
    real :: r
    r = sum(x)
  end function
  elemental function xop(a, b) result(r)
    integer, intent(in) :: a, b
    integer :: r
    r = a * 100 + b
  end function
end module
