function fcf(a, n) bind(c, name="fcf")
  use iso_c_binding
  complex(c_float_complex), value :: a
  integer(c_int), value :: n
  complex(c_float_complex) :: fcf
  fcf = a * real(n)
end function
function fcd(a, b) bind(c, name="fcd")
  use iso_c_binding
  complex(c_double_complex), value :: a, b
  complex(c_double_complex) :: fcd
  fcd = a + conjg(b)
end function
subroutine fstr(s, n) bind(c, name="fstr")
  use iso_c_binding
  integer(c_int), value :: n
  character(kind=c_char), intent(inout) :: s(n)
  integer :: i
  do i = 1, n
    s(i) = achar(iachar(s(i)) - 32)
  end do
end subroutine
integer function arrsum(a, n)
  integer, intent(in) :: n, a(n)
  integer :: i
  arrsum = 0
  do i = 1, n
    arrsum = arrsum + a(i) * i
  end do
end function
function rmul(x, y)
  double precision :: rmul, x, y
  rmul = x * y + 0.5d0
end function
