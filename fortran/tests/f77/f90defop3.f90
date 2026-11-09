module va
  type v
    integer :: a
  end type
  interface operator(+)
    module procedure vadd
  end interface
  interface operator(.x.)
    integer function ext(p, q)
      integer, intent(in) :: p, q
    end function
  end interface
contains
  function vadd(p, q)
    type(v), intent(in) :: p, q
    type(v) :: vadd
    vadd%a = p%a + q%a
  end function
end module
module wa
  use va
  type w
    integer :: b
  end type
  interface operator(+)
    module procedure wadd, wvadd
  end interface
contains
  function wadd(p, q)
    type(w), intent(in) :: p, q
    type(w) :: wadd
    wadd%b = p%b * q%b
  end function
  function wvadd(p, q)
    type(w), intent(in) :: p
    type(v), intent(in) :: q
    type(w) :: wvadd
    wvadd%b = p%b - q%a
  end function
end module
program t
  use va
  use wa
  type(v) :: x
  type(w) :: y
  x = v(3) + v(4)
  y = w(5) + w(6)
  print *, x%a, y%b
  y = y + x
  print *, y%b, 6 .x. 7
end program
integer function ext(p, q)
  integer, intent(in) :: p, q
  ext = 10*p + q
end function
