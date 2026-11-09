module ma
  interface show
    module procedure show_i
  end interface
contains
  subroutine show_i(i)
    integer i
    print *, 'int', i
  end subroutine
end module
module mb
  interface show
    module procedure show_r
  end interface
contains
  subroutine show_r(x)
    real x
    print *, 'real', x
  end subroutine
end module
module mc
  use ma
  type v
    integer :: a
  end type
  interface operator(+)
    module procedure vadd
  end interface
contains
  function vadd(p, q)
    type(v), intent(in) :: p, q
    type(v) :: vadd
    vadd%a = p%a + q%a
  end function
end module
subroutine sub2
  use mb
  use ma
  call show(2.5)
  call show(7)
end subroutine
program t
  use ma
  use mb
  call show(1)
  call show(1.5)
  call sub2
  call sub3

end program
subroutine sub3
  use mc            ! ma's SHOW and mc's + through mc
  type(v) :: x
  x = v(2) + v(40)
  call show(x%a)
end subroutine
