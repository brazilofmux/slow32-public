! A derived type renamed by USE across files: SHAPES sees GEOM's PT as
! POINT and re-exports it by that name (the .mod file carries both); the
! program renames it again, and SEG, whose components are PTs.
module geom
  implicit none
  type pt
    integer :: x, y
  end type pt
  type seg
    type(pt) :: a, b
  end type seg
  interface operator(+)
    module procedure addpt
  end interface
contains
  function addpt(p, q) result(r)
    type(pt), intent(in) :: p, q
    type(pt) :: r
    r%x = p%x + q%x; r%y = p%y + q%y
  end function
  function mid(s) result(r)
    type(seg), intent(in) :: s
    type(pt) :: r
    r = pt((s%a%x + s%b%x) / 2, (s%a%y + s%b%y) / 2)
  end function
  subroutine show(p)
    type(pt), intent(in) :: p
    print *, 'pt', p%x, p%y
  end subroutine
end module geom
