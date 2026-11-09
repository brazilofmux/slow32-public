module shapes
  implicit none
  type circle
    real :: r
  end type
  type square
    real :: s
  end type
  interface area
    module procedure carea, sarea
  end interface
  interface show
    module procedure cshow, sshow
  end interface
contains
  subroutine report(c, q)
    type(circle) :: c
    type(square) :: q
    call show(q)                 ! siblings later in the module
    call show(c, tag='c')
    print *, area(q=q), area(c)
  end subroutine
  real function carea(c)
    type(circle) :: c
    carea = 3.0 * c%r**2
  end function
  real function sarea(q)
    type(square) :: q
    sarea = q%s**2
  end function
  subroutine cshow(c, tag)
    type(circle) :: c
    character(len=*), optional :: tag
    if (present(tag)) then
      print *, 'circle ', tag, c%r
    else
      print *, 'circle', c%r
    end if
  end subroutine
  subroutine sshow(q, tag)
    type(square) :: q
    character(len=*), optional :: tag
    print *, 'square', q%s
  end subroutine
end module
program p
use shapes
type(circle) :: c
type(square) :: q
c = circle(2.0)
q = square(5.0)
call show(c)
call show(q)
call show(tag='x', q=q)
print *, area(c), area(q), area(square(1.5))
call report(c, q)
end
