! Derived types across files: the .mod file gives a module's types (and
! those of the modules it USEs, a component's type among them), one type
! however many USEs bring it; a FUNCTION whose result is one.
module geom
   implicit none
   type pt
      real :: x, y
   end type
contains
   type(pt) function midpt(a, b)
      type(pt), intent(in) :: a, b
      midpt = pt((a%x + b%x) / 2, (a%y + b%y) / 2)
   end function
   real function dist2(a, b)
      type(pt), intent(in) :: a, b
      dist2 = (a%x - b%x)**2 + (a%y - b%y)**2
   end function
end module
