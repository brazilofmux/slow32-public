! CHARACTER and COMPLEX*16 module functions, called from another file:
! the .mod file gives a CHARACTER result's length, which the caller's
! buffer has; through a generic too.
module cfm
   implicit none
   interface shout
      module procedure shouti, shoutc
   end interface
contains
   function greet(name)
      character(len=*), intent(in) :: name
      character(len=12) :: greet
      greet = 'hi ' // name
   end function
   function shouti(i)
      integer, intent(in) :: i
      character(len=4) :: shouti
      write (shouti, '(I4)') i
   end function
   function shoutc(c)
      character(len=*), intent(in) :: c
      character(len=4) :: shoutc
      shoutc = c // '!!!!'
   end function
   function rot(z)
      complex(kind=8), intent(in) :: z
      complex(kind=8) :: rot
      rot = z * (0d0, 1d0)
   end function
end module
