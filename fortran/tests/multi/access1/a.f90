! PRIVATE across files: the .mod file keeps a module's private entities,
! hidden, so that a generic can pick a private specific and USE binds
! none of them.
module accm
   implicit none
   private
   public :: twice, bump, level
   integer :: level = 1
   integer, parameter :: step = 4
   interface twice
      module procedure twicei, twicer
   end interface
contains
   integer function twicei(i)
      integer :: i
      twicei = 2 * i
   end function
   real function twicer(x)
      real :: x
      twicer = 2.0 * x
   end function
   subroutine bump()
      level = level + step
   end subroutine
end module
