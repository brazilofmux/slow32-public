! Module variables across files: the module's file defines the block
! (its initial values), the others reach it through the .mod file --
! scalars, an array, CHARACTER, an ALLOCATABLE allocated in one file and
! read in another, a renamed one.
module mvstate
   implicit none
   integer :: hits = 3
   real :: grid(2, 3)
   character(len=6) :: label = 'init'
   integer, allocatable :: work(:)
end module
