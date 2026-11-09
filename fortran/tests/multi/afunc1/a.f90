! Array-valued module functions across files: the .mod file gives the
! result's rank; the shape is the callee's, from a dummy (automatic) or
! from an assumed-shape actual.
module afmod
contains
   function evens(n)
      integer, intent(in) :: n
      integer :: evens(n)
      integer :: i
      do i = 1, n
         evens(i) = 2 * i
      end do
   end function
   function scaled(m, f)
      real, intent(in) :: m(:,:)
      real, intent(in) :: f
      real :: scaled(size(m, 1), size(m, 2))
      scaled = m * f
   end function
end module
