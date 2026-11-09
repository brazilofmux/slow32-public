! CHARACTER assumed-shape dummies across files: the .mod file says which
! dummies take a descriptor; the length travels as any CHARACTER
! dummy's does; a generic picks its specific by a CHARACTER array actual.
module chasm
   interface tally
      module procedure tally1, tally2
   end interface
contains
   integer function tally1(x)
      character(len=*), intent(in) :: x(:)
      integer :: i
      tally1 = 0
      do i = 1, size(x)
         tally1 = tally1 + len_trim(x(i))
      end do
   end function
   integer function tally2(x)
      character(len=*), intent(in) :: x(:,:)
      tally2 = 1000 * size(x, 1) + 10 * size(x, 2) + len(x)
   end function
   subroutine stamp(x, tag)
      character(len=*), intent(inout) :: x(:)
      character(len=*), intent(in) :: tag
      integer :: i
      do i = 1, size(x)
         x(i)(1:1) = tag
      end do
   end subroutine
end module
