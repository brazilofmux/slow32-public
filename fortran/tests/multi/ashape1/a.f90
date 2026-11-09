! Assumed-shape dummies across files: the .mod file says which dummies
! take a descriptor, a generic's specifics too.
module ashmod
   interface norm1
      module procedure rnorm1, dnorm1
   end interface
contains
   real function rnorm1(x)
      real :: x(:)
      rnorm1 = sum(abs(x))
   end function
   double precision function dnorm1(x)
      double precision :: x(:)
      dnorm1 = sum(abs(x))
   end function
   subroutine fill(m, v)
      real :: m(:,:)
      real :: v
      integer :: i, j
      do j = 1, size(m, 2)
         do i = 1, size(m, 1)
            m(i, j) = v + real(10 * i + j)
         end do
      end do
   end subroutine
end module
