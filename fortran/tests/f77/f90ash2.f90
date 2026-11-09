program f90ash2
   implicit none
   real :: p(2,3)
   integer :: i, j
   do j = 1, 3
      do i = 1, 2
         p(i, j) = real(i * j)
      end do
   end do
   call twice(p(:, 2:3))
   print '(3F6.1)', (p(i, :), i = 1, 2)
   print '(F6.1)', ssum(p(2, :))
contains
   subroutine twice(q)
      real :: q(:,:)
      q = 2.0 * q
   end subroutine
   real function ssum(v)
      real, dimension(:) :: v
      ssum = sum(v) + real(size(v))
   end function
end program
