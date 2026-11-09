subroutine fill(n)
   use mvstate
   implicit none
   integer :: n, i, j
   do j = 1, 3
      do i = 1, 2
         grid(i, j) = real(10 * i + j)
      end do
   end do
   allocate(work(n))
   work = (/ (i * i, i = 1, n) /)
   hits = hits + 1
   label = 'filled'
end subroutine
