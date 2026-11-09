program p
  real :: a(3) = (/ 1., 2., 3. /)
  integer, parameter :: v(4) = (/ 1, 2, 3, 4 /)
  integer :: w(2,2) = reshape((/ 1, 2, 3, 4 /), (/ 2, 2 /))
  character(len=3) :: c(2) = (/ 'abc', 'def' /)
  print *, a, v, sum(v), w, c
  print *, v(3)
end program
