! Arrays inside array constructors (Fortran 90 4.5: flattened, in array
! element order): whole arrays, sections, expressions, nested
! constructors and implied-DOs over sections, in constructors assigned
! whole and in expressions (reductions, SIZE, RESHAPE), the target among
! the values, CHARACTER ones.
program f90aconarr1
  implicit none
  integer :: k(3), m(9), g(2,3), i, j
  real :: a(4)
  character(len=2) :: c(3), d(6)
  k = (/ 1, 2, 3 /)
  m = (/ k, k * 10, 7, 8, 9 /)
  print *, m
  m(1:6) = (/ k(3:1:-1), (/ 4, 5 /), size(k) /)
  print *, m
  g = reshape((/ (k + j, j = 0, 1) /), (/ 2, 3 /))
  print *, g
  m = (/ g, k /)
  print *, m
  print *, 2 * (/ k, k /)
  print *, sum((/ k, -k, k /)), size((/ g, g /))
  k = (/ k(2:3), k(1) /)
  print *, k
  a = (/ real(k), 0.5 /)
  print *, a
  c = (/ 'ab', 'cd', 'ef' /)
  d = (/ c, c(3:1:-1) // '' /)
  print *, d
  print *, (/ (g(:, j), j = 3, 1, -1) /)
  print *, (/ (/ 1, 2 /), (/ (i, i = 3, 4) /), 5 /)
end program
