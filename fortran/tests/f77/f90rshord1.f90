! RESHAPE's ORDER (Fortran 90 13.13.90): a constant and a variable
! permutation, with PAD, rank 3, CHARACTER, and the identity order.
program f90rshord1
  implicit none
  integer :: a(6), m(2,3), q(3,2), t(2,3,2), o(2), i
  a = (/ (i, i = 1, 6) /)
  m = reshape(a, (/ 2, 3 /), order = (/ 2, 1 /))
  print *, m
  q = reshape(a(1:4), (/ 3, 2 /), (/ 0 /), (/ 2, 1 /))
  print *, q
  o = (/ 2, 1 /)
  print *, reshape(a, shape(m), order = o)
  t = reshape((/ (i, i = 1, 12) /), (/ 2, 3, 2 /), order = (/ 3, 1, 2 /))
  print *, t
  print *, reshape((/ 'a', 'b', 'c', 'd' /), (/ 2, 2 /), order = (/ 2, 1 /))
  print *, reshape(a, (/ 3, 2 /), order = (/ 1, 2 /))
end program
