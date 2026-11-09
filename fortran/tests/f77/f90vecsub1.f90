program p
  integer :: a(6), iv(3), b(3,4), k, jv(2)
  real :: x(5), y(3)
  character(len=2) :: c(4)
  a = (/ (10*k, k = 1, 6) /)
  iv = (/ 5, 1, 3 /)
  print *, a(iv), sum(a(iv)), a((/ 2, 2, 6 /))
  a(iv) = -1
  print *, a
  a(iv) = (/ 7, 8, 9 /)
  print *, a
  b = reshape((/ (k, k = 1, 12) /), (/ 3, 4 /))
  jv = (/ 4, 2 /)
  print *, b(2, jv), b(iv, 1)
  print *, b(iv(2:3), jv)
  x = (/ 1., 2., 3., 4., 5. /)
  y = x(iv) * 2.
  print *, y, maxval(x(iv))
  c = (/ 'ab', 'cd', 'ef', 'gh' /)
  print *, c(jv), c(jv)(2:2)
  where (a(iv) > 7) a(iv) = 0
  print *, a
  b(1, jv) = b(3, jv) + 100
  print *, b(1, :)
  print *, a(iv(3:1:-1))
end program
