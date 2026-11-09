program p
  integer :: i, iv(2)
  real :: x
  double precision :: d
  character(len=4) :: c
  complex :: z
  x = 1.0
  i = transfer(x, i)
  print *, i, transfer(i, 0.0), transfer(1.5d0, 0d0)
  iv = (/ 1078530011, 1 /)
  x = transfer(iv(1), x)
  print *, x
  c = transfer(1684234849, 'abcd')
  print *, c, transfer('ABCD', 0)
  d = transfer((/ 0, 1072693248 /), 1d0)
  print *, d
  z = transfer((/ 1.0, 2.0 /), (0., 0.))
  print *, z, transfer(z, 1d0) /= 0d0
end program
