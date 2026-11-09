program p
  real :: x, a(4)
  double precision :: d
  integer :: i
  x = 6.5; d = -6.5d0
  print *, modulo(-7, 3), modulo(7, -3), modulo(-7.5, 2.), modulo(7.5d0, -2d0), modulo(6, 3)
  print *, fraction(x), fraction(d), fraction(0.), set_exponent(x, 2), set_exponent(d, -1)
  print *, scale(x, 3), scale(d, -2), exponent(x)
  print *, nearest(1.0, 1.0), nearest(1.0, -1.0), nearest(1d0, 1d0) - 1d0
  print *, spacing(1.0), spacing(1d0), spacing(1000.), rrspacing(x), rrspacing(d)
  print *, bit_size(i), logical(.true.), logical(1 > 2)
  a = (/ -3.5, 2.25, 7., -0.5 /)
  print *, modulo(a, 2.), fraction(a)
  i = bit_size(0) - 1
  print *, i
end program
