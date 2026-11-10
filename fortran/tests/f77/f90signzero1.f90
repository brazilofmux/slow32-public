program p
  real :: r, a, b
  double precision :: d, da, db
  r = mod(-4., 2.);  print *, sign(1., r)
  r = modulo(-4., 2.);  print *, sign(1., r)
  r = mod(4., -2.);  print *, sign(1., r)
  r = modulo(4., -2.);  print *, sign(1., r)
  r = modulo(-4., -2.);  print *, sign(1., r)
  a = -4.; b = 2.
  r = mod(a, b);  print *, sign(1., r)
  r = modulo(a, b);  print *, sign(1., r)
  r = modulo(-a, -b);  print *, sign(1., r)
  r = modulo(a, -b);  print *, sign(1., r)
  da = -4.; db = 2.
  d = mod(da, db);  print *, sign(1.d0, d)
  d = modulo(da, db);  print *, sign(1.d0, d)
  d = modulo(-da, -db);  print *, sign(1.d0, d)
  print *, sign(1.0, fraction(-0.0)), fraction(-2.0), fraction(-2.0d0)
  a = -0.0
  print *, sign(1.0, fraction(a)), sign(1.0, a), sign(1.0, -a), sign(1.0, a*1.0), sign(1.0, aint(-0.3)), sign(1.0, anint(-0.3))
  print *, sign(1.0, sqrt(a)), sign(1.0, abs(a)), sign(2.0, -0.0), sign(1.0,nearest(a,1.0))
  print *, -0.0, a, 0.0*(-1.0)
  write(*,'(F6.2,E12.4,G12.4)') a, a, a
end
