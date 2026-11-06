! x**n of a real, n an integer: a product, whatever the size of n.  Volatile, or the
! optimizer folds what this is here to run.  An exponent converted to a real on the way
! to pow loses its low bit past 2**24, and with it the sign of a negative base.
program powi
  implicit none
  real, volatile :: a
  double precision, volatile :: d
  integer, volatile :: n
  a = -1.0
  d = -1.0d0
  n = 16777217
  if (a**n /= -1.0) error stop 'real, odd exponent past 2**24'
  if (a**(-n) /= -1.0) error stop 'real, its reciprocal'
  if (a**(n - 1) /= 1.0) error stop 'real, even exponent past 2**24'
  if (d**n /= -1.0d0) error stop 'double, odd exponent'
  n = huge(n)
  if (a**n /= -1.0) error stop 'real, huge'
  if (d**n /= -1.0d0) error stop 'double, huge'
  print '(a)', 'powi-ok'
end program
