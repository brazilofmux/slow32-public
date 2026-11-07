! The array form of BESSEL_YN past overflow: Y(n) stays -Inf, as the scalar form says.
! The recurrence alone goes from -Inf to NaN.  Volatile, or the optimizer folds it.
program besselyn
  use ieee_arithmetic, only: ieee_is_nan
  implicit none
  real, volatile :: x
  double precision, volatile :: d
  real :: r(0:60)
  double precision :: q(0:300)
  integer :: i
  x = 0.5
  d = 0.5d0
  r = bessel_yn(0, 60, x)
  q = bessel_yn(0, 300, d)
  if (any(ieee_is_nan(r))) error stop 'real, a NaN'
  if (any(ieee_is_nan(q))) error stop 'double, a NaN'
  if (r(0) >= 0 .or. r(60) >= -huge(x)) error stop 'real, the ends'
  if (q(300) >= -huge(d)) error stop 'double, the end'
  do i = 1, 60
    if (r(i) > r(i - 1)) error stop 'real, rises'
  end do
  print '(a)', 'besselyn-ok'
end program
