! MAXVAL MINVAL MAXLOC MINLOC with NaNs and MASK in libgfortran's order: the
! location of the first element taken, a NaN only when every element
! taken is one, -HUGE when none is.
program f90rednan
   real :: r(4), zero
   double precision :: d(3)
   integer :: i
   zero = 0.0
   r = (/ zero/zero, 2.0, zero/zero, 5.0 /)
   d = (/ 1d0, dble(zero/zero), 3d0 /)
   print *, maxval(r), minval(r), maxloc(r, 1), minloc(r, 1)
   print *, maxval(r, mask=r > 3.0), maxval(r, mask=r /= r), maxloc(r, 1, r /= r), maxloc(r, 1, r > 9.0)
   print *, minval(r(1:3:2)), maxloc(r(1:3:2), 1), maxval(d), minloc(d, 1), maxval(d, d /= d)
   print *, maxval(r(2:1)), maxloc(r(2:1), 1), minval(d, mask=d > 5d0)
   print *, sum(r, mask=r == r), product(d, d == d), count(r /= r)
end program
