! Fortran 95's CPU_TIME, REAL and DOUBLE PRECISION: what it must be,
! not what it is -- not negative, and not going backwards.
program cpu_one
  real :: t0, t1
  double precision :: d0, d1, x
  integer :: i
  call cpu_time(t0)
  call cpu_time(d0)
  x = 0
  do i = 1, 200000
     x = x + sqrt(dble(i))
  end do
  call cpu_time(t1)
  call cpu_time(d1)
  write (*, '(4l2)') t0 >= 0.0, t1 >= t0, d0 >= 0.0d0, d1 >= d0
  write (*, '(f16.3)') x
end program cpu_one
