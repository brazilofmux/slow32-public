program p
  character(len=8) :: d
  character(len=10) :: t
  character(len=5) :: z
  integer :: v(8), c1, c2, r, m, n, i
  integer, allocatable :: seed(:), back(:)
  real :: x, arr(3,4)
  double precision :: y, ya(5)
  call date_and_time(d, t, z, v)
  print *, len_trim(d), t(7:7), v(1) > 2000, v(2) >= 1 .and. v(2) <= 12
  call date_and_time(values=v, date=d)
  print *, d(1:2)
  call system_clock(c1, r, m)
  call system_clock(count=c2)
  print *, r, m, c2 >= c1
  call random_seed(size=n)
  allocate(seed(n), back(n))
  seed = (/ (i * 7919, i = 1, n) /)
  call random_seed(put=seed)
  call random_number(x)
  call random_number(arr)
  call random_number(ya(2:4))
  call random_number(harvest=y)
  print *, x >= 0. .and. x < 1., all(arr >= 0.) .and. all(arr < 1.), y < 1d0, ya(3) > 0d0
  call random_seed(get=back)
  print *, size(back) == n
  call random_seed(put=seed)
  call random_number(y)
  call random_seed(put=seed)
  call random_number(ya(1))
  print *, y == ya(1)
  call random_seed()
end program
