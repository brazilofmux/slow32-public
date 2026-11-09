program q10
  integer, parameter :: n = 5
  logical :: l = .not. .false.
  integer :: e = 2**3 + len('abc')
  logical, parameter :: pl = .not. .false., p2 = n > 3 .and. n /= 5, p3 = n .ge. 5 .or. .false.
  logical, parameter :: p4 = (1.5 < 2.0) .eqv. (n == 5), p5 = pl .neqv. p2, p6 = .not. n <= 4 .and. 2.0d0 >= 2
  character(len('four')) :: c = 'abcdefg'
  integer, parameter :: m = len(c) + len('abx')
  print *, l, e, pl, p2, p3, p4, p5, p6, c, len(c), m
end program
