module tables
  implicit none
  integer, private :: i
  integer, parameter :: primes(6) = (/ 2, 3, 5, 7, 11, 13 /)
  integer, parameter :: np = size(primes)
  real, parameter :: w(3) = (/ 0.25, 0.5, 0.25 /)
  character(len=3), parameter :: days(3) = (/ 'Mon', 'Tue', 'Wed' /)
  integer, parameter :: sq(5) = (/ (i*i, i = 1, 5) /)
  integer :: counts(4) = (/ (0, i = 1, 4) /)
end module
