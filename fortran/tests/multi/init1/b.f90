
program t
  use tables
  implicit none
  integer, parameter :: n = primes(3) + 1
  real :: grid(n)
  integer, parameter :: m(2,3) = reshape((/ 1, 2, 3, 4, 5, 6 /), (/ 2, 3 /))
  integer, parameter :: both(8) = (/ primes(1:2), 9, (/ 10, 11 /), sq(1), 0, -1 /)
  logical, parameter :: flags(3) = (/ .true., .false., .true. /)
  double precision, parameter :: dd(2) = (/ 1d0/3d0, 2.5d0 /)
  complex, parameter :: zz(2) = (/ (1., 2.), (3., -4.) /)
  integer :: k, j
  integer :: tri(10) = (/ ((j, k = 1, j), j = 1, 4) /)
  real :: r(4) = 1.5
  print *, size(grid), n, np
  print *, primes, sum(primes), primes(np)
  print *, w, sum(w)
  print *, days, ' ', days(2)
  print *, sq
  print *, m(2, :), m(:, 3)
  print *, both
  print *, flags, count(flags)
  print *, dd
  print *, zz
  print *, tri
  print *, r
  counts(2) = 5
  print *, counts
  do k = 1, np
    if (mod(sq(min(k,5)), 2) == 0) print *, k, primes(k)
  end do
  call show(primes)
contains
  subroutine show(a)
    integer, intent(in) :: a(:)
    print *, 'show', size(a), a(size(a))
  end subroutine
end program
