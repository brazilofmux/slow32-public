! FORALL, the statement (Fortran 95, 7.5.4): strides, turns that run no
! times, values read before any is stored (a reversal, a shift, a
! transpose), masks, a triangular pair of indexes, sections on each turn,
! REAL, CHARACTER elements and substrings, LOGICAL.
program f95forall1
  implicit none
  integer :: a(6), b(6), i, j, k, m(4,4), n
  real :: r(5), s(5)
  character(len=4) :: c(4)
  logical :: l(6)
  i = 77
  j = 88
  n = 6
  ! strides, negative strides, zero trips
  a = 0
  forall (i = 1:n:2) a(i) = i
  print *, a
  forall (i = n:1:-2) a(i) = -i
  print *, a
  forall (i = 5:1) a(i) = 999
  print *, a
  print *, i, j
  ! reversal: reads what it stores
  forall (i = 1:n) a(i) = a(n + 1 - i)
  print *, a
  ! shift
  b = (/ (k, k = 1, 6) /)
  forall (i = 2:n) b(i) = b(i - 1)
  print *, b
  ! mask reading the assigned
  b = (/ 3, -1, 4, -1, 5, -9 /)
  forall (i = 1:n, b(i) < 0) b(i) = -b(i)
  print *, b
  forall (i = 1:n - 1, b(i + 1) > b(i)) b(i) = b(i + 1)
  print *, b
  ! transpose in place
  forall (i = 1:4, j = 1:4) m(i, j) = 10 * i + j
  forall (i = 1:4, j = 1:4) m(i, j) = m(j, i)
  print *, m
  ! triangular: nested, bounds on the outer index
  m = 0
  forall (i = 1:4)
    forall (j = i:4) m(i, j) = i + j
  end forall
  print *, m
  ! an inner mask reading the assigned (with inner bounds on the outer
  ! index gfortran is not the oracle: f95forall10)
  forall (i = 1:4)
    forall (j = 1:4, j < i .and. m(j, i) > 4) m(i, j) = -m(j, i)
  end forall
  print *, m
  ! array section per turn
  forall (i = 1:4) m(i, :) = i
  print *, m
  forall (i = 1:4) m(:, i) = m(i, :) + i
  print *, m
  ! reals, characters, logicals
  r = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)
  forall (i = 1:5) s(i) = sqrt(r(i)) + r(6 - i)
  print '(5f8.3)', s
  forall (i = 1:5, r(i) > 2.5) r(i) = r(i - 1) * 2
  print '(5f8.3)', r
  c = (/ 'abcd', 'efgh', 'ijkl', 'mnop' /)
  forall (i = 1:4) c(i) = c(5 - i)
  print *, c
  forall (i = 1:4) c(i)(1:2) = c(i)(3:4)
  print *, c
  forall (i = 1:3) c(i)(i:i) = c(i + 1)(1:1)
  print *, c
  forall (i = 1:6) l(i) = mod(i, 2) == 0
  print *, l
  forall (i = 1:6, l(i)) l(7 - i) = .true.
  print *, l
  print *, i, j
end program
