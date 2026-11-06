! OUT_OF_RANGE of a REAL(8), which has no wider real here.  Expected values are the
! standard's: the conversion INT(X, KIND(MOLD)), or NINT with ROUND=.TRUE., fits.
program p
  implicit none
  integer, parameter :: n = 20
  real(8), volatile :: x(n)
  logical :: a(n), r(n)
  logical, parameter :: k1 = out_of_range(7.0e18_8, 0_8), k2 = out_of_range(1.0e19_8, 0_8)
  logical, parameter :: k3(2) = out_of_range([128.5_8, 127.5_8], 0_1)
  real(8) :: nan, inf
  integer :: i
  logical :: fails
  x = 0.0_8
  fails = .false.
  ! INTEGER(1): truncating -128.9 fits and -129 does not; rounding, -128.4 and -128.5
  x(1:8) = [127.9_8, 128.0_8, -128.9_8, -129.0_8, 127.4_8, 127.5_8, -128.4_8, -128.5_8]
  a(1:8) = out_of_range(x(1:8), 0_1)
  r(1:8) = out_of_range(x(1:8), 0_1, .true.)
  if (any(a(1:8) .neqv. [.false.,.true.,.false.,.true.,.false.,.false.,.false.,.false.])) stop 1
  if (any(r(1:8) .neqv. [.true.,.true.,.true.,.true.,.false.,.true.,.false.,.true.])) stop 2
  ! INTEGER(8): 2**63 itself does not fit, and the real below it, 2**63-1024, does
  x(9:12) = [9223372036854774784.0_8, 9223372036854775808.0_8, -9223372036854775808.0_8, -9223372036854777856.0_8]
  a(9:12) = out_of_range(x(9:12), 0_8)
  if (any(a(9:12) .neqv. [.false.,.true.,.false.,.true.])) stop 3
  ! INTEGER(4), both ways, the round chosen at run time
  x(13:14) = [2147483647.5_8, 2147483648.0_8]
  if (any(out_of_range(x(13:14), 0_4) .neqv. [.false.,.true.])) stop 4
  fails = .true.
  if (out_of_range(x(13), 0_4, fails) .neqv. .true.) stop 5
  fails = .false.
  if (out_of_range(x(13), 0_4, fails) .neqv. .false.) stop 6
  ! Not numbers, and INTEGER(16)
  inf = huge(0.0_8); inf = inf * 2.0_8; nan = inf - inf
  x(15:16) = [inf, nan]
  if (any(.not. out_of_range(x(15:16), 0_4))) stop 7
  x(17:18) = [1.0e38_8, 1.0e39_8]
  if (any(out_of_range(x(17:18), 0_16) .neqv. [.false.,.true.])) stop 8
  ! Evaluated once
  i = 0
  if (out_of_range(once(), 0_1)) stop 9
  if (i /= 1) stop 10
  ! Constants fold
  if (k1 .or. .not. k2) stop 11
  if (.not. k3(1) .or. k3(2)) stop 12
  print '(a)', 'oor-ok'
contains
  real(8) function once()
    i = i + 1
    once = 5.0_8
  end function
end program
