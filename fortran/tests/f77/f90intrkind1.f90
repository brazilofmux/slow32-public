! KIND on the intrinsics that take one (Fortran 90): CMPLX, AINT, ANINT,
! CHAR, by position and by keyword, scalar and elemental.
program f90intrkind1
  implicit none
  integer, parameter :: wp = 8
  real(wp) :: x
  real :: y, ya(3)
  integer :: i
  complex :: c
  complex(wp) :: z, za(2)
  x = 1.0_wp
  y = 2.5
  i = 3
  c = (1.5, -2.5)
  z = (0.1_wp, 0.2_wp)
  ya = (/ 1.5, -2.5, 3.7 /)
  print *, cmplx(x, x/3, wp), cmplx(x, x/3, kind=wp), cmplx(x, kind=wp), cmplx(y, y), cmplx(x, x/3)
  print *, cmplx(c, kind=8), cmplx(z, kind=4), cmplx(z), cmplx(i, kind=wp), cmplx(i, i + 1, 8), cmplx(z, kind=8)
  za = cmplx(ya(1:2), ya(2:3), kind=wp) / 3
  print *, za
  print *, cmplx(ya, kind=4)
  print *, real(i, wp) / 3, real(i, kind=wp) / 3, real(x, kind=4) / 3
  print *, int(x * 7, kind=4), int(y, 4), aint(x / 3 * 10, kind=wp), anint(y, kind=8) / 3, aint(y, 4)
  print *, aint(ya, kind=8) / 3, anint(x / 3 * 10, 4) / 3
  print *, nint(y, kind=4), dble(y) / 3, char(65, kind=1), char(66, 1)
  print *, logical(.true., kind=4), ceiling(y, kind=4), floor(y, kind=4)
end program
