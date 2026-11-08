! Fortran 90 kinds: KIND, SELECTED_REAL_KIND, kinds in declarations
! (REAL(WP), COMPLEX(DP), REAL(KIND=4)), kind suffixes on constants
! (0.1_dp is the double 0.1, not the single one widened), the inquiry
! intrinsics, SQRT, CEILING, FLOOR and MOD of reals in PARAMETERs, and a
! FUNCTION whose result's kind its own statements give (as BLAS's DNRM2).
program kinds_one
  implicit none
  integer, parameter :: sp = kind(1.e0), dp = kind(1.d0)
  integer, parameter :: wp = selected_real_kind(12, 200)
  real(dp), parameter :: tenth = 0.1_dp, third = 1.0_dp / 3
  real(sp), parameter :: stenth = 0.1_sp
  real(kind=4) :: s
  real(wp) :: w
  complex(dp) :: z
  real(dp), parameter :: big = real(radix(0._dp), dp)**ceiling((maxexponent(0._dp) - 1) * 0.5_dp)
  real(sp), parameter :: r2 = sqrt(2.0_sp)
  real(dp), parameter :: d2 = sqrt(2.0_dp), m = mod(7.5_dp, 2.0_dp)
  integer, parameter :: fl = floor(-2.5_dp), ce = ceiling(2.25)
  real(dp) :: dnorm2, v(3)
  write (*, '(4i4)') sp, dp, wp, kind(z)
  write (*, '(i4,i5,i6,i5,i4,i5)') digits(s), digits(w), minexponent(s), maxexponent(w), precision(w), range(s)
  write (*, '(es24.16e3)') tenth, third, stenth, big
  write (*, '(es16.8,es24.16,f6.2)') r2, d2, m
  write (*, '(2i4)') fl, ce
  w = 0.1_wp
  s = 0.1
  write (*, '(2es24.16)') w, dble(s)
  z = (1.5_dp, -2.0_dp)
  write (*, '(2f8.3)') z
  write (*, '(es24.16)') real(tenth, sp)
  v = (/ 3.0d0, 4.0d0, 12.0d0 /)
  write (*, '(f12.8)') dnorm2(3, v)
end program kinds_one

function dnorm2(n, x)
  integer, parameter :: wp = kind(1.d0)
  real(wp) :: dnorm2
  integer :: n, i
  real(wp) :: x(n), t
  t = 0
  do i = 1, n
     t = t + x(i)**2
  end do
  dnorm2 = sqrt(t)
end function dnorm2
