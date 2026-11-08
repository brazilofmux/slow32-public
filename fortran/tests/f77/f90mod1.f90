! Fortran 90 modules, as LAPACK's LA_CONSTANTS and LA_XISNAN have them:
! a module of PARAMETERs (kinds, a CHARACTER, a COMPLEX); a module whose
! generic interface names its procedures; internal procedures (CONTAINS
! in a FUNCTION, and in the main program); USE with ONLY and renames
! (wp=>dp), and without; a generic resolved by its actual's type.
module consts
  implicit none
  integer, parameter :: sp = kind(1.e0), dp = kind(1.d0)
  real(dp), parameter :: dhalf = 0.5_dp
  real(sp), parameter :: shalf = 0.5_sp
  character*1, parameter :: dprefix = 'D'
  complex(dp), parameter :: zone = (1.0_dp, 0.0_dp)
  real(dp), parameter :: dsafmin = real(radix(0._dp), dp)**max( &
     minexponent(0._dp) - 1, 1 - maxexponent(0._dp))
end module consts

module isnan_mod
  interface my_isnan
     module procedure s_isnan
     module procedure d_isnan
  end interface
contains
  logical function s_isnan(x)
    use consts, only: wp=>sp
    real(wp) :: x
    s_isnan = slaisnan(x, x)
  contains
    logical function slaisnan(x, y)
      real :: x, y
      slaisnan = (x .ne. y)
    end function slaisnan
  end function s_isnan
  logical function d_isnan(x)
    use consts, only: wp=>dp
    real(wp) :: x
    d_isnan = dlaisnan(x, x)
  contains
    logical function dlaisnan(x, y)
      double precision :: x, y
      dlaisnan = (x .ne. y)
    end function dlaisnan
  end function d_isnan
end module isnan_mod

program use_them
  use consts
  use isnan_mod
  implicit none
  real :: s, z
  double precision :: d
  z = 0.0
  s = 1.5
  d = dhalf
  write (*, '(2l2,1x,a,2f6.2,es24.16e3)') my_isnan(s), my_isnan(d), dprefix, real(zone), shalf, dsafmin
  s = sqrt(-z - 1.0)
  d = s
  write (*, '(3l2)') my_isnan(s), my_isnan(d), s_isnan(1.0)
  call scale_it(d)
  call scale_it(2.5d0)
contains
  subroutine scale_it(v)
    double precision :: v
    write (*, '(f6.2,l2)') dhalf * 3, my_isnan(v)
  end subroutine scale_it
end program use_them
