! A module in one file, USEd from another through its .mod: PARAMETERs,
! a generic, renames.
module m1
  integer, parameter :: wp = kind(1.d0)
  real(wp), parameter :: half = 0.5_wp
  interface twice
     module procedure twice_r, twice_d
  end interface
contains
  real function twice_r(x)
    real :: x
    twice_r = 2 * x
  end function twice_r
  double precision function twice_d(x)
    double precision :: x
    twice_d = 2 * x
  end function twice_d
end module m1
