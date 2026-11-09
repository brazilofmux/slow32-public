! A FUNCTION statement whose kind is a named constant (Fortran 90):
! REAL(WP) FUNCTION, the name its host's, its host's USEs', or its own
! USEs' -- told to the procedures that reference it before it is compiled.
module f90hdrsel2_k
  implicit none
  integer, parameter :: wp = kind(1.0d0), sp = 4
end module f90hdrsel2_k
module f90hdrsel2_m
  use f90hdrsel2_k
  implicit none
  integer, parameter :: ck = 8
contains
  real(wp) function first(x)
    real(wp), intent(in) :: x
    first = second(x) + third(x)
  end function first
  real(kind=wp) function second(x)
    real(wp), intent(in) :: x
    second = x / 3
  end function second
  real(sp) function third(x) result(r)
    real(wp), intent(in) :: x
    r = real(x, sp) / 3
  end function third
  complex(ck) function cz(x)
    real(wp), intent(in) :: x
    cz = cmplx(x, x / 3, kind=wp)
  end function cz
  real(kind(1.0d0)) function fourth(x)
    real(wp), intent(in) :: x
    fourth = x / 7
  end function fourth
  integer(selected_int_kind(5)) function fifth(k)
    integer, intent(in) :: k
    fifth = k * 2
  end function fifth
end module f90hdrsel2_m
real(dp) function ext1(x)
  use f90hdrsel2_k, only: dp => wp
  real(dp) :: x
  ext1 = x / 9
end function ext1
real(8) function ext2(x)
  real(8) :: x
  ext2 = x / 11
end function ext2
character(len=4) function ext3(k)
  integer :: k
  ext3 = 'abcdefgh'
end function ext3
program f90hdrsel2
  use f90hdrsel2_m
  implicit none
  interface
    real(dp) function ext1(x)
      use f90hdrsel2_k, only: dp => wp
      real(dp) :: x
    end function ext1
    real(8) function ext2(x)
      real(8) :: x
    end function ext2
    character(len=4) function ext3(k)
      integer :: k
    end function ext3
  end interface
  print *, first(1.0_wp), second(1.0_wp), third(1.0_wp)
  print *, cz(1.0_wp), fourth(1.0_wp), fifth(21)
  print *, ext1(1.0_wp), ext2(1.0_wp), ext3(1)
  print *, inner(2.0_wp), in2(1.0_wp)
contains
  real(wp) function inner(x)
    real(wp) :: x
    inner = in2(x) * 2
  end function inner
  real(wp) function in2(x)
    real(wp) :: x
    in2 = x / 3
  end function in2
end program f90hdrsel2
