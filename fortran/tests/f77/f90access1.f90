! PUBLIC and PRIVATE (Fortran 90 5.2.3): PRIVATE alone and a PUBLIC list,
! the attributes on declarations; the module's procedures use its private
! variables, PARAMETERs and functions; a public generic of a private
! specific; the program's own variables named as private ones.
module f90pvm
  implicit none
  private
  public :: get, put, nput
  integer :: store(10)
  integer :: nput = 0
  integer, parameter, public :: cap = 10
  integer, parameter :: secret = 42
  real, public :: total = 0.0
  interface get
    module procedure get1, get2
  end interface
contains
  subroutine put(v)
    integer :: v
    nput = nput + 1
    store(nput) = v
    total = total + v + bias()
  end subroutine
  integer function get1(i)
    integer :: i
    get1 = store(i)
  end function
  real function get2(x)
    real :: x
    get2 = x * secret
  end function
  real function bias()
    bias = 0.5
  end function
end module
program f90access1
  use f90pvm
  implicit none
  integer :: secret, store
  secret = 1
  store = 2
  call put(5)
  call put(7)
  print *, nput, get(2), get(1.0), total, cap, secret, store
end program
