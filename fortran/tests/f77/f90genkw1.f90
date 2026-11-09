! Through a generic (Fortran 90 14.1.2.3): keyword arguments, picking the
! specific by its dummies' names, an OPTIONAL one left out; and
! array-valued specifics, of two ranks, a reference evaluated whole.
module f90gkm
  interface area
    module procedure area_r, area_i
  end interface
contains
  real function area_r(w, h)
    real :: w
    real, optional :: h
    area_r = w * w
    if (present(h)) area_r = w * h
  end function
  integer function area_i(side, scale)
    integer :: side, scale
    area_i = side * side * scale
  end function
end module
module f90gam
  interface fill
    module procedure filli, fillr
  end interface
contains
  function filli(n, v)
    integer :: n, v, filli(n)
    filli = v
  end function
  function fillr(n, x)
    integer :: n
    real :: x, fillr(n, 2)
    fillr = x
  end function
end module
program f90genkw1
  use f90gkm
  use f90gam
  integer :: k(3)
  real :: r(2, 2)
  print *, area(2.0), area(h=3.0, w=2.0), area(scale=2, side=3), area(4, scale=1)
  k = fill(3, 7)
  r = fill(2, 1.5)
  print *, k, r, sum(fill(4, 2))
end program
