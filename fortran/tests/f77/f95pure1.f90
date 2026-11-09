! Fortran 95's procedure prefixes: PURE and ELEMENTAL with RECURSIVE and
! the result's type in any order; scalar references of ELEMENTAL
! procedures.  Names that only begin like a prefix are names.
module f95pure1_m
  implicit none
contains
  pure function sq(x) result(r)
    integer, intent(in) :: x
    integer :: r
    r = x * x
  end function sq
  pure integer function tw(x)
    integer, intent(in) :: x
    tw = 2 * x
  end function tw
  integer pure function th(x)
    integer, intent(in) :: x
    th = 3 * x
  end function th
  recursive pure function fact(n) result(r)
    integer, intent(in) :: n
    integer :: r
    if (n <= 1) then
      r = 1
    else
      r = n * fact(n - 1)
    end if
  end function fact
  pure recursive integer function fib(n) result(r)
    integer, intent(in) :: n
    if (n < 2) then
      r = n
    else
      r = fib(n - 1) + fib(n - 2)
    end if
  end function fib
  elemental real function half(x)
    real, intent(in) :: x
    half = x / 2.0
  end function half
  double precision elemental function dbl(x)
    double precision, intent(in) :: x
    dbl = x + x
  end function dbl
  pure character(len=3) function tag(k)
    integer, intent(in) :: k
    tag = 'abc'
    if (k > 0) tag = 'xyz'
  end function tag
  pure subroutine setit(a, v)
    integer, intent(out) :: a
    integer, intent(in) :: v
    a = v
  end subroutine setit
  elemental subroutine bump(a)
    integer, intent(inout) :: a
    a = a + 1
  end subroutine bump
end module f95pure1_m
program f95pure1
  use f95pure1_m
  implicit none
  integer :: k, purely, elementalx
  real :: pure1
  purely = 4
  elementalx = 5
  pure1 = 1.5
  print *, sq(7), tw(7), th(7), fact(6), fib(10)
  print *, half(3.0), dbl(1.25d0), tag(0), tag(1)
  call setit(k, 5)
  call bump(k)
  print *, k, purely, elementalx, pure1
end program f95pure1
