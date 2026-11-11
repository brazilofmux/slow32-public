! An ELEMENTAL subroutine called on arrays whose INTENT(IN) actual is an
! expression over the array the INTENT(OUT) actual names: the expression's
! values are those from before the call, element by element.
program elemcall
  implicit none
  integer, parameter :: n = 6
  integer :: k
  integer :: v(n), keep(n)
  keep = (/ (10 * k, k = 1, n) /)
  v = keep
  call negate((v(1:n-1)), v(2:n))
  print *, v
  v = keep
  call negate(v(1:n-1) - 3, v(2:n))
  print *, v
  v = keep
  call negate(same(v(2:n)), v(1:n-1))
  print *, v
  v = keep
  call negate(v(n:1:-1) + 0, v)
  print *, v
  v = keep
  call negate(v, v)
  print *, v
contains
  elemental subroutine negate(from, to)
    integer, intent(in) :: from
    integer, intent(out) :: to
    to = -from - 1
  end subroutine negate
  elemental function same(x)
    integer, intent(in) :: x
    integer :: same
    same = x
  end function
end program elemcall
