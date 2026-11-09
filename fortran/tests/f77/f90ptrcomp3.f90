program p
  type t
    integer, pointer :: q
    integer :: i
    real, pointer :: v(:)
  end type
  type(t) :: x, y
  integer, target :: k
  real, target :: arr(4)
  real, pointer :: pv(:)
  k = 5; arr = (/ 1., 2., 3., 4. /)
  x = t(k, 3, arr(2:4))
  print *, x%q, x%i, x%v
  k = 6
  arr(3) = 30.
  print *, x%q, sum(x%v), size(x%v)
  pv => arr
  y = t(x%q, 4, pv)
  print *, y%q, y%v
end program
