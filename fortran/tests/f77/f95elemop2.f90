module mo
  implicit none
  interface operator(.x.)
    module procedure xop
  end interface
contains
  elemental integer function xop(a, b)
    integer, intent(in) :: a, b
    xop = a * 10 + b
  end function
end module
program o1
  use mo
  implicit none
  integer :: k(4), j(4), r(4)
  k = (/ 1, 2, 3, 4 /)
  j = (/ 9, 8, 7, 6 /)
  r = (k .x. 5) + 1
  print *, r
  r = (k + 5) .x. j
  print *, r
  print *, (k + 5) + 1
  print *, (k .x. 5)
  print *, 1 + (k .x. 5)
  print *, (k .x. 5) + 1
  print *, 3 .x. 4
end program
