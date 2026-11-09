module mo
  implicit none
  interface operator(.x.)
    module procedure xop
  end interface
  interface operator(.neg.)
    module procedure xneg
  end interface
contains
  elemental integer function xop(a, b)
    integer, intent(in) :: a, b
    xop = a * 10 + b
  end function
  elemental integer function xneg(a)
    integer, intent(in) :: a
    xneg = -a
  end function
end module
program w1
  use mo
  implicit none
  integer :: k(3), j(3), i, m(2,2), s
  real :: x(3)
  character(3) :: c(2)
  complex :: z
  logical :: l(3)
  k = (/ 1, 2, 3 /); j = (/ 7, 8, 9 /)
  x = (/ 1.5, 2.5, 3.5 /)
  m = reshape((/ 1, 2, 3, 4 /), (/ 2, 2 /))
  c = (/ 'ab ', 'cd ' /)
  z = (1.0, 2.0)
  s = 4
  print *, (k)
  print *, (k + j) * 2
  print *, (k(i), i = 1, 3)
  print *, ((m(i, s - 3 + 0 * i), i = 1, 2), s = 4, 5)
  print *, (1.0, 2.0), z, (s)
  print *, (x + 1.0) * 2.0, (s + 1) * 2
  write (*, '(3f6.1)') (x + 1.0) * 2.0
  write (*, '(3f6.1)') (x(3:1:-1))
  print *, (c // 'x')
  print *, (c(1) // 'x')
  print *, (m + 1) * m
  print *, (k .x. j), (5 .x. k) + 1, 3 .x. 4
  print *, .neg. k, .neg. (k + j), (.neg. k) .x. j
  print *, 2 .x. k .x. j
  print *, k + 1 .x. j * 2
  l = (k .x. j) .gt. 25 .and. .not. k .eq. 3
  print *, l
  l = (.neg. k .lt. -1) .or. (k .x. 1) .eq. 11
  print *, l
  print *, (k(i) .x. i, i = 1, 3), ((k(i) + i) .x. j(i), i = 1, 2)
  print *, sum(.neg. k), maxval(j .x. k)
  if (any((k .x. j) == 28)) print *, 'any'
  s = 2
  print *, s .x. s, .neg. s, 1.e1, x.gt.1.e0.and.x.lt.3.
end program
