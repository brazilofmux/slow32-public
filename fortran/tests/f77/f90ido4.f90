! A constructor's implied-DO variable is its own: the variable of that
! name is untouched by it, in a loop on it too.
program s5
  implicit none
  integer :: i, j, a(5), k
  integer, parameter :: p(3) = (/ (i*i, i = 1, 3) /)
  real :: x(4)
  i = 42
  j = 17
  a = (/ (i, i = 1, 3), i, j /)
  print *, a, i
  print *, (/ (j + i, j = 1, 2) /), j
  print *, (i, i = 1, 3), i
  print *, i
  x = (/ (real(k) / 2, k = 1, 4) /)
  print *, x
  print *, p
  do i = 1, 2
    a = (/ (i * k, k = 1, 5) /)
    print *, a, (/ (i, i = 7, 8) /), i
  end do
end program
