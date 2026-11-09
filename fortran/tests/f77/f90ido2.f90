! Implied DOs in I/O lists and constructors, nested, their items array
! expressions in the DO variable.
program s3
  implicit none
  integer :: a(3), i, m(2,2)
  i = 7
  a = (/ (sum((/ i, 2*i /)), i = 1, 3) /)
  print *, a
  print *, (sum((/ i, i /)), i = 1, 3)
  m = reshape((/ 1, 2, 3, 4 /), (/ 2, 2 /))
  print *, (sum(cshift(m(:, 1), i)), maxval(m(i, :) * (/ i, 1 /)), i = 1, 2)
  print *, ((/ i, i + 1 /), i = 1, 3)
  write (*, '(6I3)') ((/ i, -i /), i = 1, 3)
end program s3
