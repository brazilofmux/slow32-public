! DOT_PRODUCT of LOGICAL arrays is ANY(A .AND. B); of INTEGER, mixed and
! COMPLEX arrays, the sum of products with the first conjugated.
program dotlog
  logical :: on(5) = (/ .true., .true., .false., .false., .true. /)
  logical :: mask(5) = (/ .false., .false., .true., .true., .false. /)
  integer :: n(4) = (/ 2, -3, 5, 7 /)
  complex :: z(2) = (/ (0.5, -1.0), (2.0, 3.0) /)
  print *, dot_product(on, mask)
  mask(5) = .true.
  print *, dot_product(on, mask), dot_product(n, n), dot_product(n, (/ 0.5, 1.0, 1.5, 2.0 /))
  print *, dot_product(z, z), dot_product(z, (/ 4.0, -2.0 /)), dot_product(n(2:3), z)
end
