! Array-valued intrinsics (Fortran 90): MATMUL TRANSPOSE RESHAPE SPREAD
! SHAPE, LBOUND and UBOUND without DIM, and array constructors inside
! expressions -- as an assignment's right side, inside other expressions,
! in reductions, SIZE, and composed with each other.
program f90avi1
   implicit none
   integer :: i, j
   real :: a(2,3), b(3,2), c(2,2), v(3), w(2), t(3,2), r(6)
   integer :: k(2), m(2,3), n3(4,2,3), z(0:2, 5)
   double precision :: d(2,2), e(2)
   logical :: la(2,2), lb(2,2), lc(2,2)
   do j = 1, 3
      do i = 1, 2
         a(i, j) = real(i + 2 * j) * 0.5
         b(j, i) = real(j - i)
         m(i, j) = 10 * i + j
      end do
   end do
   v = (/ 1.0, -2.0, 0.5 /)
   c = matmul(a, b)
   print '(2F8.2)', ((c(i, j), j = 1, 2), i = 1, 2)
   w = matmul(a, v)
   print '(2F8.2)', w
   w = matmul(v, b)
   print '(2F8.2)', w
   t = transpose(a)
   print '(2F7.2)', ((t(i, j), j = 1, 2), i = 1, 3)
   t = transpose(a) * 2.0 + b
   print '(2F7.2)', ((t(i, j), j = 1, 2), i = 1, 3)
   print '(F8.2)', sum(matmul(a, b)), maxval(transpose(m) * 1.0)
   r = reshape(a, (/ 6 /))
   print '(6F6.2)', r
   b = reshape((/ 1.0, 2.0, 3.0, 4.0 /), (/ 3, 2 /), pad = (/ 9.0, 8.0 /))
   print '(2F6.1)', ((b(i, j), j = 1, 2), i = 1, 3)
   m = reshape((/ (i * i, i = 1, 6) /), shape(m))
   print *, m
   n3 = spread(m, 1, 4)
   print *, sum(n3), n3(3, 2, 1), n3(1, 1, 3)
   m = spread((/ 7, 8 /), 2, 3)
   print *, m
   k = shape(a)
   print *, k, shape(n3), size(shape(z))
   print *, lbound(z), ubound(z), lbound(a(2, :)), ubound(a(:, 2:3))
   k = 2 * (/ 1, 2 /) + k
   print *, k
   print *, sum((/ 1, 2, 3 /) * (/ 4, 5, 6 /)), dot_product((/ 1.0, 2.0 /), (/ 3.0, 4.0 /))
   d = reshape((/ 1d0, 2d0, 3d0, 4d0 /), (/ 2, 2 /))
   d = matmul(d, transpose(d))
   print *, d
   e = matmul(transpose(d), (/ 1d0, -1d0 /))
   print *, e
   la = reshape((/ .true., .false., .true., .true. /), (/ 2, 2 /))
   lb = reshape((/ .false., .true., .true., .false. /), (/ 2, 2 /))
   lc = matmul(la, lb)
   print *, lc
   v = (/ (real(i), i = 1, 3) /) + 0.5
   print '(3F6.2)', v
end program
