! Fortran 90 reductions of array expressions: SUM PRODUCT MAXVAL MINVAL
! MAXLOC MINLOC COUNT ANY ALL DOT_PRODUCT, with MASK, over sections and
! ranks 1-3, every numeric type; SIZE LBOUND UBOUND.
program f90red2
   implicit none
   integer :: i, j, k, d
   integer :: iv(6), im(3,4), iz(5:4)
   real :: r(5), rm(2,3), r3(2,2,2)
   double precision :: dv(4), dm(3,3)
   complex :: c(3), c2(3)
   complex*16 :: z(2), z2(2)
   logical :: l(4)
   iv = (/ 3, -1, 4, 1, -5, 9 /)
   do j = 1, 4
      do i = 1, 3
         im(i, j) = 10 * i + j - 2 * mod(i * j, 3)
      end do
   end do
   r = (/ 1.5, -2.25, 3.0, 0.5, 2.0 /)
   do j = 1, 3
      do i = 1, 2
         rm(i, j) = real(i) - 0.5 * real(j)
      end do
   end do
   do k = 1, 2
      do j = 1, 2
         do i = 1, 2
            r3(i, j, k) = real(i + 2 * j + 4 * k)
         end do
      end do
   end do
   dv = (/ 1d0, 2.5d0, -0.125d0, 4d0 /)
   do j = 1, 3
      do i = 1, 3
         dm(i, j) = 1d0 / dble(i + j - 1)
      end do
   end do
   c = (/ (1.0, 2.0), (-0.5, 1.0), (3.0, -1.0) /)
   c2 = (/ (2.0, 0.0), (1.0, 1.0), (0.0, -1.0) /)
   z = (/ (1d0, -1d0), (0.5d0, 2d0) /)
   z2 = (/ (3d0, 1d0), (-1d0, 0.25d0) /)
   l = (/ .true., .false., .true., .true. /)

   print *, 'sum', sum(iv), sum(iv(2:5)), sum(iv(1:6:2)), sum(im), sum(im(2, :)), sum(im(:, 3))
   print *, 'sumx', sum(iv * 2 + 1), sum(abs(iv)), sum(iv, mask=iv > 0), sum(iv, iv < 0)
   print *, 'prod', product(iv), product(iv(1:4)), product(im(1:2, 1:2)), product(iv, iv > 1)
   print *, 'rsum', sum(r), sum(rm), sum(r3), sum(r3(:, 2, :)), sum(r * r)
   print *, 'dsum', sum(dv), sum(dm), product(dv), sum(dm(2:3, 1:2) * 2d0)
   print *, 'csum', sum(c), product(c), sum(z), product(z)
   print *, 'mixed', sum(r * 2d0), sum(iv * 0.5)
   print *, 'max', maxval(iv), minval(iv), maxval(im), minval(im(:, 2:3)), maxval(abs(iv))
   print *, 'maxm', maxval(iv, mask=iv < 4), minval(iv, iv > 0), maxval(r, r < 2.0)
   print *, 'rmax', maxval(rm), minval(r3), maxval(dm), minval(dv * dv)
   print *, 'loc', maxloc(iv, 1), minloc(iv, dim=1), maxloc(r, 1), minloc(dv, 1), maxloc(iv, 1, iv < 4)
   print *, 'cnt', count(l), count(iv > 0), count(im > 20), any(iv > 8), any(im < 0), all(l), all(iv /= 0)
   print *, 'cnt2', count(r3 > 8.0), any(l .and. iv(1:4) > 3), all(im(1, :) > 0)
   print *, 'dot', dot_product(iv(1:3), iv(4:6)), dot_product(r(1:3), r(3:5)), dot_product(dv, dv)
   print *, 'dotc', dot_product(c, c2), dot_product(z, z2), dot_product(l, l), dot_product(r(1:2), dv(1:2))
   print *, 'empty', sum(iz), product(iz), maxval(iz), minval(iz), count(iz > 0), any(iz > 0), all(iz > 0)
   print *, 'emptyr', sum(r(3:2)), maxval(r(4:1)), minval(dv(2:1)), maxval(iv, iv > 100)
   print *, 'size', size(iv), size(im), size(im, 1), size(im, dim=2), size(iv(2:5)), size(im(:, 2:3)), size(iz)
   print *, 'bnd', lbound(im, 1), ubound(im, 2), lbound(iz, 1), ubound(iz, 1), lbound(im(2:3, :), 1), ubound(im(2:3, :), 1)
   do d = 1, 2
      print *, 'dvar', d, size(im, d), lbound(im, d), ubound(im, d), size(rm, d)
   end do
   iv(1:3) = sum(im(1, :)) + iv(4:6)
   print *, 'inassign', iv
   print *, 'nested', sum(iv * sum(iv)), sum(im(:, 1) * maxval(iv))
   k = 0
   do i = 1, 3
      k = k + sum(im(i, :), mask=im(i, :) > 15)
   end do
   print *, 'loop', k
end program
