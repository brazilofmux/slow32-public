! An array SHIFT for CSHIFT and EOSHIFT, and an array BOUNDARY (Fortran
! 90 13.13.24, 32): one per line along DIM, of the array's shape without
! that dimension; along either dimension of a matrix, CHARACTER arrays,
! and the result reduced.
program f90shiftarr1
  implicit none
  integer :: m(3,4), s3(4), s4(3), b4(3), i, j
  character(len=2) :: c(2,3), cb(3)
  do j = 1, 4
    do i = 1, 3
      m(i, j) = 10 * i + j
    end do
  end do
  s3 = (/ 0, 1, -1, 2 /)
  print '(4I4)', transpose(cshift(m, s3))
  s4 = (/ 1, -2, 0 /)
  print '(4I4)', transpose(cshift(m, s4, 2))
  print '(4I4)', transpose(eoshift(m, s3))
  b4 = (/ -1, -2, -3 /)
  print '(4I4)', transpose(eoshift(m, s4, b4, 2))
  print '(4I4)', transpose(eoshift(m, (/ 1, 1, 1, 1 /), (/ 7, 8, 9, 0 /)))
  c = reshape((/ 'aa', 'bb', 'cc', 'dd', 'ee', 'ff' /), (/ 2, 3 /))
  cb = (/ '..', '::', '##' /)
  print *, eoshift(c, (/ 1, -1, 1 /), cb)
  print *, cshift(c, (/ 1, 0 /), 2)
  print *, sum(cshift(m, s3), 1)
end program
