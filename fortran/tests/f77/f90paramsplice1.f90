! A PARAMETER array in a routine small enough to splice into its caller:
! the constant's storage is the routine's, and a splice has none for it.
subroutine pick(n, which)
  integer, parameter :: weights(0:6) = (/ 4, 8, 15, 16, 23, 42, 99 /)
  integer :: n, which
  n = weights(which) + weights(0)
end subroutine pick
program psplice
  integer :: got, grid(2,3), k
  got = -1
  grid = 0
  call pick(got, 5)
  print *, got
  do k = 1, 3
    call pick(grid(2,k), k + 2)
  end do
  print *, grid
end program psplice
