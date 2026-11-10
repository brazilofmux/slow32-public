! A FORALL in an internal procedure whose index is the host's variable,
! and a FORALL on the host's arrays.
program f95forall4
  implicit none
  integer :: a(4), i, b(4)
  i = 100
  forall (i = 1:4) a(i) = hv(i)
  print *, a
  b = (/ 4, 3, 2, 1 /)
  call inner
  print *, b, a
contains
  pure integer function hv(k)
    integer, intent(in) :: k
    hv = k + i
  end function
  subroutine inner
    integer :: j
    forall (j = 1:4) b(j) = b(5 - j) * 2 + a(j)
    forall (j = 1:4, b(j) > 105) a(j) = b(j)
    forall (i = 1:3) a(i) = a(i + 1)
  end subroutine
end program
