! DOUBLE PRECISION and COMPLEX locals that are pointer targets (P => X):
! they have storage, and the unit reads them from it after the pointer
! has set them.
subroutine s(n)
  implicit none
  integer :: n
  real(8), target :: d
  complex, target :: z
  real(8), pointer :: p
  complex, pointer :: q
  d = 0.25d0 * n
  z = cmplx(n, -n)
  p => d
  q => z
  p = p + 1.125d0
  q = q * (0.0, 1.0)
  print *, d, z
  d = d * 3
  z = z + 1
  print *, p, q
end subroutine
program p2e
  call s(1)
  call s(3)
end program
