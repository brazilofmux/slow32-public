! POINTER and TARGET (Fortran 90 5.1.2.4, 7.5.2): scalar and array
! pointers associated with variables, elements, arrays and strided
! sections (lower bounds a whole array's, a section's 1), with other
! pointers; assignment through them; ALLOCATE and DEALLOCATE of pointers
! (lower bounds of their own), NULLIFY, ASSOCIATED with and without a
! TARGET; a CHARACTER pointer.
program f90ptr1
  implicit none
  real, target :: a, b, v(10), m(3, 4)
  real, pointer :: p, q, pv(:), pm(:, :)
  integer :: i, j
  character(len=5), target :: w
  character(len=5), pointer :: pw
  a = 1.0
  b = 2.0
  p => a
  print *, p, associated(p), associated(p, a), associated(p, b)
  p = 10.0
  print *, a
  q => p
  p => b
  print *, p, q, associated(q, a)
  v = (/ (real(i), i = 1, 10) /)
  pv => v
  print *, size(pv), lbound(pv), ubound(pv), pv(3)
  pv => v(2:9:3)
  print *, size(pv), pv, sum(pv)
  pv(2) = -1.0
  print *, v
  do j = 1, 4
    do i = 1, 3
      m(i, j) = i + 10 * j
    end do
  end do
  pm => m(2:3, 1:4:2)
  print *, shape(pm), pm
  pm = 0.0
  print *, m
  allocate(pv(5))
  pv = (/ 5., 4., 3., 2., 1. /)
  print *, pv, maxval(pv)
  deallocate(pv)
  print *, associated(pv)
  allocate(p)
  p = 3.5
  print *, p, a, b
  deallocate(p)
  nullify(q)
  print *, associated(q), associated(p)
  w = 'hello'
  pw => w
  pw(1:1) = 'J'
  print *, w, pw, len(pw)
  allocate(pm(0:1, 2))
  pm = 7.0
  print *, lbound(pm), ubound(pm), pm(0, 2)
end program
