! An array operand right after a dot operator (.NOT. K, A .AND. .NOT. B):
! it had not been seen as one -- L = .NOT. K, COUNT(.NOT. K) and WHERE
! (.NOT. K) were refused -- and as the target it was missed by the
! overlap check: L(2:5) = K(1:4) .AND. .NOT. L(1:4) read elements it had
! already stored (T F T F T for T F F F F).
program f90dotop1
  implicit none
  logical :: k(5), l(5), b(5)
  integer :: n(5), i
  real :: a(5)
  k = (/ .true., .false., .true., .true., .false. /)
  b = .true.
  l = .not. k
  print *, l
  l = k
  l(2:5) = .not. l(1:4)
  print *, l
  l = k .and. .not. b
  print *, l, count(.not. k), any(k .neqv. l)
  n = (/ (i, i = 1, 5) /)
  where (.not. k) n = -n
  print *, n
  a = 1.0
  a(2:5) = a(1:4) + 1.0
  print *, a
  print *, merge(1, 2, .not. k), pack(n, .not. k .or. n > 3)
  i = 3
  if (.not. k(2) .and. n(1) .eq. 1) print *, 'scalar ok'
  b = n .eq. 3 .or. .not. k
  print *, b
  call overlap
contains
  subroutine overlap
    logical :: k(5), l(5)
    k = .true.
    l = .true.
    l(2:5) = k(1:4) .and. .not. l(1:4)
    print *, l
  end subroutine
end program
