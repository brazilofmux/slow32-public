! ASSOCIATED(P, X) and P => X with X a whole assumed-shape dummy.
module m4d
contains
subroutine chk(x, p)
  integer, target :: x(:)
  integer, pointer :: p(:)
  p => x
  print *, associated(p, x), p
  p => x(2:)
  print *, associated(p, x), associated(p, x(2:)), p
end subroutine
end module
program p4d
  use m4d
  integer, target :: a(6) = (/ 1, 2, 3, 4, 5, 6 /)
  integer, pointer :: p(:)
  call chk(a, p)
  call chk(a(1:6:2), p)
  print *, p, associated(p, a(3:6:2)), associated(p, a(3:5:2)), associated(p, a(3:5))
end program
