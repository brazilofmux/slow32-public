! ASSOCIATED(P, X(::2)) in a subroutine with no local: the unit that
! left the last block current for the optimizer (a compile failure,
! "instructions after a block's terminator").
module m4
contains
  subroutine chk(x, p)
    integer, target :: x(:)
    integer, pointer :: p(:)
    print *, associated(p, x(::2))
  end subroutine
end module
program p4h
  use m4
  integer, target :: a(6) = (/ 1, 2, 3, 4, 5, 6 /)
  integer, pointer :: p(:)
  p => a
  call chk(a, p)
end program
