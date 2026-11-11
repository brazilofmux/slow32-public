! Defined assignment with the same variable on both sides, and a
! subroutine handed (X) for its INTENT(IN) and X for its INTENT(OUT): the
! right side is a value, taken before the left is defined.
module money_mod
  type money
    integer :: cents = 0
    real :: rate = 1.0
  end type money
  interface assignment (=)
    module procedure with_fee
  end interface
contains
  subroutine with_fee(to, from)
    type(money), intent(out) :: to
    type(money), intent(in) :: from
    to%cents = from%cents + 25
    to%rate = from%rate
  end subroutine
  subroutine doubled(to, from)
    type(money), intent(out) :: to
    type(money), intent(in) :: from
    to%cents = from%cents
    to%rate = from%rate * 2
  end subroutine
end module
program asgself
  use money_mod
  type(money) :: m, q, row(3)
  m%cents = 100; m%rate = 0.5
  m = m
  print *, m%cents, m%rate
  q%cents = 300; q%rate = 1.5
  call doubled(q, (q))
  print *, q%cents, q%rate
  row(1)%cents = 7; row(2)%cents = 70; row(3)%cents = 700
  row(3) = row(3)
  row(2) = row(1)
  print *, row%cents
end
