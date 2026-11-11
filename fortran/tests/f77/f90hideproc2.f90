! Names with more than one meaning in reach: a module procedure and a
! variable brought in by USE inside one procedure hide the enclosing
! module's own of those names there and nowhere else; and a local variable
! named like a sibling module procedure is the variable.
module outer_lib
  integer :: level = 10
contains
  integer function depth()
    depth = 11
  end function depth
  subroutine raise()
    level = 12
  end subroutine raise
end module outer_lib
module inner_lib
  integer :: level = -10
  integer :: visits
contains
  subroutine through_use()
    use outer_lib
    print *, depth(), level
    call raise()
    print *, level
  end subroutine through_use
  integer function depth()
    depth = -11
  end function depth
  subroutine raise()
    level = -12
  end subroutine raise
  subroutine own_names
    integer :: tally = 5
    integer count_up
    count_up = 3
    call relay(visit, tally)
    print *, tally, count_up, depth()
    call raise
    print *, level
  end subroutine own_names
  subroutine visit()
    visits = visits + 1
  end subroutine visit
  subroutine tally
  end subroutine
  integer function count_up()
    count_up = 1
  end function
end module
subroutine relay(callee, shown)
  external callee
  integer shown
  print *, shown
  call callee
end subroutine
program hide2
  use inner_lib
  visits = 0
  call through_use()
  call own_names
  print *, visits, level
end
