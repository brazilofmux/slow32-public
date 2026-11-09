module gm
  interface twice
    module procedure twice_i, twice_r
  end interface
contains
  integer function user(n)
    integer n
    user = twice(n) + int(twice(1.5))
  end function
  integer function twice_i(n)
    integer n
    twice_i = 2*n
  end function
  real function twice_r(x)
    real x
    twice_r = 2*x
  end function
end module
program p
use gm
print *, user(4)
end
