module a
  integer, parameter :: k = 7
  integer :: counter = 3
  interface twice
    module procedure twice_i
  end interface
contains
  integer function f(n)
    integer n
    f = n + k
  end function
  integer function twice_i(n)
    integer n
    twice_i = 2*n
  end function
end module
module b
  use a
end module
program p
  use b
  print *, f(1), k, counter, twice(4)
end program
