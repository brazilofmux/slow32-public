! A dummy argument, undeclared, named as an entity its procedure has by
! host association -- a module's variable or PARAMETER, a host program's
! PARAMETER -- is the dummy (F90 12.1.2.2.1).  It had been the host's:
! N + K added the PARAMETER K = 3, not the argument.
module f90hdm
   integer :: n = 5
   integer, parameter :: k = 3
contains
   subroutine s(n, k)
      n = n + k
   end subroutine
   function nk(k) result(n)
      n = 2 * k
   end function
end module
program f90hostdummy1
   use f90hdm, only: s, nk, nn => n
   integer, parameter :: k = 3
   integer :: a, b
   a = 10
   b = 1
   call s(a, b)
   print *, a, nn, nk(4)
   a = 10
   call t(a, 1)
   print *, a
contains
   subroutine t(n, k)
      n = n + k
   end subroutine
end program
