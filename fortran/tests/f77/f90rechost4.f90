! A RECURSIVE host's DOUBLE PRECISION and COMPLEX locals in its internal
! procedures (a pair of registers anywhere else: here they have storage).
module m2d
  implicit none
contains
  recursive subroutine tree(depth)
    integer, intent(in) :: depth
    real(8) :: d
    d = 0.25d0 * depth
    call note()
    print *, d
  contains
    subroutine note()
      print *, d
      d = d + 1
    end subroutine
  end subroutine
  function g(x) result(r)
    real(8) :: x, r
    r = x * 0.25d0
    call note()
    r = r * 2
  contains
    subroutine note()
      print *, r
      r = r + 1
    end subroutine
  end function
end module
program p2d
  use m2d
  implicit none
  real(8) :: x
  call tree(1)
  x = g(3d0)
  print *, x
end program
