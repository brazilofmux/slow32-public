! A RECURSIVE host's dummies and locals in its internal procedures: each
! activation's own, read and set.
module m2
  implicit none
contains
  recursive function walk(n) result(r)
    integer, intent(in) :: n
    integer :: r, acc
    acc = n
    call bump(2)
    if (n > 0) then
      r = acc + walk(n - 1)
    else
      r = acc
    end if
  contains
    subroutine bump(k)
      integer, intent(in) :: k
      acc = acc * k + n
    end subroutine bump
  end function walk
end module m2
program p2b
  use m2
  implicit none
  print *, walk(4)
end program p2b
