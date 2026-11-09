! RECURSIVE procedures with internal procedures.
program p2
  implicit none
  print *, fact(5), depth(4)
contains
  recursive function fact(n) result(r)
    integer, intent(in) :: n
    integer :: r
    if (n <= 1) then
      r = 1
    else
      r = n * fact(n - 1)
    end if
  end function fact
  recursive function depth(n) result(r)
    integer, intent(in) :: n
    integer :: r
    r = n
    if (n > 0) r = r + depth(n - 1)
  end function depth
end program p2
