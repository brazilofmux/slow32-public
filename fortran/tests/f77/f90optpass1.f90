! An absent OPTIONAL array passed on to another OPTIONAL dummy is absent
! there, whatever the two dummies' shapes are.
module relay_mod
  implicit none
contains
  subroutine top(lo, hi)
    integer, dimension(:), intent(inout), optional :: lo, hi
    print *, 'top', present(lo), present(hi)
    call fixed(lo, hi)
    call shaped(lo, hi)
  end subroutine top
  subroutine fixed(lo, hi)
    integer, dimension(3), intent(inout), optional :: lo, hi
    print *, 'fixed', present(lo), present(hi)
    if (present(hi)) hi(3) = hi(3) + hi(1)
    call shaped(lo, hi)
  end subroutine fixed
  subroutine shaped(lo, hi)
    integer, dimension(:), intent(inout), optional :: lo, hi
    print *, 'shaped', present(lo), present(hi)
    if (present(hi)) hi = hi * 2
  end subroutine shaped
end module relay_mod
program optpass
  use relay_mod
  implicit none
  integer :: three(3), nine(9)
  three = (/ 5, 6, 7 /)
  call top(hi=three)
  print *, three
  nine = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
  call top(hi=nine(1:9:4))
  print *, nine
  call top()
end program optpass
