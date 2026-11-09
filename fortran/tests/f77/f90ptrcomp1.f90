module m
  type grid
    integer :: n
    real, pointer :: v(:,:)
    integer, pointer :: k(:)
  end type
  type outer
    type(grid) :: g
  end type
contains
  real function tot(a)
    real :: a(:,:)
    tot = sum(a)
  end function
  subroutine dbl(a)
    integer :: a(:)
    a = 2 * a
  end subroutine
end module
program p
  use m
  type(grid) :: g
  type(outer) :: o
  integer :: i
  allocate(g%v(2,3), g%k(0:4))
  g%v = reshape((/ (real(i), i = 1, 6) /), (/ 2, 3 /))
  g%k = (/ (i*i, i = 0, 4) /)
  print *, shape(g%v), lbound(g%k), ubound(g%k)
  print *, tot(g%v), g%v(2,:)
  call dbl(g%k)
  print *, g%k
  print *, g%k(::2), count(g%k > 10)
  where (g%k > 10) g%k = -1
  print *, g%k
  g%v(:,2) = g%v(:,1) + g%v(:,3)
  print *, g%v
  o%g = g
  print *, sum(o%g%v), size(o%g%k), o%g%k(1)
  deallocate(g%k)
  print *, associated(g%k), associated(o%g%k)
end program
