program p
  integer :: a(6), iv(3), k
  real :: x(5)
  a = (/ (10*k, k = 1, 6) /)
  iv = (/ 5, 1, 3 /)
  x = (/ 1., 2., 3., 4., 5. /)
  call show(a(iv))
  call showr(x(iv), size(iv))
  print *, total(a(iv)), total(a(iv(2:3)))
contains
  subroutine show(v)
    integer, intent(in) :: v(:)
    print *, size(v), v
  end subroutine
  subroutine showr(v, n)
    integer n
    real v(n)
    print *, v
  end subroutine
  integer function total(v)
    integer, intent(in) :: v(:)
    total = sum(v)
  end function
end program
