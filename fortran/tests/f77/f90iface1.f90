! Interface bodies (Fortran 90 12.3.2.1) for externals of this file: an
! assumed-shape dummy (refused without one), an OPTIONAL one, a FUNCTION
! with RESULT; sections and an expression as actuals.
program f90iface1
  implicit none
  interface
    subroutine f90ibshow(v, label)
      real, intent(in) :: v(:,:)
      character(len=*), intent(in), optional :: label
    end subroutine
    function f90ibnorm(x) result(r)
      real, intent(in) :: x(:)
      real :: r
    end function
  end interface
  real :: m(3, 4)
  integer :: i, j
  do j = 1, 4
    do i = 1, 3
      m(i, j) = i + 10 * j
    end do
  end do
  call f90ibshow(m(2:3, 1:4:2), 'part')
  call f90ibshow(m)
  print *, f90ibnorm(m(:, 2)), f90ibnorm((/ 3.0, 4.0 /))
end program
subroutine f90ibshow(v, label)
  real, intent(in) :: v(:,:)
  character(len=*), intent(in), optional :: label
  if (present(label)) print *, label
  print *, shape(v), sum(v)
end subroutine
function f90ibnorm(x) result(r)
  real, intent(in) :: x(:)
  real :: r
  r = sqrt(sum(x * x))
end function
