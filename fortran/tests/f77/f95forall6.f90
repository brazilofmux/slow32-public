! An ELEMENTAL defined operator and an ELEMENTAL defined assignment in a
! FORALL; substrings.
module f95forall6m
  implicit none
  type v2
    integer :: x, y
  end type
  interface operator(+)
    module procedure add
  end interface
  interface assignment(=)
    module procedure fromint
  end interface
contains
  elemental function add(a, b) result(c)
    type(v2), intent(in) :: a, b
    type(v2) :: c
    c = v2(a%x + b%x, a%y + b%y)
  end function
  elemental subroutine fromint(a, k)
    type(v2), intent(out) :: a
    integer, intent(in) :: k
    a%x = k
    a%y = -k
  end subroutine
end module
program f95forall6
  use f95forall6m
  implicit none
  type(v2) :: p(4), q(4)
  integer :: i
  character(len=6) :: s, w(3)
  forall (i = 1:4) p(i) = i
  print *, p
  forall (i = 1:4) q(i) = p(i) + p(5 - i)
  print *, q
  forall (i = 1:4) p(i) = p(5 - i) + q(i)
  print *, p
  forall (i = 1:3) p(i) = p(i + 1)
  print *, p
  s = 'abcdef'
  forall (i = 1:6) s(i:i) = s(7 - i:7 - i)
  print *, s
  forall (i = 1:3) w(i) = s(i:i + 2) // 'xyz'
  print *, w
  forall (i = 1:3) w(i) = w(4 - i)(4:6) // w(i)(1:3)
  print *, w
end program
