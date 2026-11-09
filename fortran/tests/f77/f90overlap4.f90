! An ELEMENTAL defined assignment whose value is its own target's
! elements: the right side is an expression in parentheses (F90
! 12.3.2.1.2), so every value is taken before the first call.
module m
  implicit none
  type pt
    real :: x
  end type
  type nm
    character(6) :: s
    integer :: n
  end type
  interface assignment(=)
    module procedure pa, na, nc, cn
  end interface
contains
  elemental subroutine pa(a, b)
    type(pt), intent(out) :: a
    type(pt), intent(in) :: b
    a%x = b%x + 1.0
  end subroutine
  elemental subroutine na(a, b)
    type(nm), intent(out) :: a
    type(nm), intent(in) :: b
    a%s = b%s(2:6) // b%s(1:1)
    a%n = b%n + 100
  end subroutine
  elemental subroutine nc(a, b)
    type(nm), intent(out) :: a
    character(*), intent(in) :: b
    a%s = b
    a%n = len(b)
  end subroutine
  elemental subroutine cn(a, b)
    character(*), intent(out) :: a
    type(nm), intent(in) :: b
    a = b%s
  end subroutine
end module
program f90overlap4
  use m
  implicit none
  type(pt) :: a(3)
  type(pt), target :: t(4)
  type(pt), pointer :: p(:)
  type(nm) :: ns(4)
  character(4) :: cs(4)
  integer :: i
  a%x = (/ 1.0, 2.0, 3.0 /)
  a = a(3:1:-1)
  print *, a%x
  a(2:3) = a(1:2)
  print *, a%x
  a = a(2)
  print *, a%x
  a = (/ a(3), a(1), a(2) /)
  print *, a%x
  t%x = (/ 1.0, 2.0, 3.0, 4.0 /)
  p => t
  p = t(4:1:-1)
  print *, t%x
  t(1:3) = p(4:2:-1)
  print *, t%x
  do i = 1, 4
    ns(i)%s = 'abcdef'
    ns(i)%s(1:1) = char(48 + i)
    ns(i)%n = i
  end do
  ns(2:4) = ns(1:3)
  print '(4(a6,i5))', ns
  ns(1:3) = 'xy' // ns(2:4)%s
  print '(4(a6,i5))', ns
  ns(2:4) = ns(1:3)%s
  print '(4(a6,i5))', ns
  cs = ns
  print '(4(a4,1x))', cs
  ns(1:2) = cs(3:4)
  print '(4(a6,i5))', ns
  ns = ns(4)
  print '(4(a6,i5))', ns
end program
