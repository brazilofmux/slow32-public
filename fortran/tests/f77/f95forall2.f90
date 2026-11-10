! FORALL in module procedures and on structures: a linked list made by
! pointer assignments, => NULL(), structures and their array components,
! a scalar structure that reads itself, adjustable and assumed-shape
! dummies, ALLOCATABLE and POINTER arrays.
module f95forall2m
  implicit none
  type node
    integer :: v
    type(node), pointer :: next => null()
  end type
  type pt
    real :: x, y
    integer :: tag(3)
  end type
contains
  subroutine rev(a, n)
    integer :: n, a(n), i
    forall (i = 1:n) a(i) = a(n + 1 - i)
  end subroutine
  subroutine shp(a)
    integer :: a(:, :), i, j
    forall (i = 1:size(a, 1), j = 1:size(a, 2), i /= j) a(i, j) = a(j, i) + 100
  end subroutine
  pure integer function sq(k)
    integer, intent(in) :: k
    sq = k * k
  end function
end module
program f95forall2
  use f95forall2m
  implicit none
  type(node), target :: x(5)
  type(node), pointer :: p
  type(pt) :: q(4), one
  integer :: i, j, a(7), m(3, 3)
  integer, allocatable :: al(:)
  integer, pointer :: pp(:)
  integer, target :: t(6)
  forall (i = 1:5) x(i)%v = i * 11
  forall (i = 1:4) x(i)%next => x(i + 1)
  p => x(1)
  do while (associated(p))
    print *, p%v
    p => p%next
  end do
  forall (i = 1:5) x(i)%next => null()
  print *, associated(x(1)%next), associated(x(4)%next)
  forall (i = 1:4) q(i) = pt(real(i), real(-i), (/ i, 2 * i, 3 * i /))
  forall (i = 1:4) q(i)%x = q(5 - i)%y
  print '(4f6.1)', q%x
  forall (i = 1:4) q(i) = q(5 - i)
  print '(4f6.1)', q%x
  print '(4f6.1)', q%y
  forall (i = 1:4, j = 1:3) q(i)%tag(j) = q(i)%tag(4 - j) + q(5 - i)%tag(j)
  print '(12i4)', (q(i)%tag, i = 1, 4)
  forall (i = 1:4) q(i)%tag = q(i)%tag(3:1:-1)
  print '(12i4)', (q(i)%tag, i = 1, 4)
  one = q(2)
  forall (i = 1:3) one%tag(i) = one%tag(4 - i) * 2
  print *, one%tag
  a = (/ 1, 2, 3, 4, 5, 6, 7 /)
  call rev(a, 7)
  print *, a
  m = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), (/ 3, 3 /))
  call shp(m)
  print *, m
  allocate (al(5))
  forall (i = 1:5) al(i) = sq(i)
  forall (i = 1:5) al(i) = al(6 - i) - al(i)
  print *, al
  t = (/ 1, 2, 3, 4, 5, 6 /)
  pp => t(2:6)
  forall (i = 1:5) pp(i) = t(i)
  print *, t
  forall (i = 1:6) t(i) = pp(min(i, 5))
  print *, t
  forall (i = 1:5) pp(i) = pp(6 - i)
  print *, t
end program
