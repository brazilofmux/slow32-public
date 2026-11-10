! Pointer assignments in a FORALL: scalar and array pointer components,
! masks, a component of a component.
module f95forall8m
  implicit none
  type cell
    integer :: v
    integer, pointer :: p => null()
    type(cell), pointer :: next => null()
  end type
  type box
    integer, pointer :: arr(:) => null()
  end type
  type holder
    type(cell) :: c(4)
    integer :: n(4)
  end type
end module
program f95forall8
  use f95forall8m
  implicit none
  type(cell), target :: x(4)
  type(holder), target :: h
  type(box) :: bx(3)
  integer, target :: t(4), big(12)
  integer :: i, j
  integer, pointer :: q
  t = (/ 10, 20, 30, 40 /)
  forall (i = 1:4) x(i)%v = i
  forall (i = 1:4) x(i)%p => t(5 - i)
  print *, (x(i)%p, i = 1, 4)
  forall (i = 1:4, mod(i, 2) == 0) x(i)%p => null()
  print *, (associated(x(i)%p), i = 1, 4)
  forall (i = 1:3)
    x(i)%next => x(i + 1)
    x(i)%p => x(i + 1)%v
  end forall
  print *, x(1)%next%next%v, x(2)%p, x(3)%p
  ! through the pointers set above: values, not pointers
  forall (i = 1:3) x(i)%p = i * 100
  print *, x%v
  forall (i = 1:4) h%c(i)%v = i * 7
  forall (i = 1:3) h%c(i)%next => h%c(i + 1)
  forall (i = 1:4) h%n(i) = h%c(5 - i)%v
  print *, h%n, h%c(1)%next%next%v
  forall (i = 1:4) h%n(i) = h%n(5 - i)
  print *, h%n
  forall (i = 1:12) big(i) = i
  forall (i = 1:3) bx(i)%arr => big(4 * i - 3:4 * i)
  print *, bx(2)%arr, size(bx(3)%arr)
  forall (i = 1:3) bx(i)%arr(1) = -i
  print *, big
  forall (i = 1:3, j = 2:4) bx(i)%arr(j) = i * 100 + j
  print *, big
end program
