module m5
  implicit none
  type t
    integer :: n = 5
    real, allocatable :: a(:)
    character(3) :: c = 'xy'
  end type
contains
  subroutine fill(x, k)
    type(t), intent(inout) :: x
    integer :: k, i
    if (allocated(x%a)) deallocate(x%a)
    allocate(x%a(k))
    do i = 1, k
      x%a(i) = i * 1.5
    end do
    x%n = k
  end subroutine
  subroutine out1(x)
    type(t), intent(out) :: x
    print *, 'out1', x%n, allocated(x%a), x%c
  end subroutine
  subroutine outn(x, n)
    integer :: n
    type(t), intent(out) :: x(n)
    print *, 'outn', x(n)%n, allocated(x(n)%a), allocated(x(1)%a)
  end subroutine
  subroutine loc(k)
    integer :: k
    type(t) :: l, la(k), lb(2)
    type(t), allocatable :: al(:)
    type(t), pointer :: p, pa(:)
    print *, l%n, allocated(l%a), la(k)%n, allocated(la(k)%a), lb(2)%c, allocated(lb(1)%a)
    call fill(l, 3); call fill(la(k), 2); call fill(lb(1), 4)
    print *, l%a, la(k)%a, lb(1)%a
    allocate(al(k))
    print *, al(k)%n, allocated(al(k)%a), al(1)%c
    call fill(al(1), 2)
    call fill(al(k), 1)
    print *, al(1)%a, al(k)%a
    allocate(p, pa(2))
    print *, p%n, allocated(p%a), pa(2)%n, allocated(pa(2)%a)
    call fill(p, 2); call fill(pa(2), 3)
    print *, p%a, pa(2)%a
    lb(2) = p
    print *, lb(2)%n, lb(2)%a
    deallocate(p, pa)
  end subroutine
end module
program q5
  use m5
  implicit none
  type(t) :: x, y(3)
  integer :: i
  call fill(x, 2)
  call out1(x)
  print *, x%n, allocated(x%a)
  do i = 1, 3
    call fill(y(i), i)
  end do
  call outn(y, 3)
  print *, y(2)%n, allocated(y(2)%a)
  do i = 1, 200
    call loc(2)
  end do
  call loc(3)
end program
