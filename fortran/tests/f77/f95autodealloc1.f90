! ALLOCATABLE locals are deallocated at RETURN and at END (Fortran 95
! 6.3.3.1), unless SAVEd or a module's: every call finds them
! unallocated, in a module procedure, a FUNCTION, a RECURSIVE procedure
! and an internal one, and a structure's ALLOCATABLE components with it.
module f95autodealloc1m
  implicit none
  integer, allocatable :: mv(:)
  type t
    integer, allocatable :: c(:)
  end type
contains
  subroutine s(n)
    integer, intent(in) :: n
    integer, allocatable :: x(:), y(:, :)
    integer, allocatable, save :: k(:)
    real, allocatable :: r(:)
    character(len=3), allocatable :: c(:)
    type(t) :: u
    type(t), allocatable :: w(:)
    integer :: st
    print *, allocated(x), allocated(y), allocated(k), allocated(r), allocated(c), allocated(u%c), allocated(w)
    allocate (x(n), y(n, 2), r(n), c(n), u%c(n), w(n), stat = st)
    print *, st, size(x), size(y), size(r), size(c), size(u%c), size(w)
    if (.not. allocated(k)) then
      allocate (k(n))
      k = n
    end if
    if (.not. allocated(mv)) allocate (mv(n))
    allocate (w(1)%c(n))
    x = n
    print *, x, k, size(mv), size(w(1)%c)
    if (n == 3) return
    print *, 'end', n
  end subroutine
  integer function f(n)
    integer, intent(in) :: n
    integer, allocatable :: z(:)
    allocate (z(n))
    z = n
    f = sum(z)
  end function
  recursive subroutine rec(n)
    integer, intent(in) :: n
    integer, allocatable :: q(:)
    print *, 'rec', n, allocated(q)
    allocate (q(n))
    q = n
    if (n > 1) call rec(n - 1)
    print *, 'out', n, q
  end subroutine
end module
program f95autodealloc1
  use f95autodealloc1m
  implicit none
  integer :: i
  do i = 1, 4
    call s(i)
  end do
  print *, f(3), f(4), f(5)
  call rec(3)
  call inner
  call inner
contains
  subroutine inner
    integer, allocatable :: a(:)
    print *, allocated(a)
    allocate (a(2))
    a = 1
    print *, a
  end subroutine
end program
