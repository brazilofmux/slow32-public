! ... and the storage goes back: 5,000 calls that each leave 120 KB
! allocated, by RETURN and by END, fit a heap that holds a few.
program f95autodealloc2
  implicit none
  type t
    integer, allocatable :: c(:)
  end type
  integer :: i, tot
  tot = 0
  do i = 1, 5000
    call s(i, tot)
  end do
  print *, tot
contains
  subroutine s(n, tot)
    integer, intent(in) :: n
    integer, intent(inout) :: tot
    integer, allocatable :: x(:)
    type(t) :: u
    type(t), allocatable :: w(:)
    allocate (x(10000), u%c(10000), w(2))
    allocate (w(2)%c(10000))
    x(n) = 1
    u%c(n) = 1
    w(2)%c(n) = 1
    tot = tot + x(n) + u%c(n) + w(2)%c(n)
    if (mod(n, 2) == 0) return
    tot = tot + 1
  end subroutine
end program
