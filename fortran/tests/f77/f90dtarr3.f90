! Arrays of structures of a module's type: components of sections,
! assignment through them, intrinsics over a component of every element.
module mm
  implicit none
  type pt
    integer :: x, y
  end type pt
contains
  subroutine show(v)
    type(pt), intent(in) :: v(:)
    print *, size(v), v
  end subroutine show
  function total(v) result(r)
    type(pt), intent(in) :: v(:)
    integer :: r
    r = sum(v%x) + sum(v%y)
  end function total
end module mm
program p1b
  use mm
  implicit none
  type(pt) :: a(3), b(3), c(2,2)
  type(pt), allocatable :: d(:)
  integer :: i
  a = (/ (pt(i, 10*i), i = 1, 3) /)
  b = (/ a(2:3), a(1) /)
  print *, b
  call show(a(2:3))
  call show(a)
  print *, total(a(1:3:2)), total(b)
  open (10, status='scratch')
  write (10, *) 7, 8, 9, 10, 11, 12
  rewind (10)
  read (10, *) b(1:2), b(3)
  print *, b
  write (10, '(6I4)') a
  rewind (10)
  read (10, '(6I4)') (b(i), i = 3, 1, -1)
  print *, b
  c = reshape((/ a, a(1) /), (/ 2, 2 /))
  print *, c(2,1), c(1,2)
  allocate (d(3))
  d = a
  d = (/ d(3), d(2), d(1) /)
  print *, d
  print *, merge(a, b, (/ .true., .false., .true. /))
  print *, pack(a, a%x > 1)
  print *, cshift(a, 1)
  where (a%x > 1) b = a
  print *, b
end program p1b
