! Arrays of structures in array expressions: whole arrays and sections as
! values, a component of every element (A%X), constructors of structures,
! as arguments and function results.
module mm
  implicit none
  type pt
    integer :: x, y
    real :: w(2)
    character(len=4) :: tag
  end type pt
contains
  subroutine show(v)
    type(pt), intent(in) :: v(:)
    print *, size(v), v%x, v%tag
    print *, v
  end subroutine show
  function total(v) result(r)
    type(pt), intent(in) :: v(:)
    integer :: r
    r = sum(v%x) + sum(v%y) + int(sum(v%w(2)))
  end function total
  subroutine bump(v)
    type(pt), intent(inout) :: v(:)
    integer :: i
    v%x = v%x + 100
    v(size(v):1:-1) = v
    v%w(1) = (/ (real(i), i = 1, size(v)) /)
  end subroutine bump
  subroutine fill(v, n)
    integer, intent(in) :: n
    type(pt), intent(out) :: v(n)
    integer :: i
    v = (/ (pt(i, -i, (/ 0.5*i, 2.0*i /), 'abc'), i = 1, n) /)
  end subroutine fill
end module mm
program s1
  use mm
  implicit none
  type(pt) :: a(4), c(2,2)
  type(pt), allocatable :: d(:)
  type(pt), pointer :: p(:)
  type(pt), target :: t(4)
  integer :: i
  call fill(a, 4)
  call show(a(2:3))
  call show(a)
  print *, total(a(1:3:2)), total(a)
  call bump(a)
  call show(a)
  allocate (d(4))
  d = a
  print *, d%x, sum(d%y), d(2)%w
  call bump(d(2:4))
  call show(d)
  print *, total(d), allocated(d), size(d), lbound(d), ubound(d)
  d = cshift(d, -1)
  print *, d%x
  d = (/ d(4), d(3), d(2), d(1) /)
  print *, d%x, d%tag
  d(1:2) = pack(a, a%y < -2)
  print *, d%y
  deallocate (d)
  t = a
  p => t(1:4:3)
  print *, p%x, size(p)
  p%y = 77
  print *, t%y
  call show(p)
  c = reshape(a, (/ 2, 2 /))
  print *, c(2,1)%x, c(1,2)%tag, c%y
  c = transpose(c)
  print *, c%y
  print *, merge(a, t, a%y > -3)
  print *, spread(a(2), 1, 2)
  print *, eoshift(a, 1, pt(0, 0, 0.0, 'zzz'))
  print *, unpack(a(1:2), (/ .true., .false., .false., .true. /), t)
  where (a%x > 102) a = t
  print *, a
  print *, size((/ a, t /)), (/ a(1), t(2) /)
end program s1
