program b
  use ma
  implicit none
  real :: x(4), y(4), big(2,2)
  integer :: k(4), j(4), n
  integer, allocatable :: q(:)
  type(v2) :: u(3), w(3), o
  real :: d(3)
  logical :: t(3)
  x = (/ 1.0, 2.0, 3.0, 4.0 /)
  k = (/ 1, 2, 3, 4 /)
  j = (/ 9, 8, 7, 6 /)
  y = cube(x)
  print *, y
  print *, dbl(k), dbl(x), dbl(5), dbl(0.5)
  call swap(k, j)
  print *, k, j
  call swap(k(1:2), j(3:4))
  print *, k, j
  call swap(k(1), j(1))
  print *, k, j
  print *, plain(cube(x)), plain(x)
  call show(cube(x), 4)
  call shows(dbl(k))
  write (*, '(4f8.2)') cube(x)
  x = merge(cube(x), 0.0, x > 2.0)
  print *, x
  allocate(q(sum(dbl(k(1:2))) - 30))
  print *, size(q)
  where (x > 0.0)
    y = cube(x) / 2.0
  elsewhere
    y = dbl(y)
  end where
  print *, y
  big = reshape(cube(y), (/ 2, 2 /))
  print *, big
  print *, dot_product(cube(y), y)
  n = 2
  print *, cube(y(n+1:)) + cube(y(:n))
  print *, k .x. j
  print *, (k .x. 5) + 1
  print *, 3 .x. 4
  t = (/ .true., .false., .true. /)
  print *, t .x. .true., (k > 2) .x. (j > 2)
  print *, kind_of(k), kind_of(t), kind_of(k > 2), kind_of(7), kind_of(.true.)
  u = (/ 1.0, 2.0, 3.0 /)
  w = 0.5
  o = v2(10.0, 20.0)
  print *, u
  print *, u + w
  print *, u + o
  print *, (u + w) .dot. u, o .dot. o
  d = u + w + o
  print *, d
  d(3:1:-1) = u
  print *, d
  w = t
  print *, w
  w(2:) = (/ .true., .false. /)
  print *, w
  where (t) u = 9.0
  print *, u
contains
  subroutine show(a, n)
    integer :: n
    real :: a(n)
    print *, a
  end subroutine
  subroutine shows(a)
    integer :: a(:)
    print *, a, size(a)
  end subroutine
end program
