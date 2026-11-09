module em
  implicit none
  type pt
    real :: x, y
  end type
  interface twice
    module procedure twi, twr
  end interface
contains
  elemental function cube(x) result(r)
    real, intent(in) :: x
    real :: r
    r = x * x * x
  end function
  elemental integer function twi(i)
    integer, intent(in) :: i
    twi = 2 * i
  end function
  elemental real function twr(r)
    real, intent(in) :: r
    twr = 2.0 * r
  end function
  elemental function hyp(a, b) result(h)
    real, intent(in) :: a, b
    real :: h
    h = sqrt(a * a + b * b)
  end function
  elemental function norm(p) result(r)
    type(pt), intent(in) :: p
    real :: r
    r = sqrt(p%x**2 + p%y**2)
  end function
  elemental function flip(p) result(q)
    type(pt), intent(in) :: p
    type(pt) :: q
    q%x = p%y
    q%y = p%x
  end function
  elemental function up(c) result(r)
    character(len=*), intent(in) :: c
    character(len=3) :: r
    integer :: i
    r = c
    do i = 1, 3
      if (r(i:i) >= 'a' .and. r(i:i) <= 'z') r(i:i) = char(ichar(r(i:i)) - 32)
    end do
  end function
  elemental function ilen(c) result(n)
    character(len=*), intent(in) :: c
    integer :: n
    n = len_trim(c)
  end function
  elemental subroutine split(v, w, f)
    real, intent(in) :: v
    integer, intent(out) :: w
    real, intent(out) :: f
    w = int(v)
    f = v - w
  end subroutine
  elemental subroutine bump(p, d)
    type(pt), intent(inout) :: p
    real, intent(in) :: d
    p%x = p%x + d
    p%y = p%y - d
  end subroutine
end module
program e1
  use em
  implicit none
  real :: x(4), y(4), m(2,3), z(2,3), w(3)
  integer :: k(4), iw(4), i
  real :: fr(4)
  type(pt) :: ps(3), qs(3)
  character(len=3) :: nm(3)
  character(len=5) :: lg(2)
  logical :: lm(4)
  x = (/ 1.0, 2.0, 3.0, 4.0 /)
  k = (/ 1, 2, 3, 4 /)
  y = cube(x)
  print *, y
  y = cube(x) - cube(x(4:1:-1))
  print *, y
  y = cube(cube(x) / 10.0)
  print *, y
  print *, twice(k), twice(x)
  print *, twice(k + 1)
  print *, twice(3), twice(1.5)
  y = hyp(x, 2.0)
  print *, y
  y = hyp(3.0, x)
  print *, y
  y = hyp(x, x(4:1:-1))
  print *, y
  m = reshape((/ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 /), (/ 2, 3 /))
  z = cube(m)
  print *, z
  z = hyp(m, m + 1.0)
  print *, z
  w = cube(m(2, :))
  print *, w
  print *, sum(cube(x)), maxval(cube(x)), product(twice(k))
  print *, cube(x((/ 4, 1, 2 /)))
  where (cube(x) > 10.0) y = -1.0
  print *, y
  lm = cube(x) > 5.0
  print *, lm
  if (any(cube(x) > 60.0)) print *, 'big'
  if (all(twice(k) < 9)) print *, 'small'
  print *, count(cube(x) > 5.0)
  do i = 1, 3
    ps(i) = pt(real(i), real(i + 1))
  end do
  print *, norm(ps)
  qs = flip(ps)
  print *, qs
  print *, norm(flip(ps))
  print *, cube(ps%x)
  nm = (/ 'abc', 'dEf', 'g h' /)
  print *, up(nm)
  lg = (/ 'ab   ', 'abcd ' /)
  print *, ilen(lg), ilen('xy  ')
  print *, ilen(nm(2:3))
  call split(x + 0.25, iw, fr)
  print *, iw, fr
  call split(2.75, iw(1), fr(1))
  print *, iw(1), fr(1)
  call split(x(2:3) * 1.5, iw(3:4), fr(1:2))
  print *, iw, fr
  call bump(ps, 0.5)
  print *, ps
  call bump(ps(2:3), cube(x(1:2)))
  print *, ps
  call bump(ps(1), 1.0)
  print *, ps
  print *, cube(matmul(m, (/ 1.0, 1.0, 1.0 /)))
  print *, twice(size(x)), twice(k(2))
end program
