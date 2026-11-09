module mx
  implicit none
  type v2
    real :: x, y
  end type
  type cnt
    integer :: n
  end type
  type plain
    integer :: n
  end type
  type box
    type(cnt) :: c(3)
    real :: r(3)
  end type
  interface assignment(=)
    module procedure v_from_r, r_from_v, c_from_c, p_from_p, s_from_c, i_from_c
  end interface
contains
  elemental subroutine v_from_r(a, r)
    type(v2), intent(out) :: a
    real, intent(in) :: r
    a%x = r; a%y = -r
  end subroutine
  elemental subroutine r_from_v(r, a)
    real, intent(out) :: r
    type(v2), intent(in) :: a
    r = a%x + a%y * 10.0
  end subroutine
  elemental subroutine c_from_c(a, b)
    type(cnt), intent(out) :: a
    type(cnt), intent(in) :: b
    a%n = b%n + 1000
  end subroutine
  subroutine p_from_p(a, b)
    type(plain), intent(out) :: a
    type(plain), intent(in) :: b
    a%n = b%n + 5000
  end subroutine
  elemental subroutine s_from_c(s, b)
    character(*), intent(out) :: s
    type(cnt), intent(in) :: b
    s = repeat(char(64 + b%n), 6)
  end subroutine
  elemental subroutine i_from_c(i, b)
    integer, intent(out) :: i
    type(cnt), intent(in) :: b
    i = -b%n
  end subroutine
  function mk(n) result(r)
    integer :: n, i
    type(cnt) :: r(n)
    do i = 1, n
      r(i)%n = 10 * i
    end do
  end function
end module
program o4
  use mx
  implicit none
  type(v2) :: a(3), a2(2,2)
  type(cnt) :: c(3), d(3)
  type(plain) :: p(3), q(3)
  type(box) :: bx
  real :: f(3), g(3), g2(2,2)
  character(4) :: s(3)
  character(8) :: t(3)
  integer :: i, k(3)
  double precision :: dd(3)
  f = (/ 1.0, 2.0, 3.0 /)
  a = (/ 1.5, 2.5, 3.5 /)
  print *, a
  a = (/ (real(i) * 2.0, i = 1, 3) /)
  print *, a
  a2 = reshape((/ 1.0, 2.0, 3.0, 4.0 /), (/ 2, 2 /))
  print *, a2
  g2 = a2
  print *, g2
  g2(:, 1) = a2(1, :)
  print *, g2
  a = f + 1.0
  g = a
  print *, g
  g = (/ v2(1.0, 2.0), v2(3.0, 4.0), v2(5.0, 6.0) /)
  print *, g
  do i = 1, 3
    c(i)%n = i
    p(i)%n = i
  end do
  d = c
  print *, d
  d = (/ cnt(7), cnt(8), cnt(9) /)
  print *, d
  d = cnt(4)
  print *, d
  q = p
  print *, q
  q(1) = p(1)
  print *, q
  q = plain(3)
  print *, q
  bx%c = c
  print *, bx%c
  bx%c(2:3) = c(1:2)
  print *, bx%c
  s = c
  print *, s
  t = 'abcdefgh'
  t(:)(3:5) = c
  print *, t
  s(2:3) = c(1:2)
  print *, s
  k = c
  print *, k
  k(3:1:-1) = c
  print *, k
  where (f > 1.5) g = a
  print *, g
  where (f > 1.5)
    d = c
  elsewhere
    d = cnt(-1)
  end where
  print *, d
end program
