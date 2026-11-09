module frac
  implicit none
  type rat
    integer :: n, d
  end type
  integer :: ncopies = 0
  interface assignment(=)
    module procedure rat_from_int, real_from_rat, rat_copy, str_from_rat
  end interface
  interface operator(+)
    module procedure radd, radd_i
  end interface
  interface operator(<)
    module procedure rlt
  end interface
  interface operator(.inv.)
    module procedure rinv
  end interface
contains
  subroutine rat_from_int(r, i)
    type(rat), intent(out) :: r
    integer, intent(in) :: i
    r%n = i; r%d = 1
  end subroutine
  subroutine real_from_rat(x, r)
    real, intent(out) :: x
    type(rat), intent(in) :: r
    x = real(r%n) / real(r%d)
  end subroutine
  subroutine rat_copy(a, b)
    type(rat), intent(out) :: a
    type(rat), intent(in) :: b
    integer :: g, p, q, t
    p = abs(b%n); q = abs(b%d)
    do while (q /= 0)
      t = mod(p, q); p = q; q = t
    end do
    g = p
    if (g == 0) g = 1
    a%n = b%n / g; a%d = b%d / g
    ncopies = ncopies + 1
  end subroutine
  subroutine str_from_rat(s, r)
    character(len=*), intent(out) :: s
    type(rat), intent(in) :: r
    write (s, '(I0,A,I0)') r%n, '/', r%d
  end subroutine
  function radd(a, b)
    type(rat), intent(in) :: a, b
    type(rat) :: radd
    radd%n = a%n*b%d + b%n*a%d
    radd%d = a%d*b%d
  end function
  function radd_i(a, i)
    type(rat), intent(in) :: a
    integer, intent(in) :: i
    type(rat) :: radd_i
    radd_i%n = a%n + i*a%d
    radd_i%d = a%d
  end function
  logical function rlt(a, b)
    type(rat), intent(in) :: a, b
    rlt = a%n*b%d < b%n*a%d
  end function
  function rinv(a)
    type(rat), intent(in) :: a
    type(rat) :: rinv
    rinv = rat(a%d, a%n)
  end function
end module

program t
  use frac
  implicit none
  type(rat) :: a, b, c
  real :: x
  character(len=12) :: s
  integer :: i
  a = 3
  b = rat(1, 6)
  c = a + b + b
  print *, c%n, c%d, ncopies
  x = c
  print *, x
  s = c
  print *, '[', s, ']'
  c = c + 2
  s = c
  print *, s
  print *, b < c, c < b
  c = .inv. c + 1
  s = c
  print *, s, ncopies
  do i = 1, 3
    a = a + rat(1, i*2)
  end do
  s = a
  print *, s
end program
