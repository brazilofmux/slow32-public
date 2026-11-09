module cplxm
  implicit none
  type zz
    double precision :: re, im
  end type
  interface operator(*)
    module procedure zmul
  end interface
  interface operator(+)
    module procedure zadd
  end interface
  interface operator(**)
    module procedure zpow
  end interface
  interface operator(.conj.)
    module procedure zconj
  end interface
contains
  function zmul(a, b)
    type(zz), intent(in) :: a, b
    type(zz) :: zmul
    zmul = zz(a%re*b%re - a%im*b%im, a%re*b%im + a%im*b%re)
  end function
  function zadd(a, b)
    type(zz), intent(in) :: a, b
    type(zz) :: zadd
    zadd = zz(a%re + b%re, a%im + b%im)
  end function
  recursive function zpow(a, n) result(r)
    type(zz), intent(in) :: a
    integer, intent(in) :: n
    type(zz) :: r
    if (n == 0) then
      r = zz(1d0, 0d0)
    else
      r = a * a ** (n - 1)         ! uses its own module's operators
    end if
  end function
  function zconj(a)
    type(zz), intent(in) :: a
    type(zz) :: zconj
    zconj = zz(a%re, -a%im)
  end function
end module

program t
  use cplxm
  implicit none
  type(zz) :: a, b
  interface operator(.dist.)
    double precision function dist(p, q)
      use cplxm
      type(zz), intent(in) :: p, q
    end function
  end interface
  interface operator(.twice.)
    function twice(s)
      character(len=*), intent(in) :: s
      character(len=8) :: twice
    end function
  end interface
  a = zz(1d0, 2d0)
  b = a ** 3 + .conj. a
  print '(2F10.3)', b%re, b%im
  print '(F10.4)', a .dist. b
  print *, '[', .twice. 'ab', ']'
  call inner
contains
  subroutine inner
    type(zz) :: c
    c = a * .conj. a
    print '(2F10.3)', c%re, c%im
    print '(F10.4)', (c .dist. a) + 1d0
  end subroutine
end program

double precision function dist(p, q)
  use cplxm
  type(zz), intent(in) :: p, q
  dist = sqrt((p%re - q%re)**2 + (p%im - q%im)**2)
end function

function twice(s)
  character(len=*), intent(in) :: s
  character(len=8) :: twice
  twice = s // s
end function
