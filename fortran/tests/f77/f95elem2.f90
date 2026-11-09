elemental function ext(a, b)
  integer, intent(in) :: a
  integer, intent(in), optional :: b
  integer :: ext
  if (present(b)) then
    ext = a * 10 + b
  else
    ext = a * 10
  end if
end function
elemental subroutine neg(a)
  integer, intent(inout) :: a
  a = -a
end subroutine
subroutine work(a, b, n)
  interface
    elemental function ext(a, b)
      integer, intent(in) :: a
      integer, intent(in), optional :: b
      integer :: ext
    end function
    elemental subroutine neg(a)
      integer, intent(inout) :: a
    end subroutine
  end interface
  integer :: n
  integer :: a(:), b(n)
  print *, ext(a), ext(b, a(1:n))
  print *, ext(b=a, a=a)
  call neg(a)
  call neg(b(2:n))
  print *, a, b
end subroutine
program e2
  implicit none
  interface
    elemental function ext(a, b)
      integer, intent(in) :: a
      integer, intent(in), optional :: b
      integer :: ext
    end function
    elemental subroutine neg(a)
      integer, intent(inout) :: a
    end subroutine
    subroutine work(a, b, n)
      integer :: n
      integer :: a(:), b(n)
    end subroutine
  end interface
  type t
    integer :: v(3)
    integer :: s
  end type
  type(t) :: a, bb(2)
  integer, allocatable :: al(:)
  integer, pointer :: pp(:)
  integer, target :: tg(6)
  integer :: e(0), i, z(3), fr(3), to(3)
  allocate(al(3))
  al = (/ 1, 2, 3 /)
  tg = (/ 1, 2, 3, 4, 5, 6 /)
  pp => tg(2:6:2)
  print *, ext(al), ext(pp), ext(al, pp)
  call neg(al)
  call neg(pp)
  print *, al, tg
  call work(al, tg(1:3), 3)
  print *, al, tg
  a%v = (/ 5, 6, 7 /)
  a%s = 9
  print *, ext(a%v), ext(a%v, a%s)
  call neg(a%v)
  print *, a%v
  bb(1)%s = 1; bb(2)%s = 2
  bb(1)%v = 0; bb(2)%v = 1
  print *, ext(bb%s)
  call neg(bb%s)
  print *, bb%s
  call neg(bb(2)%v)
  print *, bb(2)%v
  print *, 'zero:', ext(e)
  call neg(e)
  print *, (/ ext(al), 77 /)
  print *, (ext(al(i)), i = 1, 3)
  print *, (/ (ext(al) + i, i = 1, 2) /)
  z = (/ (ext(i), i = 1, 3) /)
  print *, z
  print *, inner(z), inner(z + 1)
  call isub(z)
  print *, z
  print *, shape(ext(tg(1:4))), maxloc(ext(al))
  fr = (/ 255, 15, 3 /)
  to = 0
  call mvbits(fr, 0, 4, to, 4)
  print *, to
  call mvbits(fr(1), 0, (/ 1, 2, 3 /), to, 0)
  print *, to
  call mvbits(7, 0, 3, i, 0)
  print *, iand(i, 7)
contains
  elemental function inner(q) result(r)
    integer, intent(in) :: q
    integer :: r
    r = q + 1
  end function
  elemental subroutine isub(q)
    integer, intent(inout) :: q
    q = q * 2
  end subroutine
end program
