! A renamed type through a module that re-exports it under the new name,
! renamed again by the program: operators, constructors, arrays, an
! internal procedure, components of the renamed type.
module geom
  implicit none
  type pt
    integer :: x, y
  end type pt
  type seg
    type(pt) :: a, b
  end type seg
  interface operator(+)
    module procedure addpt
  end interface
contains
  function addpt(p, q) result(r)
    type(pt), intent(in) :: p, q
    type(pt) :: r
    r%x = p%x + q%x; r%y = p%y + q%y
  end function
  function mid(s) result(r)
    type(seg), intent(in) :: s
    type(pt) :: r
    r = pt((s%a%x + s%b%x) / 2, (s%a%y + s%b%y) / 2)
  end function
  subroutine show(p)
    type(pt), intent(in) :: p
    print *, 'pt', p%x, p%y
  end subroutine
end module geom
module shapes
  use geom, point => pt
  implicit none
  type box
    type(point) :: lo, hi
  end type
contains
  function corner(b) result(p)
    type(box), intent(in) :: b
    type(point) :: p
    p = b%lo + b%hi
  end function
  function diag(b) result(s)
    type(box), intent(in) :: b
    type(seg) :: s
    s = seg(b%lo, b%hi)
  end function
end module shapes
program p3b
  use shapes, vec => point, line => seg
  use geom, only: pt, show, operator(+)
  implicit none
  type(vec) :: a, c(3)
  type(pt) :: b
  type(line) :: l
  type(box) :: bx
  integer :: i
  a = vec(1, 2)
  b = pt(10, 20)
  a = a + b
  call show(a)
  b = a
  call show(b)
  bx = box(a, vec(3, 4))
  call show(corner(bx))
  l = diag(bx)
  call show(mid(l))
  c = (/ (vec(i, -i), i = 1, 3) /)
  print *, c
  c(2) = l%b
  print *, c(2)%x, c(2)%y, l
  call inner(c(3))
contains
  subroutine inner(q)
    type(vec) :: q
    type(pt) :: r
    r = q
    call show(r + q)
  end subroutine
end program p3b
