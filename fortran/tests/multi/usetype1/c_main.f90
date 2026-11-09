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
