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
