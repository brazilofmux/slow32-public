program dtuse
   use geom
   use shapes2
   implicit none
   type(box) :: b
   type(pt) :: p
   b = box(pt(0.0, 0.0), pt(3.0, 2.0), 'abc')
   p = b%hi
   print *, area(b), dist2(b%lo, p), b%label, b
end program
