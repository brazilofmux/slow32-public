program dtuse
   use geom
   use shapes2
   implicit none
   type(box) :: b
   type(pt) :: p
   b = box(pt(0.0, 0.0), pt(3.0, 2.0), 'abc')
   p = b%hi
   print *, area(b), dist2(b%lo, p), b%label, b
   p = midpt(b%lo, b%hi)
   print *, p, midpt(p, pt(1.0, 1.0))
end program
